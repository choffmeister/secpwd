package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.io.{File, FileInputStream, FileOutputStream}
import de.choffmeister.secpwd.utils.BinaryReaderWriter._
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.RichStream._
import de.choffmeister.secpwd.security.Encryptor._
import de.choffmeister.secpwd.security.RandomGenerator._
import de.choffmeister.securestring.SecureString

abstract class BaseEntry {
  val id: UUID
}

case class Database(
  id: UUID,
  timeStamp: Date,
  versions: List[DatabaseVersion],
  passwords: List[PasswordEntry] = Nil
) extends BaseEntry {
  def passwordById(id: UUID): Option[PasswordEntry] =
    passwords.find(_.id == id)

  def currentPasswordByKey(key: String): Option[PasswordEntry] =
    versions(0).passwordIds.map(passwordById(_).get).find(_.key == key)

  def currentPasswords: List[PasswordEntry] =
    versions(0).passwordIds.map(passwordById(_).get)

  def passwords(version: DatabaseVersion): List[PasswordEntry] = 
    version.passwordIds.map(passwordById(_).get)    
}

case class DatabaseVersion(
  versionId: UUID,
  timeStamp: Date,
  depth: Int,
  parentVersionIds: List[UUID] = Nil,
  passwordIds: List[UUID] = Nil
)

case class CustomEntry(
  id: UUID,
  key: String,
  value: String
) extends BaseEntry

case class PasswordEntry(
  id: UUID,
  timeStamp: Date,
  key: String,
  name: String,
  password: SecureString,
  userName: String = "",
  description: String = "",
  customFields: List[CustomEntry] = Nil
) extends BaseEntry

case class DatabaseCryptoInfo(deriveIterations: Int, macSalt: Array[Byte], encSalt: Array[Byte], iv: Array[Byte])

class DatabaseSerializationException(message: String) extends Exception(message)

object Database {
  val MAGIC_BYTES = Array[Byte](115, 99, 112, 100)
  val VERSION = 1.toByte

  def create(): Database = {
    val id = UUID.randomUUID()
    val now = new Date()
    Database(id, now, List(DatabaseVersion(id, now, 0)))
  }

  def addPassword(db: Database, password: PasswordEntry): Database =
    db.currentPasswordByKey(password.key) match {
      case Some(pwd) => throw new Exception(s"Password with key ${password.key} already exists")
      case _ => alter(db, password.id :: db.versions(0).passwordIds, password :: db.passwords)
    }

  def removePasswordById(db: Database, id: UUID): Database =
    db.passwordById(id) match {
      case Some(pwd) => alter(db, db.versions(0).passwordIds.filter(_ != id), db.passwords)
      case _ => throw new Exception(s"Password with id ${id} does not exist")
    }

  def removePasswordByKey(db: Database, key: String): Database =
    db.currentPasswordByKey(key) match {
      case Some(pwd) => removePasswordById(db, pwd.id)
      case _ => throw new Exception(s"Password with key ${key} does not exist")
    }

  def updatePassword(db: Database, password: PasswordEntry): Database =
    db.currentPasswordByKey(password.key) match {
      case Some(pwd) => 
        if (db.passwordById(password.id).isDefined) throw new Exception(s"Cannot update a password with a password of same id")
        alter(db, password.id :: db.versions(0).passwordIds.filter(_ != pwd.id), password :: db.passwords)
      case _ => throw new Exception(s"Password with key ${password.key} does not exist")
    }

  def updatePassword(db: Database, key: String, password: SecureString): Database =
    db.currentPasswordByKey(key) match {
      case Some(pwd) => updatePassword(db, pwd.copy(id = UUID.randomUUID(), password = password))
      case _ => throw new Exception(s"Password with key ${key} does not exist")
    }

  def diff(db: Database, v1: DatabaseVersion, v2: DatabaseVersion): Map[String, (Option[PasswordEntry], Option[PasswordEntry])] = {
    val pwds1 = db.passwords(v1)
    val pwds2 = db.passwords(v2)

    val removedKeys = pwds1.map(_.key).diff(pwds2.map(_.key))
    val addedKeys = pwds2.map(_.key).diff(pwds1.map(_.key))
    val remainingKeys = pwds1.map(_.key).intersect(pwds2.map(_.key))

    val removed = removedKeys.map(k => pwds1.find(_.key == k).get)
      .map(pwd => (pwd.key, (Some(pwd), None))).toMap
    val added = addedKeys.map(k => pwds2.find(_.key == k).get)
      .map(pwd => (pwd.key, (None, Some(pwd)))).toMap
    val changed = remainingKeys.map(k => (pwds1.find(_.key == k). get, pwds2.find(_.key == k).get))
      .filter(pwds => pwds._1 != pwds._2)
      .map(pwds => (pwds._1.key, (Some(pwds._1), Some(pwds._2))))

    removed ++ added ++ changed
  }

  def merge(db1: Database, v1: DatabaseVersion, db2: Database, v2: DatabaseVersion): Database = {
    @scala.annotation.tailrec
    def replay(keys: List[String], diff1: Map[String, (Option[PasswordEntry], Option[PasswordEntry])],
        diff2: Map[String, (Option[PasswordEntry], Option[PasswordEntry])], passwordIds: List[UUID],
        passwords: List[PasswordEntry]): (List[UUID], List[PasswordEntry]) =
      keys match {
        case key :: rest =>
          (diff1.get(key), diff2.get(key)) match {
            case (Some(c1), None) =>
              c1 match {
                case (Some(a), None) => replay(rest, diff1, diff2, passwordIds.filter(_ != a.id), passwords)
                case (None, Some(b)) => replay(rest, diff1, diff2, b.id :: passwordIds, passwords)
                case (Some(a), Some(b)) => replay(rest, diff1, diff2, b.id :: passwordIds.filter(_ != a.id), passwords)
                case (None, None) => throw new Exception("Impossible case")
              }
            case (None, Some(c2)) =>
              c2 match {
                case (Some(a), None) => replay(rest, diff1, diff2, passwordIds.filter(_ != a.id), passwords)
                case (None, Some(b)) => replay(rest, diff1, diff2, b.id :: passwordIds, passwords)
                case (Some(a), Some(b)) => replay(rest, diff1, diff2, b.id :: passwordIds.filter(_ != a.id), passwords)
                case (None, None) => throw new Exception("Impossible case")
              }
            case (Some(c1), Some(c2)) =>
              (c1, c2) match {
                // TODO supply conflict resolution strategy
                case _ => throw new Exception("Merge conflict")
              }
            case (None, None) => throw new Exception("Impossible case")
          }
        case _ => (passwordIds, passwords)
      }

    val lca = lowestCommonAncestor(db1, v1, db2, v2)
    if (lca == v1) return Database.fromVersion(db2, v2)
    if (lca == v2) return Database.fromVersion(db1, v1)

    val diff1 = Database.diff(db1, lca, v1)
    val diff2 = Database.diff(db2, lca, v2)
    val keys = diff1.map(_._1).toList.union(diff2.map(_._1).toList).distinct
    val versions1 = gatherVersions(db1, v1).toList
    val versions2 = gatherVersions(db2, v2).toList
    val pwds1 = db1.passwords.filter(pwd => versions1.flatMap(_.passwordIds).contains(pwd.id))
    val pwds2 = db2.passwords.filter(pwd => versions2.flatMap(_.passwordIds).contains(pwd.id))
    val pwdsCommon = intersect(pwds1, pwds2)
    val pwds1Only = diff(pwds1, pwds2)
    val pwds2Only = diff(pwds2, pwds1)

    val id = UUID.randomUUID()
    val timeStamp = new Date()
    val passwords = replay(keys, diff1, diff2, lca.passwordIds, pwds1Only ::: pwds2Only ::: pwdsCommon)
    val priorVersions = (versions1 ::: versions2).distinct.sortWith(_.depth > _.depth)
    val depth = Math.max(v1.depth, v2.depth) + 1
    val version = DatabaseVersion(id, timeStamp, depth, List(v1.versionId, v2.versionId), passwords._1)
    Database(id, timeStamp, version :: priorVersions, passwords._2)
  }

  private def gatherVersions(db: Database, v: DatabaseVersion): List[DatabaseVersion] =
    v :: v.parentVersionIds.map(pid => db.versions.find(_.versionId == pid).get).flatMap(gatherVersions(db, _)).sortWith(_.depth > _.depth).distinct

  def lowestCommonAncestor(db1: Database, v1: DatabaseVersion, db2: Database, v2: DatabaseVersion): DatabaseVersion = {
    @scala.annotation.tailrec
    def recursion(l1: List[DatabaseVersion], l2: List[DatabaseVersion]): DatabaseVersion =
      l1.intersect(l2) match {
        case lca :: rest :: Nil => throw new Exception("Lowest common ancestor must be unique")
        case lca :: Nil => lca
        case _ =>
          if (l1.isEmpty || l2.isEmpty) throw new Exception("Cannot find a common ancestor")
          val md1 = l1.map(_.depth).max
          val md2 = l2.map(_.depth).max
          var nl1 = l1
          var nl2 = l2
          
          if (md1 >= md2) {
            val h = nl1.head
            nl1 = nl1.tail
            for (p <- h.parentVersionIds.map(pid => db1.versions.find(_.versionId == pid).get))
              nl1 = sortedInsert[DatabaseVersion](nl1, p, (a, b) => a.depth >= b.depth)
          } else {
            val h = nl2.head
            nl2 = nl2.tail
            for (p <- h.parentVersionIds.map(pid => db2.versions.find(_.versionId == pid).get))
              nl2 = sortedInsert[DatabaseVersion](nl2, p, (a, b) => a.depth >= b.depth)
          }
          recursion(nl1, nl2)
      }

    recursion(List(v1), List(v2))
  }

  private def alter(db: Database, passwordId: List[UUID], passwords: List[PasswordEntry]): Database = {
    val id = UUID.randomUUID()
    val now = new Date()
    db.copy(id = id, timeStamp = now, versions = DatabaseVersion(id, now, db.versions(0).depth + 1, List(db.id), passwordId) :: db.versions, passwords = passwords)
  }

  def fromVersion(db: Database, v: DatabaseVersion): Database = {
    val versions = gatherVersions(db, v)

    Database(
      v.versionId,
      v.timeStamp,
      versions,
      db.passwords.filter(pwd => versions.flatMap(_.passwordIds).contains(pwd.id))
    )
  }

  def serializeDatabase(db: Database): Array[Byte] = {
    val tmp = new ByteArrayOutputStream()
    serializeDatabase(tmp, db)
    tmp.toByteArray
  }

  def serializeDatabase(output: OutputStream, db: Database): Unit = {
    output.writeUUID(db.id)
    output.writeDate(db.timeStamp)
    output.writeInt32(db.versions.length)
    db.versions.foreach(serializeDatabaseVersion(output, _))
    output.writeInt32(db.passwords.length)
    db.passwords.foreach(serializePasswordEnty(output, _))
  }

  def serializeDatabaseVersion(output: OutputStream, version: DatabaseVersion): Unit = {
    output.writeUUID(version.versionId)
    output.writeDate(version.timeStamp)
    output.writeInt32(version.depth)
    output.writeInt32(version.parentVersionIds.length)
    version.parentVersionIds.foreach(output.writeUUID(_))
    output.writeInt32(version.passwordIds.length)
    version.passwordIds.foreach(output.writeUUID(_))
  }
  
  def serializePasswordEnty(output: OutputStream, pwd: PasswordEntry): Unit = {
    output.writeUUID(pwd.id)
    output.writeDate(pwd.timeStamp)
    output.writeString(pwd.key)
    output.writeString(pwd.name)
    output.writeSecureString(pwd.password)
    output.writeString(pwd.userName)
    output.writeString(pwd.description)
    output.writeInt32(pwd.customFields.length)
    pwd.customFields.foreach(serializeCustomEntry(output, _))
  }

  def serializeCustomEntry(output: OutputStream, cf: CustomEntry): Unit = {
    output.writeUUID(cf.id)
    output.writeString(cf.key)
    output.writeString(cf.value)
  }

  def deserializeDatabase(bytes: Array[Byte]): Database = {
    val tmp = new ByteArrayInputStream(bytes)
    deserializeDatabase(tmp)
  }
  
  def deserializeDatabase(input: InputStream): Database = {
    Database(
      input.readUUID(),
      input.readDate(),
      (1 to input.readInt32()).map(i => deserializeDatabaseVersion(input)).toList,
      (1 to input.readInt32()).map(i => deserializePasswordEntry(input)).toList
    )
  }

  def deserializeDatabaseVersion(input: InputStream): DatabaseVersion = {
    DatabaseVersion(
      input.readUUID(),
      input.readDate(),
      input.readInt32(),
      (1 to input.readInt32()).map(i => input.readUUID()).toList,
      (1 to input.readInt32()).map(i => input.readUUID()).toList
    )
  }

  def deserializePasswordEntry(input: InputStream): PasswordEntry = {
    PasswordEntry(
      input.readUUID(),
      input.readDate(),
      input.readString(),
      input.readString(),
      input.readSecureString(),
      input.readString(),
      input.readString(),
      (1 to input.readInt32()).map(i => deserializeCustomEntry(input)).toList
    )
  }

  def deserializeCustomEntry(input: InputStream): CustomEntry = {
    CustomEntry(
      input.readUUID(),
      input.readString(),
      input.readString()
    )
  }

  def serialize(passphrase: SecureString, db: Database): Array[Byte] = {
    val cryptinfo = DatabaseCryptoInfo(1024 * 16, generateRandomOctets(128), generateRandomOctets(128), generateRandomOctets(16))
    val bs = new ByteArrayOutputStream()

    bs.writeBytesRaw(MAGIC_BYTES)
    bs.writeInt8(VERSION)

    writeBlock(bs) { ms =>
      ms.writeInt32(cryptinfo.deriveIterations)
      ms.writeBinary(cryptinfo.macSalt)
      ms.writeBinary(cryptinfo.encSalt)
      ms.writeBinary(cryptinfo.iv)
    }

    writeBlock(bs) { ms =>
      ms.writeBinary(encryptAes128(serializeDatabase(db), passphrase, cryptinfo.deriveIterations, cryptinfo.encSalt, cryptinfo.iv))
    }

    bs.writeBytesRaw(hmacSha512(bs.toByteArray, passphrase, cryptinfo.deriveIterations, cryptinfo.macSalt))
    bs.toByteArray
  }

  def deserialize(passphrase: SecureString, bytes: Array[Byte]): Database = {
    val bs = new ByteArrayInputStream(bytes)

    val magicbytes = bs.readBytesRaw(4)
    if (!compareByteArrays(magicbytes, MAGIC_BYTES)) throw new DatabaseSerializationException("Invalid magic bytes")
    val version = bs.readInt8()
    if (version != VERSION) throw new DatabaseSerializationException("Invalid version")

    val cryptoinfo = readBlock(bs) { ms =>
      DatabaseCryptoInfo(
        ms.readInt32(),
        ms.readBinary(),
        ms.readBinary(),
        ms.readBinary()
      )
    }

    val signature = bytes.drop(bytes.length - 64)
    val calcSignature = hmacSha512(bytes.take(bytes.length - 64), passphrase, cryptoinfo.deriveIterations, cryptoinfo.macSalt)
    if (!compareByteArrays(calcSignature, signature)) throw new DatabaseSerializationException("Invalid passphrase")

    val encrypted = readBlock(bs) { ms =>
      ms.readBinary()
    }

    deserializeDatabase(decryptAes128(encrypted, passphrase, cryptoinfo.deriveIterations, cryptoinfo.encSalt, cryptoinfo.iv))
  }

  def writeBlock(output: OutputStream)(inner: OutputStream => Any): Unit = {
    output.cached(cs => output.writeInt32(cs.size)) { cs =>
      inner(cs)
    }
  }

  def readBlock[T](stream: InputStream)(inner: InputStream => T): T = {
    var result: Option[T] = None
    stream.preSizedInner(stream.readInt32()) { is =>
      result = Some(inner(is))
    }
    return result.get
  }

  private def compareByteArrays(arr1: Array[Byte], arr2: Array[Byte]): Boolean = {
    compareByteArrayChunks(arr1, 0, arr2, 0, Math.max(arr1.length, arr2.length))
  }

  private def compareByteArrayChunks(arr1: Array[Byte], off1: Int, arr2: Array[Byte], off2: Int, len: Int): Boolean = {
    if (len == 0) true
    else if (arr1.length <= off1 || arr2.length <= off2) false
    else if (arr1(off1) != arr2(off2)) false
    else compareByteArrayChunks(arr1, off1 + 1, arr2, off2 + 1, len - 1)
  }

  private def intersect[T](l1: List[T], l2: List[T]): List[T] = l1 match {
    case first :: rest =>
      if (l2.contains(first)) first :: intersect(rest, l2)
      else intersect(rest, l2)
    case _ => Nil
  }

  private def diff[T](l1: List[T], l2: List[T]): List[T] = l1 match {
    case first :: rest =>
      if (!l2.contains(first)) first :: diff(rest, l2)
      else diff(rest, l2)
    case _ => Nil
  }

  private def sortedInsert[T](l: List[T], it: T, order: (T, T) => Boolean): List[T] = l match {
    case Nil => List(it)
    case first :: rest =>
      if (order(it, first)) it :: l
      else first :: sortedInsert(rest, it, order)
  }
}