package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io._
import java.util.{UUID, Date}
import de.choffmeister.securestring.SecureString
import de.choffmeister.secpwd.utils.SftpClientSpec
import de.choffmeister.secpwd.utils.NullCommandLineInterface

@RunWith(classOf[JUnitRunner])
class SyncSpec extends Specification {
  def tmp = new File(System.getProperty("java.io.tmpdir"), UUID.randomUUID.toString)

  "sync" in {
    val localDir = tmp
    val remoteConnInfo = SftpClientSpec.testConnInfo
    val remoteConnInfo2 = SftpClientSpec.testConnInfo2
    val remoteDir = "/tmp/" + UUID.randomUUID().toString
    val pp = SecureString("password".toCharArray)
    val main = new Main(localDir, new NullCommandLineInterface())
    main.init(pp)
    val db1 = main.current(pp)

    Sync.synchronize(main, pp, remoteConnInfo, remoteDir) === db1
    main.current(pp) === db1

    Sync.synchronize(main, pp, remoteConnInfo, remoteDir) === db1
    main.current(pp) === db1

    val pwd1 = main.add(pp, "gmail", Left(SecureString("pass".toCharArray)), "Google Mail", "", "user", "http://www.googlemail.com", Map.empty)
    val db2 = main.current(pp)

    val pwd2 = main.add(pp, "gmail2", Left(SecureString("pass2".toCharArray)), "Google Mail 2", "", "user2", "http://www.googlemail2.com", Map.empty)
    val db3 = main.current(pp)

    Sync.synchronize(main, pp, remoteConnInfo, remoteDir) === db3
    main.current(pp) === db3

    db3.currentPasswords === List(pwd2, pwd1)
    db3.passwords === List(pwd2, pwd1)

    main.setHead(db1.id)

    Sync.synchronize(main, pp, remoteConnInfo, remoteDir) === db3
    main.current(pp) === db3

    main.setHead(db2.id)
    val pwd3 = main.add(pp, "gmail3", Left(SecureString("pass3".toCharArray)), "Google Mail 3", "", "user3", "http://www.googlemail3.com", Map.empty)
    val db4 = main.current(pp)

    db4.currentPasswords === List(pwd3, pwd1)
    db4.passwords === List(pwd3, pwd1)

    val synced = Sync.synchronize(main, pp, remoteConnInfo, remoteDir)

    synced !== db1
    synced !== db2
    synced !== db3
    synced !== db4
    synced === main.current(pp)

    // TODO check why order in currentPasswords and passwords is different
    synced.currentPasswords === List(pwd2, pwd3, pwd1)
    synced.passwords === List(pwd3, pwd2, pwd1)

    Sync.synchronize(main, pp, remoteConnInfo2, remoteDir) === synced
    synced === main.current(pp)

    ok
  }
}