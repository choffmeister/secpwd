# secpwd [![Build Status](https://travis-ci.org/choffmeister/secpwd.png?branch=master)](https://travis-ci.org/choffmeister/secpwd)

_secpwd_ provides a highly secure way to generate and store strong passwords for all your accounts. Unlike other tools _secpwd_ is a purely commandline base application.



#Usage

The following shows a common workflow of creating a new database store, add a password with a self choosen password and afterwards update the password with a generated one. Note, that double curlies like `{{my-passphrase}}` stand for silently entered content.

```bash
$ secpwd init
Passphrase: {{my-passphrase}}
Repeat: {{my-passphrase}}
[success] Created new password store
```

```bash
$ secpwd list
Passphrase: {{my-passphrase}}
```

```bash
$ secpwd add gmail
Passphrase: {{my-passphrase}}
Name [gmail]: Google Mail
Description:
URL: https://googlemail.com/
Username: invalid.user@googlemail.com
Password: {{my-gmail-pass}}
Repeat: {{my-gmail-pass}}
[success] Added password
```

```bash
$ secpwd list
Passphrase: {{my-passphrase}}
[gmail] Google Mail (73 bits) Sat Nov 30 15:33:14 CET 2013
```

```bash
$ secpwd show gmail
Passphrase: {{my-passphrase}}
[gmail] Password information
  Timestamp: Sat Nov 30 15:33:14 CET 2013
  Name: Google Mail
  Username: invalid.user@googlemail.com
  URL: https://googlemail.com/
  Strength: 73 bits
  Password: ***
```

```bash
$ secpwd renew gmail
Passphrase: {{my-passphrase}}
Password:
Password length [32]: 24
Use lower alpha characters? [true]:
Use upper alpha characters? [true]:
Use number characters? [true]:
Use special characters? [true]: false
[success] Updated password
```

```bash
$ secpwd show gmail
Passphrase: {{my-passphrase}}
[gmail] Password information
  Timestamp: Sat Nov 30 15:33:14 CET 2013
  Name: Google Mail
  Username: invalid.user@googlemail.com
  URL: https://googlemail.com/
  Strength: 143 bits
  Password: ***
```

```bash
$ secpwd show -p gmail
Passphrase: {{pp}}
[gmail] Password information
  Timestamp: Sat Nov 30 15:40:01 CET 2013
  Name: Google Mail
  Username: invalid.user@googlemail.com
  URL: https://googlemail.com/
  Strength: 143 bits
  Password: 9BN5zsRrwHnzEYKp0USD19MB
```



# Caution

_secpwd_ is still alpha and not yet ready for production use. Feel free to try it out and contribute, but make sure that you do __not__ store important passwords solely in _secpwd_ by now.



# Security

All your passwords are stored in one binary file. To secure the content of your password store you use a single master passphrase. _secpwd_ uses the following technique to deduce security from your passphrase (this steps are executed every time you change something in your store like adding or removing a password):

* You choose a master passphrase (_pp_)
* Generate random salt for AES key (_s1_)
* Generate random salt for HMAC key (_s2_)
* Generate random initial vector for AES (_iv_)
* Derive AES key (_k1_) from _pp_ and _s1_ with PBKDF2 like specified in RFC-2898
* Derive HMAC key (_k2_) from _pp_ and _s2_ with PBKDF2 like specified in RFC-2898
* Encrypt your whole store with AES-128 using _k1_ as key and _iv_ as initial vector
* Sign the encrypted store with HMAC-SHA-512 using _k2_ as key



# Requirements

_secpwd_ is developed in Scala 2.10.3 and therefore the only thing you need to run _secpwd_ is one of the available Java Runtime Environments (JRE) in version 6 or higher.



# License

_secpwd_ is licensed under the Apache 2.0 license. For further information please refer to the LICENSE file in this repository.



# References
* [RFC-2898](http://www.ietf.org/rfc/rfc2898.txt)
* [Scala 2.10.3](http://www.scala-lang.org/)
* [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html)

