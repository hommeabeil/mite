mite
=====

An OTP application

Build
-----

    $ rebar3 compile
    
Install Certificate
-------------------

```sh
mkdir /usr/share/ca-certificates/mite
chmod 755 /usr/share/ca-certificates/mite
cp apps/mite/priv/cert.pem /usr/share/ca-certificates/mite/mite.crt
chmod 644 /usr/share/ca-certificates/mite/mite.crt
update-ca-certificates
```

Modify the host file
--------------------
Add a line to your hosts file
```sh
echo '127.0.2.1 your.target.proxy' >> /etc/hosts
```
