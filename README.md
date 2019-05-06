mite
=====

An OTP application

Build
-----

    $ rebar3 compile
    
    
    
Generate Certificate
--------------------

```sh
openssl req -x509 -new -key key.pem -out cert.pem -extensions req_ext -config mite.conf
```

Install Certificate
-------------------

```sh
mkdir /usr/local/share/ca-certificates/mite
chmod 755 /usr/local/share/ca-certificates/mite
cp apps/mite/priv/cert.pem /usr/local/share/ca-certificates/mite/mite.crt
chmod 644 /usr/local/share/ca-certificates/mite/mite.crt
update-ca-certificates -f
```

Modify the host file
--------------------
Add a line to your hosts file
```sh
echo '127.0.2.1 your.target.proxy' >> /etc/hosts
```
