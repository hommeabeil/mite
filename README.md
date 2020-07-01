mite
=====

An Erlang application which proxy raw local TLS connection to a remote server.
The main purpose is to provide an simple proxy for debug purpose. The proxy
might not support all the TLS options right now, but its configuration is has
simple as specifying 2 ports and an IP.

Build
-----

```sh
rebar3 compile
```
    
Run
---
```sh
rebar3 shell
```
    
    
Generate Certificate
--------------------

If you need to generate a nwe certificate, you can run the following commands.
The proxy already come with a self signed certificate (already expired), with
the private key already **publicly** commited. I assume that you already
understand what you are trying to do with this proxy, so it should not be a
problem to use it.

If you want to generate a new certificate you can run the following commands:

```sh
cd apps/mite/priv
openssl req -x509 -new -keyout key.pem -out cert.pem -extensions req_ext -config mite.conf -nodes
```

Install Certificate
-------------------

On Linux you can run the followings commands. This is needed only if you cannot
tell your client to ignore the certificate validation (curl -k option).

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
