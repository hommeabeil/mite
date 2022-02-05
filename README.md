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

Configuration
----------

2 ways exist to configure this proxy, one is via the `config/sys.config` file.
This is the normal erlang config file.

The more user friendly way to configure it, is via the environment variable
`MITE_CONNECTIONS`. The value is comma separate list of connections, which are
in the same format as the local foward option (`-L`) for SSH. Don't panic, here
are some examples:

```sh
# The full connection string, the proxy will listen on 127.0.0.1:8080 and will connect to www.site.com:443
MITE_CONNECTIONS="127.0.0.1:8080:www.site.com:443"

# The host section can be omited, the proxy will listen on 0.0.0.0:8080 and will connect to www.site.com:443
MITE_CONNECTIONS="8080:www.site.com:443"

# Multiple connections can be configured
MITE_CONNECTIONS="8080:www.site.com:443,8081:www.site2.com:443"
```

For now it is not possible to specify ipv6 in the environmen connection string,
this will implemented later.
    
    
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

Run in Docker
-------

It is also possbile to run this project inside a Docker container. Simply build
the `Dockerfile` and run it. Do not forget to specify your connections and to
publish your ports !!

```sh
docker build --file Dockerfile --tag mite .
docker run --name mite --rm --publish 8080:8080 --env "MITE_CONNECTIONS=8080:www.site.com:443" mite

# To stop it, ctrl-c will not work
docker stop mite
```
