FROM erlang:23

COPY . /opt/mite

WORKDIR /opt/mite

RUN rebar3 as prod tar \
    && mkdir -p /usr/local/lib/mite \
    && tar -xzf _build/prod/rel/mite/mite-0.1.0.tar.gz -C /usr/local/lib/mite \
    && rm -rf /opt/mite

ENTRYPOINT [ "/usr/local/lib/mite/bin/mite" ]

CMD [ "foreground" ]
