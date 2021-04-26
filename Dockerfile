FROM erlang:23 as build

COPY . /opt/mite

WORKDIR /opt/mite

RUN rebar3 as prod tar

FROM erlang:23

COPY --from=build /opt/mite/_build/prod/rel/mite/mite-0.1.0.tar.gz /tmp

RUN mkdir -p /opt/mite && tar -xzf /tmp/mite-0.1.0.tar.gz -C /opt/mite

ENTRYPOINT ["/opt/mite/bin/mite"]

CMD ["foreground"]