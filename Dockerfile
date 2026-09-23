#Copyright 2017-2026 Vadim Pavlov ioc2rpz[at]gmail[.]com
#
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.

#ioc2rpz container

# Pinned to a specific Erlang/OTP minor line rather than the floating `alpine`
# tag, so a rebuild produces the same runtime. For a fully reproducible build
# replace the tag with a digest: FROM erlang:27-alpine@sha256:<digest>
FROM erlang:27-alpine

# MAINTAINER is deprecated; LABEL is the supported form.
LABEL org.opencontainers.image.authors="Vadim Pavlov <ioc2rpz@gmail.com>" \
      org.opencontainers.image.title="ioc2rpz" \
      org.opencontainers.image.description="ioc2rpz custom DNS server" \
      org.opencontainers.image.source="https://github.com/Homas/ioc2rpz" \
      org.opencontainers.image.licenses="Apache-2.0"

WORKDIR /opt/ioc2rpz

#RUN mkdir /opt/ioc2rpz/ebin /opt/ioc2rpz/cfg /opt/ioc2rpz/db /opt/ioc2rpz/include /opt/ioc2rpz/src /opt/ioc2rpz/scripts /opt/ioc2rpz/log && apk add bind-tools curl python3
#ADD ebin/ioc2rpz.app /opt/ioc2rpz/ebin/
#ADD scripts/* /opt/ioc2rpz/scripts/
#ADD ioc2rpz_app.config  /opt/ioc2rpz/
#RUN erlc -I include/ -o ebin/ src/*.erl
#ENTRYPOINT ["erl", "-noshell", "-pa", "./ebin", "-sname", "ioc2rpz", "-eval", "application:start(ioc2rpz,permanent)", "-config", "ioc2rpz_app"]
#CMD ["/bin/sh", "/opt/ioc2rpz/scripts/run_ioc2rpz.sh"]

# Runtime packages only. `php`, `lftp` and `ripgrep` were dropped: no code path
# uses them and they are not on the `shell:` source allowlist. bind-tools (dig)
# is kept for troubleshooting; curl and gawk are the fetch/transform tools most
# `shell:` sources are written against. Add whatever your own `shell:` sources
# need (e.g. python3 for a decoder script) in a derived image.
RUN mkdir -p /opt/ioc2rpz/cfg /opt/ioc2rpz/ssl /opt/ioc2rpz/db /opt/ioc2rpz/include /opt/ioc2rpz/src /opt/ioc2rpz/log \
    && apk add --no-cache bind-tools curl gawk

# COPY rather than ADD (ADD's URL/tar-extraction behaviour is not wanted here).
# rebar.lock is copied WITH rebar.config: without it rebar3 resolved cowlib and
# ranch to whatever satisfied the constraints at build time, unpinned and with
# no hash verification, so two builds of the same source could ship different
# dependency versions.
COPY rebar.config rebar.lock /opt/ioc2rpz/
COPY src/ /opt/ioc2rpz/src/
COPY include/ /opt/ioc2rpz/include/
COPY config/ /opt/ioc2rpz/config/

RUN rebar3 eunit && rebar3 release -d false

# Run as a non-root user. The release binds 53/853/443, which are privileged
# ports, so the container needs NET_BIND_SERVICE (Docker grants it by default in
# the default capability set) - it does not need to run as root.
RUN addgroup -S ioc2rpz && adduser -S -G ioc2rpz -h /opt/ioc2rpz ioc2rpz \
    && chown -R ioc2rpz:ioc2rpz /opt/ioc2rpz

VOLUME ["/opt/ioc2rpz/cfg", "/opt/ioc2rpz/db"]

# 53   DNS over UDP/TCP
# 443  DNS over HTTPS (?PortDoH) - was missing from this list
# 853  DNS over TLS (?PortTLS)
# 8443 REST management API (?PortREST)
EXPOSE 53/tcp 53/udp 443/tcp 853/tcp 8443/tcp

ENV CD=/opt/ioc2rpz
ENV DB=/opt/ioc2rpz/db
ENV NODE_NAME=ioc2rpz

# NOTE: IO2Cookie is deliberately NOT baked in. It used to default to the literal
# string "ioc2rpz", i.e. a well-known Erlang distribution cookie in every image:
# anyone able to reach the distribution port of such a node can evaluate
# arbitrary code in the VM. The entrypoint below generates a random cookie when
# none is supplied, so the default is unique per container instead of public.
# Supply your own when you need several nodes to talk to each other:
#   docker run -e IO2Cookie="$(openssl rand -hex 32)" ...
# The cookie is passed via -setcookie in config/vm.args and is therefore visible
# in `ps` inside the container. Do not publish the distribution port. If Erlang
# distribution is not needed at all, comment out -sname/-setcookie in
# config/vm.args.

USER ioc2rpz

# Liveness check: query the built-in sample zone over TCP on loopback. dig exits
# 0 for ANY DNS response and 9 when no server could be reached, so this asserts
# "the DNS listener accepted a connection and answered" without depending on the
# configuration: a REFUSED (no TSIG, or rate-limited) still counts as alive.
# Deliberately not `bin/ioc2rpz ping`, which needs the distribution cookie - the
# healthcheck does not see the random cookie the entrypoint generates.
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD dig +tcp +time=3 +tries=1 @127.0.0.1 sample-zone.ioc2rpz SOA > /dev/null 2>&1 || exit 1

# `exec` so the release keeps PID 1 and receives SIGTERM/SIGINT directly.
ENTRYPOINT ["/bin/sh","-c","export IO2Cookie=\"${IO2Cookie:-$(head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \\n')}\"; exec /opt/ioc2rpz/_build/default/rel/ioc2rpz/bin/ioc2rpz foreground"]
