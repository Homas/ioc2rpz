#Copyright 2017-2018 Vadim Pavlov ioc2rpz[at]gmail[.]com
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

FROM erlang:alpine
MAINTAINER Vadim Pavlov<ioc2rpz@gmail.com>
WORKDIR /opt/ioc2rpz

#RUN mkdir /opt/ioc2rpz/ebin /opt/ioc2rpz/cfg /opt/ioc2rpz/db /opt/ioc2rpz/include /opt/ioc2rpz/src /opt/ioc2rpz/scripts /opt/ioc2rpz/log && apk add bind-tools curl python3
#ADD ebin/ioc2rpz.app /opt/ioc2rpz/ebin/
#ADD scripts/* /opt/ioc2rpz/scripts/
#ADD ioc2rpz_app.config  /opt/ioc2rpz/
#RUN erlc -I include/ -o ebin/ src/*.erl
#ENTRYPOINT ["erl", "-noshell", "-pa", "./ebin", "-sname", "ioc2rpz", "-eval", "application:start(ioc2rpz,permanent)", "-config", "ioc2rpz_app"]
#CMD ["/bin/sh", "/opt/ioc2rpz/scripts/run_ioc2rpz.sh"]

RUN mkdir /opt/ioc2rpz/cfg /opt/ioc2rpz/db /opt/ioc2rpz/include /opt/ioc2rpz/src /opt/ioc2rpz/log && apk add bind-tools curl python3 gawk
ADD src/* /opt/ioc2rpz/src/
ADD include/* /opt/ioc2rpz/include/
ADD config/* /opt/ioc2rpz/config/
ADD rebar.config /opt/ioc2rpz/

RUN rebar3 release -d false

VOLUME ["/opt/ioc2rpz/cfg", "/opt/ioc2rpz/db"]

EXPOSE 53/tcp 53/udp 853/tcp 8443/tcp

ENV CD=/opt/ioc2rpz
ENV DB=/opt/ioc2rpz/db
ENTRYPOINT ["/opt/ioc2rpz/_build/default/rel/ioc2rpz/bin/ioc2rpz", "foreground"]
