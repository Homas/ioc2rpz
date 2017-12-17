#Copyright 2017 Vadim Pavlov pvm(dot)del[at]gmail[.]com
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
MAINTAINER Vadim Pavlov<pvm.del@gmail.com>
WORKDIR /opt/ioc2rpz

RUN mkdir /opt/ioc2rpz/ebin /opt/ioc2rpz/cfg /opt/ioc2rpz/db /opt/ioc2rpz/include /opt/ioc2rpz/src /opt/ioc2rpz/log
ADD ebin/ioc2rpz.app /opt/ioc2rpz/ebin/
ADD src/* /opt/ioc2rpz/src/
ADD include/* /opt/ioc2rpz/include/

RUN erlc -I include/ -o ebin/ src/*.erl

VOLUME ["/opt/ioc2rpz/cfg", "/opt/ioc2rpz/db"]

EXPOSE 53/tcp 53/udp

ENTRYPOINT ["erl", "-noshell", "-pa", "./ebin", "-eval", "application:start(ioc2rpz)"]
