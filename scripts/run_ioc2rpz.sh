#!/bin/sh
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

if [[ -z "${CONF}" ]]; then
	echo "[{ioc2rpz,[{ipv4,\"\"},{ipv6,\"\"},{conf_file,\"$CONF\"},{db_dir,\"./db\"}]}]." > /opt/ioc2rpz/ioc2rpz_app.conf
else
	echo "[{ioc2rpz,[{ipv4,\"\"},{ipv6,\"\"},{conf_file,\"./cfg/ioc2rpz.conf\"},{db_dir,\"./db\"}]}]." > /opt/ioc2rpz/ioc2rpz_app.conf
fi

erl -noshell -pa ./ebin -sname ioc2rpz -eval "application:start(ioc2rpz,permanent)" -config ioc2rpz_app