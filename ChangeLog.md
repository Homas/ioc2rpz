# ioc2rpz change log
[CB] - Changed Behaviour
## 2019-07-21 v1.0.0.0
- Released v1.0.0.0

## 2019-07-20 v0.9.5.0
- Bug fixes related to IXFR zone update and transfer
- [CB] Source IXFR update "from" time will be keept the same until we get "non zero" update.
- Retry for unavailable sources (see ioc2rpz.hrl)
- IXFR table management optimization

## 2019-06-13 v0.9.4.0
- Fixed bugs:
	- #10
	- live zones, wrong records count in the hot cache
- Rule generation code was optimized
- Configuration can be split by multiple files using "include" 
- Key groups for RPZs and SRV

## 2019-03-11 v0.9.3.1
- REST API
- added rebar3 to manage dependencies

## 2019-03-01 v0.9.2.1
- bug with configuration reload

## 2019-02-24 v0.9.2.0
- DoT (DNS over TLS) support for zone transfer, SOA and management requests (DNS Notify is not supported).

## 2019-02-22 v0.9.1.1
- UDP service moved under supervisor

## 2019-02-15 v0.9.1
- [CB] Connection and key validation log messages were formated in CEF
- Request to reload TSIG keys list only.

## 2018-09-22
- IPv6 support
- Configuration file name and IPs are moved to an app config file

## 2018-08-16
- concurent zone creation in a cache

## 2018-08-14
- [CB] tcp_send errors handeling
- Query class and type in text in the logs

## 2018-07-25
#- [CB] Individual indicators are converted into low case instead of converting a full source file. It was updated because of possible issues with REGEX.
#It is require more memory. If memory is limited uncomment marked lines in ioc2rpz_conn.erl.

## 2018-04-11
Added a reference to ioc2rpz.gui
No code change

## 2018-01-07 v0.9.0-2018010701
Initial release