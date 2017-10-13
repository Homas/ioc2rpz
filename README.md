#  IOC2RPZ - turns your threat intelligence into RPZ feeds.

## How to start ioc2rpz service

## Configuration
### Config file
### Constants

## TODO features
1. [ ] IOC individual expiration time - update time will be used as minimum time for full zone refresh + check the SOA
1. [ ] Incremental zone transfer/IXFR
1. [ ] Cache IXFR and rebuild AXFR cache
1. [ ] Save cache config
1. [ ] Support DNS UDP SOA
1. [ ] DNS Notify messages
1. [ ] Add RPZ zone transfer
1. [ ] Add MySQL indicators lookup

## Other TODO
1. [ ] Clean up the code & add comments
1. [ ] Move connectors to a new file
1. [ ] Documentation

### RFC

Packets
https://tools.ietf.org/html/rfc6895

Records
https://www.ietf.org/rfc/rfc1035.txt

IXFR
https://tools.ietf.org/html/rfc1995

DNS Headers
https://github.com/blackberry/Erlang-OTP/blob/master/lib/kernel/src/inet_dns.hrl

RPZ
https://tools.ietf.org/html/draft-ietf-dnsop-dns-rpz-00

TSIG etc
https://tools.ietf.org/html/rfc2845
https://tools.ietf.org/html/rfc2104
https://tools.ietf.org/html/rfc4635

DNS over TCP
https://tools.ietf.org/html/rfc5966

DNS Notify
https://tools.ietf.org/html/rfc1996
