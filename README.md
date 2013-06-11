# mod_videobridge - pure ejabberd module implementation of jitsi-videobridge

mod_videobridge is a pure erlang ejabberd module implementing jitsi-videobridge protocol.


## Installation

```shell
$> make
$> make install
```

## Configuration

Edit /etc/ejabberd/ejabberd.cfg and add module configuration

```erlang
{modules, [
  ...
  {mod_videobridge, [
                     {public_ip, "192.168.0.1"},
                     {rtp_range, {10000-20000}}
                    ]}
}.
```

- `public_ip`: the public ip of your server mod_videobridge RTP ports will listen to
- `rtp_range`: allocated ports range for RTP/RTCP traffic

NOTES:
- ejabberd server must be accessible through given `public_ip` without NAT
- `rtp_range` ports must be opened on your firewall. For example, here is iptables rules:

```shell
$> iptables -A INPUT  -p udp -m udp --dport 10000:20000 -j ACCEPT
$> iptables -A OUTPUT -p udp -m udp --sport 10000:20000 -j ACCEPT
```

## Statistics

You can display statistics about active conferences:
```shell
$> ejabberdctl videobridge_stats

- conference (id=f6e9b66505cee707, type=audio):
  . channel #7fe9eab7922a9a49:
    - rtp  port#19042 connected to 10.0.42.42:33876
      . recv: 126 pkts, 1 kbytes / 1599 pkts, 23 kbytes
      . send: 128 pkts, 11 kbytes / 1618 pkts, 137 kbytes
    - rtcp port#19043 connected to 10.0.42.42:34070
  . channel #58884dde8784ea54:
    - rtp  port#10108 connected to 10.0.121.71:5012
      . recv: 128 pkts, 11 kbytes / 1621 pkts, 137 kbytes
      . send: 126 pkts, 1 kbytes / 1599 pkts, 23 kbytes
    - rtcp port#10109 connected to 10.0.121.71:5013

- conference (id=f6e9b66505cee707, type=video):
  . channel #64b42602f99e1439:
    - rtp  port#10250 connected to 10.0.42.42:58845
      . recv: 69 pkts, 66 kbytes / 676 pkts, 669 kbytes
      . send: 38 pkts, 2 kbytes / 294 pkts, 49 kbytes
    - rtcp port#10251 connected to 10.0.42.42:60887
  . channel #2ff932acd6e91c1b:
    - rtp  port#13410 connected to 10.0.121.71:5014
      . recv: 38 pkts, 2 kbytes / 296 pkts, 49 kbytes
      . send: 69 pkts, 66 kbytes / 676 pkts, 669 kbytes
    - rtcp port#13411 connected to 10.0.121.71:5015
```

## TODO

- RTP timeout (close port when no packets received after X seconds)
- Add SSRC, used codec, RTCP stats in stats
- unittests
- error cases
- implement http://xmpp.org/extensions/xep-0092.html (jabber:iq:version)
- implement http://xmpp.org/extensions/xep-0012.html (jabber:iq:last)
- refactoring


## NOT IMPLEMENTED

Compared to original Jisti Videobridge component, following features are not implemented:
- RTP transcoding
- Mixing

## Authors

- Guillaume Bour <guillaume@bour.cc>

## License

mod_videobridge is distributed under GNU AGPL License (see `COPYING` file).
