# mod_videobridge - pure ejabberd module implementation of jitsi-videobridge

mod_videobridge is a pure erlang ejabberd module implementing jisti-videobridge protocol.


## Install

```shell
$> make
$> make install
```

### Configuration

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
- `rtp_range`: 

## TODO

## Authors

- Guillaume Bour <guillaume@bour.cc>

## License

mod_videobridge is distributed under GNU AGPL License (see `COPYING` file).
