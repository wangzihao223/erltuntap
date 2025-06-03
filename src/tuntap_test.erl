-module(tuntap_test).

-export([init/0, start/2, write/1, read/1, main/0]).

init() ->
  tuntap:load(),
  Device = tuntap:tuntap_init(),
  Device.

start(Device, IP) ->
  tuntap:tuntap_start(Device, 16#0002, 257),
  tuntap:tuntap_up_nif(Device),
  tuntap:tuntap_set_ip_nif(Device, IP, 24).

write(Device) ->
  tuntap:tuntap_write_nif(Device, <<"hello,world">>).

read(Device) ->
  R = tuntap:tuntap_read_nif(Device),
  size(R).

main() ->
  D = init(),
  start(D, "192.168.3.57"),
  D.
