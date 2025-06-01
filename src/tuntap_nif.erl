-module(tuntap_nif).

-export([main/0]).

main() ->
  tuntap:load(),
  F = tuntap:tuntap_init(),
  tuntap:tuntap_destroy(F).
