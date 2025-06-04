-module(tuntap).

-export([tuntap_init/0, tuntap_destroy/1, tuntap_start/3, tuntap_get_hwaddr_nif/2,
         tuntap_down_nif/1, tuntap_up_nif/1, tuntap_set_ip_nif/3, tuntap_read_nif/1,
         tuntap_write_nif/2, tuntap_get_fd_nif/1, tuntap_wait_read_nif/3, tuntap_set_hwaddr_nif/2,
         tuntap_get_readable_nif/1]).

-on_load init/0.

init() ->
  PrivDir = code:priv_dir(?MODULE),
  NifPath = filename:join(PrivDir, "tuntap_nif"),
  case erlang:load_nif(NifPath, 0) of
    ok ->
      ok;
    {error, Reason} ->
      exit({load_nif_failed, Reason})
  end.

tuntap_init() ->
  erlang:nif_error(undef).

tuntap_destroy(_Device) ->
  erlang:nif_error(undef).

tuntap_start(Device, Mode, Unit) when is_integer(Mode) andalso is_integer(Unit) ->
  tuntap_start_nif(Device, Mode, Unit);
tuntap_start(_Device, _Mode, _Unit) ->
  false.

tuntap_start_nif(_Device, _Mode, _Unit) ->
  erlang:nif_error(undef).

tuntap_get_hwaddr_nif(_, _) ->
  erlang:nif_error(undef).

tuntap_set_hwaddr_nif(_, _) ->
  erlang:nif_error(undef).

tuntap_down_nif(_) ->
  erlang:nif_error(undef).

tuntap_up_nif(_) ->
  erlang:nif_error(undef).

tuntap_set_ip_nif(_, _, _) ->
  erlang:nif_error(undef).

tuntap_read_nif(_) ->
  erlang:nif_error(undef).

tuntap_write_nif(_, _) ->
  erlang:nif_error(undef).

tuntap_get_fd_nif(_) ->
  erlang:nif_error(undef).

tuntap_wait_read_nif(_Device, _Fd, _Pid) ->
  erlang:nif_error(undef).

tuntap_get_readable_nif(_Device) ->
  erlang:nif_error(undef).
