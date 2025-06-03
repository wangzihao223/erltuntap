-module(counter).

%% 维护一个计数器进程，生成递增ID
-export([start_id_generator/0, get_identification/1]).

start_id_generator() ->
  spawn(fun id_generator_loop/0).

id_generator_loop() ->
  id_generator_loop(0).

id_generator_loop(Counter) ->
  receive
    {get_id, Caller} ->
      Caller ! {id, Counter},
      Next = (Counter + 1) band 16#FFFF, % 保证16位循环
      id_generator_loop(Next)
  end.

%% 获取ID的同步调用示例
get_identification(Pid) ->
  Pid ! {get_id, self()},
  receive
    {id, ID} ->
      ID
  after 1000 ->
    timeout
  end.
