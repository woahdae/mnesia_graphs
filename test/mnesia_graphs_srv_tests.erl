-module(mnesia_graphs_srv_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-record(stat, {metric, value, timestamp}).

full_test() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(testing, []),
  mnesia:wait_for_tables([testing], 1000),
  mnesia_graphs_srv:start_link(),
  timer:sleep(100),

  % test recording messages in existing tables
  mnesia:transaction(fun() -> mnesia:write({testing, hello, world}) end),
  CurrentState = mnesia_graphs_srv:inspect(),
  ?assertEqual({ok, 1}, dict:find(testing, CurrentState)),

  % test ignoring stats for the schema
  mnesia:create_table(ignore_me_please, []),
  CurrentState2 = mnesia_graphs_srv:inspect(),
  ?assertEqual(error, dict:find(schema, CurrentState2)),

  % test auto-subscribing to new tables
  mnesia:create_table(auto, []),
  timer:sleep(1000),
  mnesia:transaction(fun() -> mnesia:write({auto, hello, world}) end),
  CurrentState3 = mnesia_graphs_srv:inspect(),
  ?assertEqual({ok, 1}, dict:find(auto, CurrentState3)),

  % test saving state to mnesia
  mnesia_graphs_srv:persist(),
  timer:sleep(100),
  F = fun() ->
          qlc:e(qlc:q([Op || Op <- mnesia:table(mnesia_graphs),
                             Op#stat.metric =:= testing,
                             Op#stat.value  =:= 1]))
      end,
  {atomic, [Persisted]} = mnesia:transaction(F),
  ?assertEqual(testing, Persisted#stat.metric),
  ?assertEqual(1, Persisted#stat.value),

  % test lookup function
  {ok, Result} = dict:find(testing, mnesia_graphs_srv:read()),
  Stat = lists:last(Result),
  ?assertEqual(testing, Stat#stat.metric),
  ?assertEqual(1, Stat#stat.value),

  mnesia:stop(),
  mnesia:delete_schema([node()]).
