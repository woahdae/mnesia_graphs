-module(mnesia_graphs_store_tests).
-include_lib("eunit/include/eunit.hrl").

-record(stat, {metric, value, timestamp}).

% I don't want to create_schema/start/stop/delete_schema every test,
% but not sure how you do it before/after the tests in a file
full_test() ->
  mnesia:create_schema([node()]),
  mnesia:clear_table(mnesia_graphs),
  mnesia:start(),
  mnesia_graphs_store:init(),

  ?assert(lists:member(mnesia_graphs, mnesia:system_info(tables))),

  mnesia_graphs_store:save(hits, 100),
  {atomic, [Actual]} = mnesia:transaction(fun() -> mnesia:read(mnesia_graphs, hits) end),
  ?assertEqual(hits, Actual#stat.metric),
  ?assertEqual(100, Actual#stat.value),

  {ok, [Got]} = dict:find(hits, mnesia_graphs_store:get()),
  ?assertEqual(Actual, Got),

  mnesia:stop(),
  mnesia:delete_schema([node()]).

