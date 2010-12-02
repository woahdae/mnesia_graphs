%% @author Woody Peterson <woody.peterson@gmail.com>
%% @copyright 2010 Woody Peterson <woody.peterson@gmail.com>

-module(mnesia_graphs_store).

-include_lib("stdlib/include/qlc.hrl").

-record(stat, {metric, value, timestamp}).

%% API
-export([init/0, save/1, save/2, get/0]).

-define(TABLE_NAME, mnesia_graphs).

init() ->
  create_table(?TABLE_NAME),
  mnesia:wait_for_tables([?TABLE_NAME], 1000).

%% Metrics is a dictionary
save(Metrics) ->
  F = fun() ->
    lists:foreach(
      fun(Key) ->
        Value = case dict:find(Key, Metrics) of
          {ok, Count} -> Count;
          error -> 0
        end,
        mnesia:write(?TABLE_NAME, 
                     #stat{ metric=Key,
                            value=Value,
                            timestamp=os:timestamp() },
                     write)
      end,
      mnesia:dirty_all_keys(?TABLE_NAME))
  end,
  commit(F).

save(Metric, Value) ->
  F = fun() ->
    mnesia:write(?TABLE_NAME, 
                 #stat{ metric=Metric,
                        value=Value,
                        timestamp=os:timestamp() },
                 write)
  end,
  commit(F).

get() ->
  lists:foldl(
    fun(Metric, StatsByMetric) ->
      Stats = mnesia:async_dirty(fun() -> 
                                   mnesia:read(?TABLE_NAME, Metric)
                                 end),
      dict:store(Metric, Stats, StatsByMetric)
    end,
    dict:new(),
    mnesia:dirty_all_keys(?TABLE_NAME)).

%% Internal functions
create_table(Name) ->
  mnesia:create_table(Name, [ {attributes,
                                record_info(fields, stat)},
                              {type, bag},
                              {record_name, stat},
                              {disc_copies, [node()]} ]).

commit(Fun) ->
  case mnesia:transaction(Fun) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> 
      io:format("Mnesia write aborted: ~p~n", [Reason])
  end.
