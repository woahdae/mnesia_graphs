-module(mnesia_graphs_srv).
-behavior(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0, read/0, persist/0, inspect/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MNESIA_TABLE, mnesia_graphs).

%% Client API

start_link() ->
  start_link([]).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
  gen_server:call(?MODULE, stop).

read() ->
  gen_server:call(?MODULE, read).

persist() ->
  gen_server:cast(?MODULE, persist).

inspect() ->
  gen_server:call(?MODULE, inspect).

%% gen_server Callbacks

init(Options) ->
  process_flag(trap_exit, true),

  IgnoredTables = case proplists:get_value(ignored_tables, Options) of
    undefined -> [];
    Defined -> Defined
  end,
  put(ignored_tables, [?MNESIA_TABLE|IgnoredTables]),

  mnesia_graphs_store:init(),
  gen_server:cast(?MODULE, subscribe_all),
  timer:send_interval(5 * 60 * 1000, persist),
  {ok, dict:new()}.

handle_call(stop, _From, _RecentStats) ->
  {stop, normal, _RecentStats};

handle_call(read, _From, _RecentStats) ->
  {reply, mnesia_graphs_store:get(), _RecentStats};

handle_call(inspect, _From, RecentStats) ->
  {reply, RecentStats, RecentStats};

handle_call(persist, _From, _RecentStats) ->
  persist(),
  {reply, ignored, _RecentStats};

handle_call(_Request, _From, _RecentStats) ->
  {reply, ignored, _RecentStats}.

handle_cast(subscribe_all, _RecentStats) ->
  mnesia:wait_for_tables([schema], 1000),
  mnesia:subscribe(system),
  SubscribeTo = lists:subtract(mnesia:system_info(tables), get(ignored_tables)),
  lists:foreach(fun subscribe/1, SubscribeTo),
  {noreply, _RecentStats};

handle_cast(persist, RecentStats) ->
  mnesia_graphs_store:save(RecentStats),
  {noreply, dict:new()};

handle_cast(_Msg, _RecentStats) ->
  {noreply, _RecentStats}.

handle_info({mnesia_table_event, {write, Data, _ActivityId}}, _RecentStats) when element(1, Data) =:= schema ->
  Table = element(2, Data),
  timer:sleep(1000), % mnesia:wait_for_tables times out, but this works...
  subscribe(Table),
  {noreply, _RecentStats};

handle_info({mnesia_table_event, {write, Data, _ActivityId}}, RecentStats) ->
  Table = element(1, Data),
  UpdatedValue = case dict:find(Table, RecentStats) of
    {ok, OldValue} -> OldValue + 1;
    error -> 1
  end,
  {noreply, dict:store(Table, UpdatedValue, RecentStats)};

handle_info({mnesia_system_event, _Event}, _RecentStats) ->
  {noreply, _RecentStats};

handle_info(persist, _RecentStats) ->
  persist(),
  {noreply, _RecentStats};

handle_info(_Info, _RecentStats) ->
  {noreply, _RecentStats}.

terminate(_Reason, _RecentStats) ->
  ok.

code_change(_OldVsn, _RecentStats, _Extra) ->
  {ok, _RecentStats}.

%% Internal functions

subscribe(Table) ->
  % so we can always know what we're subscribed to by checking
  % the mnesia_graphs table, as well as it's just handy to see a
  % starting point
  mnesia_graphs_store:save(Table, 0),

  mnesia:subscribe({table, Table, simple}).
