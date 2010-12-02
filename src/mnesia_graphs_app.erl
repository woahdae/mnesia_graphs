%% @author Woody Peterson <woody.peterson@gmail.com>
%% @copyright 2010 Woody Peterson <woody.peterson@gmail.com>

%% @doc Callbacks for the mnesia_graphs application.

-module(mnesia_graphs_app).
-author("Woody Peterson <woody.peterson@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mnesia_graphs.
start(_Type, _StartArgs) ->
    mnesia_graphs_deps:ensure(),
    mnesia_graphs_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mnesia_graphs.
stop(_State) ->
    ok.
