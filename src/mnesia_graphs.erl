%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mnesia_graphs.

-module(mnesia_graphs).
-author("Woody Peterson <woody.peterson@gmail.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mnesia_graphs server.
start() ->
    mnesia_graphs_deps:ensure(),
    ensure_started(crypto),
    application:start(mnesia_graphs).


%% @spec stop() -> ok
%% @doc Stop the mnesia_graphs server.
stop() ->
    application:stop(mnesia_graphs).
