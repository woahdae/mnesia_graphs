%% @author Woody Peterson <woody.peterson@gmail.com>
%% @copyright 2010 Woody Peterson <woody.peterson@gmail.com>

%% @doc Web server for mnesia_graphs.

-module(mnesia_graphs_web).
-author("Woody Peterson <woody.peterson@gmail.com>").

-export([start/1, stop/0, loop/2]).

-record(stat, {metric, value, timestamp}).
-define(APP_ID, mnesia_graphs).

%% External API

start(Options) ->
    compile_templates(),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
               ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop}, {port, 10000} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    compile_templates(), % need to find a way to turn this off in production

    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "" -> render_index(Req);
                    _ -> Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ -> Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

render_index(Req) ->
    RawStats = mnesia_graphs_srv:read(),
    % RawStats = dict:store( channelfireball,
    %                        [ {stat, channelfireball, 10, SecondsFromZeroHour},
    %                          {stat, channelfireball, 5, SecondsFromZeroHour - 60} ],
    %                        dict:new()),

    Jsonable = lists:map(
      fun(Entry) ->
        Stats = element(2, Entry),
        BareStats = lists:map(
          fun(Stat) ->
            {MegaSeconds, Seconds, _MicroSeconds} = Stat#stat.timestamp,
            RoundedSecondsInMs = (MegaSeconds * 1000000 + Seconds) * 1000,
            {struct, [{value, Stat#stat.value},
                      {timestamp, RoundedSecondsInMs}]}
          end, Stats),
        {struct, [{key, element(1, Entry)}, {values, BareStats}]}
      end,
      dict:to_list(RawStats) ),

    Json = mochijson2:encode(Jsonable),
    % Json = mochijson2:encode([{struct, [{key,
    %                                      channelfireball},
    %                                     {values,
    %                                      [{struct,
    %                                        [{value,
    %                                          10},
    %                                         {timestamp,
    %                                          1290769298}]},
    %                                       {struct,
    %                                        [{value,
    %                                          5},
    %                                         {timestamp,
    %                                          1290769238}]}]}]}]),
    Html = mnesia_graphs_index:render(Json),

    Req:ok({"text/html",Html}).

compile_templates() ->
    erltl:compile(
        code:priv_dir(?APP_ID) ++ "/www/mnesia_graphs_index.et",
        [{outdir, code:lib_dir(?APP_ID) ++ "/ebin"}] ).
