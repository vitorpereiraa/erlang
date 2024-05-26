- module(router).
- export([start/0]).

start() ->
    RouterPid  = spawn(fun() -> start_router() end),
    spawn(fun() -> router_monitor(RouterPid) end).

router_monitor(RouterPid) ->
    io:format("Monitor ~w starting to monitor pid ~w~n", [self(), RouterPid]),
    process_flag(trap_exit, true),
    link(RouterPid),
    receive 
        {'EXIT', Pid, Reason} ->
            io:format("Router ~w died because it was ~w~n", [Pid, Reason]),
            NewPid = spawn(fun() -> start_router() end),
            router_monitor(NewPid)
    end.

start_router() ->
    io:format("Starting router registered as atom 'router' with pid ~w.~n",[self()]),
    register(router, self()),
    process_flag(trap_exit, true),
    listen([], []).

listen(Servers, ServerMonitor) -> 
    receive
        {list_servers, Client} -> 
            Client ! {router, [Topics || {_, Topics} <- Servers]},
            listen(Servers, ServerMonitor);
        {get_server_pid, Client, Topic} ->
            Client ! {router, lists:filter(fun({_, T}) -> Topic =:= T end, Servers)},
            listen(Servers, ServerMonitor);
        {add_server, Server, Topic} ->
            UpdatedServers = [{Server, Topic}|Servers],
            Server ! {router, ok},
            listen(UpdatedServers, ServerMonitor);

        {'EXIT', Pid, Reason} ->
            io:format("Monitor ~w died because it was ~w~n", [Pid, Reason]),
            RouterPid = self(),
            spawn_link(fun() -> router_monitor(RouterPid) end),
            listen(Servers, ServerMonitor);

        {link_server_monitor, Topic, Router, ServerMonitorPid, ServerPid} ->
            io:format("Router starting to monitor Server Monitor ~w.~n", [ServerMonitorPid]),
            monitor(process, ServerMonitorPid),
            listen(Servers, [{Topic, Router, ServerMonitorPid, ServerPid}|ServerMonitor]);
        {'DOWN', _, process, Pid,  _} ->
            io:format("Server Monitor ~w died.~n", [Pid]),
            FindServerMonitor = lists:filter(fun({_,_,P, _}) -> P =:= Pid end, ServerMonitor),
            case FindServerMonitor  of
                [] -> listen(Servers, ServerMonitor);
                [{Topic, Router, _, ServerPid}] ->
                    case rpc:call('server@VAL-235', erlang, is_process_alive, [ServerPid]) of
                        true  -> ServerPid ! {spawn_monitor};
                        false -> rpc:call('server@VAL-235', server, start, [Topic, Router])
                    end
            end,
            listen(Servers, ServerMonitor)
    end.