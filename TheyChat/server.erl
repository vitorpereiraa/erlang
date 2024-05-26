- module(server).
- export([start/1]).

start([Topic, Router]) -> 
    ServerPid = spawn(fun() -> start_server(Topic, Router) end),
    spawn(fun() -> server_monitor(Topic, Router, ServerPid) end).

server_monitor(Topic, Router, ServerPid) ->
    io:format("Monitor ~w starting to monitor pid ~w~n", [self(), ServerPid]),
    connect_with_router(Router, 3),
    {router, Router} ! {link_server_monitor, Topic, Router, self(), ServerPid},
    process_flag(trap_exit, true),
    link(ServerPid),
    receive 
        {'EXIT', Pid, Reason} ->
            io:format("Server ~w died because it was ~w~n", [Pid, Reason]),
            NewPid = spawn(fun() -> start_server(Topic, Router) end),
            server_monitor(Topic, Router, NewPid)
    end.

start_server(Topic, Router) ->
    io:format("Server starting with pid ~w~n",[self()]),
    connect_with_router(Router, 3),
    add_server_to_router(Router, Topic),
    process_flag(trap_exit, true),
    listen(Router,Topic, [], []).

listen(Router, Topic, Clients, Refs) -> 
    receive
        {add_client, Client} ->
            Ref = monitor(process, Client),
            UpdatedClients = [Client |Clients],
            UpdatedRefs = [{Client, Ref}| Refs],
            Client ! {self(), ok},
            io:format("Client ~w joined.~n",[Client]),
            listen(Router,Topic, UpdatedClients, UpdatedRefs);

        {remove_client, Client} ->
            FindRef = lists:filter(fun({C, _}) -> C =:= Client end, Refs),
            case FindRef of 
                [] -> listen(Router,Topic, Clients, Refs);
                [{_, Ref}] -> 
                    demonitor(Ref),
                    UpdatedClients = lists:delete(Client, Clients),
                    UpdatedRefs = lists:delete({Client, Ref}, Refs),
                    Client ! {self(), ok},
                    io:format("Client ~w left.~n",[Client]),
                    listen(Router,Topic, UpdatedClients, UpdatedRefs)
            end; 
                
        {list_clients, Client} -> 
            Client ! {self(), Clients},
            io:format("List clients request, sent: ~w.~n",[Clients]),
            listen(Router,Topic, Clients, Refs);

        {message, Client, ClientName, Message} -> 
            io:format("[~w with pid ~w] sent '~s'~n", [ClientName, Client, Message]),
            lists:foreach(fun(C) -> C ! {message, Topic, ClientName, Message} end, Clients),
            listen(Router,Topic, Clients, Refs);

        {spawn_monitor} ->
            io:format("spawn monitor request received from router~n"),
            ServerPid = self(),
            spawn(fun() -> server_monitor(Topic, Router, ServerPid) end),
            listen(Router, Topic, Clients, Refs);

        {'EXIT', Client, _ } -> 
            UpdatedClients = lists:delete(Client, Clients),
            io:format("Client ~w was removed due to exit signal.~n",[Client]),
            listen(Router,Topic, UpdatedClients, Refs)
    end.

add_server_to_router(Router, Topic) ->
    {router, Router} ! {add_server, self(), Topic},
    receive
        {router, ok} -> 
            io:format("Server added in router state. ~n");
        {router, notOk} -> 
            io:format("Topics must be unique.~n"),
            exit(routerDeclinedServerTopic)
    end.

connect_with_router(_, 0) -> 
    io:format("Failed to connect with Router. Exiting...~n"),
    exit(unableToConnectWithRouter);
connect_with_router(Router, Retries) ->
    case net_adm:ping(Router) of
        pong -> 
            io:format("Successfully connected with Router ~s.~n", [Router]);
        pang -> 
            io:format("Failed to connect with Router ~s.~n", [Router]),
            timer:sleep(3000),
            io:format("Retrying to connect...~n"),
            connect_with_router(Router, Retries - 1)
    end.