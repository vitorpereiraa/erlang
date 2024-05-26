- module(server).
- export([start/1]).

start([Topic, Router]) -> 
    connect_with_router(Router, 3),
    add_server_to_router(Router, Topic),
    io:format("Server connected with router and starting with topic: ~s~n", [Topic]),
    register(Topic, self()),
    listen(Topic, []).

listen(Topic, Clients) -> 
    receive
        {add_client, Client, ClientName} ->
            Client ! {self(), ok},
            listen(Topic, [{Client, ClientName}|Clients]);

        {message, Client, ClientName, Message} -> 
            io:format("[~w ~w] sent '~s'~n", [ClientName, Client, Message]),
            lists:foreach(fun({C, _}) -> C ! {message, Topic, ClientName, Message} end, Clients),
            listen(Topic, Clients)
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