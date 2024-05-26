- module(client).
- export([start/1]).
- export([list_servers_from_router/1]).
- export([join_server/1]).
- export([leave_server/1]).
- export([send_message/1]).
- export([list_clients/1]).

start([ClientName, Router]) ->
    connect_with_router(Router, 3),
    register(ClientName, spawn(fun() -> 
        process_flag(trap_exit, true), 
        listen(ClientName, Router, []) end)).
    
listen(ClientName, Router, Servers) -> 
    receive
        {list_servers, Pid} ->
            { router, Router } ! { list_servers, self() },
            receive
                {router, Reply} ->
                    Pid ! {ClientName, Reply}
            end,
            listen(ClientName, Router, Servers);

        {join_server, Pid, Topic} ->
            {router, Router} ! {get_server_pid, self(), Topic},
            receive
                {router, [{ServerPid, _}]} ->
                    ServerPid ! {add_client, self()},
                    receive
                        {ServerPid, ok} -> 
                            AddedServer = [{ServerPid, Topic} | Servers],
                            Pid ! {ClientName, ok},
                            listen(ClientName, Router, AddedServer)
                    end;
                {router, []} ->
                    Pid ! {ClientName, notOk, "Topic was not found.~n"},
                    listen(ClientName, Router, Servers)
            end;

        {list_clients, Pid, Topic} ->
            FindTopic = lists:filter(fun({_, T}) -> T =:= Topic end, Servers),
            case FindTopic of
                [] -> Pid ! {ClientName, notOk, "Server with Topic ~w was not found, Please join the server first! Joined Servers:~w. ~n", [Topic, Servers]};
                [{ServerPid, _}] -> 
                    ServerPid ! {list_clients, self()},
                    receive
                        {ServerPid, Clients} ->
                            Pid ! {ClientName, ok, Clients},
                            listen(ClientName, Router, Servers)
                    end
            end;

        {leave_server, Pid, Topic} ->
            FindTopic = lists:filter(fun({_, T}) -> T =:= Topic end, Servers),
            case FindTopic of
                [] -> Pid ! {ClientName, notOk, "Server with Topic ~w was not found, Please join the server first! Joined Servers:~w. ~n", [Topic, Servers]};
                [{ServerPid, ServerTopic}] -> 
                    ServerPid ! {remove_client, self()},
                    receive
                        {ServerPid, ok} ->
                            Pid ! {ClientName, ok},
                            UpdatedServers = lists:delete({ServerPid, ServerTopic}, Servers),
                            listen(ClientName, Router, UpdatedServers)
                    end
            end;

        {send_message, Pid, Topic, Message} ->
            FindTopic = lists:filter(fun({_, T}) -> T =:= Topic end, Servers),
            case FindTopic of
                [] -> Pid ! {ClientName, notOk, "Server with Topic ~w was not found, Please join the server first! Joined Servers:~w. ~n", [Topic, Servers]};
                [{ServerPid, _}] ->
                    ServerPid ! {message, self(), ClientName, Message},
                    Pid ! {ClientName, ok}, 
                    listen(ClientName, Router, Servers)
            end;

        {message, Topic, From,  Message} ->
            io:format("[~w][~w] sent ~s ~n", [Topic, From, Message]),
            listen(ClientName, Router, Servers)
    end.

list_servers_from_router([ClientName]) ->
    ClientName ! {list_servers, self()},
    receive
        {ClientName, Reply} ->
            io:format("List of available servers:~w~n", [Reply])
    end.

join_server([ClientName, Topic]) ->
    ClientName ! {join_server, self(), Topic},
    receive
        {ClientName, ok} ->
            io:format("Joined ~w~n", [Topic]);
        {ClientName, notOk, Reason} ->
            io:format("~w~n", [Reason])
    end.

leave_server([ClientName, Topic]) ->
    ClientName ! {leave_server, self(), Topic},
    receive
        {ClientName, ok} ->
            io:format("Left ~w~n", [Topic])
    end.

list_clients([ClientName, Topic]) ->
    ClientName ! {list_clients, self(), Topic},
    receive
        {ClientName, ok, Clients} ->
            io:format("Clients: ~w~n", [Clients]);
        {ClientName, notOk, Reason} ->
            io:format("~w~n", [Reason])
    end.

send_message([ClientName, Topic, Message]) ->
    ClientName ! {send_message, self(), Topic, Message},
    receive
        {ClientName, ok} ->
            io:format("Message sent.~n");
        {ClientName, notOk, Reason} ->
            io:format("~w~n", [Reason])
    end.


connect_with_router(_, 0) -> 
    io:format("Failed to connect with Router. Exiting...~n"),
    exit(unableToConnectWithRouter);
connect_with_router(Router, Retries) ->
    case net_adm:ping(Router) of
        pong -> 
            io:format("Successfully connected with Router ~s.~n", [Router]),
            ok;
        pang -> 
            io:format("Failed to connect with Router ~s.~n", [Router]),
            timer:sleep(3000),
            io:format("Retrying to connect...~n"),
            connect_with_router(Router, Retries - 1)
    end.