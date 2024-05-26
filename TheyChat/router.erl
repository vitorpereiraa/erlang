- module(router).
- export([start/0]).

start() ->
    register(router, self()),
    io:format("Router registered as atom router with pid ~w.~n",[self()]),
    listen([]).

listen(Servers) -> 
    receive
        {list_servers, Client} -> 
            Client ! {router, [Topics || {_, Topics} <- Servers]},
            listen(Servers);
        {get_server_pid, Client, Topic} ->
            Client ! {router, lists:filter(fun({_, T}) -> Topic =:= T end, Servers)},
            listen(Servers);
        {add_server, Server, Topic} ->
            {R, UpdatedServers} = validateServer(Servers, Server, Topic),
            Server ! {router, R},
            listen(UpdatedServers)
    end.

% Topics should be unique
validateServer(Servers, Server, Topic) ->
    TopicsList = [Topics || {_, Topics} <- Servers],
    TopicsSet = sets:from_list(TopicsList),
    TopicsSetWithNewServer = sets:add_element(Topic, TopicsSet),
    case sets:size(TopicsSetWithNewServer) =:= sets:size(TopicsSet) of
        false -> 
            {ok, [{Server, Topic}|Servers]};
        true -> 
            {notOk, Servers}
    end.
