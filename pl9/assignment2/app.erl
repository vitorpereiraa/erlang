- module(app).
- export([run/0]).

run() -> 
    io:format("Spawning server...~n"),
    Server = spawn(fun() -> server([]) end),
    io:format("Spawning subscribers...~n"),
    spawn_subs(Server, 10),
    io:format("Broadcasting...~n"),
    broadcast(Server, "Hello my children").

broadcast(Server, M) ->
    receive
    after 1000 -> 
        Server ! {publish, M}
    end.

spawn_subs(_, 0) -> ok;
spawn_subs(Server, X) -> 
    spawn(fun() -> subscriber(Server) end),
    spawn_subs(Server, X-1).

subscriber(Server) -> 
    io:format("~w subscribing to ~w...~n", [self(), Server]),
    Server ! {subscribe, self()},
    subscriber_loop(Server).

subscriber_loop(Server) -> 
    receive
        {message, Message} ->
            io:format("[~w] received ~s~n",[self(),Message]),
            subscriber_loop(Server)
    end.

server(Subscribers) ->
    receive
        {subscribe, Pid} ->
            io:format("~w subscribed ~n", [Pid]),
            server([Pid | Subscribers]);
        {publish, Message} ->
            io:format("publishing...~n"),
            lists:foreach(fun(Pid) -> Pid ! {message, Message} end, Subscribers),
            server(Subscribers)
    end.