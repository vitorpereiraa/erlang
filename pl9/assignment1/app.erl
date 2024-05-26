- module(app).
- export([run/0]).

run() -> 
    Pid = spawn(fun printer/0),
    Pid ! {self(), "Hello World!"},
    listen().


printer() -> 
    receive 
        { Pid, Message } -> 
            io:fwrite("~w:~s~n",[Pid, Message]),
            Pid ! {self(), Message},
            printer()
    end.

listen() ->
    receive 
        { Pid, Message } -> 
            io:fwrite("~w:~s~n",[Pid, Message]),
            listen()
    end.