-module(app).
-export([run/0]).

run() -> 
    runner(fun() -> square(2) end, "square"),
    runner(fun() -> temp_convert(212) end, "fahrenheit to celcius"),
    runner(fun() -> factorial(5) end , "factorial"),
    runner(fun() -> fib(12) end, "fib"),
    runner(fun() -> count([1,3,34,1]) end , "count"),
    runner(fun() -> member([1,3,34,1],7) end, "member").

runner(Function, FunctionName) ->
    io:fwrite("~s~n",[FunctionName]),
    R = Function(),
    io:fwrite("~w~n", [R]).

square(N) ->
    N * N.

temp_convert(F) -> 
    (5/9)*(F-32).

factorial(0) -> 1;
factorial(N) -> 
    N * factorial(N-1).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> 
    fib(N-1) + fib(N-2).

count([]) -> 0;
count([_|T]) -> 1 + count(T).

member([], _) -> false;
member([H|_], N) when H == N -> true;
member([H|T], N) -> member(T, N).








