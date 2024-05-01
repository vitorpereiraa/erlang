-module(app).
-export([run/0]).

run() -> 
    runner(fun square/1, "square", 2),
    runner(fun temp_convert/1, "convert fahrenheit to celcius", 212),
    runner(fun factorial/1, "factorial", 5),
    runner(fun fib/1, "fib", 12).

runner(Function, FunctionName, Argument) ->
    io:fwrite("~s~n",[FunctionName]),
    R = Function(Argument),
    io:fwrite("~w~n", [R]).

square(N) ->
    N * N.

temp_convert(F) -> 
    (5/9)*(F-32).

factorial(0) -> 
    1;
factorial(N) -> 
    N * factorial(N-1).

fib(0) -> 0;
fib(1) -> 1;
fib(N) ->
    fib(N-1) + fib(N-2).
