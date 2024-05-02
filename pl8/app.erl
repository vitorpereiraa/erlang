-module(app).
-export([run/0]).

run() -> 
    runner(fun() -> square(2) end, "square"),
    runner(fun() -> temp_convert(212) end, "fahrenheit to celcius"),
    runner(fun() -> factorial(5) end , "factorial"),
    runner(fun() -> fib(12) end, "fib"),
    runner(fun() -> count([1,3,34,1]) end , "count"),
    runner(fun() -> member([1,3,34,1],7) end, "member"),
    runner(fun() -> delete([1,3,34,1],3) end, "delete"),
    runner(fun() -> deleteTailRecursive([1,3,34,1],3,[]) end, "deleteTailRecursive"),
    runner(fun() -> reverse([1,3,34,1]) end, "reverse"),
    runner(fun() -> average([1,2,3,4,5]) end, "average"),
    runner(fun() -> sum_max([1,2,7,4,5], [1,2,3,4,5]) end, "sum_max"),
    runner(fun() -> int([1,2.7,6.4,4,5]) end, "int").

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
member([_|T], N) -> member(T, N).

delete([], _) -> [];
delete([H|T], ToDelete) when H == ToDelete -> [] ++ delete(T, ToDelete);
delete([H|T], ToDelete) -> [H] ++ delete(T,ToDelete).

deleteTailRecursive([], _, R) -> R;
deleteTailRecursive([H|T], ToDelete, L) when H == ToDelete -> deleteTailRecursive(T, ToDelete, L);
deleteTailRecursive([H|T], ToDelete,L ) -> deleteTailRecursive(T,ToDelete, [H] ++ L).

reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

average(L) -> sum(L)/count(L).

max(L) -> max(L, 0). 

max([], MAX) -> MAX; 
max([H|T], MAX) when H > MAX -> max(T, H); 
max([_|T], MAX) -> max(T, MAX). 

sum_max(L1, L2) -> max(L1) + max(L2).

decimal_part(Number) ->
    FloatPart = math:floor(Number),
    Number - FloatPart.

int([]) -> [];
int([H|T]) when is_integer(H) -> [H] ++ int(T); 
int([H|T]) when is_float(H) -> 
    case decimal_part(H) < 0.5 of 
        true -> [trunc(H)] ++ int(T);
        false -> [round(H)] ++ int(T)
    end.


