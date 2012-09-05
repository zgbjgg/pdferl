
-module(xml).

-export([encode/1, set_header/1]).


set_header(Body) ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"++ Body.

encode({Node, Params, Elements}) ->
    NodeStr = to_str(Node),
    "<"++NodeStr++" "++Params++">"++encode(Elements)++"</"++NodeStr++">";
encode({Node,Elements}) ->
    NodeStr = to_str(Node),
    "<"++NodeStr++">"++encode(Elements)++"</"++NodeStr++">";
encode([]) -> "";
encode([{Key,Value}|T]) ->
    {KeyStr,ValStr} = {to_str(Key),to_str(Value)},
    "<"++KeyStr++">"++encode(ValStr)++"</"++KeyStr++">"++ encode(T);
encode(Single) ->
    SingleStr = to_str(Single),
    SingleStr.

to_str(X) when is_list(X)   -> 
    X;
to_str(X) when is_atom(X)   -> 
    atom_to_list(X);
to_str(X) when is_float(X)  ->
    lists:flatten(io_lib:format("~.2f", [X]));
to_str(X) when is_integer(X) -> 
    integer_to_list(X).
