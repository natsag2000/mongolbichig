-module(config).
-compile([export_all]).

read(File) ->
    file:consult(File).

get(_Key, []) ->
    {error, not_found};
get(Key, [{Key, Value} | _Cfg]) ->
    {ok, Value};
get(Key, [{_K, _V} | Cfg]) ->
    get(Key, Cfg).
