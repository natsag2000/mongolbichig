-module(config).
-compile([export_all]).

-define(configfile, "../config/application.cfg").

read() ->
    file:consult(?configfile).

get(_Key, []) ->
    {error, not_found};
get(Key, [{Key, Value} | _Cfg]) ->
    {ok, Value};
get(Key, [{_K, _V} | Cfg]) ->
    get(Key, Cfg).

get_glyph_path() ->
    {ok, Terms} = read(),
    {ok, Root} = get(root_folder, Terms),
    {ok, Gpath} = get(glyph_folder, Terms),
    filename:join([Root, Gpath]).
