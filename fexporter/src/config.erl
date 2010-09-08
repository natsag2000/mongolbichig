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

get_export_type() ->
    {ok, Terms} = read(),
    get(export_type, Terms).

get_export_class_fullpath() ->
    {ok, Terms} = read(),
    {ok, Root} = get(root_folder, Terms),
    {ok, ExFolder } = get(export_folder, Terms),
    ExportFolder = filename:join([Root, ExFolder, "classes"]),
    {ok, ExportFolder}.

get_root_folder() ->
    {ok, Terms} = read(),
    get(root_folder, Terms).

