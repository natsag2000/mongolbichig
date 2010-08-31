-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1]).
-include("../include/features.hrl").


export_dot({feature, Name, Lookups}, FeatureFolder, TargetFolder) ->
    io:format("Feature: ~p, feature count:~p~n", [Name, length(Lookups)]),
    {ok, created} = create_folder(filename:join([TargetFolder, Name])),
    export_dot_lookup(Lookups, FeatureFolder, []).

export_dot_lookup([], F, Buf) ->
    {ok, Buf};
export_dot_lookup([C|Rest], F, Buf) ->
    Name = C#lookup.name,
    Tables = C#lookup.lookups,
    {dot, Dot} = generate_dot(Tables, F, []),
    export_dot_lookup(Rest, F, [{dot, Dot}|Buf]).

generate_dot([], F, Buf) ->
    {dot, Buf};
generate_dot([C|Rest], F, Buf) ->
    Subs = C#lookuptable.sub,
    Bys = C#lookuptable.by.
TO BE DONE....HAVE TO SLEEP :(
