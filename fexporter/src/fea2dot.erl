-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1]).
-include("../include/features.hrl").
-include("../include/fea2dot.hrl").

export_dot({feature, Name, Lookups}, FeatureFolder, TargetFolder) ->
    io:format("Feature: ~p, feature count:~p~n", [Name, length(Lookups)]),
    {ok, created} = create_folder(filename:join([TargetFolder, Name])),
    export_dot_lookup(Lookups, FeatureFolder, []),
    %%% TEMP
    generate_dot([], [], []).

export_dot_lookup([], _F, Buf) ->
    {ok, Buf};
export_dot_lookup([C|Rest], F, Buf) ->
    Name = C#lookup.name,
    Tables = C#lookup.lookups,
    {dot, Dot} = generate_dot(Tables, F, []),
    export_dot_lookup(Rest, F, [{dot, Dot}|Buf]).

%% generate dot file
generate_dot([], F, Buf) ->
    {dot, Buf};
generate_dot([C|Rest], F, Buf) ->
    Subs = C#lookuptable.sub,
    Bys = C#lookuptable.by. %% HERE HERE

%%
generate([], Buf) ->
    {ok, Buf};
generate([{?ampers, Feature}|Rest], Buf) ->
    generate(Rest, Buf);
generate([{?amperaphost, Feature}|Rest], Buf) ->
    generate(Rest, Buf);
generate([{?multiple, Features}|Rest], Buf) ->
    generate(Rest, Buf);
generate([{?multipleaphost, Features}|Rest], Buf) ->
    generate(Rest, Buf);
generate([{?aphost, Feature}|Rest], Buf) ->
    {ok, Glyph} = general_dotglyph(Feature, true),
    generate(Rest, [Glyph|Buf]);
generate([{?normal, Feature}|Rest], Buf) ->
    {ok, Glyph} = general_dotglyph(Feature, false),
    generate(Rest, [Glyph|Buf]).

%% generate dotglyph record from glyph name
general_dotglyph(Feature, false) ->
    {ok, Norm} = normalize_name(Feature),
    {ok, #dotglyph{cluster_name=?cluster_name(Norm),
              label=Feature,
              icon_name=?icon_name(Norm)}};

general_dotglyph(Feature, true) ->
    {ok, Norm} = normalize_name(Feature),
    {ok, #dotglyph{cluster_name=?cluster_name(Norm),
              label=Feature,
              icon_name=?icon_name(Norm),
              is_aphost=true}}.

%% normalize name
normalize_name(Name) ->
    normalize_name(Name, []).
normalize_name([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_name("."++Rest, Acc) ->
    normalize_name(Rest, Acc);
normalize_name([C|Rest], Acc) ->
    normalize_name(Rest, [C|Acc]).
