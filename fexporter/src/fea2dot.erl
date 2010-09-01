-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1, read_class/1]).
-include("../include/features.hrl").
-include("../include/fea2dot.hrl").

export_dot(Test, ClassFolder, TargetFolder) ->
    {feature, Name, Lookups} = Test,
    io:format("FEAture: ~p, feature count:~p~n", [Name, length(Lookups)]),
    Folder = filename:join([TargetFolder, Name]),
    io:format("Creating: ~p~n", [Folder]),
    {ok, created} = create_folder(Folder),
    export_dot_lookup(Lookups, ClassFolder, []).


export_dot_lookup([], _CF, Buf) ->
    {ok, lists:reverse(Buf)};
export_dot_lookup([C=#lookup{name=Name, lookups=Tables}|Rest], CF, Buf) ->
    %Name = C#lookup.name,
    %Tables = C#lookup.lookups,
    io:format("Count: ~p~n", [length(Tables)]),
    {ok, Dot} = generate_dot(Tables, CF, []),
    export_dot_lookup(Rest, CF, [{dot, Dot}|Buf]).

%% generate dot file
generate_dot([], _CF, Buf) ->
    {dot, Buf};
generate_dot([C|Rest], CF, Buf) ->
    Subs = C#lookuptable.sub,
    Bys = C#lookuptable.by,
    io:format("Subs count: ~p", [length(Bys)]),
    {ok, ESubs} = generate(Subs, CF),
    generate(Bys, CF).

%% generate export ready records for glyph names and classes
generate(Glyphs, CF) ->
    generate(Glyphs, CF, []).
generate([], _CF, Buf) ->
    {ok, lists:reverse(Buf)};
generate([{?ampers, Feature}|Rest], CF, Buf) ->
    {ok, CGlyphs} = read_class(filename:join([CF,Feature])),
    {ok, CGlyphs1} = generate_multi(CGlyphs, false),
    generate(Rest, CF, [{multi, CGlyphs1} | Buf]);
generate([{?amperaphost, Feature}|Rest], CF, Buf) ->
    {ok, CGlyphs} = read_class(filename:join([CF,Feature])),
    {ok, CGlyphs1} = generate_multi(CGlyphs, true),
    generate(Rest, CF, [{multi, CGlyphs1} | Buf]);
generate([{?multiple, Features}|Rest], CF, Buf) ->
    {ok, Multiglyphs} = generate_multi(Features, false),
    generate(Rest, CF, [{multi, Multiglyphs} | Buf]);
generate([{?multipleaphost, Features}|Rest], CF, Buf) ->
    {ok, Multiglyphs} = generate_multi(Features, true),
    generate(Rest, CF, [{multi, Multiglyphs} | Buf]);
generate([{?aphost, Feature}|Rest], CF, Buf) ->
    {ok, Glyph} = general_dotglyph(Feature, true),
    generate(Rest, CF, [Glyph|Buf]);
generate([{?normal, Feature}|Rest], CF, Buf) ->
    {ok, Glyph} = general_dotglyph(Feature, false),
    generate(Rest, CF, [Glyph|Buf]);
generate([Feature|Rest], CF, Buf) ->
    {ok, Glyph} = general_dotglyph(Feature, false),
    generate(Rest, CF, [Glyph|Buf]).

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

%% generate dotglyph record from multiple glyph names
generate_multi(Features, Is_Colored) ->
    generate_multi(Features, [], Is_Colored).

generate_multi([], Buf, _Colored) ->
    {ok, lists:reverse(Buf)};
generate_multi([C|Rest], Buf, Colored) ->
    {ok, Glyph} = general_dotglyph(C, Colored),
    generate_multi(Rest, [Glyph|Buf], Colored).

%% normalize name
normalize_name(Name) ->
    normalize_name(Name, []).
normalize_name([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_name("."++Rest, Acc) ->
    normalize_name(Rest, Acc);
normalize_name([C|Rest], Acc) ->
    normalize_name(Rest, [C|Acc]).
