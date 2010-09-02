-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1, read_class/1]).
-include("../include/features.hrl").
-include("../include/fea2dot.hrl").

export_dot({feature, Name, Lookups}, ClassFolder, TargetFolder) ->
    ExportFolder = filename:join([TargetFolder, Name]),
    {ok, created} = create_folder(ExportFolder),
    export_dot_lookup(Lookups, ClassFolder, ExportFolder).

export_dot_lookup([], _CF, _EF) ->
    io:format("FINISH"),
    {ok, done};
export_dot_lookup([_C=#lookup{name=Name, lookups=Tables}|Rest], CF, EF) ->
    {ok, Dots} = generate_dot(Tables, CF, []),
    {ok, done} = write_dot(Dots, filename:join([EF, Name])),
    export_dot_lookup(Rest, CF, EF).

%% generate dot file
generate_dot([], _CF, Buf) ->
    {ok, lists:reverse(Buf)};
generate_dot([C|Rest], CF, Buf) ->
    Subs = C#lookuptable.sub,
    Bys = C#lookuptable.by,
    {ok, ESubs} = generate(Subs, CF),
    {ok, EBys} = generate(Bys, CF),
    case length(Buf) of
        0 -> generate_dot(Rest, CF, [{dot, ESubs, EBys}]);
        _ -> generate_dot(Rest, CF, [{dot, ESubs, EBys}|Buf])
    end.

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

%% write to dot file
write_dot(Dots, Filename) ->
    {ok, FileDescription} = file:open(Filename, [write]),
    {ok, done} = write_dot_head(FileDescription),
    {ok, done} = write_dotI(Dots, FileDescription),
    {ok, done} = write_dot_footer(FileDescription),
    file:close(FileDescription),
    {ok, done}.

write_dotI([], _FD) ->
    {ok, done};
write_dotI([{dot, Bys, Subs}|Rest], FD) ->
    {ok, done} = write_dot_file(Subs, FD),
%%%% HERE MUST DOT FORMATED!!
    write_dotI(Rest, FD).

write_dot_file([], _FD) ->
    {ok, done};
write_dot_file([H|T], FD) ->
    ok = io:format(FD, "Test ~w", [H]),
    write_dot_file(T, FD).
