-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1, read_class/1]).
-import(config, [get_glyph_path/0]).
-include("../include/features.hrl").
-include("../include/fea2dot.hrl").
-include("../include/dottext.hrl").

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
    ok = file:close(FileDescription),
    {ok, done}.

write_dotI([], _FD) ->
    {ok, done};
write_dotI([{dot, Subs, Bys}|Rest], FD) ->
    {ok, done} = write_dot_subgraphs(Subs, FD),
    {ok, done} = write_dot_subgraphs(Bys, FD),
    {ok, done} = write_dot_connect(Subs, Bys, FD),
    write_dotI(Rest, FD).

write_dot_subgraphs([], _FD) ->
    {ok, done};
write_dot_subgraphs([H|T], FD) ->
    {ok, done} = write_dot_nodes(H, FD),

    write_dot_subgraphs(T, FD).

write_dot_nodes(_H=#dotglyph{cluster_name=ClusterName,
                           label=Label,
                           icon_name=IconName,
                            is_aphost=_IsAphost}, FD) ->
    ok = io:format(FD, ?dot_subgraph(ClusterName), []),
    ok = io:format(FD, ?dot_sub_label(Label), []),
    ok = io:format(FD, ?dot_labelloc, []),
    ok = io:format(FD, ?dot_icon_name(IconName), []),
    ImagePath = filename:join([get_glyph_path(), Label++".png" ]),
    ok = io:format(FD, ?dot_icon(IconName, ImagePath), []),
    {ok, done};
%% multi dot!!!
write_dot_nodes({multi, Glyphs}, FD) ->
    FirstGlyph=lists:nth(1, Glyphs),
    NewSubgraphName = lists:append("sub_", FirstGlyph#dotglyph.cluster_name),
    ok = io:format(FD, ?dot_subgraph(NewSubgraphName), []),
    {ok, done} = write_dot_subgraphs(Glyphs, FD),
    ok = io:format(FD, ?dot_end_graph, []),
    {ok, done}.

%% dot connection nodes
write_dot_connect(S, B, FD) ->
    {ok, L} = read_first_element(S),
    {ok, L1} = read_first_element(B),
    write_connect(lists:append(L, L1), FD).

%% read only first element
read_first_element(List) ->
    read_first_element(List, []).
read_first_element([], Acc) ->
    lists:reverse(Acc);
read_first_element([_H = #dotglyph{icon_name=IconName}|T], Acc) ->
    read_first_element(T, [IconName|Acc]);
read_first_element([{multi, Glyph}|T], Acc) ->
    F = lists:nth(1, Glyph),
    read_first_element(T, [F#dotglyph.icon_name|Acc]).

%% make dot connection from list
write_connect([H|T], FD) when length(T) == 1 ->
    Last = lists:nth(1, T),
    ok = io:format(FD, ?dot_last_part(H,Last), []),
    {ok, done};
write_connect([H|T], FD) ->
    Next = lists:nth(1, T),
    ok = io:format(FD, ?dot_node_conn(H,Next), []),
    write_connect(T, FD).
%% get first element
get_first_element(_El=#dotglyph{cluster_name=CName}) ->
    CName;
get_first_element({multi, Glyphs}) ->
    G = lists:nth(1, Glyphs),
    take_name(G).

take_name(_C = #dotglyph{icon_name=Iname}) ->
    Iname;
take_name(Iname) ->
    Iname.

%% insert head  and footer part
write_dot_head(FD) ->
    ok = io:format(FD, ?dot_head_part, []),
    {ok, done}.
write_dot_footer(FD) ->
    ok = io:format(FD, ?dot_footer, []),
    {ok, done}.
