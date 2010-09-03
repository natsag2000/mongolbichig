-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1, read_class/1, get_uniq_name/2]).
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
    {ok, done} = write_dot(Dots, filename:join([EF, Name++".dot"])),
    export_dot_lookup(Rest, CF, EF).

%% generate dot file
generate_dot([], _CF, Buf) ->
    {ok, lists:reverse(Buf)};
generate_dot([C|Rest], CF, Buf) ->
    Subs = C#lookuptable.sub,
    Bys = C#lookuptable.by,
    {ok, ESubs, IconList} = generate(Subs, CF, []),
    {ok, EBys, _IList} = generate(Bys, CF, IconList),
    case length(Buf) of
        0 -> generate_dot(Rest, CF, [{dot, ESubs, EBys}]);
        _ -> generate_dot(Rest, CF, [{dot, ESubs, EBys}|Buf])
    end.

%% generate export ready records for glyph names and classes
generate(Glyphs, CF, IList) ->
    generate(Glyphs, CF, [], IList).
generate([], _CF, Buf, IList) ->
    {ok, lists:reverse(Buf), IList};
generate([{?ampers, Feature}|Rest], CF, Buf, IList) ->
    {ok, CGlyphs} = read_class(filename:join([CF,Feature])),
    {ok, NList, CGlyphs1} = generate_multi(CGlyphs, false, IList),
    generate(Rest, CF, [{class, Feature, CGlyphs1} | Buf], NList);
generate([{?amperaphost, Feature}|Rest], CF, Buf, IList) ->
    {ok, CGlyphs} = read_class(filename:join([CF,Feature])),
    {ok, NList, CGlyphs1} = generate_multi(CGlyphs, true, IList),
    generate(Rest, CF, [{class, Feature, CGlyphs1} | Buf], NList);
generate([{?multiple, Features}|Rest], CF, Buf, IList) ->
    {ok, NList, Multiglyphs} = generate_multi(Features, false, IList),
    generate(Rest, CF, [{multi, Multiglyphs} | Buf], NList);
generate([{?multipleaphost, Features}|Rest], CF, Buf, IList) ->
    {ok, NList, Multiglyphs} = generate_multi(Features, true, IList),
    generate(Rest, CF, [{multi, Multiglyphs} | Buf], NList);
generate([{?aphost, Feature}|Rest], CF, Buf, IList) ->
    {ok, NList, Glyph} = general_dotglyph(Feature, true, IList),
    generate(Rest, CF, [Glyph|Buf], NList);
generate([{?normal, Feature}|Rest], CF, Buf, IList) ->
    {ok, NList, Glyph} = general_dotglyph(Feature, false, IList),
    generate(Rest, CF, [Glyph|Buf], NList);
generate([Feature|Rest], CF, Buf, IList ) ->
    {ok, NList, Glyph} = general_dotglyph(Feature, false, IList),
    generate(Rest, CF, [Glyph|Buf], NList).

%% generate dotglyph record from glyph name
general_dotglyph(Feature, false, IList) ->
    {ok, Norm, NList} = normalize_name(Feature, IList),
    {ok, NList, #dotglyph{cluster_name=?cluster_name(Norm),
                          label=Feature,
                          icon_name=?icon_name(Norm)}};

general_dotglyph(Feature, true, IList) ->
    {ok, Norm, NList} = normalize_name(Feature, IList),
    {ok, NList, #dotglyph{cluster_name=?cluster_name(Norm),
                          label=Feature,
                          icon_name=?icon_name(Norm),
                          is_aphost=true}}.

%% generate dotglyph record from multiple glyph names
generate_multi(Features, Is_Colored, IList) ->
    generate_multi(Features, [], Is_Colored, IList).

generate_multi([], Buf, _Colored, IList) ->
    {ok, IList, lists:reverse(Buf)};
generate_multi([C|Rest], Buf, Colored, IList) ->
    {ok, NList, Glyph} = general_dotglyph(C, Colored, IList),
    generate_multi(Rest, [Glyph|Buf], Colored, NList).

%% normalize name
normalize_name(Name, IList) ->
    normalize_name(Name, [], IList).
normalize_name([], Acc, IList) ->
    NName = lists:reverse(Acc),
    {ok, UName, NList} = get_uniq_name(NName, IList),
    {ok, UName, NList};
normalize_name("."++Rest, Acc, IList) ->
    normalize_name(Rest, Acc, IList);
normalize_name([C|Rest], Acc, IList) ->
    normalize_name(Rest, [C|Acc], IList).

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
    ok = io:format(FD, ?dot_sub_label(lists:flatten("LIST")), []),
    ok = io:format(FD, ?dot_compound, []),
    ok = io:format(FD, ?dot_rankdir_tb, []),
    ok = io:format(FD, ?dot_labelloc_top, []),
    {ok, done} = write_dot_subgraphs(Glyphs, FD),
    ok = io:format(FD, ?dot_end_graph, []),
    {ok, done};
%% class file
write_dot_nodes({class, CName, Glyphs}, FD) ->
    FirstGlyph=lists:nth(1, Glyphs),
    NewSubgraphName = lists:append("sub_", FirstGlyph#dotglyph.cluster_name),
    ok = io:format(FD, ?dot_subgraph(NewSubgraphName), []),
    ok = io:format(FD, ?dot_sub_label("@"++CName), []),
    ok = io:format(FD, ?dot_compound, []),
    ok = io:format(FD, ?dot_rankdir_tb, []),
    ok = io:format(FD, ?dot_labelloc_top, []),
    {ok, done} = write_dot_subgraphs(Glyphs, FD),
    ok = io:format(FD, ?dot_end_graph, []),
    {ok, done}.

%% dot connection nodes
write_dot_connect(S, B, FD) ->
    {ok, L} = read_first_elements(S),
    {ok, L1} = read_first_elements(B),
    write_connect(lists:append(L, L1), FD).

%% read only first element
read_first_elements(List) ->
    read_first_elements(List, []).
read_first_elements([], Acc) ->
    {ok, lists:reverse(Acc)};
read_first_elements([_H = #dotglyph{icon_name=IconName}|T], Acc) ->
    read_first_elements(T, [IconName|Acc]);
read_first_elements([{class, _CName, Glyph}|T], Acc) ->
    F = lists:nth(1, Glyph),
    read_first_elements(T, [F#dotglyph.icon_name|Acc]);
read_first_elements([{multi, Glyph}|T], Acc) ->
    F = lists:nth(1, Glyph),
    read_first_elements(T, [F#dotglyph.icon_name|Acc]).

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
