-module(fea2dot).
-compile([export_all]).
-import(utils, [create_folder/1, get_list_from_file/1, get_uniq_name/2, get_random_str/1, get_group_name/1]).
-import(config, [get_glyph_path/0, get_export_type/0, get_export_class_fullpath/0]).
-import(fea2html, [write_list_glyphs_to_html/3]).
-include("../include/features.hrl").
-include("../include/fea2dot.hrl").
-include("../include/dottext.hrl").

-define(new_name(N,C), lists:flatten(N++"_"++integer_to_list(C))).
-define(random_name(S,N), lists:flatten(S++get_random_str(N))).

%% API zone
%% ========
export_dot({feature, Name, Lookups}, ClassFolder, TargetFolder) ->
    ExportFolder = filename:join([TargetFolder, Name]),
    {ok, created} = create_folder(ExportFolder),
    export_dot_lookup(Lookups, ClassFolder, ExportFolder).

export_class_dot(ClassFile, ExportFolder) ->
    io:format("~p::~p", [ClassFile, ExportFolder]),
    ExportClassFolder = filename:join([ExportFolder, lists:flatten("classes")]),
    io:format("~p~n", [ExportClassFolder]),
    {ok, created} = create_folder(ExportClassFolder),
    {ok, Glyphs} = get_list_from_file(ClassFile),
    Basename = filename:basename(ClassFile),
    {ok, done} = write_list_glyphs_to_dot(Glyphs, Basename, ExportClassFolder).

%% Local zone
%% ==========

write_list_glyphs_to_dot(List, Basename, ExportFolder) ->
    {ok, GroupList} = split_glyph_list(10, List, Basename),
    {ok, FormedGlyphs} = form_list_glyph(GroupList),
    {ok, done} = write_class_dot(FormedGlyphs, ExportFolder),
    {ok, done}.

write_class_dot([], _EF) ->
    {ok, done};
write_class_dot([{?group_glyph, Name, DotGlyphs}|T], EF) ->
    Filename = lists:flatten(filename:join(EF, Name++".dot")),
    {ok, FileDescription} = file:open(Filename, [write]),
    {ok, done} = write_dot_head_tb(FileDescription),
    {ok, done} = write_dot_list(DotGlyphs, FileDescription),
    {ok, done} = write_dot_footer(FileDescription),
    ok = file:close(FileDescription),
    write_class_dot(T, EF).

%% write dot formated list
write_dot_list([], _) ->
    {ok, done};
write_dot_list([H|T], FD) ->
    {ok, done} = write_dot_nodes(H, FD),
    write_dot_list(T, FD).

%% dot form a list glyph
form_list_glyph(List) ->
    form_list_glyph(List, []).
form_list_glyph([], Buf) ->
    {ok, lists:reverse(Buf)};
form_list_glyph([{?group_glyph, Name, Glyphs}|T], Buf) ->
    NGlyphs = [{?normal, X} || X <- Glyphs],
    {ok, GGlyphs, _} = generate(NGlyphs, "", [], false),
    form_list_glyph(T, [{?group_glyph, Name, GGlyphs}|Buf]).

%% split a list of glyphs with N
split_glyph_list(N, Glyphs, Name) ->
    split_glyph_list(N, Glyphs, [], Name, 0).
split_glyph_list(_N, [], Buf, _Name, _Counter) when length(Buf) > 0 ->
    {ok, lists:reverse(Buf)};
split_glyph_list(N, Glyphs, _Buf, Name, 0) when N > length(Glyphs) ->
    {ok, [{?group_glyph, Name, Glyphs}]};
split_glyph_list(N, Glyphs, Buf, Name, Counter) when N > length(Glyphs) ->
    {ok, lists:reverse([{?group_glyph, ?new_name(Name, Counter+1), Glyphs}|Buf])};
split_glyph_list(N, Glyphs, Buf, Name, Counter) ->
    {L1, L2} = lists:split(N, Glyphs),
    NewCounter = Counter + 1,
    split_glyph_list(N, L2,
                     [{?group_glyph, ?new_name(Name, NewCounter), L1}|Buf],
                     Name, NewCounter).

%% lookups
export_dot_lookup([], _CF, _EF) ->
    {ok, done};
export_dot_lookup([_C=#lookup{name=Name, lookups=Tables}|Rest], CF, EF) ->
    {ok, Dots} = generate_dot(Tables, EF),
    {ok, done} = write_dot(Dots, filename:join([EF, Name++".dot"])),
    export_dot_lookup(Rest, CF, EF).

%% generate dot file
generate_dot(Tables, EF) ->
    generate_dot(Tables, EF, [], []).
generate_dot([], _EF, Buf, _IList) ->
    {ok, lists:reverse(Buf)};
generate_dot([C|Rest], EF, Buf, IList) ->
    Subs = C#lookuptable.sub,
    Bys = C#lookuptable.by,
    {ok, ESubs, IconList} = generate(Subs, EF, IList, false),
    {ok, EBys, IconList1} = generate(Bys, EF, IconList, true),
    case length(Buf) of
        0 -> generate_dot(Rest, EF, [{dot, ESubs, EBys}], IconList1);
        _ -> generate_dot(Rest, EF, [{dot, ESubs, EBys}|Buf], IconList1)
    end.

%% generate export ready records for glyph names and classes
generate(Glyphs, EF, IList, IsBy) ->
    generate(Glyphs, EF, [], IList, IsBy).
generate([], _EF, Buf, IList, _IsBy) ->
    {ok, lists:reverse(Buf), IList};
generate([{?ampers, Feature}|Rest], EF, Buf, IList, IsBy) ->
    {ok, NList, CGlyph} = make_class(Feature, get_color_if_by_multi(IsBy), IList, EF),
    generate(Rest, EF, [ CGlyph|Buf], NList, IsBy);
generate([{?amperaphost, Feature}|Rest], EF, Buf, IList, IsBy) ->
    {ok, NList, CGlyph} = make_class(Feature, ?color_green, IList, EF),
    generate(Rest, EF, [ CGlyph|Buf], NList, IsBy);
generate([{?multiple, Features}|Rest], EF, Buf, IList, IsBy) ->
    {ok, Basename} = get_group_name(EF),
    case get_export_type() of
        {ok, dot} ->
            {ok, done} = write_list_glyphs_to_dot(Features, Basename, EF);
        {ok, html} ->
            {ok, done} = write_list_glyphs_to_html(Features, Basename, EF)
    end,
    Color = get_color_if_by(IsBy),
    {ok, NList, CGlyph} = make_group(Basename, Features, Color, IList),
    generate(Rest, EF, [CGlyph | Buf], NList, IsBy);
generate([{?multipleaphost, Features}|Rest], EF, Buf, IList, IsBy) ->
    {ok, Basename} = get_group_name(EF),
    case get_export_type() of
        {ok, dot} ->
            {ok, done} = write_list_glyphs_to_dot(Features, Basename, EF);
        {ok, html} ->
            {ok, done} = write_list_glyphs_to_html(Features, Basename, EF)
    end,
    {ok, NList, CGlyph} = make_group(Basename, Features, ?color_green, IList),
    generate(Rest, EF, [CGlyph | Buf], NList, IsBy);
generate([{?aphost, Feature}|Rest], EF, Buf, IList, IsBy) ->
    {ok, NList, Glyph} = general_dotglyph(Feature, ?color_green, IList),
    generate(Rest, EF, [Glyph|Buf], NList, IsBy);
generate([{?normal, Feature}|Rest], EF, Buf, IList, IsBy) ->
    Color = get_color_if_by(IsBy),
    {ok, NList, Glyph} = general_dotglyph(Feature, Color, IList),
    generate(Rest, EF, [Glyph|Buf], NList, IsBy);
generate([Feature|Rest], EF, Buf, IList, IsBy) ->
    Color = get_color_if_by(IsBy),
    {ok, NList, Glyph} = general_dotglyph(Feature, Color, IList),
    generate(Rest, EF, [Glyph|Buf],  NList, Color).

make_class(ClassName, Color, IList, EF) ->
    case get_export_type() of
        {ok, dot } ->
            group_dotglyph(lists:flatten("@"++ClassName), Color, IList);
        {ok, html} ->
            {ok, FullClassPath} = get_export_class_fullpath(),
            Filename = filename:join([FullClassPath, ClassName]),
            {ok, Glyphs} = get_list_from_file(Filename),
            {ok, IList, {multi,"@"++ClassName, Color, Glyphs}}
    end.

make_group(Name, List, Color, IList) ->
    case get_export_type() of
        {ok, dot} ->
            group_dotglyph(lists:flatten(Name), Color, IList);
        {ok, html} ->
            {ok, IList, {multi, Name, Color, List}}
    end.

get_color_if_by_multi(true) ->
    ?color_blue;
get_color_if_by_multi(_) ->
    ?color_orange.
get_color_if_by(true) ->
    ?color_blue;
get_color_if_by(_) ->
    ?color_black.

%% generate dotglyph record from glyph name % ?? TODO::not so clean
general_dotglyph(Feature, Color, IList) ->
    {ok, Norm, NList} = normalize_name(Feature, IList),
    {ok, NList, #dotglyph{cluster_name=?cluster_name(Norm),
                          label=Feature,
                          icon_name=?icon_name(Norm),
                         color=Color}}.

group_dotglyph(Feature, Color, IList) ->
    GroupName = lists:flatten("group"++get_random_str(7)),
    {ok, IList, #dotglyph{cluster_name=?cluster_name(GroupName),
                          label=Feature,
                          icon_name=?icon_name(GroupName),
                          color=Color}}.

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
                             color=Color}, FD) ->
    ok = io:format(FD, ?dot_subgraph(ClusterName), []),
    ok = io:format(FD, ?dot_sub_label(Label), []),
    ok = io:format(FD, ?dot_labelloc, []),
    ok = io:format(FD, ?dot_sub_color(Color), []),
    ok = io:format(FD, ?dot_icon_name(IconName), []),
    ImagePath = filename:join([get_glyph_path(), Label++".png" ]),
    ok = io:format(FD, ?dot_icon(IconName, ImagePath), []),
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
write_dot_head_tb(FD) ->
    ok = io:format(FD, ?dot_head_part_tb, []),
    {ok, done}.
write_dot_head(FD) ->
    ok = io:format(FD, ?dot_head_part, []),
    {ok, done}.
write_dot_footer(FD) ->
    ok = io:format(FD, ?dot_footer, []),
    {ok, done}.
