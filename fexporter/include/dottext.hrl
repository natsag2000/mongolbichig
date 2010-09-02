%% nodes format:
% dot_subgraph
% dot_sub_label
% dot_label_loc
% dot_icon_name
% dot_icon
%% nodes
%% =====
-define(f(V), lists:flatten(V)).
-define(dot_head, ?f("digraph test {\n")).
-define(dot_compound, ?f("  compound=true;\n")).
-define(dot_rankdir, ?f("  rankdir=LR;\n")).
-define(dot_rankdir_tb, ?f("  rankdir=TB;\n")).
-define(dot_node, ?f("  node [shape=plaintext];\n")).
-define(dot_edge, ?f("  edge [arrowsize=0, style=invisible];\n")).

-define(dot_head_part, lists:append([?dot_head,
                                     ?dot_compound,
                                     ?dot_rankdir,
                                     ?dot_node,
                                     ?dot_edge
                                    ])).

-define(dot_footer, ?f("}")).
-define(dot_end_graph, ?f("  };\n")).

-define(dot_subgraph(V), ?f(io_lib:format("  subgraph ~p {\n", [V]))).
-define(dot_sub_label(V), ?f(io_lib:format("    label=~p;\n", [V]))).
-define(dot_labelloc, ?f("    labelloc=b;\n")).
-define(dot_icon_name(V), ?f(io_lib:format("    ~p};\n", [V]))).
-define(dot_icon(V1,V2), ?f(io_lib:format("    ~p [ shape=box, style=invisible, shapefile=~p];\n", [V1,V2]))).

%% edges
%% =====
-define(dot_node_conn(V1,V2), ?f(io_lib:format("  ~p -> ~p;\n",[V1, V2]))).
-define(dot_last_part(V1,V2), ?f(io_lib:format("  ~p -> ~p [arrowsize=1, color=black, arrowhead=\"vee\"];\n",[V1, V2]))).
