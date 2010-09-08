%% nodes format:
% dot_subgraph
% dot_sub_label
% dot_label_loc
% dot_icon_name
% dot_icon
%% nodes
%% =====
-define(f(V), lists:flatten(V)).
-define(table_head, ?f("<table><tr></tr>\n")).
-define(table_footer, ?f("</table>")).
-define(table_row_start, ?f("  <tr>\n")).
-define(table_data(V1,V2,V3), ?f("    <td valign=top class="++V3++" width=60><img src=../png/"++V1++" border=0/><br/>"++V2++"</td>\n")).
-define(table_row_end, ?f("  </tr>\n")).

-define(table_list_data_hd(V), ?f("    <td valign=top class="++V++">\n")).
-define(table_list_data_ft, ?f("    </td>\n")).
-define(table_inner_head(V), ?f("        <table><tr>"++V++"</tr>\n")).
-define(table_inner_data(V1,V2), ?f("        <td valign=top class=multi width=60><img src=../png/"++V1++" border=0/><br/>"++V2++"</td>\n")).
-define(table_inner_foot, ?f("        </table>\n")).
