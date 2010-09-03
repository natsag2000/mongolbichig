-record(dotglyph,
        {
          cluster_name="",
          label="",
          icon_name="",
          color="black"
         }).

-define(cluster_name(V), "cluster_"++V).
-define(icon_name(V), V++"_icon").
-define(color_black, "black").
-define(color_green, "green").
-define(color_blue, "blue").
-define(color_orange, "orange").
