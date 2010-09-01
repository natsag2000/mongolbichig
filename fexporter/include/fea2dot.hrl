-record(dotglyph,
        {
          cluster_name="",
          label="",
          icon_name="",
          is_aphost=false
         }).

-define(cluster_name(V), "cluster_"++V).
-define(icon_name(V), V++"_icon").
