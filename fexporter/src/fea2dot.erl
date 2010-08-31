-module(fea2dot).
-compile([export_all]).

export_dot({feature, Name, Lookups}, FeatureFolder, TargetFolder) ->
    io:format("Feature: ~p, feature count:~p~n", [Name, length(Lookups)]).
