-module(main).
-compile([export_all]).
-import(config, [read/1, get/2]).
-import(fextractor, [extract_feature/1]).
-import(fea2dot, [export_dot/3]).

-define(configfile, "../config/application.cfg").
-define(testfile, "calt").

start() ->
    {ok, Terms} = read(?configfile),
    {ok, Root } = get(root_folder, Terms),
    {ok, FFolder} = get(feature_folder, Terms),
    {ok, EFolder} =get(export_folder, Terms),
    Filename = filename:join([ Root, FFolder, ?testfile]),
    io:format("FFolder: ~p~nEFolder: ~p~nFilename: ~p~n",
              [FFolder, EFolder, Filename]),
    {ok, {feature, Name, Features}} = extract_feature(Filename),
    io:format("Feature: ~p, feature count:~p~n", [Name, length(Features)]),
    export_dot({feature, Name, Features}, FFolder, EFolder).

