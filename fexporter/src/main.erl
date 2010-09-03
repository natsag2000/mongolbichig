%%  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
%% |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
%% | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
%% | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
%% |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
%%

-module(main).
-export([start/0]).

-import(config, [read/0, get/2]).
-import(fextractor, [extract_feature/1]).
-import(fea2dot, [export_dot/3, export_class_dot/2]).


-define(testfile, "calt").

start() ->
    {ok, Terms} = read(),
    {ok, Root } = get(root_folder, Terms),
    {ok, FFolder} = get(feature_folder, Terms),
    {ok, CFolder} = get(class_folder, Terms),
    {ok, EFolder} =get(export_folder, Terms),
    {ok, FList} = file:list_dir(filename:join(Root, FFolder)),
    {ok, CList} = file:list_dir(filename:join(Root, CFolder)),
    {ok, done} = class_export(CList, Root, CFolder, EFolder),
    doit(FList, Root, FFolder, CFolder, EFolder).

doit([], _, _, _, _) ->
    done;
doit(["aalt"|Rest], R, FF, CF, EF) ->
    doit(Rest, R, FF, CF, EF);
doit([Filename|Rest], R, FF, CF, EF) ->
    FFile = filename:join([R,FF,Filename]),
    {ok, {feature, Name, Features}} = extract_feature(FFile),
    io:format("Feature: ~p, feature count:~p~n", [Name, length(Features)]),
    export_dot({feature, Name, Features},
               filename:join([R, CF]),
               filename:join([R, EF])),
    doit(Rest, R, FF, CF, EF).

class_export([], _, _, _) ->
    io:format("Classes are exported!"),
    {ok, done};
class_export([Filename|Rest], R, CF, EF) ->
    CFile = filename:join([R,CF,Filename]),
    {ok, done} = export_class_dot(CFile, EF),
    class_export(Rest, R, CF, EF).
