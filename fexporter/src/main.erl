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
-import(fea2html, [export_html/3, export_class_html/2]).


-define(testfile, "calt").

start() ->
    {ok, Terms} = read(),
    {ok, Root } = get(root_folder, Terms),
    {ok, FFolder} = get(feature_folder, Terms),
    {ok, CFolder} = get(class_folder, Terms),
    {ok, EFolder} =get(export_folder, Terms),
    {ok, EType} =get(export_type, Terms),
    {ok, FList} = file:list_dir(filename:join(Root, FFolder)),
    {ok, CList} = file:list_dir(filename:join(Root, CFolder)),
    {ok, done} = class_export(CList, Root, CFolder, EFolder, EType),
    doit(FList, Root, FFolder, CFolder, EFolder, EType).

doit([], _, _, _, _, _) ->
    done;
doit(["aalt"|Rest], R, FF, CF, EF, EType) ->
    doit(Rest, R, FF, CF, EF, EType);
doit([Filename|Rest], R, FF, CF, EF, EType) ->
    FFile = filename:join([R,FF,Filename]),
    {ok, {feature, Name, Features}} = extract_feature(FFile),
    io:format("Feature: ~p, feature count:~p~n", [Name, length(Features)]),
    case EType of
        dot ->
            export_dot({feature, Name, Features},
                       filename:join([R, CF]),
                       filename:join([R, EF]));
        html ->
            export_html({feature, Name, Features},
                        filename:join([R, CF]),
                        filename:join([R, EF]))
    end,
    doit(Rest, R, FF, CF, EF, EType).

class_export([], _, _, _, _) ->
    io:format("Classes are exported!"),
    {ok, done};
class_export([Filename|Rest], R, CF, EF, EType) ->
    CFile = filename:join([R,CF,Filename]),
    ExFolder = filename:join([R,EF]),
    case EType of
        html ->
            {ok, done} = export_class_html(CFile, ExFolder);
        dot ->
            {ok, done} = export_class_dot(CFile, ExFolder)
    end,
    class_export(Rest, R, CF, EF, EType).
