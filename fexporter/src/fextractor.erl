%%  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
%% |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
%% | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
%% | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
%% |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
%%
%% feature extractor
%%
-module(fextractor).
-export([extract_feature/1]).
-include("../include/features.hrl").

extract_feature(File) ->
    {ok, Content} = file:read_file(File),
    {ok, BContent, Feature} = extract_feature_body(Content, [], [], #feature{}, start),
    extract_feature_lookups(list_to_binary(BContent), [], #lookup{}, Feature, start).
%% ----------------------------------------------------------------
%% extract feature body
%% ----------------------------------------------------------------
extract_feature_body(<<"feature ", Body/binary>>, Buf, Content, Features, start) ->
    extract_feature_body(Body, Buf, Content, Features, name);
extract_feature_body(<<" {", Body/binary>>, Buf, Content, Features, name) ->
    NewFeatures = Features#feature{name = lists:reverse(Buf)},
    extract_feature_body(Body, [], Content, NewFeatures, body);
extract_feature_body(<<Char, Body/binary>>, Buf, Content, Features, name) ->
    extract_feature_body(Body, [Char|Buf], Content, Features, name);
extract_feature_body(<<"} ", Body/binary>>, Buf, Content, Features, body) ->
    extract_feature_body(Body, Buf, Content, Features, end_name);
extract_feature_body(<<";", Body/binary>>, Buf, Content, Features, end_name) ->
    D = lists:reverse(Buf),
    case Features#feature.name of
        D ->
            {ok, lists:reverse(Content), Features};
        _ ->
            Spart = "} "++lists:reverse(Buf)++";",
            extract_feature_body(Body, [], [lists:reverse(Spart) | Content], Features, body)
    end;
extract_feature_body(<<Char, Body/binary>>, Buf, Content, Features, end_name) ->
    extract_feature_body(Body, [Char|Buf], Content, Features, end_name);
extract_feature_body(<<Char, Body/binary>>, Buf, Content, Features, body) ->
    extract_feature_body(Body, Buf, [Char | Content], Features, body).
%% ----------------------------------------------------------------
%% extract lookups
%% ----------------------------------------------------------------
extract_feature_lookups(<<"lookup ", Rest/binary>>, [], Lookup, Feature, start) ->
    extract_feature_lookups(Rest, [], Lookup, Feature, name);
extract_feature_lookups(<<_C, Rest/binary>>, _Buf, Lookup, Feature, start) ->
    extract_feature_lookups(Rest, [], Lookup, Feature, start);
extract_feature_lookups(<<>>, _Buf, _Lookup, Feature, start) ->
    {ok, Feature};
extract_feature_lookups(<<" {", Rest/binary>>, Buf, Lookup, Feature, name) ->
    NewLookup = Lookup#lookup{name=lists:reverse(Buf)},
    extract_feature_lookups(Rest, [], NewLookup, Feature, body);
%%% empty lookups! e.g lookup calt32;
extract_feature_lookups(<<";", Rest/binary>>, _Buf, _Lookup, Feature, name) ->
    extract_feature_lookups(Rest, [], #lookup{}, Feature, start);
extract_feature_lookups(<<C, Rest/binary>>, Buf, Lookup, Feature, name) ->
    extract_feature_lookups(Rest, [C|Buf], Lookup, Feature, name);
extract_feature_lookups(<<"}", Rest/binary>>, Buf, Lookup, Feature, body) ->
    {ok, Subtables} = extract_feature_table(lists:reverse(Buf)), %% !!!!
    Tables = Lookup#lookup.lookups,
    NewList = add_to_list(Subtables, Tables),
    NewLookup=Lookup#lookup{lookups=lists:reverse(NewList)},
    NewFeatureLookups = Feature#feature.lookups,
    NewFeature = Feature#feature{lookups=lists:reverse([NewLookup|NewFeatureLookups])},
    extract_feature_lookups(Rest, [], #lookup{}, NewFeature, start);
extract_feature_lookups(<<C,Rest/binary>>, Buf, Lookup, Feature, body) ->
    extract_feature_lookups(Rest, [C|Buf], Lookup, Feature, body).
%extract_feature_lookups(<<";", Rest/binary>>, Buf, Lookup, Feature, end_name) ->
%    extract_feature_lookups(Rest, [], #lookup{}, NewFeature, start);
%%extract_feature_lookups(<<_C,Rest/binary>>, Buf, Lookup, Feature, end_name) ->
%%    extract_feature_lookups(Rest, Buf, Lookup, Feature, end_name).

add_to_list(List, []) ->
    List;
add_to_list(List, L1) ->
    [List|L1].
%% ----------------------------------------------------------------
%% extract substitution table
%% ----------------------------------------------------------------
extract_feature_table(Subs) ->
    extract_sub_by(list_to_binary(Subs), []).

%% ----------------------------------------------------------------
%% extract sub XXX by YYY
%% NOTE: lookupflag ignored!!! TODO!!
%% ----------------------------------------------------------------
extract_sub_by(<<"sub", Rest/binary>>, LTable) ->
    {ok, Subpart, Rest1} = extract_sub(Rest, [], []),
    {ok, Byspart, Rest2} = extract_by(Rest1, [], []),
    Lkup = #lookuptable{sub=Subpart, by=Byspart},
    extract_sub_by(Rest2, [ Lkup | LTable]);
extract_sub_by(<<>>, LTable) ->
    {ok, lists:reverse(LTable)};
extract_sub_by(<<_C, Rest/binary>>, LTable) ->
    extract_sub_by(Rest, LTable).

%% extract sub XXX part
extract_sub(<<" ", Rest/binary>>, [], []) ->
    extract_sub(Rest, [], []);
extract_sub(<<"by", Rest/binary>>, _Buf, Subs) ->
    {ok, lists:reverse(Subs), Rest};
extract_sub(<<" ", Rest/binary>>, {?ampers, Buff}, Subs) ->
    extract_sub(Rest, [], [ {?ampers, lists:reverse(Buff)} | Subs ]);
extract_sub(<<" ", Rest/binary>>, {?aphost, Buff}, Subs) ->
    extract_sub(Rest, [], [{?aphost, Buff}|Subs]);
extract_sub(<<" ", Rest/binary>>, {?multiple, Buff}, Subs) ->
    extract_sub(Rest, [], [{?multiple, Buff}|Subs]);
extract_sub(<<" ", Rest/binary>>, {?amperaphost, Buff}, Subs) ->
    extract_sub(Rest, [], [{?amperaphost, Buff}|Subs]);
extract_sub(<<" ", Rest/binary>>, {?multipleaphost, Buff}, Subs) ->
    extract_sub(Rest, [], [{?multipleaphost, Buff}|Subs]);
extract_sub(<<" ", Rest/binary>>, Buf, Subs) ->
    extract_sub(Rest, [], [{?normal, lists:reverse(Buf)}|Subs]);
extract_sub(<<"[", Rest/binary>>, _Buf, Subs) ->
    {ok, Brackets, Rest1} = extract_bracket(Rest, [], []),
    extract_sub(Rest1, {?multiple, lists:reverse(Brackets)}, Subs);
extract_sub(<<"'", Rest/binary>>, {?ampers, Buf}, Subs) ->
    extract_sub(Rest, {?amperaphost, lists:reverse(Buf)}, Subs);
extract_sub(<<"'", Rest/binary>>, {?multiple, Buf}, Subs) ->
    extract_sub(Rest, {?multipleaphost, Buf}, Subs);
extract_sub(<<"'", Rest/binary>>, Buff, Subs) ->
    extract_sub(Rest, {?aphost, lists:reverse(Buff)}, Subs);
extract_sub(<<"@", Rest/binary>>, _Buff, Subs) ->
    extract_sub(Rest, {?ampers, []}, Subs);
extract_sub(<<C,Rest/binary>>, {?ampers, Buf}, Subs) ->
    extract_sub(Rest, {?ampers, [C|Buf]}, Subs);
extract_sub(<<C,Rest/binary>>, Buf, Subs) ->
    extract_sub(Rest, [C|Buf], Subs).

%% extract "by YYY" part
extract_by(<<" ", Rest/binary>>, [], [] ) ->
    extract_by(Rest, [], []);
extract_by(<<";", Rest/binary>>, {?ampers, Buf}, Bys) ->
    {ok, lists:reverse([{?ampers, lists:reverse(Buf)} | Bys]), Rest};
extract_by(<<";", Rest/binary>>, {?multiple, Buf}, Bys) ->
    {ok, lists:reverse([{?multiple, Buf} | Bys]), Rest};
extract_by(<<";", Rest/binary>>, Buf, Bys) ->
    {ok, lists:reverse([lists:reverse(Buf) | Bys]), Rest };
extract_by(<<" ", Rest/binary>>, {?ampers, Buf}, Bys) ->
    extract_by(Rest, [], [{?ampers, lists:reverse(Buf)}|Bys]);
extract_by(<<" ", Rest/binary>>, {?multiple, Buf}, Bys) ->
    extract_by(Rest, [], [{?multiple, lists:reverse(Buf)}|Bys]);
extract_by(<<" ", Rest/binary>>, Buf, Bys) ->
    extract_by(Rest, [], [{?normal, lists:reverse(Buf)}|Bys]);
extract_by(<<"[", Rest/binary>>, _Buf, Bys) ->
    {ok, Brackets, Rest1} = extract_bracket(Rest, [], []),
    extract_by(Rest1, {?multiple, Brackets}, Bys);
extract_by(<<C, Rest/binary>>, Buf, Bys) ->
    extract_by(Rest, [C|Buf], Bys).

%% extract [XXX,XXXX] bracket content
extract_bracket(<<"]", Rest/binary>>, Buf, Ligas) ->
    L1 = [lists:reverse(Buf) | Ligas],
    {ok, lists:reverse(L1), Rest };
extract_bracket(<<" ", Rest/binary>>, [], []) ->
    extract_bracket(Rest, [], []);
extract_bracket(<<" ", Rest/binary>>, Buf, Ligas) ->
    %%extract_bracket(Rest, [], [{?normal, lists:reverse(Buf)}|Ligas]);
    extract_bracket(Rest, [], [lists:reverse(Buf)|Ligas]);
extract_bracket(<<C, Rest/binary>>, Buf, Ligas) ->
    extract_bracket(Rest, [C|Buf], Ligas).
