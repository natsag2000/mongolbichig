-module(fextractor).
-export([extract_feature/1]).
-include("../include/features.hrl").


extract_feature(File) ->
    {ok, Content} = file:read_file(File),
    {ok, BContent, Feature} = extract_feature_body(Content, [], [], #feature{}, start),
    {ok, FeatureLast} = extract_feature_lookups(list_to_binary(BContent), [], #lookup{}, Feature, start),
    FeatureLast.
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
    {ok, Subtables} = extract_feature_table(lists:reverse(Buf)),
    Tables = Lookup#lookup.lookups,
    NewLookup=Lookup#lookup{lookups=lists:reverse([Subtables|Tables])},
    NewFeatureLookups = Feature#feature.lookups,
    NewFeature = Feature#feature{lookups=lists:reverse([NewLookup|NewFeatureLookups])},
    extract_feature_lookups(Rest, [], #lookup{}, NewFeature, start);
extract_feature_lookups(<<C,Rest/binary>>, Buf, Lookup, Feature, body) ->
    extract_feature_lookups(Rest, [C|Buf], Lookup, Feature, body).
%extract_feature_lookups(<<";", Rest/binary>>, Buf, Lookup, Feature, end_name) ->
%    extract_feature_lookups(Rest, [], #lookup{}, NewFeature, start);
%%extract_feature_lookups(<<_C,Rest/binary>>, Buf, Lookup, Feature, end_name) ->
%%    extract_feature_lookups(Rest, Buf, Lookup, Feature, end_name).

%% ----------------------------------------------------------------
%% extract substitution table
%% ----------------------------------------------------------------
extract_feature_table(Subs) ->
    {ok, Subs}.
