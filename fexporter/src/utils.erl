-module(utils).
-compile([export_all]).

-define(RCHARS, "1234567890qwertyuiopasdfghjklzxcvbnm").

create_folder(FPath) ->
    case file:make_dir(FPath) of
        ok ->
            {ok, created};
        {error, eexist} ->
            {ok, created};
        Error ->
            Error
    end.

%% read class files and return a list of glyphs
get_list_from_file(FPath) ->
    io:format("Reading:~p~n", [FPath]),
    {ok, Binary} = file:read_file(FPath),
    read_features(Binary).
read_features(Bin) ->
    read_features(Bin, [], []).
read_features(<<>>, _Buf, Features) ->
    {ok, lists:reverse(Features)};
read_features(<<"\n", Rest/binary>>, [], Features) ->
    read_features(Rest, [], Features);
read_features(<<"\n", Rest/binary>>, Buf, Features) ->
    read_features(Rest, [], [lists:reverse(Buf)|Features]);
read_features(<<C, Rest/binary>>, Buf, Features) ->
    read_features(Rest, [C|Buf], Features).

%% generate uniq name
get_uniq_name(Name, List) ->
    get_uniq_name(Name, List, lists:member(Name, List)).
get_uniq_name(Name, [], false) ->
    {ok, lists:flatten(Name), [Name]};
get_uniq_name(Name, List, false) ->
    {ok, lists:flatten(Name), [Name|List]};
get_uniq_name(Name, List, true) ->
    NName = Name++get_random_str(7),
    get_uniq_name(NName, List, lists:member(NName, List)).

%% generate random strings
get_random_str(Len) ->
    lists:flatten("_"++random_str(Len, ?RCHARS)).
random_str(0, _Chars) -> [];
random_str(Len, Chars) -> [random_char(Chars)|random_str(Len-1, Chars)].
random_char(Chars) -> lists:nth(random:uniform(length(Chars)), Chars).
