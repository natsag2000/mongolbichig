-module(utils).
-compile([export_all]).

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
read_class(FPath) ->
    io:format("Reading class:~p~n", [FPath]),
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

