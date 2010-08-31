-module(utils).
-compile([export_all]).

create_folder(FPath) ->
    case filename:make_dir(FPath) of
        ok ->
            {ok, created};
        {error, eexist} ->
            {ok, created};
        Error ->
            Error
    end.

