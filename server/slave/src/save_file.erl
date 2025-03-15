-module(save_file).

-export([save_file/2]).

save_file(FileName, FileContent) ->
    Dir = "files",
    ensure_directory_exists(Dir),
    FilePath = filename:join(Dir, FileName),
    case file:write_file(FilePath, FileContent) of
        ok -> ok;
        {error, Reason} -> 
            io:format("Error writing file: ~p~n", [Reason])
    end.
    file:close(File).

ensure_directory_exists(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, Reason} ->
            io:format("Impossible to create directory ~s: ~p~n", [Dir, Reason]),
            {error, Reason}
    end.