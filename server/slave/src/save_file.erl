-module(save_file).

-export([save_file/3, delete_chunk/2]).

save_file(FileName, FileContent, NodeName) ->
    Dir = "files_" ++ atom_to_list(NodeName),
    ensure_directory_exists(Dir),
    FilePath = filename:join(Dir, FileName),
    case file:write_file(FilePath, FileContent) of
        ok -> ok;
        {error, Reason} -> 
            io:format("[ERROR] Error writing file: ~p~n", [Reason])
    end.

delete_chunk(FileName, NodeName) ->
    Dir = "files_" ++ atom_to_list(NodeName),
    FilePath = filename:join(Dir, FileName),
    case file:delete(FilePath) of
        ok -> ok;
        {error, enoent} -> ok; % File già non esistente
        {error, Reason} ->
            io:format("[ERROR] Error deleting file: ~p~n", [Reason]),
            {error, Reason}
    end.

ensure_directory_exists(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, Reason} ->
            io:format("[ERROR] Impossible to create directory ~s: ~p~n", [Dir, Reason]),
            {error, Reason}
    end.