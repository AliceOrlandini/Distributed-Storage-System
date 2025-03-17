-module(retrieve_file).

-export([retrieve_file/2]).

retrieve_file(FileName, NodeName) ->
    Dir = "files_" ++ atom_to_list(NodeName),
    FilePath = filename:join(Dir, FileName),
    case file:read_file(FilePath) of
        {ok, FileContent} ->
            {ok, FileContent};
        {error, enoent} ->
            {error, file_not_found};
        {error, Reason} ->
            {error, Reason}
    end.
