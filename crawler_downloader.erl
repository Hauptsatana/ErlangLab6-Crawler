-module(crawler_downloader).
-behaviour(gen_server).

% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, download/2]).



start_link() ->
	{ok, PID} = gen_server:start_link(?MODULE, [], []),
	PID.

download(DownloaderPID, URL) ->
	gen_server:cast(DownloaderPID, {download, URL}).



init(_Args) -> {ok, []}.
terminate(_Reason, _State) -> {ok}.

handle_call(_Request, _From, State) -> {reply, {ok}, State}.

handle_cast({download, URL}, Visited) ->
	case lists:member(URL, Visited) of 
		true -> {noreply, Visited};
		false -> save_to_file(URL), {noreply, Visited ++ [URL]}
	end;
handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldCode, State, _Extra) -> {ok, State}.

save_to_file(URL) ->
	{R, {{_Type, Code, _Ok}, _Headers, Body}} =  httpc:request(URL),
	
    if	(R =:= ok) and (Code =:= 200) ->
			Filename = filename:basename(URL),
			file:make_dir("downloads"),
        	case file:write_file("downloads/" ++ Filename, Body) of
				ok -> io:format("File \"~s\" written.~n", [Filename]);
				{error, Reason} -> io:format("Unable to write file. Reason = ~w.~n", [Reason])
			end;
		true ->
			io:format("Discarded: R=~w. Code=~w. URL=~s~n", [R, Code, URL]),
			{ok}
    end.

ensure_dir(_Dir, [_FileName]) -> ok;
ensure_dir(Dir, [NextDir|RestDirs]) ->
    DirName = filename:join(Dir, NextDir),
    file:make_dir(DirName),
    ensure_dir(DirName, RestDirs). 