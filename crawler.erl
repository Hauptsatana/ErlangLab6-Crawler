-module(crawler).
-behaviour(gen_server).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([start_crawler/2, visit_url/4, site_tree/1]).

init([Domain, StorePID, DownloaderPID]) -> {ok, [Domain, StorePID, DownloaderPID]}.
terminate(_Reason, _State) -> {ok}.

handle_call({site_tree}, _From, [Domain, StorePID, DownloaderPID]) -> 
	{reply, crawler_store:get_visited(StorePID), [Domain, StorePID, DownloaderPID]}.

handle_cast({run, URL}, [Domain, StorePID, DownloaderPID]) -> 
	visit_url(Domain, URL, StorePID, DownloaderPID),
	{noreply, [Domain, StorePID, DownloaderPID]};
handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldCode, State, _Extra) -> {ok, State}.


site_tree(CrawlerPID) ->
	gen_server:call(CrawlerPID, {site_tree}).

start_crawler(URL, MaxHrefs) ->
	inets:start(),
	ssl:start(),
    case http_uri:parse(URL) of
		{error, Reason} -> 
			io:format("Error with http_uri:parse. Reason = ~s~n", [Reason]);
		{ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query}} ->
			{ok, PID} = gen_server:start_link(?MODULE, [Host, crawler_store:start_link(MaxHrefs), crawler_downloader:start_link()], []),
			gen_server:cast(PID, {run, URL}),
			PID
    end.
	

% Скачать все изображения и посетить все ссылки, указывающие на этот же сайт
visit_url(Domain, URL, StorePID, DownloaderPID) ->
	case http_uri:parse(URL) of
		{error, Reason} -> 
			io:format("Error with http_uri:parse. URL = ~s. Reason = ~w~n", [URL, Reason]);
		{ok, {Scheme, _UserInfo, Host, _Port, Path, _Query}} ->
			% Находимся на том же сайте, продолжаем обрабатывать страницы
			if 	(Host =:= Domain) ->
					%io:format("~s~s~n", [string:copies("  ", 0), URL]),
					% Помечаем страницу посещенной					
					crawler_store:mark_visited(StorePID, URL),
					case httpc:request(URL) of
					    {error, Reason} ->
							io:format("Error with httpc:request(\"~s\"). Reason = ~w~n", [URL, Reason]),
							{error};
					    {ok, {_, _, Body}} -> 
							Tree = mochiweb_html:parse(Body),
						    Hrefs = remove_duplicates(mochiweb_xpath:execute("//a/@href", Tree)),
						    Images = remove_duplicates(mochiweb_xpath:execute("//img/@src", Tree)),
						    % Скачиваем каждое изображение
						    lists:foreach(
						    	fun(Image) ->
						    		AbsImage = absolute_url(Scheme, Host, Path, binary_to_list(Image)),
						    		crawler_downloader:download(DownloaderPID, AbsImage)
						    	end,
						    	Images
						    ),
						    % Каждую ссылку переводим в абсолютный формат, посещаем ее, если не посетили ранее
						    lists:foreach(
						    	fun(Href) ->
						    		AbsHref = absolute_url(Scheme, Host, Path, remove_id(binary_to_list(Href))),
									% Проверка что это веб-страница
									case lists:member(filename:extension(AbsHref), ["", ".html", ".php", ".jsp", ".asp", ".aspx"]) of
										false ->
											% io:format("Ignored ~s~n", [AbsHref]),
											{href_ignored};
										true ->
											% timer:sleep(1000),									 
											case crawler_store:can_visit(StorePID, AbsHref) of
												true -> 
													%io:format("Visiting ~s...~n", [AbsHref]),
													% crawler_store:mark_visited(StorePID, AbsHref),
													spawn(?MODULE, visit_url, [Host, AbsHref, StorePID, DownloaderPID]);
												false ->
													{already_visited}
											end
									end
						    	end,
						    	Hrefs
						   	)
					end;
				true ->
					%io:format("Out of domain.~n"), 
					{out_of_target_domain}
			end
	end.

remove_duplicates(L) ->
    sets:to_list(sets:from_list(L)).

% Убрать из ссылки указатель на id элемента
remove_id(URL) -> 
	IdSign = string:chr(URL, $#),
	if	(IdSign > 0) -> string:left(URL, IdSign - 1);
		true -> 
			%QuerySign = string:chr(URL, $?),
			%if	(QuerySign > 0) -> string:left(URL, QuerySign - 1);
			%	true -> URL
			%end
			URL
	end.

%% Перевод ссылки в абсолютную
absolute_url(_Scheme, _Domain, _Path, Href="http://" ++ _) -> Href;
absolute_url(_Scheme, _Domain, _Path, Href="https://" ++ _) -> Href;
absolute_url(Scheme, Domain, Path, _Href="../" ++ T) ->
	DirPath = filename:dirname(Path),
	ParentDir = string:left(DirPath, string:rchr(DirPath, $/)),
	absolute_url(Scheme, Domain, ParentDir, T);
	%io:format("Relative found. (~s ~s) ~s.~nReduced to ~s.~n~n", [Domain, Path, _Href, AbsUrl]);
absolute_url(Scheme, Domain, Path, Href) ->
	case filename:pathtype(Href) of
		absolute -> Href;
		volumerelative -> atom_to_list(Scheme) ++ "://" ++ Domain ++ Href;
		relative ->
			DirPath = case filename:dirname(Path) of
				"." -> "";
				"/" -> "";
				SomePath -> SomePath
			end, 
			atom_to_list(Scheme) ++ "://" ++ Domain ++ DirPath ++ "/" ++ Href
	end.

    