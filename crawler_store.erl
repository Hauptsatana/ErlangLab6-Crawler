-module(crawler_store).
-behaviour(gen_server).

% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, can_visit/2, mark_visited/2, get_visited/1]).



start_link(MaxNum) ->
	{ok, PID} = gen_server:start_link(?MODULE, [MaxNum], []),
	PID.

can_visit(StorePID, URL) ->
	gen_server:call(StorePID, {can_visit, URL}).

mark_visited(StorePID, URL) ->
	gen_server:cast(StorePID, {mark_visited, URL}).

get_visited(StorePID) ->
	gen_server:call(StorePID, {get_visited}).	



init([MaxNum]) -> {ok, [MaxNum, 0, []]}.
terminate(_Reason, _State) -> {ok}.

handle_call({get_visited}, _From, [MaxNum, CurNum, Visited]) -> {reply, Visited, [MaxNum, CurNum, Visited]};
handle_call({can_visit, URL}, _From, [MaxNum, CurNum, Visited]) -> {reply, not(lists:member(URL, Visited)) and (CurNum < MaxNum), [MaxNum, CurNum, Visited]};
handle_call(_Request, _From, State) -> {reply, {ok}, State}.

handle_cast({mark_visited, URL}, [MaxNum, CurNum, Visited]) -> 
	{noreply, [MaxNum, CurNum + 1, Visited ++ [URL]]};
handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldCode, State, _Extra) -> {ok, State}.



