-module(plugins.d20_roll).

-author("Marat.Akhin@gmail.com").

-behaviour(gen_event).

-export([
    init/1,
    terminate/2,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3
]).

-import(random).
-import(io).
-import(io_lib).
-import(lists).
-import(d20_dice).

init(_Args) ->
    random:seed(now()),
    {ok, []}.

terminate(_Args, _State) ->
    ok.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [Sender,
                   _User,
                   <<"PRIVMSG">>,
                   <<Receiver/binary>>,
                   <<"!",Rest/binary>>]} ->
            NewState = case Receiver of
                <<"#",_Channel/binary>> ->
                    process_roll(State, Ref, Sender, Receiver, Rest);
                _ ->
                    process_roll(State, Ref, Sender, Sender, Rest)
            end,
            {ok, NewState};
        _ ->
            {ok, State}
    end.

process_roll(State, Ref, Nick, Receiver, Cmd) ->
    Results = d20_dice:parse_and_process(binary_to_list(Cmd), State),
    case Results of
        {fail, _} ->
            io:format("~p~n", [Results]),
            Ref:privmsg(<<Receiver/binary>>,
                        "WTF???"),
            State;
        {ok, {Msgs, NewState}} ->
            lists:foreach(
                fun(Msg) ->
                    Ref:privmsg(<<Receiver/binary>>,
                                io_lib:format("~s ~s",
                                              [Nick, Msg]))
                end,
                Msgs),
            NewState
    end.

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

