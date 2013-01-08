-module(plugins.dicer).

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
-import(orddict).
-import(dicer).

init(_Args) ->
    random:seed(now()),
    State = orddict:new(),
    {ok, State}.

terminate(_Args, _State) ->
    ok.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _User, <<"PRIVMSG">>, <<_Receiver/binary>>, <<"!join ",Channel/binary>>]} ->
            Ref:join(<<Channel/binary>>),
            {ok, State};
        {in, Ref, [_Sender, _User, <<"PRIVMSG">>, <<"#",Receiver/binary>>, <<"!part">>]} ->
            Ref:part(<<"#",Receiver/binary>>),
            {ok, State};
        {in, Ref, [Sender, _User, <<"PRIVMSG">>, <<Receiver/binary>>, <<"!",Cmd/binary>>]} ->
            NewState = case Receiver of
                <<"#",_Channel/binary>> -> process_roll(State, Ref, Sender, Receiver, Cmd);
                _ ->                       process_roll(State, Ref, Sender, Sender,   Cmd)
            end,
            {ok, NewState};
        _ ->
            {ok, State}
    end.

process_roll(State, Ref, Nick, Receiver, Cmd) ->
    Results = dicer:parse_and_process(binary_to_list(Cmd), Receiver, State),
    case Results of
        {fail, _} ->
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

