-module(dicer_plugin_dicer).

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

-include("dicer_records.hrl").

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
                <<"#",_Channel/binary>> -> process_cmd(State, Ref, Sender, Receiver, Cmd);
                _ ->                       process_cmd(State, Ref, Sender, Sender,   Cmd)
            end,
            {ok, NewState};
        _ ->
            {ok, State}
    end.

process_cmd(State, Ref, Nick, Receiver, Cmd) ->
    Processed = dicer:parse_and_process(binary_to_list(Cmd), Receiver, State),
    case Processed of
        {fail, _} ->
            Ref:privmsg(<<Receiver/binary>>,
                        "WTF???"),
            State;
        {ok, {Results, NewState}} ->
            lists:foreach(
                fun(Res) -> process_result(Res, Ref, Nick, Receiver) end,
                Results),
            NewState
    end.

process_result(Result, Ref, Nick, Receiver) ->
    #result{msgs = Msgs, color = Color, scope = Scope} = Result,
    lists:foreach(
        fun(Msg) ->
            case Scope of
                public  -> send_msg(Msg, Color, Ref,  Nick, Receiver);
                private -> send_msg(Msg, Color, Ref, "You", Nick)
            end
        end,
        Msgs).

send_msg(Msg, _Color, Ref, Nick, Receiver) ->
    Ref:privmsg(<<Receiver/binary>>, io_lib:format("~s ~s", [Nick, Msg])).

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

