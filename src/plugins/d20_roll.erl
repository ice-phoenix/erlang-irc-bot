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
                   <<"!roll ",Rest/binary>>]} ->
            case Receiver of
                <<"#",_Channel/binary>> ->
                    process_roll(Ref, Sender, Receiver, Rest);
                _ ->
                    process_roll(Ref, Sender, Sender, Rest)
            end;
        _ ->
            ok
    end,
    {ok, State}.

process_roll(Ref, Nick, Receiver, Roll) ->
    Results = d20_dice:parse_and_roll(binary_to_list(Roll)),
    case Results of
        {fail, _} ->
            Ref:privmsg(<<Receiver/binary>>,
                        "WTF???");
        _ ->
            lists:foreach(
                fun({Result, Desc}) ->
                    Ref:privmsg(<<Receiver/binary>>,
                                io_lib:format("~s rolled ~B -> ~s",
                                              [Nick, Result, Desc]))
                end,
                Results)
    end.

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

