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
                   <<"#",Channel/binary>>,
                   <<"!roll",Rest/binary>>]} ->
            Parsed = d20_dice:parse(binary_to_list(Rest)),
            case Parsed of
                {fail, _} ->
                    Ref:privmsg(<<"#",Channel/binary>>,
                                "WTF???");
                _ ->
                    {result, Result} = d20_dice:roll(Parsed),
                    Ref:privmsg(<<"#",Channel/binary>>,
                                io_lib:format("~s rolled ~B",
                                              [Sender, Result]))
            end;
        _ ->
            ok
    end,
    {ok, State}.

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

