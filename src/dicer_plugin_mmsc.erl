-module(dicer_plugin_mmsc).

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

init(Args) ->
    {ok, Args}.

terminate(_Args, _State) ->
    ok.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [Sender,
                   _User,
                   <<"PRIVMSG">>,
                   <<Receiver/binary>>,
                   <<"$",Rest/binary>>]} ->
            Cmd = binary_to_list(Rest),
            NewState = case Receiver of
                <<"#",_Channel/binary>> ->
                    process_command(Cmd, State, Ref, Sender, Receiver);
                _ ->
                    process_command(Cmd, State, Ref, Sender, Sender)
            end,
            {ok, NewState};
        _ ->
            {ok, State}
    end.

process_command("servers", State, Ref, _Nick, Receiver) ->
    User = proplists:get_value(user, State),
    Pass = proplists:get_value(pass, State),
    Url = proplists:get_value(url, State),
    Json = mmm_rest_api:get_servers(User, Pass, Url),
    io:format("~p~n", [Json]),
    State;

process_command(_Cmd, State, _Ref, _Nick, _Receiver) ->
    State.

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pprint Erlang terms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pprint(Term, Sep) ->
    pprint_aux(Term, "", Sep).

% pprint(Term) ->
%     pprint_aux(Term, "", ",").

pprint_aux(Number, Str, _Sep) when is_integer(Number) ->
    Str ++ io_lib:format("~B", [Number]);

pprint_aux([Number], Str, _Sep) when is_integer(Number) ->
    Str ++ io_lib:format("~B", [Number]);

pprint_aux([Number | T], Str, Sep) when is_integer(Number) ->
    pprint_aux(T, Str ++ io_lib:format("~B,", [Number]), Sep);

pprint_aux([Term], Str, _Sep) ->
    Str ++ Term;

pprint_aux([], Str, _Sep) ->
    Str;

pprint_aux([Term | T], Str, Sep) ->
    pprint_aux(T, Str ++ Term ++ Sep, Sep).
