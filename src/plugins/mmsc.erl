-module(plugins.mmsc).

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

-import(file).
-import(io).
-import(io_lib).
-import(lists).
-import(proplists).

init(Args) ->
    StatusFile = proplists:get_value(status_file, Args),
    case StatusFile of
        undefined -> {error, "No status file specified"};
        _ -> {ok, [{status_file, StatusFile}]}
    end.

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
    StatusFile = proplists:get_value(status_file, State),
    {ok, Status} = file:consult(StatusFile),
    lists:foreach(
        fun(E) ->
            {Server, Players} = E,
            Msg = case Players of
                [] -> "Anybody home???";
                _ -> pprint(Players, ", ")
            end,
            Ref:privmsg(<<Receiver/binary>>,
                        io_lib:format("~s : ~s", [Server, Msg]))
        end,
        Status
    ),
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

pprint(Term) ->
    pprint_aux(Term, "", ",").

pprint_aux(Number, Str, Sep) when is_integer(Number) ->
    Str ++ io_lib:format("~B", [Number]);

pprint_aux([Number], Str, Sep) when is_integer(Number) ->
    Str ++ io_lib:format("~B", [Number]);

pprint_aux([Number | T], Str, Sep) when is_integer(Number) ->
    pprint_aux(T, Str ++ io_lib:format("~B,", [Number]), Sep);

pprint_aux([Term], Str, Sep) ->
    Str ++ Term;

pprint_aux([], Str, Sep) ->
    Str;

pprint_aux([Term | T], Str, Sep) ->
    pprint_aux(T, Str ++ Term ++ Sep, Sep).

