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

-include("mmm_rest_api_records.hrl").

init(_Args) ->
    {ok, []}.

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
    Servers = mmm_rest_api:get_servers(User, Pass, Url),
    FServers = lists:filter(
        fun(E) -> E#server.empty == false end,
        Servers
    ),
    case length(FServers) of
        0 -> Ref:privmsg(<<Receiver/binary>>, "Nobody's home...");
        _ -> lists:foreach(
                 fun(E) -> send_server_status_msg(State, E, Ref, Receiver) end,
                 FServers
             )
    end,
    State;

process_command(_Cmd, State, _Ref, _Nick, _Receiver) ->
    State.

send_server_status_msg(_State, Server, Ref, Receiver) ->
    Msg = io_lib:format("~B (~s): ~s",
        [
            Server#server.port,
            Server#server.status,
            string:join(Server#server.players, ", ")
        ]),
    Ref:privmsg(<<Receiver/binary>>, Msg).

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
