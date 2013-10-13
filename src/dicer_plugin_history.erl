-module(dicer_plugin_history).

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

-include("history_records.hrl").

init(_Args) ->
    {ok, #history_state{}}.

terminate(_Args, _State) ->
    ok.

handle_event(Msg, InState) ->
    {ok, State} = update_channel_history(Msg, InState),
    case Msg of
        {in, Ref, [Sender, _User, <<"PRIVMSG">>, <<Receiver/binary>>, <<"!history">>]} ->
            Channel = case Receiver of
                <<"#",_/binary>> -> Receiver;
                _ -> Sender
            end,
            ChannelStates = State#history_state.channel_states,
            ChannelState = case orddict:find(Channel, ChannelStates) of
                error -> #channel_state{};
                {ok, Value} -> Value
            end,
            Log = ChannelState#channel_state.log,
            lists:foreach(
                fun(E) -> Ref:privmsg(<<Channel/binary>>, E) end,
                bqueue:to_list(Log)
            ),
            {ok, State};
        _ ->
            {ok, State}
    end.

update_channel_history(Msg, State) ->
    case Msg of
        {in, _Ref, [Sender, _User, <<"PRIVMSG">>, <<Receiver/binary>>, <<Message/binary>>]} ->
            Channel = case Receiver of
                <<"#",_/binary>> -> Receiver;
                _ -> Sender
            end,
            ChannelStates = State#history_state.channel_states,
            ChannelState = case orddict:find(Channel, ChannelStates) of
                error -> #channel_state{};
                {ok, Value} -> Value
            end,
            Log = ChannelState#channel_state.log,
            M = io_lib:format("> ~s: ~s", [Sender, Message]),
            NewLog = bqueue:in(M, Log),
            NewChannelState = ChannelState#channel_state{log = NewLog},
            NewChannelStates = orddict:store(Channel, NewChannelState, ChannelStates),
            {ok, State#history_state{channel_states = NewChannelStates}};
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
