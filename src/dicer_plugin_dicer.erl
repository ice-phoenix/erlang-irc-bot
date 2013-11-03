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
    Colors = [
        {red,    #color_desc{name = "red",    fg = "4"            }},
        {orange, #color_desc{name = "orange", fg = "7",  bg = "1" }},
        {yellow, #color_desc{name = "yellow", fg = "8",  bg = "1" }},
        {green,  #color_desc{name = "green",  fg = "3"            }},
        {teal,   #color_desc{name = "teal",   fg = "10"           }},
        {blue,   #color_desc{name = "blue",   fg = "2"            }},
        {purple, #color_desc{name = "purple", fg = "6"            }},
        {silver, #color_desc{name = "silver", fg = "15", bg = "1" }},
        {black,  #color_desc{name = "black",  fg = "1"            }},
        {white,  #color_desc{name = "white",  fg = "0",  bg = "1" }}
    ],
    ColorsDict = orddict:from_list(Colors),
    {ok, #dicer_state{colors = ColorsDict}}.

terminate(_Args, _State) ->
    ok.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<_Receiver/binary>>, <<"!join ",Channel/binary>>]} ->
            Ref:join(<<Channel/binary>>),
            {ok, State};
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!part">>]} ->
            Ref:part(<<"#",Channel/binary>>),
            {ok, State};
        {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<Receiver/binary>>, <<"!",Rest/binary>>]} ->
            Cmd = binary_to_list(Rest),
            Channel = case Receiver of
                <<"#",_/binary>> -> Receiver;
                _ -> Nick
            end,
            NewState = process_command(Cmd, State, Ref, Nick, Channel),
            {ok, NewState};
        _ ->
            {ok, State}
    end.

process_command(Cmd, State, Ref, Nick, Channel) ->
    Processed = dicer:parse_and_process(Cmd, Channel, State),
    case Processed of
        {fail, _} ->
            Ref:privmsg(<<Channel/binary>>, "WTF???"),
            State;
        {ok, {Results, NewState}} ->
            lists:foreach(
                fun(Res) -> process_result(State, Res, Ref, Nick, Channel) end,
                Results),
            NewState
    end.

process_result(State, Result, Ref, Nick, Channel) ->
    #result{msgs = Msgs, scope = Scope, color = Color} = Result,
    lists:foreach(
        fun(Msg) ->
            case Scope of
                public  -> send_msg(State, Msg, Color, Ref,  Nick, Channel);
                private -> send_msg(State, Msg, Color, Ref, "You", Nick)
            end
        end,
        Msgs).

send_msg(State, Msg, Color, Ref, Nick, Channel) ->
    Ref:privmsg(<<Channel/binary>>, format_msg(State, Nick, Msg, Color)).

format_msg(State, Nick, Msg, Color) ->
    BaseMsg = io_lib:format("~s ~s", [Nick, Msg]),
    case orddict:find(Color, State#dicer_state.colors) of
        error -> BaseMsg;
        {ok, #color_desc{name = ColorName, fg = ColorFg, bg = none}} ->
            [3] ++ ColorFg ++ BaseMsg ++ io_lib:format(" (~s)", [ColorName]);
        {ok, #color_desc{name = ColorName, fg = ColorFg, bg = ColorBg}} ->
            [3] ++ ColorFg ++ "," ++ ColorBg ++ BaseMsg ++ io_lib:format(" (~s)", [ColorName])
    end.

handle_call(_Request, State) ->
    {ok, not_implemented, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
