-module(irc_bot).

-export([start/0]).

start() ->
    {ok, Settings} = file:consult("settings.cfg"),
    IrcBot = ircbot_fsm:new(Settings),
    IrcBot:connect().

