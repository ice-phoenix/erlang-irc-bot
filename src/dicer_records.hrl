
-record(dicer_state, {channel_states = orddict:new(), colors = orddict:new()}).
-record(color_desc, {name = "", fg = "1", bg = none}).
-record(channel_state, {deck = []}).

-record(command,  {steps = [], scope = public}).
-record(roll_cmd, {tree = {none}, color = none}).
-record(deck_cmd, {tree = {none}}).

-record(result, {msgs=[], color = none, scope = public}).
