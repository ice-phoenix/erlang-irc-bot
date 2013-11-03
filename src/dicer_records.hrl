
-record(dicer_state, {channel_states = orddict:new(), colors = orddict:new()}).
-record(color_desc, {name = "", fg = "1", bg = none}).
-record(channel_state, {deck = []}).

-record(command,  {steps = [], scope = none, color = none}).
-record(roll_cmd, {tree = {none}}).
-record(deck_cmd, {tree = {none}}).

-record(result, {msgs=[], scope = none, color = none}).
