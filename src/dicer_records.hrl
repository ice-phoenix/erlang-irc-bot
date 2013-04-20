
-record(channel_state, {deck = []}).

-record(command,  {steps = [], scope = public}).
-record(roll_cmd, {tree = {none}, color = none}).
-record(deck_cmd, {tree = {none}}).

-record(result, {msgs=[], color = none, scope = public}).
