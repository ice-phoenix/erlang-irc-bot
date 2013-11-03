
-record(history_state, {channel_states = orddict:new()}).
-record(channel_state, {log = bqueue:new(100)}).
