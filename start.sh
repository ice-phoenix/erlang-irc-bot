#!/bin/bash

erl -sname icebot@localhost \
    -pa ebin/ \
    -sasl errlog_type error \
    -s ircbot_app \
    -conf settings-app.cfg
