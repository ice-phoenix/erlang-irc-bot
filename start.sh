#!/bin/bash

erl -sname icebot@localhost \
    -pa ebin/ \
    -sasl errlog_type error \
    -s inets \
    -s ircbot_app \
    -conf settings-app.cfg
