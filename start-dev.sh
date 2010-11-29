#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
# ANOTHER NOTE: usually mnesia will be already started,
# but for dev purposes we do it here
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -s mnesia start \
    -sname mnesia_graphs_dev \
    -s mnesia_graphs \
    -s reloader
