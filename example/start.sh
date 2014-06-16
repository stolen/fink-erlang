#!/usr/bin/env sh
rebar compile -C rebar.conf
erl -sname lfe\
    -pa deps/*/ebin\
    -pa apps/*/ebin\
    -boot start_sasl\
    -s fink_example_app start
