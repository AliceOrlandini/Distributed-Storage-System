#!/bin/bash

# Numero di repliche da lanciare
NUM_REPLICAS=1

for i in $(seq 0 $NUM_REPLICAS); do
    if [ "$i" -eq 0 ]; then
        gnome-terminal -- bash -c "PORT=8080 rebar3 as bootstrap shell --name master0@127.0.0.1 --setcookie master; exec bash"
    else
        gnome-terminal -- bash -c "PORT=808${i} rebar3 as replica shell --name master${i}@127.0.0.1 --setcookie master; exec bash"
    fi
done
