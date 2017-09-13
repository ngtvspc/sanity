#!/bin/bash
cd ~/simplelegal
tmux new-session -n distant \; split-window -h \; split-window -v -t +2 \; split-window -v -t +1 \; detach
tmux new-window -n backend \; split-window -h \; split-window -v -t +2
tmux new-window -n web \; split-window -h \; split-window -v -t +2 \; split-window -v -t +1
