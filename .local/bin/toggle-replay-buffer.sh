#!/bin/sh

video_path="$HOME/Videos/Replays"
mkdir -p "$video_path"

if pidof -q gpu-screen-recorder; then
  notify-send "Stopping Replay Buffer"
  killall -SIGINT gpu-screen-recorder
else
  notify-send "gpu-screen-recorder" "Starting Replay Buffer..."
  gpu-screen-recorder -w screen -f 60 -a "$(pactl get-default-sink).monitor" -a "$(pactl get-default-source)" -c mkv -bm cbr -r 30 -o "$video_path"
fi
