#!/bin/sh

video_path="$HOME/Videos/Recordings"
mkdir -p "$video_path"

if pidof -q gpu-screen-recorder; then
  notify-send "Stopping Recording"
  killall -SIGINT gpu-screen-recorder
else
  notify-send "gpu-screen-recorder" "Starting Recording..."
  gpu-screen-recorder -w screen -f 60 -a "$(pactl get-default-sink).monitor" -a "$(pactl get-default-source)" -o "$video_path/$(date +%Y-%m-%d_%H-%M-%S).mp4"
fi
