#!/bin/sh

video_path="$HOME/Videos/Replays"
mkdir -p "$video_path"

if [[ -e "$video_path/replay_running" ]]; then
  notify-send "Stopping Replay Buffer"
  rm $video_path/replay_running
  killall -SIGINT gpu-screen-recorder
else
  notify-send "gpu-screen-recorder" "Starting Replay Buffer..."
  touch $video_path/replay_running
  gpu-screen-recorder -w screen -f 60 -a "$(pactl get-default-sink).monitor" -a "$(pactl get-default-source)" -c mp4 -r 30 -o "$video_path"
fi
