#!/bin/bash

# Set to 1 or use -d option to print module outputs to stderr
DBUG=0
if [ "$1" = "-d" -o "$1" = "--debug" ]; then
	DBUG=1
fi

lemonpipe="/tmp/lemonbar.fifo"

source ~/.config/lemonbar/colors
source ~/.config/lemonbar/config

echo "Starting..."
if [[ -p $lemonpipe ]]; then
	echo "Using existing pipe"
else
	echo "No pipe. Creating..."
	mkfifo $lemonpipe || {
		echo 'Could not make lemonpipe' >&2
		exit 1
	}
fi

KIDS=""

# Removes the lemonpipe file, then kills all child processes.
shutdown() {
	echo "Shutting down."
	rm $lemonpipe && echo "lemonpipe removed."
	echo "Killing child processes."
	for c in $KIDS; do
		kill $c
	done
	echo "Done."
	exit 0
}

trap shutdown INT QUIT

echo "Modules start"
modules=("${left[@]}" "${center[@]}" "${right[@]}")
for mod in "${modules[@]}"; do
	if [[ "$mod" != "spacer" ]]; then
		~/.config/lemonbar/mod_$mod | sed --unbuffered "s/^/MOD $mod /g" >"$lemonpipe" &
		KIDS="$KIDS $!"
	fi
done

echo "Main loop start"
declare -A mod_cache

mod_cache["spacer"]="|"
while IFS=' ' read -ra elems; do
	mod_cache["${elems[1]}"]="${elems[@]:2}"

	[[ "$DBUG" -eq 1 ]] && echo "${elems[@]}" >/dev/stderr

	printf "%%{l}"
	for l in $left; do
		printf "%s" "${mod_cache["$l"]}"
	done

	printf "%%{c}"
	for c in $center; do
		printf "%s" "${mod_cache["$c"]}"
	done

	printf "%%{r}"
	for r in $right; do
		printf "%s" "${mod_cache["$r"]}"
	done
	printf "\n"
done <$lemonpipe | lemonbar -F "$FG" -B "$BG" -f "JetBrainsMono Nerd Font Mono"
