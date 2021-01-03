#!/usr/bin/env bash
cache_file="$XDG_CACHE_HOME/xmonadSpotCache.txt"
files=$(fd --base-directory '/home/fedeizzo/' '.pdf|.png|.jpg|.mp4')
echo "" > $cache_file
for f in $files; do
    filename=$(basename $f)
    echo "$filename;$f" >> $cache_file
done
