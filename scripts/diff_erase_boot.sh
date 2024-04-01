#!/usr/bin/env bash
doas mkdir -p /mnt
doas mount -o subvol=/ /dev/mapper/nixenc /mnt
fs-diff
