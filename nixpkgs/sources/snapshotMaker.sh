#!/usr/bin/env bash

MAX_BOOT=2
MAX_HOOK=1
MAX_CRON=3

now=$(date "+%Y-%m-%d_%H:%M:%S")
helpMessage="this is the help message"

count_snapshot() {
    btrfs su list /snapshot | grep $1 | awk -F' ' '{print $NF}' | sort | wc -l
}
get_snapshot() {
   btrfs su list /snapshot | grep $1 | awk -F' ' '{print $NF}' | sort | head -n $2
}
delete_snapshots() {
    btrfs su delete /snapshot/$1
}
make_snapshot() {
    btrfs su snapshot -r "$1" /snapshot/"$now""$2"
}

pacman_hook_remove() {
    tmp=$(count_snapshot "pacman")
    root_to_delete=$(($tmp - $MAX_HOOK))
    snapshots=$(get_snapshot "pacman" $root_to_delete)
    for s in $snapshots; do
        delete_snapshots $s
    done
}

pacman_hook_make() {
    make_snapshot "/" "_root_pacman"
}

boot_remove() {
    tmp=$(count_snapshot "root_boot")
    root_to_delete=$(($tmp - $MAX_BOOT))
    tmp=$(count_snapshot "home_boot")
    home_to_delete=$(($tmp - $MAX_BOOT))
    snapshots=$(get_snapshot "root_boot" $root_to_delete)
    for s in $snapshots; do
        delete_snapshots $s
    done
    snapshots=$(get_snapshot "home_boot" $home_to_delete)
    for s in $snapshots; do
        delete_snapshots $s
    done
}

boot_make() {
    make_snapshot "/" "_root_boot"
    make_snapshot "/home" "_home_boot"
}

cron_remove() {
    tmp=$(count_snapshot "cron")
    home_to_delete=$(($tmp - $MAX_CRON))
    snapshots=$(get_snapshot "cron" $home_to_delete)
    for s in $snapshots; do
        delete_snapshots $s
    done
}

cron_make() {
    make_snapshot "/home" "_home_cron"
}

case $1 in
    "hook")
        pacman_hook_make
        pacman_hook_remove
        ;;
    "boot")
        boot_make
        boot_remove
        ;;
    "cron")
        cron_make
        cron_remove
        ;;
    *)
        echo $helpMessage
        ;;
esac
