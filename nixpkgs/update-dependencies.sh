#!/bin/bash
cd $(dirname $0)
nix-shell --run "niv update"
