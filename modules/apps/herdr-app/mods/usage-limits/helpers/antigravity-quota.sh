#!/bin/sh
# Prints Antigravity's quota as agy's print-mode JSON for `/quota` and exits.
# `/quota` is a read-only slash command: agy answers it without starting a turn
# (num_turns 0, zero tokens) and authenticates itself.
# Exit: 0 = printed the reply, 127 = agy not found, other = agy failed.
AGY=$(command -v "${HERDR_USAGE_AGY_BIN:-agy}" 2>/dev/null) || exit 127
cd "$HOME" || exit 1
exec timeout 20 "$AGY" -p /quota --output-format json 2>/dev/null
