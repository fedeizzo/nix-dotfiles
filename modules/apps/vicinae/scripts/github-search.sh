#!/usr/bin/env bash
# @vicinae.schemaVersion 1
# @vicinae.title github-search
# @vicinae.mode fullOutput
# @vicinae.exec ["/bin/bash"]
# @vicinae.argument1 { "type": "text", "placeholder": "query" }

open "https://github.com/search?q=${1}&type=code"
