#!/usr/bin/env bash
# @vicinae.schemaVersion 1
# @vicinae.title confluence-s8s-search
# @vicinae.mode inline
# @vicinae.exec ["/bin/bash"]
# @vicinae.argument1 { "type": "text", "placeholder": "query" }

open "https://datadoghq.atlassian.net/wiki/search?text=${1}&product=confluence&spaces=SYN"
