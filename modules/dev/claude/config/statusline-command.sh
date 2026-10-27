#!/bin/sh
# Claude Code statusline: shows 5-hour and 7-day rate limit usage.

input=$(cat)

# Expose the latest rate-limit snapshot so agents can read their remaining budget.
mkdir -p "$HOME/.cache/claude-statusline"
echo "$input" | jq -c '{at: (now | todate), session: .session_id, rate_limits: .rate_limits}' \
  > "$HOME/.cache/claude-statusline/latest.json" 2>/dev/null

five=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
week=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')

out=""
if [ -n "$five" ]; then
  out="5h: $(printf '%.0f' "$five")%"
fi
if [ -n "$week" ]; then
  if [ -n "$out" ]; then
    out="$out | 7d: $(printf '%.0f' "$week")%"
  else
    out="7d: $(printf '%.0f' "$week")%"
  fi
fi

if [ -z "$out" ]; then
  out="rate limits unavailable"
fi

printf '\033[2m%s\033[0m' "$out"
