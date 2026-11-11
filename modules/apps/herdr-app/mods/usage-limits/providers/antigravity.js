// Antigravity: `agy -p /quota --output-format json` (helpers/antigravity-quota.sh).
// Local addition, not upstream. agy reports *remaining* fractions per bucket.
import { makeWindow, okReport, errorReport, unavailableReport, toEpoch } from "../lib/report.js";

const SOURCE = "agy CLI";
const DURATION = { "5h": 5 * 3600, weekly: 7 * 24 * 3600 };
const SHORT = { "5h": "5h", weekly: "wk" };
// Short bar labels for the known groups; others fall back to their name.
const GROUP = { "Gemini Models": "Gemini", "Claude and GPT models": "3P" };

export function parse(text, now) {
  let msg;
  try {
    msg = JSON.parse(String(text ?? "").trim());
  } catch {
    return errorReport(SOURCE, "No reply from the agy CLI", now);
  }
  if (msg.status !== "SUCCESS") return errorReport(SOURCE, String(msg.error ?? msg.status ?? "agy could not read quota"), now);
  const windows = [];
  for (const g of msg.command?.data?.groups ?? []) {
    const group = GROUP[g.name] ?? g.name;
    for (const b of g.buckets ?? []) {
      if (b?.remaining_fraction == null) continue;
      const win = SHORT[b.window] ?? b.window;
      windows.push(makeWindow({
        id: b.id,
        short: `${group} ${win}`,
        long: `${g.name} · ${String(b.name).replace(/ Remaining$/, "")}`, // bar shows used, not left
        percent: (1 - Number(b.remaining_fraction)) * 100,
        resetsAt: toEpoch(b.reset_time),
        durationSec: DURATION[b.window] ?? null,
      }));
    }
  }
  if (windows.length === 0) return unavailableReport(SOURCE, "No quota buckets", now);
  return okReport(SOURCE, now, windows);
}

export default {
  id: "antigravity",
  name: "Antigravity",
  icon: "icons/antigravity.svg",
  helper: "helpers/antigravity-quota.sh",
  parse,
};
