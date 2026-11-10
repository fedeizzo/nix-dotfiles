// Reads the vault's ops files once: { tasks, dels, notes, errors }. Read-only.

import * as fs from "fs/promises";
import { readTextFile, exec } from "herdr";
import { parseTasks, parseDelegations, frontmatter, taskIdOf } from "./parse.js";

async function listNotes(dir) {
  try { return (await fs.readdir(dir)).filter((n) => n.endsWith(".md")); } catch {}
  try {
    const r = await exec("ls", ["-1", dir]);
    return r.code === 0 ? r.stdout.split("\n").filter((n) => n.endsWith(".md")) : [];
  } catch { return []; }
}

export async function loadOps(vault) {
  const ops = `${vault}/ops`;
  const errors = [];
  const read = (name) => readTextFile(`${ops}/${name}`).catch((e) => { errors.push(`${name}: ${e?.message ?? e}`); return null; });
  const [tasks, dels] = await Promise.all([read("Tasks.md"), read("Delegations.md")]);
  if (tasks === null && !errors.length) errors.push(`No ${ops}/Tasks.md (vault: ${vault})`);
  const notes = {}; // task id -> { file, fm }
  const dir = `${ops}/tasks`;
  for (const name of await listNotes(dir)) {
    const file = `${dir}/${name}`;
    const text = await readTextFile(file).catch(() => null);
    const { fm } = frontmatter(text ?? "");
    const id = (fm.task && /^T-\d+$/.test(fm.task) ? fm.task : null) ?? taskIdOf(name);
    if (id) notes[id] = { file, fm };
  }
  return {
    tasks: tasks === null ? null : parseTasks(tasks),
    dels: dels === null ? null : parseDelegations(dels),
    notes,
    errors,
  };
}

// The note of a board task: its note: link when that file exists, else by task id.
export function noteFor(data, vault, task) {
  if (task?.note) {
    const file = `${vault}/${task.note.replace(/\.md$/i, "")}.md`;
    const hit = Object.values(data?.notes ?? {}).find((n) => n.file === file);
    if (hit) return hit;
  }
  return data?.notes?.[task?.id ?? task] ?? null;
}
