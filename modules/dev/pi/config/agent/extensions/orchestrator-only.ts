/**
 * Lean-Orchestrator Mode Extension
 *
 * Forces the main agent to act as a lean orchestrator: all mutation and heavy
 * work is delegated via the subagent tool, and every subagent call is pushed to
 * write its full report to a file (file-only) so the orchestrator receives only
 * a compact reference. This keeps the orchestrator's own context as small as
 * possible while the details live on disk and survive context compaction.
 *
 * Stacks on top of pi-subagents. Does not replace it.
 *
 * Behavior when enabled:
 *   - write, edit removed from the active toolset (setActiveTools); bash kept
 *   - subagent({ async: true }) hard-rejected via tool_call hook
 *   - context message injected each turn telling the model it is a lean orchestrator
 *
 * Toggle: /orchestrator command, Ctrl+Alt+O, or --orchestrator CLI flag.
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Key } from "@earendil-works/pi-tui";

// Mutating tools removed from the orchestrator's active set. bash stays available
// for inspection/verification; only direct file mutation is delegated to workers.
const MUTATING_TOOLS = new Set(["write", "edit"]);

// Tools this extension manages (so restore logic knows what it added/removed).
const MANAGED_TOOLS = new Set([...MUTATING_TOOLS]);

interface OrchestratorState {
  enabled: boolean;
  toolsBeforeOrchestrator?: string[];
}

function uniqueToolNames(toolNames: string[]): string[] {
  return [...new Set(toolNames)];
}

function getOrchestratorTools(activeToolNames: string[]): string[] {
  // Keep everything except mutating tools. Preserves bash + read/ls/grep/find/search_web/subagent/ctx_*.
  return uniqueToolNames(activeToolNames.filter((name) => !MUTATING_TOOLS.has(name)));
}

export default function orchestratorOnlyExtension(pi: ExtensionAPI): void {
  let orchestratorEnabled = false;
  let toolsBeforeOrchestrator: string[] | undefined;

  pi.registerFlag("orchestrator", {
    description: "Start in lean-orchestrator mode (delegate work, no direct file edits, bash kept)",
    type: "boolean",
    default: false,
  });

  function updateStatus(ctx: ExtensionContext): void {
    if (orchestratorEnabled) {
      ctx.ui.setStatus("orchestrator-only", ctx.ui.theme.fg("accent", "🎛 orchestrator"));
    } else {
      ctx.ui.setStatus("orchestrator-only", undefined);
    }
  }

  function enableOrchestratorTools(): void {
    if (toolsBeforeOrchestrator === undefined) {
      toolsBeforeOrchestrator = pi.getActiveTools();
    }
    pi.setActiveTools(getOrchestratorTools(toolsBeforeOrchestrator));
  }

  function restoreFullTools(): void {
    pi.setActiveTools(toolsBeforeOrchestrator ?? pi.getActiveTools());
    toolsBeforeOrchestrator = undefined;
  }

  function persistState(): void {
    pi.appendEntry("orchestrator-only", {
      enabled: orchestratorEnabled,
      toolsBeforeOrchestrator,
    });
  }

  function toggleOrchestrator(ctx: ExtensionContext): void {
    orchestratorEnabled = !orchestratorEnabled;

    if (orchestratorEnabled) {
      enableOrchestratorTools();
      ctx.ui.notify("Lean-orchestrator mode enabled. write/edit disabled (bash kept); delegate work via subagent tool.");
    } else {
      restoreFullTools();
      ctx.ui.notify("Orchestrator-only mode disabled. Full tools restored.");
    }
    updateStatus(ctx);
    persistState();
  }

  pi.registerCommand("orchestrator", {
    description: "Toggle orchestrator-only mode (delegation via subagents, no direct mutation)",
    handler: async (_args, ctx) => toggleOrchestrator(ctx),
  });

  pi.registerShortcut(Key.ctrlAlt("o"), {
    description: "Toggle orchestrator-only mode",
    handler: async (ctx) => toggleOrchestrator(ctx),
  });

  // Hard-reject async subagent launches even if the model passes async:true.
  // asyncByDefault:false in config sets the default, but this seals the escape hatch.
  pi.on("tool_call", async (event) => {
    if (!orchestratorEnabled || event.toolName !== "subagent") return;

    const input = event.input as { async?: boolean };
    if (input?.async === true) {
      return {
        block: true,
        reason:
          "Lean-orchestrator mode: async subagents disabled. Use sync (async:false or omit). " +
          "Blocking + file-only output keeps parent context lean.",
      };
    }
  });

  // Inject context each turn so the model knows it must delegate implementation work.
  pi.on("before_agent_start", async () => {
    if (!orchestratorEnabled) return;

    return {
      message: {
        customType: "orchestrator-only-context",
        content: `[LEAN-ORCHESTRATOR MODE ACTIVE]
You are a lean orchestrator. Keep your own context as small as possible. Do not implement, mutate, or dump large output yourself — delegate it.

Toolset:
- bash is available for INSPECTION/VERIFICATION only (git status/diff/log, builds, tests, grep, find, file probes). Never use bash to create, edit, move, or delete project files — that is a worker's job.
- read, ls, grep, find, search_web: situational awareness + reading subagent report files.
- write, edit are disabled. All mutation goes through subagents.

Subagent contract (this is what keeps your context small):
- Run subagents synchronously (blocking). Do NOT pass async:true.
- On EVERY subagent call pass: output: "<agent>.md", outputMode: "file-only".
  - The subagent writes its full report to ~/.pi/subagent-outputs/<agent>.md and you receive only a compact file reference.
  - Read that file with read (or bash) only when you need the detail. Never paste its contents into your context.
- Tasks must be small and atomic: one concrete objective, scoped paths/files, and an exact deliverable. Never hand an agent a vague or open-ended mission. Give it a precise starting seam and state what it must return.
- Pick the right agent: scout (recon), worker (implement), reviewer (verify), researcher (external facts), oracle (risky decision).

Orchestrator loop:
1. If you need code facts, use scout (or read/grep) to get a compact map — do not read whole files yourself.
2. Delegate each concrete unit of work to the matching agent, passing output + outputMode as above.
3. Verify by reading the subagent's report file and/or running bash checks (git diff, tests, builds).
4. Report to the user a short synthesis: cite files/paths and what changed. Do not reproduce the subagent report.`,
        display: false,
      },
    };
  });

  // Restore persisted state on session start/resume.
  pi.on("session_start", async (_event, ctx) => {
    if (pi.getFlag("orchestrator") === true) {
      orchestratorEnabled = true;
    }

    const entries = ctx.sessionManager.getEntries();
    const orchestratorEntry = entries
      .filter(
        (e: { type: string; customType?: string }) =>
          e.type === "custom" && e.customType === "orchestrator-only",
      )
      .pop() as { data?: OrchestratorState } | undefined;

    if (orchestratorEntry?.data) {
      orchestratorEnabled = orchestratorEntry.data.enabled ?? orchestratorEnabled;
      toolsBeforeOrchestrator = orchestratorEntry.data.toolsBeforeOrchestrator ?? toolsBeforeOrchestrator;
    }

    if (orchestratorEnabled) {
      enableOrchestratorTools();
    }
    updateStatus(ctx);
  });
}
