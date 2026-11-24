// The tab bar's "Ops" button (props.role = "button"): a small block the work profile
// puts right of the tabs-jj bar, drawn like its Files / Changes buttons. It opens
// and closes the ops board in the side panel (the panel-body stack, so it replaces
// Files or Changes there, as they replace each other); in the ops layout, where the
// board has its own column, it shows and hides that column instead.

import { svg } from "gpui-kit";
import { h_flex } from "gpui-base";
import { run, theme, layout, shared, setShared } from "herdr";

const PANELS = "workspace.panels"; // tabs-jj remembers the open panel per space for these

function find(node, id) {
  if (node?.id === id) return node;
  for (const child of node?.children ?? []) {
    const found = find(child, id);
    if (found) return found;
  }
  return null;
}

export class Button {
  init() {
    // tabs-jj's own default, so adding ours doesn't hide its Issues button.
    const panels = shared(PANELS) ?? ["files", "changes", "gh-issues"];
    if (!panels.includes("ops")) setShared(PANELS, [...panels, "ops"]);
  }

  render() {
    const t = theme();
    const root = layout();
    const column = find(root, "ops-board");
    const panel = find(root, "panel"), body = find(root, "panel-body");
    const inPanel = (body?.children ?? []).some((c) => c.id === "ops");
    const on = column ? !column.hidden : Boolean(panel && !panel.hidden && body?.active === "ops");
    const toggle = () => {
      if (column) run("layout.toggle", { id: "ops-board" });
      else if (on) run("layout.hide", { id: "panel" });
      else if (inPanel) { run("layout.select", { stack: "panel-body", child: "ops" }); run("layout.show", { id: "panel" }); }
      else run("mod.open", { name: "ops" }); // no panel slot: the board window
    };
    return h_flex().size_full().pr(10).items_center().justify_end().bg(t.chrome).border_b(1).border_color(t.border)
      .child(h_flex().id("panel-ops").h(26).px(8).gap(5).flex_none().items_center().rounded(6)
        .text_size(12).bg(on ? t.selected : t.chrome).text_color(on ? t.foreground : t.muted)
        .hover((s) => s.bg(t.hover)).on_mouse_down("left", (_e, cx) => cx.stop_propagation())
        .on_click(() => toggle())
        .child(svg("mods/ops/ops.svg").size(14)).child("Ops"));
  }
}
