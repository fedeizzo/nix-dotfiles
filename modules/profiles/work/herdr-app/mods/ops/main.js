// Ops: the orchestrator workflow in the window. One mod, three roles (props.role):
//   (none)   the ops board: task lanes and delegations from the vault, with live status
//   "watch"  an invisible floating block: attention toasts for delegate/standing agents,
//            and the key commands (ops.orchestrator, ops.board, ops.<standing agent>)
//   "note"   a task note viewer pane, opened by the board
//   "calendar" the Due calendar as a window (ops.calendar, or the daily due toast)
// Settings (vault, quick actions, sound) come from props, see config.js.

import { View } from "gpui-kit";
import { props } from "herdr";
import { Board } from "./board.js";
import { Watch } from "./watch.js";
import { Note } from "./note.js";
import { CalendarView } from "./calendar.js";

export default class Ops extends View {
  init(_props, cx) {
    const role = props()?.role;
    const Role = { watch: Watch, note: Note, calendar: CalendarView }[role] ?? Board;
    this.impl = new Role();
    this.impl.init(cx);
  }

  render(cx) {
    return this.impl.render(cx);
  }

  close(cx) {
    return this.impl.close?.(cx);
  }
}
