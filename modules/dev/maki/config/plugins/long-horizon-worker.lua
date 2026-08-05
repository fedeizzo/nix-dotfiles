local WORKER_TITLE = "Long-horizon worker"

local function response(ok, fields)
    local value = fields or {}
    value.ok = ok
    return value
end

local function encode(value)
    local json, err = maki.json.encode(value)
    return json or ("failed to encode result: " .. tostring(err))
end

local function find_worker()
    local live, live_err = maki.session.live()
    if live then
        for _, session in ipairs(live) do
            if session.title == WORKER_TITLE then
                return session, true
            end
        end
    elseif live_err and live_err ~= "no interactive UI attached" then
        return nil, nil, live_err
    end

    local stored, stored_err = maki.session.list()
    if stored then
        for _, session in ipairs(stored) do
            if session.title == WORKER_TITLE then
                return session, false
            end
        end
    elseif stored_err then
        return nil, nil, stored_err
    end

    return nil, false
end

local function worker_status()
    local session, live, err = find_worker()
    if err then
        return response(false, { error = err })
    end
    if not session then
        return response(true, { status = "not_started" })
    end
    return response(true, {
        session_id = session.id,
        status = live and session.status or "stored",
        focused = session.focused or false,
        updated_at = session.updated_at,
    })
end

local function task_prompt(goal)
    local objective
    local lower_goal = goal:lower()
    if not goal:find("[\r\n]")
        and (lower_goal:match("%.md$") or lower_goal:match("%.markdown$"))
    then
        objective = "Read the Markdown plan at `" .. goal .. "` and execute it completely."
    else
        objective = goal
    end

    return [[You are the only long-horizon worker for this project.

Work autonomously on the objective below. Inspect the repository, implement the
change, run appropriate validation, and review the final diff. Do not stop after
planning or merely suggest changes. Prefer Jujutsu (`jj`) over Git. Do not create
additional subagents, push, publish, deploy, or perform destructive version-control
operations. Follow the project's instructions and Maki's normal permission policy.
If blocked by missing access or conflicting requirements, explain the blocker and
wait for supervisor guidance. When complete, provide a concise report with changed
files, validation results, and residual risks.

Objective:
]] .. objective
end

local function start_worker(goal)
    goal = tostring(goal or ""):match("^%s*(.-)%s*$")
    if goal == "" then
        return response(false, { error = "provide an inline objective or Markdown plan path" })
    end

    local existing, _, find_err = find_worker()
    if find_err then
        return response(false, { error = find_err })
    end
    if existing then
        return response(false, {
            error = "a long-horizon worker session already exists",
            session_id = existing.id,
        })
    end

    local id, create_err = maki.session.new({
        prompt = task_prompt(goal),
        focus = false,
    })
    if not id then
        return response(false, { error = create_err or "could not create worker session" })
    end

    local renamed, rename_err = maki.session.set_title({ id = id, title = WORKER_TITLE })
    if not renamed then
        maki.session.delete(id)
        return response(false, { error = rename_err or "could not name worker session" })
    end

    return response(true, {
        session_id = id,
        status = "started",
        message = "worker started in a background Maki session",
    })
end

local function steer_worker(instruction)
    instruction = tostring(instruction or ""):match("^%s*(.-)%s*$")
    if instruction == "" then
        return response(false, { error = "steering instruction is required" })
    end

    local session, live, err = find_worker()
    if err then
        return response(false, { error = err })
    end
    if not session then
        return response(false, { error = "no long-horizon worker session exists" })
    end
    if not live then
        return response(false, {
            error = "worker is not live; focus it before sending more work",
            session_id = session.id,
        })
    end

    local state, prompt_err = maki.session.prompt(
        "Supervisor instruction:\n\n" .. instruction,
        { session = session.id }
    )
    if not state then
        return response(false, { error = prompt_err or "could not steer worker" })
    end
    return response(true, {
        session_id = session.id,
        status = state,
    })
end

local function focus_worker()
    local session, _, err = find_worker()
    if err then
        return response(false, { error = err })
    end
    if not session then
        return response(false, { error = "no long-horizon worker session exists" })
    end

    local focused, focus_err = maki.session.focus(session.id)
    if not focused then
        return response(false, { error = focus_err or "could not focus worker" })
    end
    return response(true, {
        session_id = session.id,
        status = "focused",
    })
end

local function stop_worker()
    local session, _, err = find_worker()
    if err then
        return response(false, { error = err })
    end
    if not session then
        return response(false, { error = "no long-horizon worker session exists" })
    end

    local deleted, delete_err = maki.session.delete(session.id)
    if not deleted then
        return response(false, {
            error = delete_err or "could not delete worker session",
            hint = "the focused session cannot be deleted; focus another session first",
        })
    end
    return response(true, {
        session_id = session.id,
        status = "stopped_and_deleted",
    })
end

local function flash_result(result)
    if not result.ok then
        maki.ui.flash("long-horizon-worker: " .. tostring(result.error))
        return
    end
    maki.ui.flash(string.format(
        "long-horizon-worker: %s%s",
        tostring(result.status),
        result.session_id and (" (" .. result.session_id .. ")") or ""
    ))
end

maki.api.register_command({
    name = "/long-horizon-worker",
    description = "Start and control one background Maki worker session.",
    nargs = "*",
    handler = function(opts)
        local args = tostring(opts.args or ""):match("^%s*(.-)%s*$")
        local action, remainder = args:match("^(%S+)%s*(.-)$")
        local result

        if args == "" or action == "status" then
            result = worker_status()
        elseif action == "steer" then
            result = steer_worker(remainder)
        elseif action == "focus" or action == "result" then
            result = focus_worker()
        elseif action == "stop" then
            result = stop_worker()
        else
            result = start_worker(args)
        end
        flash_result(result)
    end,
})

maki.api.register_tool({
    name = "long_horizon_worker_start",
    description = "Start the single long-horizon worker as a background Maki session. It inherits the current project's model, tools, MCP access, and permission policy.",
    schema = {
        type = "object",
        properties = {
            objective = { type = "string" },
            goal_file = { type = "string", description = "Markdown plan path in the current project." },
        },
        anyOf = {
            { required = { "objective" } },
            { required = { "goal_file" } },
        },
    },
    handler = function(input)
        return encode(start_worker(input.goal_file or input.objective))
    end,
})

maki.api.register_tool({
    name = "long_horizon_worker_status",
    description = "Return the current background worker session ID and Maki status.",
    schema = { type = "object", properties = {} },
    handler = function()
        return encode(worker_status())
    end,
})

maki.api.register_tool({
    name = "long_horizon_worker_steer",
    description = "Queue a supervisor instruction for the background worker session.",
    schema = {
        type = "object",
        properties = { instruction = { type = "string" } },
        required = { "instruction" },
    },
    handler = function(input)
        return encode(steer_worker(input.instruction))
    end,
})

maki.api.register_tool({
    name = "long_horizon_worker_focus",
    description = "Focus the worker session to inspect its conversation and latest result.",
    schema = { type = "object", properties = {} },
    handler = function()
        return encode(focus_worker())
    end,
})

maki.api.register_tool({
    name = "long_horizon_worker_stop",
    description = "Cancel and delete the worker session. Focus another session first if the worker is currently focused.",
    schema = { type = "object", properties = {} },
    handler = function()
        return encode(stop_worker())
    end,
})
