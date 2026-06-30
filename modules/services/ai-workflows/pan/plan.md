# pan optimize: Test-Driven Prompt Engineering

## Overview
`pan optimize` is a proposed CLI command that acts as an autonomous, embedded Prompt Engineer for the `pan` ecosystem. Its goal is to iteratively refine and optimize an agent's `prompt.md` until the agent perfectly passes a suite of behavioral tests.

## Architecture & Workflow

### 1. The Inputs
To function, the command requires three components:
- **Target Agent**: The specific agent being optimized (e.g., `--agent=lunchmoney`).
- **Test Suite**: A set of declarative scenarios (defined in Go, YAML, or JSON) that outline user inputs and expected agent behaviors (e.g., *Expected Tool Call: adk_request_confirmation*).
- **Meta-Prompt**: An internal system prompt that instructs a secondary LLM (the Optimizer) on how to analyze test failures and rewrite prompts safely without destroying the agent's core persona.

### 2. The Optimization Loop
The command executes a closed-loop system capped at a maximum number of iterations:

1. **Evaluation (The Dry-Run)** 
   The harness spins up the target agent in a specialized execution environment where `context.WithValue(ctx, "dry_run", true)` is injected. The agent runs through the test scenarios.
2. **Scoring** 
   The harness evaluates the agent's output and tool usage against the test suite's assertions. If all tests pass, the command exits with success.
3. **Feedback & Optimization** 
   If tests fail, the harness captures the exact failure reasons. It sends the Meta-Prompt, the current `prompt.md`, and the failure logs to the Optimizer LLM. The Optimizer diagnoses the missing instructions and generates a revised `prompt.md`.
4. **Update** 
   The harness overwrites the `prompt.md` file on disk with the new version and loops back to Step 1.

## The Dry Run Implementation
The most critical part of this architecture is handling tool execution safely during the automated loop. 

- **Read-Only Tools (Real Data):** Tools that fetch state (like `GetCategories` or `GetTags`) continue to hit the real APIs. This ensures the Optimizer operates on grounded, realistic data schemas rather than overfitting to synthetic mocks.
- **Mutating Tools (Dry Run):** Tools that change state (like `UpdateTransaction`) must check the context for the `dry_run` flag. If true, the tool performs local validation on the arguments passed by the LLM and immediately returns a simulated success response (e.g., `{"success": true}`). This prevents the optimizer from destroying production data over dozens of iterations.

## Summary
By implementing this, `pan` brings Test-Driven Development (TDD) directly into the prompt engineering workflow. Instead of manually tweaking prompts and running tests by hand, a developer simply writes a failing behavioral test, runs `pan optimize`, and commits the auto-generated, perfectly tuned `prompt.md`.
