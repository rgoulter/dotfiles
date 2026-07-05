---
name: agent-plain
description: >
  Run shell commands through agent-plain to suppress ANSI color and terminal
  control codes in captured output (agent-shell, ACP clients, headless agents).
  Use when running cargo, gh, devenv, or any command where escape sequences
  would pollute tool output. Triggers on agent-plain, devenv shell, cargo test,
  cargo build, gh api, gh pr, ANSI escapes, control codes, CLICOLOR_FORCE,
  NO_COLOR, or plain/monochrome terminal output in editor agent buffers.
metadata:
  short-description: "Wrap commands with agent-plain for clean output"
---

# agent-plain — plain shell output

Some agent runtimes inject color-forcing environment variables into bash tool
subprocesses (`CLICOLOR_FORCE`, `FORCE_COLOR`, `COLORTERM=truecolor`, etc.),
even when `NO_COLOR` is set. Captured output (Emacs agent-shell, ACP clients,
non-TTY pipes) then shows raw ANSI escapes and control codes instead of readable
text.

`agent-plain` is a small wrapper that unsets the force vars and sets monochrome
env before exec'ing the real command.

## When to use

**Always prefix** `agent-plain` when the bash tool will run:

| Tool | Examples |
|------|----------|
| **devenv** | `devenv shell`, `devenv shell -- make test`, `devenv test` |
| **cargo** | `cargo build`, `cargo test`, `cargo check`, `cargo clippy` |
| **gh** | `gh pr`, `gh issue`, `gh run`, `gh api`, `gh api graphql` |

Also use for any other command known to emit ANSI or OSC sequences into
non-TTY captures (nix builds, colored test runners, etc.).

### When agent-plain is optional

- **cargo** and most **gh** subcommands already respect `NO_COLOR` even when
  `CLICOLOR_FORCE` is set. Wrapping is still fine — cheap insurance.
- **devenv** often ignores `NO_COLOR` and follows `CLICOLOR_FORCE` — wrapping is
  **required** for clean output.
- **`gh api graphql`** without `--jq` syntax-highlights JSON regardless of
  `NO_COLOR`. Prefer `gh api graphql ... --jq '<expr>'`, or wrap with
  `agent-plain` and still use `--jq` when you need parseable output.

## How to run

`agent-plain` lives at `~/.local/bin/agent-plain` (installed via Home Manager
dotfiles). It must be on `PATH`; if a command fails with "not found", use the
full path.

**Pattern:** put `agent-plain` first, then the real command and all its args.

```bash
agent-plain devenv shell -- make test
agent-plain cargo test --workspace
agent-plain cargo clippy --all-targets
agent-plain gh pr view 42
agent-plain gh api graphql -f query='{ viewer { login } }' --jq .data.viewer.login
```

For compound shell (pipes, `&&`, redirects), wrap the whole pipeline in `sh -c`:

```bash
agent-plain sh -c 'cargo test 2>&1 | tail -20'
```

Do **not** rely on inline `env -u CLICOLOR_FORCE` unless `agent-plain` is
unavailable — the wrapper also clears `COLORTERM`, sets `TERM=dumb`, and sets
`CARGO_TERM_COLOR=never` / `CARGO_TERM_PROGRESS_WHEN=never`.

## What agent-plain sets

```bash
env \
  -u CLICOLOR_FORCE \
  -u COLORTERM \
  NO_COLOR=1 \
  CLICOLOR=0 \
  FORCE_COLOR=0 \
  TERM=dumb \
  CARGO_TERM_COLOR=never \
  CARGO_TERM_PROGRESS_WHEN=never \
  "$@"
```

## Rules for the agent

1. **Default to wrapping** — if the planned bash command starts with `cargo`,
   `gh`, or `devenv`, prefix `agent-plain` without asking.
2. **Read output as plain text** — do not assume colors or cursor movement in
   tool results; if escapes still appear, mention it and retry with
   `agent-plain`.
3. **Prefer `--jq` for gh api/graphql** — structured extraction beats pretty
   highlighted JSON.
4. **Do not strip at the prompt** — fixing env at execution time is more
   reliable than post-filtering in the client.

## Slash command

`/agent-plain` — apply these rules for the current task (run cargo/gh/devenv
through the wrapper).
