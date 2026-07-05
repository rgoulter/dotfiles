# Agent instructions for this repository

Personal dotfiles managed with [Home Manager](https://github.com/nix-community/home-manager) and Nix flakes.

## Home Manager

Use **`just`** from the repo root (see `justfile`). Managed paths are declared in `dotfiles.nix` (and `grok.nix` / `themes.nix` where noted).

Home Manager **builds** a home configuration and, on switch, **installs symlinks and managed files** under `~/`. Editing files in this repo does not change what apps read until you apply — e.g. `doom/config.el` here is not live at `~/.config/doom/` until switch runs.

**After editing dotfiles, verify with a build** (does not change the live system):

```sh
just                  # default: build
just build
```

This runs `home-manager build --flake .#${USER}-${system}` and checks the configuration compiles.

### Nix flakes only see git-tracked files

This flake's source is the git tree. **New or edited paths are invisible to `just build` until they are at least staged** (`git add`). If build fails with "not tracked by Git", stage the paths (or commit them) and rebuild.

The same applies to new files under `agents/skills/` referenced from `dotfiles.nix`.

**Apply changes** (makes the built config live — rewrites symlinks and managed files under `~/`):

```sh
just switch           # alias: just apply
```

The flake attribute must match `$USER` and the current system (for example `richard.goulter-aarch64-darwin`, `rgoulter-x86_64-linux`). See `flake.nix` → `homeConfigurations` if build or switch fails.

Other recipes:

```sh
just fmt              # treefmt via the flake
```

Agents: prefer **`just build`** as the post-change check. Do not run `just switch` unless the user asks to apply.

## Emacs: Doom is primary

Day-to-day Emacs is **Doom Emacs**, launched via **Chemacs2**.

| What | Path |
| --- | --- |
| Doom install (not in this repo) | `~/doom-emacs` |
| `$DOOMDIR` (symlinked from repo) | `~/.config/doom/` → `doom/` |
| Chemacs profile selector | `~/.emacs-profile` → `chemacs/profile` (default: `doom`) |
| Chemacs profiles | `chemacs/profiles.el` |
| Machine-local Emacs Lisp | `~/.config/doom/local.el` (not tracked; see `local.el.template`) |

Tracked Doom files (via `just switch` → `~/.config/doom/`):

- `doom/init.el` — enabled modules (`doom sync` after changes)
- `doom/packages.el` — extra packages (`doom sync` after changes)
- `doom/config.el` — private config (`use-package!`, `after!`, `map!`, etc.)
- `doom/lisp/*.el` — local libraries (e.g. `agent-shell-grok.el`)

A legacy **straight.el** profile still exists as Chemacs `"default"` (`emacs-rgoulter/`), but treat **Doom** as the canonical Emacs setup unless asked otherwise.

### Doom conventions in `doom/config.el`

- Prefer **`use-package!`** for packages you own (hooks, keys, defer) — see `blamer`, `copilot`, `ranger`.
- Use **`after!`** for post-load tweaks to Doom modules; keep **`after!` blocks in alphabetical order** at the end of the file.
- Use **`use-package!`** for extra packages from `packages.el` (especially when they need `:commands` autoloads or keybindings at startup).
- Wrap package-specific settings in `after!` / `use-package!` so Doom loads them in the right order.
- After changing `doom/packages.el` or `doom/init.el`, also run **`doom sync`** in `~/doom-emacs`.
- A running Emacs session may need `M-x doom/reload` to pick up `config.el` / `lisp/` keybinding changes after switch.

### AI agents in Emacs

Grok is integrated via **agent-shell** (ACP) in `doom/lisp/agent-shell-grok.el`, wired from `doom/config.el` with `use-package! agent-shell` (requires `agent-shell-grok`, registers the agent, binds keys). Only `(package! agent-shell)` is needed in `packages.el`; straight installs `acp` and `shell-maker` as dependencies.

## Other layout notes

- **Shell**: fish — `fish/`
- **Grok CLI config**: `grok/config.toml` and `grok/sandbox.toml` → `~/.grok/` (via `grok.nix`)
- **Pi agent assets**: `pi/agent/` (themes/extensions; separate from Doom)
- **Nix module entrypoint**: `dotfiles.nix` exports `homeManagerModules.dotfiles`
- **Agent skills**: `agents/skills/` → `~/.agents/skills/` via Home Manager (cross-runtime; see [Agent Skills](https://agentskills.io))

When editing config, match existing style in the target file. Keep changes focused; do not refactor unrelated dotfiles.

## Git and pull requests

For commit and PR conventions across Richard's repositories, follow the user-scope **`git-workflow`** skill (`agents/skills/git-workflow/`, deployed to `~/.agents/skills/git-workflow/`). Summary: atomic commits; smaller focused PRs; amend or squash fixups on unpublished branches instead of chains of "fixed …" commits.
