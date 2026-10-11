# agent-harbor.el

Agent Harbor integration for Emacs.

[Agent Harbor](https://agent-harbor.com) runs coding agents in isolated
workspaces and records every session. This package is the editor half of
that: it talks to the local `ah` daemon over its REST and SSE endpoints, so
tasks can be started, watched, replied to and reviewed without leaving Emacs.

**Version 0.6.8**, published from [`ide-plugins/emacs`][source] at
[`ed781cfefad4`][commit] in the Agent Harbor monorepo. This repository is
generated — see [CONTRIBUTING.md](CONTRIBUTING.md) before opening a pull
request.

## Requirements

- **Emacs 29.1 or newer.**
- **The `ah` CLI**, version **0.6.8 or newer**, on `exec-path`.
  [Install instructions](https://docs.agent-harbor.com/installation).

No Elisp dependencies beyond Emacs itself.

## Install

<details open>
<summary><b>MELPA</b></summary>

```elisp
(use-package agent-harbor
  :ensure t
  :commands (agent-harbor agent-harbor-task-entry-open))
```
</details>

<details>
<summary><b>straight.el</b></summary>

```elisp
(straight-use-package
 '(agent-harbor :type git :host github :repo "agent-harbor/agent-harbor.el"))
```
</details>

<details>
<summary><b>package-vc (Emacs 29+, no package manager)</b></summary>

```elisp
(package-vc-install "https://github.com/agent-harbor/agent-harbor.el")
```
</details>

<details>
<summary><b>Doom Emacs</b></summary>

In `packages.el`:

```elisp
(package! agent-harbor
  :recipe (:host github :repo "agent-harbor/agent-harbor.el"))
```

A ready-made module lives in `doom/modules/tools/agent-harbor/` in this
repository; copy it into your private module directory and enable
`tools agent-harbor` in `init.el`.
</details>

<details>
<summary><b>Spacemacs</b></summary>

A layer lives in `spacemacs/layers/agent-harbor/` in this repository. Copy it
under `~/.emacs.d/private/` and add `agent-harbor` to
`dotspacemacs-configuration-layers`.
</details>

<details>
<summary><b>Nix</b></summary>

The monorepo flake exposes the package, built from the same sources this
mirror is generated from:

```
nix build github:agent-harbor/agent-harbor#agent-harbor-emacs
```

In a home-manager configuration:

```nix
programs.emacs.extraPackages = epkgs: [
  inputs.agent-harbor.packages.${pkgs.system}.agent-harbor-emacs
];
```
</details>

## Use

| Command | |
|---|---|
| `M-x agent-harbor` | Open the Agent Harbor side window |
| `M-x agent-harbor-task-entry-open` | Task entry form (the editor's `ah task`) |
| `M-x agent-harbor-scm-open` | Source-control view for the current task |
| `M-x agent-harbor-reply-read-and-submit` | Reply to the running agent |
| `M-x agent-harbor-reply-open-popup` | Reply in a popup buffer |
| `M-x agent-harbor-reply-voice-submit` | Record a reply and send its transcript |
| `M-x agent-harbor-share-task` | Copy a shareable `agent-harbor://` URI |
| `M-x agent-harbor-open-in-new-instance` | Open a task in a new Emacs instance |
| `M-x agent-harbor-daemon-status` | Report the daemon connection state |
| `M-x agent-harbor-doctor` | Self-diagnosis; see below |
| `M-x agent-harbor-open-documentation` | Open the online documentation |

`agent-harbor://` URIs are registered as the `ah:` Org link type, so
`[[ah://task/...]]` in an Org file is clickable.

## Task prompts in Emacs

`ah task` and `ah plan` can hand the prompt to `$EDITOR`. They name the
scratch file `ah-task-prompt-*.ahtask` / `ah-plan-prompt-*.ahplan`, this
package recognises those names via `agent-harbor-task-prompt-mode`, and
inside such a buffer `/`, `~` and `@` complete against
`ah autocomplete --json` — the same command the `ah` dashboard editor uses,
so the two cannot drift apart on what a trigger offers.

Completion is wired through `completion-at-point-functions`, so `TAB` works
out of the box and corfu or company pick it up without configuration.
`C-c C-c` submits the prompt, `C-c C-k` aborts.

## Diagnosing

```
M-x agent-harbor-doctor
```

Reports the Emacs version, this package's version, whether `ah` is present
and new enough, whether `ah autocomplete` exists, and the daemon state.
Include its output in any bug report.

## Documentation

<https://docs.agent-harbor.com/editor-support/emacs>

## Versioning

Releases track the `ah` binary: package 0.6.8 is published from the same
monorepo tag as `ah` 0.6.8. The two are installed separately, though, so
the package declares the oldest `ah` it works with
(`agent-harbor-ah-minimum-version`, currently **0.6.8**) and
`agent-harbor-doctor` reports a mismatch. Keeping the two within a release of
each other is the supported configuration.

## License

AGPL-3.0-only. See [LICENSE](LICENSE).

[source]: https://github.com/agent-harbor/agent-harbor/tree/main/ide-plugins/emacs
[commit]: https://github.com/agent-harbor/agent-harbor/commit/ed781cfefad48de3d9cf77f7fb59debd33951bd6
