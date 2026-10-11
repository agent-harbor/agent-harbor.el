# Contributing to agent-harbor/agent-harbor.el

**This repository is generated. Changes made here are overwritten.**

`agent-harbor/agent-harbor.el` is published from [`ide-plugins/emacs`][source] in the Agent Harbor
monorepo by [`scripts/ide-publish-editor-mirrors.cjs`][publisher], which
rewrites the whole tree on every release. A commit pushed here survives until
the next release and no longer, so pull requests against it cannot be merged
in any meaningful sense.

## Where to send things

| | |
|---|---|
| Bug reports, feature requests | [agent-harbor/agent-harbor issues][issues] |
| Code changes | [agent-harbor/agent-harbor][monorepo], under `ide-plugins/emacs` |

Opening the issue in the monorepo rather than here keeps it next to the `ah`
binary the plugin talks to, which is where most editor-visible bugs actually
live.

## Why a mirror at all

A plugin manager asked to install from the monorepo clones the whole thing —
hundreds of megabytes of Rust, a web UI and VM recipes — to put a few tens of
kilobytes of plugin code on the runtime path. The mirror is the delivery
artifact: small, tagged per release, and carrying only what the editor needs.

The monorepo stays the single source of truth, so the two cannot disagree
about what the plugin does.

[source]: https://github.com/agent-harbor/agent-harbor/tree/main/ide-plugins/emacs
[publisher]: https://github.com/agent-harbor/agent-harbor/blob/main/scripts/ide-publish-editor-mirrors.cjs
[issues]: https://github.com/agent-harbor/agent-harbor/issues
[monorepo]: https://github.com/agent-harbor/agent-harbor
