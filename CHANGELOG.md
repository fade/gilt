# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2026-02-02

### Added

- Initial release of Gilt - Git Interface for Lisp Terminal
- **Core UI**
  - LazyGit-inspired 5-panel layout with colored output
  - Pure ANSI rendering (no ncurses dependency)
  - 256-color support with syntax highlighting
  - Context-sensitive help bar with version display
  - Help overlay (`?` key)
- **File Operations**
  - Stage/unstage files with `Space`
  - Stage all with `a`
  - Discard changes with `d`
  - Hunk staging mode with `e`
  - Conflict resolution (`o` for ours, `t` for theirs, `X` to abort)
- **Commit Operations**
  - Create commits with multi-line message support
  - Squash commits (`S`)
  - Cherry-pick commits (`C`)
  - Revert commits (`R`)
  - Search commits by message/author (`/`)
- **Branch Operations**
  - Create new branches (`n`)
  - Checkout branches (`Enter`)
  - Merge branches (`M`)
  - Delete branches (`D`)
  - Cherry-pick from other branches (`C` on branches panel)
  - Toggle Local/Remotes/Tags view (`w`)
- **Tag Support**
  - View tags in branches panel (cycle with `w`)
  - Create tags on commits (`t` on commits panel)
  - Create tags on HEAD (`t` in tags view)
  - Delete tags (`D` in tags view)
- **Remote Operations**
  - Push to origin (`P`) with async output display
  - Pull from origin (`p`) with async output display
  - Fetch from remotes (`f`)
  - Track remote branches
- **Stash Operations**
  - Stash changes (`s`)
  - Pop stash (`g`)
  - Apply stash (`Enter`)
- **Blame View**
  - View git blame for files (`b`)
  - Navigate blame lines with selection highlight
  - View commit details for any line (`Enter`)
- **Status Bar**
  - Branch tracking info (ahead/behind upstream)
  - Repository state indicator (MERGING, REBASING, etc.)

[Unreleased]: https://github.com/parenworks/gilt/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/parenworks/gilt/releases/tag/v0.1.0
