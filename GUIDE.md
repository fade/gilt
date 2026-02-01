# Gilt User Guide

A comprehensive guide to using Gilt, the LazyGit-style Git TUI written in Common Lisp.

## Overview

Gilt provides a terminal-based interface for Git, inspired by LazyGit. The interface is divided into panels that let you navigate your repository, stage changes, commit, and perform advanced Git operations.

## Screen Layout

```
┌─────────────────────────────────┬────────────────────────────────────────────┐
│ [1] Status                      │ [0] Main (Diff/Patch view)                 │
├─────────────────────────────────┤                                            │
│ [2] Files                       │                                            │
├─────────────────────────────────┤                                            │
│ [3] Local branches              │                                            │
├─────────────────────────────────┤                                            │
│ [4] Commits                     ├────────────────────────────────────────────┤
├─────────────────────────────────┤ Command Log                                │
│ [5] Stash                       │                                            │
└─────────────────────────────────┴────────────────────────────────────────────┘
 Help bar with context-sensitive keybindings
```

## Panels

| Panel | Description |
|-------|-------------|
| **Status** `[1]` | Shows repository name and current branch |
| **Files** `[2]` | Lists modified, staged, and untracked files |
| **Branches** `[3]` | Shows local branches (current branch marked with `*`) |
| **Commits** `[4]` | Displays commit history with hash, author initials, and message |
| **Stash** `[5]` | Lists stashed changes |
| **Main** `[0]` | Shows diff/patch for selected file or commit details |
| **Command Log** | Displays recent Git commands executed |

## Visual Indicators

### File Status Colors

| Color | Status | Indicator |
|-------|--------|-----------|
| Yellow | Modified | `M` |
| Green | Added | `A` |
| Red | Deleted | `D` |
| Magenta | Untracked | `?` |
| Cyan | Renamed | `R` |

### Commit Display

Each commit line shows:
- **Yellow** - Commit hash (e.g., `a1b2c3d`)
- **Cyan** - Author initials (e.g., `GT` for Glenn Thompson)
- **Green** - Commit indicator (`●` for HEAD, `○` for other commits)
- **White** - Commit message

### Other Indicators

- **Panel item counts** - Bottom-right corner shows "X of Y" (e.g., "3 of 12")
- **Current branch** - Highlighted in green with `*` prefix
- **Focused panel** - Bright cyan border and bold magenta title

---

## Keybindings

### Global Navigation

| Key | Action |
|-----|--------|
| `j` / `↓` | Move selection down |
| `k` / `↑` | Move selection up |
| `Tab` / `l` | Switch to next panel |
| `h` | Switch to previous panel |
| `1-5` | Jump directly to panel by number |
| `r` | Refresh all data |
| `q` | Quit Gilt |

### Files Panel `[2]`

| Key | Action |
|-----|--------|
| `Space` | Stage/unstage selected file |
| `a` | Stage all files |
| `e` | Enter hunk staging mode (partial staging) |
| `d` | Discard changes to selected file |
| `s` | Stash all changes |
| `c` | Open commit dialog |
| `P` (capital) | Push to remote |
| `p` (lowercase) | Pull from remote |

### Branches Panel `[3]`

| Key | Action |
|-----|--------|
| `Enter` | Checkout selected branch |
| `n` | Create new branch |
| `M` (capital) | Merge selected branch into current |
| `D` (capital) | Delete selected branch |

### Commits Panel `[4]`

| Key | Action |
|-----|--------|
| `S` (capital) | Squash commits (select target commit) |
| `C` (capital) | Cherry-pick selected commit |
| `R` (capital) | Revert selected commit |

### Stash Panel `[5]`

| Key | Action |
|-----|--------|
| `s` | Create new stash |
| `g` | Pop (apply and remove) top stash |

### Dialog Navigation

| Key | Action |
|-----|--------|
| `Tab` | Switch between buttons/input |
| `Enter` | Confirm action (or newline in multiline input) |
| `Escape` | Cancel dialog |
| `Backspace` | Delete character |

---

## Common Workflows

### Staging and Committing Changes

1. Navigate to the **Files panel** (`Tab` or press `2`)
2. Use `j`/`k` to select a file
3. Press `Space` to stage/unstage the file
4. Or press `a` to stage all files
5. Press `c` to open the commit dialog
6. Type your commit message (multiline supported)
7. Press `Tab` to select the "Commit" button
8. Press `Enter` to confirm

### Partial Staging (Hunk Mode)

Stage only specific parts of a file:

1. Select an unstaged modified file in the **Files panel**
2. Press `e` to enter hunk staging mode
3. The **Main panel** shows individual hunks
4. Use `j`/`k` to navigate hunks
5. Press `Space` to stage a specific hunk
6. Press `Escape` to exit hunk mode

### Creating a New Branch

1. Navigate to the **Branches panel** (`Tab` or press `3`)
2. Press `n` to open the new branch dialog
3. Type the branch name
4. Press `Enter` to create and checkout the new branch

### Merging Branches

1. Make sure you're on the target branch (the one you want to merge INTO)
2. Navigate to the **Branches panel**
3. Select the branch you want to merge FROM
4. Press `M` (capital) to merge
5. Confirm in the dialog

### Squashing Commits

Combine multiple commits into one:

1. Navigate to the **Commits panel** (`Tab` or press `4`)
2. Select the oldest commit you want to include in the squash
   - For example, to squash the last 3 commits, select the 3rd commit from the top
3. Press `S` (capital) to squash
4. Enter a new commit message for the combined commit
5. Press `Tab` to select "Squash", then `Enter`

### Cherry-Picking a Commit

Apply a specific commit to your current branch:

1. Navigate to the **Commits panel**
2. Select the commit you want to cherry-pick
3. Press `C` (capital)
4. Confirm in the dialog

### Reverting a Commit

Create a new commit that undoes a previous commit:

1. Navigate to the **Commits panel**
2. Select the commit you want to revert
3. Press `R` (capital)
4. Confirm in the dialog (creates a new revert commit)

### Stashing Changes

Temporarily save your changes:

1. Press `s` from the **Files panel** or **Stash panel** to stash all changes
2. Navigate to the **Stash panel** to see stashed items
3. Press `g` to pop (apply and remove) the top stash

### Pushing and Pulling

1. Press `P` (capital) from any panel to push
2. Press `p` (lowercase) from any panel to pull
3. A status indicator shows "Pushing..." or "Pulling..." during the operation

---

## Troubleshooting

### Terminal Issues

| Problem | Solution |
|---------|----------|
| Colors not displaying | Ensure your terminal supports 256 colors (`TERM=xterm-256color`) |
| Box characters broken | Use a font with Unicode box-drawing support (e.g., JetBrains Mono, Fira Code) |
| Keys not working | Some terminals send different escape sequences; try a different terminal emulator |
| Backspace not working | Try `Ctrl+H` as an alternative |

### Git Issues

| Problem | Solution |
|---------|----------|
| No files showing | Make sure you're in a Git repository |
| Push/pull slow | Network operations depend on your connection and remote server |
| Merge conflicts | Resolve conflicts in your editor, then stage the resolved files |

### Getting Help

- Press `Tab` to see context-sensitive keybindings in the help bar at the bottom
- Check the Command Log panel to see what Git commands are being executed

---

## Tips and Tricks

1. **Quick panel switching** - Use number keys `1-5` to jump directly to a panel
2. **View diffs** - Select a file in the Files panel to see its diff in the Main panel
3. **Multiline commits** - The commit dialog supports multiple lines; press `Enter` to add new lines
4. **Check command history** - The Command Log shows all Git commands executed, useful for debugging
5. **Refresh data** - Press `r` to refresh all panels if data seems stale
