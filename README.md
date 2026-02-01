# Gilt

A LazyGit-style Git TUI written in Common Lisp.

## Features

- **Pure ANSI rendering** - No ncurses dependency, direct terminal control
- **LazyGit-inspired UI** - Familiar panel layout and keybindings
- **Full color diffs** - 256-color and true-color support
- **Fast and responsive** - Lightweight terminal interface
- **CLOS architecture** - Clean, extensible object-oriented design

## Requirements

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- Git

## Installation

### Build Standalone Executable

```bash
make build
```

This creates a `gilt` executable you can run directly:

```bash
./gilt
```

### Install System-Wide

```bash
sudo make install
```

Then run from anywhere:

```bash
gilt
```

### Development Setup

Link to Quicklisp local-projects:

```bash
cd ~/quicklisp/local-projects
ln -s /path/to/gilt .
```

## Usage

### Run Executable

```bash
./gilt
# or if installed:
gilt
```

### From REPL (Development)

```lisp
(ql:quickload :gilt)
(gilt:run)
```

### Makefile Targets

| Target      | Description                          |
|-------------|--------------------------------------|
| `build`     | Build standalone executable          |
| `run`       | Run from source (requires Quicklisp) |
| `run-bin`   | Run the built executable             |
| `install`   | Install to /usr/local/bin            |
| `uninstall` | Remove from /usr/local/bin           |
| `clean`     | Remove build artifacts               |
| `repl`      | Start SBCL with gilt loaded          |
| `check`     | Check if code compiles               |

### Keybindings

#### Global
| Key | Action |
|-----|--------|
| `1` | Status view |
| `2` | Log view |
| `3` | Branches view |
| `q` | Quit |

#### Navigation
| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `Tab` | Switch panel |

#### Status View
| Key | Action |
|-----|--------|
| `Space` | Stage/unstage file |
| `c` | Commit |
| `r` | Refresh |

#### Branches View
| Key | Action |
|-----|--------|
| `Enter` | Checkout branch |
| `n` | New branch |

## Architecture

- `ansi.lisp` - ANSI escape sequence library
- `terminal.lisp` - Raw terminal input handling
- `ui.lisp` - Panel and box drawing primitives
- `git.lisp` - Git command interface
- `views.lisp` - Main UI views (status, log, branches)
- `main.lisp` - Application entry point

## License

MIT
