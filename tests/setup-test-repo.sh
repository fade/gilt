#!/usr/bin/env bash
#
# setup-test-repo.sh — Create a fully-populated test repository for Gilt
#
# Usage:
#   ./tests/setup-test-repo.sh [target-dir]
#
# If target-dir is omitted, creates ./gilt-test-repo
# If GITHUB_REMOTE is set, pushes to that remote (e.g. git@github.com:parenworks/gilt-test-repo.git)
#
set -euo pipefail

TARGET="${1:-$(pwd)/gilt-test-repo}"
GITHUB_REMOTE="${GITHUB_REMOTE:-git@github-parenworks:parenworks/gilt-test-repo.git}"

echo "=== Gilt Test Repo Setup ==="
echo "Target: $TARGET"

# Clean slate
if [ -d "$TARGET" ]; then
    echo "Removing existing $TARGET..."
    rm -rf "$TARGET"
fi

mkdir -p "$TARGET"
cd "$TARGET"
git init
git config user.name "Test User"
git config user.email "test@example.com"
git config protocol.file.allow always

# ─── Initial commit ───────────────────────────────────────────────
echo "# Gilt Test Repository" > README.md
echo "This repo is used to test all Gilt features." >> README.md
git add README.md
git commit -m "Initial commit"

# ─── Create a realistic file structure ────────────────────────────
mkdir -p src/utils src/core docs
cat > src/core/main.lisp << 'EOF'
(defun main ()
  (format t "Hello from gilt-test-repo~%")
  (run-app))
EOF

cat > src/core/app.lisp << 'EOF'
(defun run-app ()
  (format t "App running~%")
  (process-input))

(defun process-input ()
  (format t "Processing...~%"))
EOF

cat > src/utils/helpers.lisp << 'EOF'
(defun trim-string (s)
  (string-trim '(#\Space #\Tab) s))

(defun join-strings (lst sep)
  (format nil (concatenate 'string "~{~A~^" sep "~}") lst))
EOF

cat > docs/guide.md << 'EOF'
# Test Guide
This is a test guide document.
## Section 1
Content here.
## Section 2
More content.
EOF

echo "*.log" > .gitignore
echo "tmp/" >> .gitignore

git add -A
git commit -m "Add project structure with src, utils, and docs"

# ─── Create multiple commits for history ──────────────────────────
echo "(defun feature-a () (format t \"Feature A~%\"))" >> src/core/main.lisp
git add -A && git commit -m "Add feature A to main"

echo "(defun feature-b () (format t \"Feature B~%\"))" >> src/core/app.lisp
git add -A && git commit -m "Add feature B to app"

echo "## Section 3" >> docs/guide.md
echo "Even more content." >> docs/guide.md
git add -A && git commit -m "Update documentation with section 3"

echo "(defun utility-fn () t)" >> src/utils/helpers.lisp
git add -A && git commit -m "Add utility function"

echo "v1.0 release notes" > RELEASE.md
git add -A && git commit -m "Add release notes"

# ─── Create tags ──────────────────────────────────────────────────
git tag v0.1.0
git tag -a v1.0.0 -m "First stable release"

# ─── Create feature branches ─────────────────────────────────────
git checkout -b feature/login
echo "(defun login (user pass) (check-credentials user pass))" > src/core/login.lisp
git add -A && git commit -m "Add login feature"
echo "(defun logout () (clear-session))" >> src/core/login.lisp
git add -A && git commit -m "Add logout function"

git checkout master

git checkout -b feature/dashboard
echo "(defun render-dashboard () (format t \"Dashboard~%\"))" > src/core/dashboard.lisp
git add -A && git commit -m "Add dashboard rendering"

git checkout master

git checkout -b bugfix/typo
sed -i 's/Hello from gilt-test-repo/Hello from Gilt Test Repo/' src/core/main.lisp
git add -A && git commit -m "Fix typo in greeting"

git checkout master

# ─── Create a branch that diverges (for merge conflict testing) ──
git checkout -b conflict-branch
echo "# Conflict version of README" > README.md
echo "This line will conflict." >> README.md
git add -A && git commit -m "Change README on conflict-branch"

git checkout master
echo "# Master version of README" > README.md
echo "This is the master version." >> README.md
git add -A && git commit -m "Change README on master (will conflict)"

# ─── Create a submodule (uses a public tiny repo) ────────────────
echo ""
echo "--- Creating submodule ---"
# Create a local bare repo to act as submodule source
SUBMOD_SRC="$TARGET/../gilt-test-submodule"
if [ -d "$SUBMOD_SRC" ]; then
    rm -rf "$SUBMOD_SRC"
fi
mkdir -p "$SUBMOD_SRC"
cd "$SUBMOD_SRC"
git init
git config user.name "Test User"
git config user.email "test@example.com"
echo "# Submodule" > README.md
git add -A && git commit -m "Submodule initial commit"

cd "$TARGET"
git -c protocol.file.allow=always submodule add "$SUBMOD_SRC" vendor/test-submodule
git commit -m "Add test submodule"

# ─── Create stashes ──────────────────────────────────────────────
echo "stash content 1" > stash-test-1.txt
git add stash-test-1.txt
git stash push -m "Test stash 1: with message"

echo "stash content 2" > stash-test-2.txt
git stash push -u -m "Test stash 2: include untracked"

# ─── Create unstaged/staged/untracked files for testing ──────────
# These must come AFTER stashes so they remain in the working tree
echo "This is an untracked file" > untracked-file.txt
echo "debug output" > debug.log
echo "Modified content" >> src/core/main.lisp
echo "Staged content" > staged-file.txt
git add staged-file.txt

# ─── Create custom commands config ───────────────────────────────
mkdir -p ~/.config/gilt
cat > ~/.config/gilt/commands.conf << 'EOF'
# Gilt custom commands
# Format: key=shell command
# These run as fallback after all built-in keybindings
# Example: pressing the key runs the command in the repo directory
#
# Uncomment to test:
# 9=git log --oneline -5
EOF

# ─── Add remote if specified ─────────────────────────────────────
if [ -n "$GITHUB_REMOTE" ]; then
    echo ""
    echo "--- Adding remote: $GITHUB_REMOTE ---"
    git remote add origin "$GITHUB_REMOTE"
    git push -u origin master --tags --force
    git push origin feature/login feature/dashboard bugfix/typo conflict-branch --force
    echo "Remote branches pushed."
fi

# ─── Summary ─────────────────────────────────────────────────────
echo ""
echo "=== Test Repo Ready ==="
echo "Location: $TARGET"
echo ""
echo "State:"
echo "  Branches: master, feature/login, feature/dashboard, bugfix/typo, conflict-branch"
echo "  Tags: v0.1.0 (lightweight), v1.0.0 (annotated)"
echo "  Commits: $(git log --oneline | wc -l) commits on master"
echo "  Stashes: $(git stash list | wc -l) stashes"
echo "  Submodule: vendor/test-submodule"
echo "  Untracked: untracked-file.txt, debug.log"
echo "  Modified: src/core/main.lisp"
echo "  Staged: staged-file.txt"
echo "  .gitignore: *.log, tmp/"
echo ""
echo "To test with Gilt:"
echo "  cd $TARGET && gilt"
echo ""
echo "To test with GitHub remote:"
echo "  GITHUB_REMOTE=git@github.com:parenworks/gilt-test-repo.git ./tests/setup-test-repo.sh"
