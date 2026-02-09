# Claude Status Tools - Installation Guide

## Quick Install
```bash
# 1. Download the zip file to your project
# 2. Unzip directly to project root (files extract to correct locations)
unzip claude-status-tools.zip

# 3. Make scripts executable
chmod +x scripts/*.sh

# 4. Update documentation (optional but recommended)
# See claude-status-tools-README.md for package documentation
```

## What's in the Package
```
.claude/commands/
├── continue.md      # /continue skill
└── condense.md      # /condense skill
scripts/
├── status-helper.sh     # Auto-cleanup helper
└── update-status-example.sh  # Usage example
claude-status-tools-README.md  # Package documentation
```

## Test Installation
```bash
# Test that files are in the right place
ls -la .claude/commands/continue.md .claude/commands/condense.md
ls -la scripts/status-helper.sh

# Test the skills
/continue
/condense --help
```

## Update Your Documentation

### 1. Update CLAUDE.md
Add to the "State Tracking" section:
```markdown
**New Status Management Commands:**
- `/continue` - Efficiently resume work by extracting recent session context
- `/condense [N]` - Archive old content, keep last N lines (default: 200)
- `scripts/status-helper.sh` - Auto-cleanup helper for updates

**Auto-Cleanup Pattern:** When updating `.claude-current-status`, check if file exceeds 300 lines and consider running `/condense`.
```

### 2. Update .claude/commands/README.md
Add to your commands list:
```markdown
- `condense` - Condense .claude-current-status by archiving old content
- `continue` - Efficiently resume work by extracting recent session
```

## Usage Examples

### Basic Workflow
```bash
# Resume work after context limit
/continue

# Work, update status (ALWAYS APPEND, never prepend)
echo "## Update ($(date))" >> .claude-current-status
echo "Working..." >> .claude-current-status

# Check file size
scripts/status-helper.sh summary

# Condense if large
/condense 250
```

### Critical: Append-Only Requirement

**WARNING: Always append to `.claude-current-status`, never prepend.**

- **Appending (correct)**: `echo "text" >> .claude-current-status`
  New content added to end of file, preserved during condensation.

- **Prepending (incorrect)**: `cat new_content .claude-current-status > temp && mv temp .claude-current-status`
  New content added to top of file, will be archived by `/condense`, losing recent work.

**Symptoms of prepending:**
- `/continue` shows old work, missing recent sessions
- Archive files have newer timestamps than main file
- Recent work appears at top of file (should be at bottom)

**Fix:** Always use `>>` (append) when updating `.claude-current-status`.

### With Helper Script
```bash
# Add entry
scripts/status-helper.sh add "Completed task" "Feature"

# Show recent
scripts/status-helper.sh recent 5

# Check size
scripts/status-helper.sh check
```

## Zip File Location
The zip file is at: `/home/rstudio/code/phd-unemployment-model/claude-status-tools.zip` (7.7KB)

## Complete Documentation
See `claude-status-tools-README.md` for full documentation.