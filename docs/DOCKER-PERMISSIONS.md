# Docker File Permissions Guide

## Problem

When working in the RStudio Docker container, files created by R/Quarto are owned by `root` or the `rstudio` user inside the container. This causes issues when accessing files from the host filesystem, requiring manual `chown` operations.

## Solutions

### Solution 1: Automated Permission Fix (Recommended)

Run this script **from inside the Docker container** after generating files:

```bash
./scripts/set-ownership.sh
```

This automatically sets ownership to match your host user (default UID:GID = 1000:1000).

**Add to workflow**: You can add this to your R scripts:

```r
# At end of analysis script
system("./scripts/set-ownership.sh", wait = FALSE)
```

### Solution 2: Manual Fix from Host

If files are already owned by root, run this **from your host machine**:

```bash
./scripts/fix-permissions.sh
```

This will prompt for confirmation before changing ownership with `sudo chown`.

### Solution 3: Docker Run with Custom UID/GID

If you're starting the container yourself (not using a pre-configured setup), pass your host UID/GID:

```bash
# Get your UID and GID
echo "UID: $(id -u), GID: $(id -g)"

# Start container with matching UID/GID
docker run -d \
  -e HOST_UID=$(id -u) \
  -e HOST_GID=$(id -g) \
  -v /path/to/phd-unemployment-model:/home/rstudio/code/phd-unemployment-model \
  rocker/rstudio-quarto:latest
```

### Solution 4: Post-Session Cleanup Alias

Add to your host `~/.bashrc` or `~/.zshrc`:

```bash
alias fixphd='sudo chown -R $(id -u):$(id -g) ~/code/phd-unemployment-model'
```

Then simply run `fixphd` from host after working in Docker.

## Recommended Workflow

### For Daily Development

1. Work in Docker container as usual
2. Before switching to host filesystem, run:
   ```bash
   ./scripts/set-ownership.sh
   ```
3. Files will be accessible from host without permission issues

### For Automated Scripts

Add this to your R data processing scripts:

```r
# At the start of script - load helper
source(here::here("scripts", "ensure-permissions.R"))

# Your analysis code here
# ...

# At the end - fix permissions
fix_permissions()
```

Let me create this helper:

```r
# scripts/ensure-permissions.R
fix_permissions <- function() {
  script_path <- here::here("scripts", "set-ownership.sh")
  if (file.exists(script_path)) {
    system(script_path, wait = FALSE)
    message("✓ Permissions updated")
  }
}
```

## Why This Happens

Docker containers run processes as specific users. By default:
- RStudio Server runs as `rstudio` user (UID 1000)
- But some operations (file creation via Quarto, system commands) may run as `root`
- When these files are written to mounted volumes, they appear on the host with container UIDs

The volume mount doesn't automatically translate UIDs between container and host, causing ownership mismatches.

## Verification

Check current file ownership:

```bash
# From host
ls -la data-raw/ipums_data.rds

# If owned by root
-rw-r--r-- 1 root root 674M Nov  7 10:00 ipums_data.rds

# After fix
-rw-r--r-- 1 youruser youruser 674M Nov  7 10:00 ipums_data.rds
```

## Related Issues

- GitHub Issue #11: Data completeness validation for 2000-2025
- Project uses RStudio Quarto Docker container (see [CLAUDE.md](../CLAUDE.md))

## References

- [Docker Volume Permissions Documentation](https://docs.docker.com/storage/volumes/)
- [RStudio Docker Stack](https://github.com/rocker-org/rocker-versioned2)
