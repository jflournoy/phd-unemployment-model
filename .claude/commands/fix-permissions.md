---
description: Fix Docker file ownership permissions
tags: [project, docker, utility]
---

# Fix File Permissions

Run the set-ownership script to fix Docker file ownership issues.

**Task**: Execute `./scripts/set-ownership.sh` to change file ownership from root to the rstudio user.

**Context**: When running operations in the Docker container that create files as root, this script fixes ownership so the rstudio user can access and modify them.

**Instructions**:
1. Run the set-ownership script: `./scripts/set-ownership.sh`
2. Report the result to the user
3. Do not ask for confirmation - just run it

**Expected behavior**: Script will recursively change ownership of project files to rstudio:rstudio.
