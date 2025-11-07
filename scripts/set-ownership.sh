#!/bin/bash
#
# Set Ownership - Run Inside Docker Container
#
# This script fixes file ownership from inside the Docker container
# Run this after generating files to ensure they match the host user
#
# Usage from inside container:
#   ./scripts/set-ownership.sh
#
# Or add to .Rprofile to run automatically:
#   system("./scripts/set-ownership.sh", wait = FALSE)

set -e

# Get the project directory
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Default to rstudio user (1000:1000) which typically matches host user
# Override with environment variables if needed:
#   docker run -e HOST_UID=1001 -e HOST_GID=1001 ...
TARGET_UID="${HOST_UID:-1000}"
TARGET_GID="${HOST_GID:-1000}"

echo "Setting ownership to UID:GID = $TARGET_UID:$TARGET_GID"

# Find all files owned by root or rstudio and fix them
find "$PROJECT_DIR" -type f \( -user root -o -user rstudio \) -exec chown "$TARGET_UID:$TARGET_GID" {} + 2>/dev/null || true
find "$PROJECT_DIR" -type d \( -user root -o -user rstudio \) -exec chown "$TARGET_UID:$TARGET_GID" {} + 2>/dev/null || true

echo "✓ Ownership updated"
