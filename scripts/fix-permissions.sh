#!/bin/bash
#
# Fix File Permissions - Docker Ownership Helper
#
# Problem: Files created in Docker container are owned by root
# Solution: This script resets ownership to host user
#
# Usage:
#   From host: ./scripts/fix-permissions.sh
#   Or add to shell alias: alias fixperm='docker exec phd-container /home/rstudio/code/phd-unemployment-model/scripts/fix-permissions.sh'

set -e

# Detect if running inside Docker container
if [ -f /.dockerenv ]; then
    echo "ERROR: This script should be run from the HOST, not inside the Docker container"
    exit 1
fi

# Get the project directory (parent of scripts/)
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "=== Docker File Ownership Fix ==="
echo "Project directory: $PROJECT_DIR"
echo ""

# Get current user
HOST_USER=$(whoami)
HOST_UID=$(id -u)
HOST_GID=$(id -g)

echo "Host user: $HOST_USER (UID: $HOST_UID, GID: $HOST_GID)"
echo ""

# Check for root-owned files
ROOT_FILES=$(find "$PROJECT_DIR" -user root 2>/dev/null | wc -l)

if [ "$ROOT_FILES" -eq 0 ]; then
    echo "✓ No root-owned files found - permissions are correct!"
    exit 0
fi

echo "Found $ROOT_FILES root-owned files"
echo ""

# Ask for confirmation
read -p "Change ownership to $HOST_USER:$HOST_USER? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Cancelled"
    exit 0
fi

# Fix permissions
echo "Fixing permissions..."
sudo chown -R "$HOST_UID:$HOST_GID" "$PROJECT_DIR"

echo ""
echo "✓ Permissions fixed! All files now owned by $HOST_USER"
