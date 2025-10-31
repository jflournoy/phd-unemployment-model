#!/bin/bash
# Check if running in the correct RStudio Docker environment

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🔍 Checking development environment..."
echo ""

# Check if in Docker container
if [ -f /.dockerenv ]; then
    echo -e "${GREEN}✓${NC} Running inside Docker container"
    IN_DOCKER=true
else
    echo -e "${RED}✗${NC} NOT running in Docker container"
    IN_DOCKER=false
fi

# Check if R is available
if command -v R &> /dev/null; then
    R_VERSION=$(R --version | head -n1)
    echo -e "${GREEN}✓${NC} R is available: $R_VERSION"
    HAS_R=true
else
    echo -e "${RED}✗${NC} R is NOT installed"
    HAS_R=false
fi

# Check if RStudio Server is available
if command -v rstudio-server &> /dev/null; then
    echo -e "${GREEN}✓${NC} RStudio Server is available"
    HAS_RSTUDIO=true
else
    echo -e "${YELLOW}⚠${NC}  RStudio Server not found (may not be needed for CLI)"
    HAS_RSTUDIO=false
fi

# Check if Quarto is available
if command -v quarto &> /dev/null; then
    QUARTO_VERSION=$(quarto --version)
    echo -e "${GREEN}✓${NC} Quarto is available: $QUARTO_VERSION"
    HAS_QUARTO=true
else
    echo -e "${YELLOW}⚠${NC}  Quarto not found"
    HAS_QUARTO=false
fi

# Check for key R packages (if R is available)
if [ "$HAS_R" = true ]; then
    echo ""
    echo "Checking R packages..."
    R --slave --no-save --no-restore -e '
    packages <- c("devtools", "testthat", "brms", "cmdstanr", "dplyr", "ggplot2")
    for (pkg in packages) {
        if (requireNamespace(pkg, quietly = TRUE)) {
            cat(sprintf("  ✓ %s\n", pkg))
        } else {
            cat(sprintf("  ✗ %s (not installed)\n", pkg))
        }
    }
    ' 2>/dev/null || echo "  (Unable to check R packages)"
fi

echo ""
echo "═══════════════════════════════════════════════════"

# Final verdict
if [ "$IN_DOCKER" = false ]; then
    echo -e "${RED}⚠️  WARNING: Not running in Docker container!${NC}"
    echo ""
    echo "This project should be developed in the RStudio Quarto Docker container."
    echo ""
    echo "To connect to your running container:"
    echo "  docker exec -it <container-name> bash"
    echo ""
    echo "Or if using Docker Compose:"
    echo "  docker-compose exec rstudio bash"
    echo ""
    exit 1
elif [ "$HAS_R" = false ]; then
    echo -e "${RED}⚠️  WARNING: R is not available!${NC}"
    echo "You may be in the wrong container or environment."
    exit 1
else
    echo -e "${GREEN}✅ Environment looks good!${NC}"
    echo "You're ready to develop R/Stan statistical models with TDD."
    exit 0
fi
