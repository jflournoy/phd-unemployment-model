#!/bin/bash
# Deploy report to GitHub Pages after model refit
# Usage: bash scripts/deploy-report.sh
#
# Prerequisites:
#   - Model fit at models/ode-state-space-edu-parallel-fit.rds with CSV files
#   - Run from project root directory
#
# This script:
#   1. Regenerates all report figures from current model fit
#   2. Generates residual diagnostic plots
#   3. Renders the Quarto report
#   4. Copies output to docs/index.html for GitHub Pages

set -e

echo "=== Deploying Report to GitHub Pages ==="
echo ""

# Check we're in project root
if [ ! -f "reports/state-space-comparison.qmd" ]; then
  echo "ERROR: Must be run from project root directory"
  echo "  cd /home/rstudio/code/phd-unemployment-model && bash scripts/deploy-report.sh"
  exit 1
fi

# Check model fit exists
if [ ! -f "models/ode-state-space-edu-parallel-fit.rds" ]; then
  echo "ERROR: Model fit not found at models/ode-state-space-edu-parallel-fit.rds"
  echo "  Run the model first: Rscript scripts/run-edu-parallel.R"
  exit 1
fi

# Check CSV files exist
CSV_COUNT=$(ls models/ode-state-space-edu-parallel-fit_csv/*.csv 2>/dev/null | wc -l)
if [ "$CSV_COUNT" -eq 0 ]; then
  echo "ERROR: No CSV files found in models/ode-state-space-edu-parallel-fit_csv/"
  echo "  The fit must be saved with save_cmdstan_fit() to preserve CSV files"
  exit 1
fi
echo "  CSV files: $CSV_COUNT"

# Step 1: Regenerate all report figures from current fit
echo ""
echo "  [1/4] Regenerating report figures from current fit..."
Rscript scripts/update-report-figures.R 2>&1
echo "  Done."

# Step 2: Generate residual diagnostic plots
echo ""
echo "  [2/4] Generating residual diagnostic plots..."
Rscript /tmp/residual_plots.R 2>&1 | tail -3
echo "  Done."

# Step 3: Render the Quarto report
echo ""
echo "  [3/4] Rendering Quarto report..."
quarto render reports/state-space-comparison.qmd 2>&1 | tail -5
echo "  Done."

# Step 4: Deploy to docs/index.html
echo ""
echo "  [4/4] Deploying to docs/index.html..."
cp reports/state-space-comparison.html docs/index.html
echo "  Copied: reports/state-space-comparison.html -> docs/index.html"

# Verify
echo ""
echo "=== Verification ==="
echo "  Report: reports/state-space-comparison.html ($(du -h reports/state-space-comparison.html | cut -f1))"
echo "  Pages:  docs/index.html ($(du -h docs/index.html | cut -f1))"
RESID_COUNT=$(grep -c "Residual Diagnostics" docs/index.html 2>/dev/null || echo 0)
echo "  Residual Diagnostics section present: $([ "$RESID_COUNT" -gt 0 ] && echo 'yes' || echo 'no')"

echo ""
echo "=== Deployment complete ==="
echo "  Commit and push to deploy:"
echo "    git add docs/index.html reports/state-space-comparison.html"
echo "    git commit -m 'fix: update report after refit'"
echo "    git push"
echo ""
