#!/bin/bash
# Overnight refitting script for LOO-CV analysis
# Usage: ./scripts/run_refitting.sh

set -e

echo "==========================================="
echo "OVERNIGHT MODEL REFITTING FOR LOO-CV ANALYSIS"
echo "==========================================="
echo ""
echo "Current time: $(date)"
echo ""

# Create log directory
LOG_DIR="models/refitting-logs"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/refitting-$(date +%Y%m%d-%H%M%S).log"

echo "Log file: $LOG_FILE"
echo ""

# Run the refitting script
echo "Starting refitting script..."
echo ""

# Run R script and tee output to log file
Rscript run_overnight_refitting.R 2>&1 | tee "$LOG_FILE"

echo ""
echo "==========================================="
echo "REFITTING COMPLETE"
echo "==========================================="
echo ""
echo "Log saved to: $LOG_FILE"
echo "Next steps:"
echo "1. Check log file for any errors"
echo "2. Run LOO-CV computation: tar_make(c('loo_edu_parallel', 'loo_education_trend'))"
echo "3. Compare models: tar_make('loo_comparison')"
echo ""