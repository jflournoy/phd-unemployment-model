#!/bin/bash
#
# Interactive Report Evaluation Script
#
# Usage:
#   ./scripts/evaluate-report-interactive.sh [report-file]
#
# If no report file is provided, shows a list of available reports
# and uses the most recently modified one by default.

set -e

# Color codes for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Parameter Recovery Report Evaluation${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# If report file provided as argument, use it
if [ $# -ge 1 ]; then
  REPORT_FILE="$1"
  VERBOSE=""
  if [[ "$*" == *"--verbose"* ]]; then
    VERBOSE="--verbose"
  fi

  if [ ! -f "$REPORT_FILE" ]; then
    echo -e "${YELLOW}Error: Report file not found: $REPORT_FILE${NC}"
    exit 1
  fi

  echo -e "${GREEN}Evaluating:${NC} $REPORT_FILE"
  echo ""
  Rscript scripts/evaluate-report.R "$REPORT_FILE" $VERBOSE
  exit $?
fi

# No argument provided - find and list available reports
echo "📊 Searching for reports in reports/ directory..."
echo ""

# Find all HTML and QMD/Rmd files, sorted by modification time (newest first)
REPORTS=$(find reports -maxdepth 1 -type f \( -name "*.html" -o -name "*.qmd" -o -name "*.Rmd" \) -printf "%T@ %p\n" 2>/dev/null | sort -rn | cut -d' ' -f2-)

if [ -z "$REPORTS" ]; then
  echo -e "${YELLOW}No reports found in reports/ directory${NC}"
  echo ""
  echo "Expected report files:"
  echo "  - reports/*.html (rendered Quarto reports)"
  echo "  - reports/*.qmd (Quarto source files)"
  echo "  - reports/*.Rmd (R Markdown source files)"
  exit 1
fi

# Count reports
NUM_REPORTS=$(echo "$REPORTS" | wc -l)
echo -e "${GREEN}Found $NUM_REPORTS report(s):${NC}"
echo ""

# List reports with numbers
i=1
while IFS= read -r report; do
  # Get file size and modification time
  SIZE=$(du -h "$report" | cut -f1)
  MTIME=$(stat -c "%y" "$report" 2>/dev/null | cut -d' ' -f1)

  # Determine file type
  EXT="${report##*.}"
  if [ "$EXT" = "html" ]; then
    TYPE="[Rendered HTML]"
  elif [ "$EXT" = "qmd" ]; then
    TYPE="[Quarto Source]"
  else
    TYPE="[RMarkdown]"
  fi

  echo -e "  ${BLUE}$i)${NC} $(basename "$report") ${TYPE}"
  echo "     Size: $SIZE | Modified: $MTIME"
  echo "     Path: $report"
  echo ""

  # Store for selection
  eval "REPORT_$i=\"$report\""
  i=$((i + 1))
done <<< "$REPORTS"

# Get most recent report (first in list)
MOST_RECENT=$(echo "$REPORTS" | head -1)

echo -e "${GREEN}Most recent:${NC} $(basename "$MOST_RECENT")"
echo ""

# Prompt user for selection
if [ -t 0 ]; then
  # Interactive terminal - prompt user
  echo -e "Select report to evaluate (1-$NUM_REPORTS) or press Enter for most recent:"
  read -r selection

  if [ -z "$selection" ]; then
    # Use most recent
    SELECTED_REPORT="$MOST_RECENT"
  elif [ "$selection" -ge 1 ] && [ "$selection" -le "$NUM_REPORTS" ] 2>/dev/null; then
    # Use selected number
    eval "SELECTED_REPORT=\$REPORT_$selection"
  else
    echo -e "${YELLOW}Invalid selection. Using most recent report.${NC}"
    SELECTED_REPORT="$MOST_RECENT"
  fi
else
  # Non-interactive (piped input) - use most recent
  SELECTED_REPORT="$MOST_RECENT"
fi

echo ""
echo -e "${GREEN}Evaluating:${NC} $(basename "$SELECTED_REPORT")"
echo ""

# Run evaluation
Rscript scripts/evaluate-report.R "$SELECTED_REPORT"
