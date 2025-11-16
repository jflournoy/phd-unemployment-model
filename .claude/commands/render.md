---
allowed-tools: [Bash]
description: Render Quarto documents (QMD) to HTML
approach: direct-execution
token-cost: ~50 (efficient wrapper around quarto render)
best-for: Regenerating reports after code changes
---

# Render Quarto Document

Efficiently render Quarto documents to HTML with proper error handling and status reporting.

## Usage

```bash
#!/bin/bash

# Parse arguments
if [ $# -eq 0 ]; then
  echo "Usage: /render <qmd-file>"
  echo ""
  echo "Examples:"
  echo "  /render reports/factor-smooth/factor-smooth-unemployment-analysis.qmd"
  echo "  /render reports/factor-smooth-parameter-recovery.qmd"
  echo ""
  echo "Available reports:"
  find reports -name "*.qmd" 2>/dev/null | sort
  exit 1
fi

QMD_FILE="$1"

# Validate file exists
if [ ! -f "$QMD_FILE" ]; then
  echo "Error: File not found: $QMD_FILE"
  exit 1
fi

# Render with quarto
echo "🔄 Rendering $QMD_FILE..."
echo ""

if quarto render "$QMD_FILE"; then
  echo ""
  echo "✅ Render successful"

  # Show output file info
  HTML_FILE="${QMD_FILE%.qmd}.html"
  if [ -f "$HTML_FILE" ]; then
    echo "📄 Output: $HTML_FILE"
    ls -lh "$HTML_FILE" | awk '{print "   Size: " $5 ", Modified: " $6 " " $7 " " $8}'
  fi
else
  echo ""
  echo "❌ Render failed - check errors above"
  exit 1
fi
```

## Notes

- Renders Quarto (`.qmd`) files to HTML
- Shows output file size and timestamp
- Lists available reports if no file specified
- Validates file exists before attempting render
- Proper error handling and exit codes

## Common Use Cases

1. **After code changes**: Re-render reports to reflect updated R functions
2. **After fixing bugs**: Verify fixes appear correctly in output
3. **Quick iteration**: Render while developing new analyses
4. **Validation**: Ensure report compiles without errors

## Related Commands

- `/evaluate-report`: Evaluate report quality after rendering
- `/hygiene`: Check project health before rendering
- `/commit`: Commit rendered reports
