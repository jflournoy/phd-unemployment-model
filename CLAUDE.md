# CLAUDE.md - Project AI Guidelines

## Project Overview

This project models PhD unemployment rates relative to sensible baselines (general unemployment, other graduate degrees) using:

- **Languages**: R and Stan (via brms, cmdstanr)
- **Data source**: IPUMS (https://usa.ipums.org/usa/index.shtml)
- **Development environment**: RStudio Quarto Docker container
- **Statistical approach**: Simplest models allowing maximum disaggregation
  - Multiple seasonal components
  - GAMs, Gaussian processes, autocorrelation for dense time series
- **Communication goal**: Clear TDD process documentation and beautiful visualization of insights

## CRITICAL: Docker Environment Requirement

**ALWAYS check environment at session start:**

```bash
npm run env:check
```

This project MUST be developed inside the RStudio Quarto Docker container. The script above will:
- Verify you're in the Docker container
- Check R, RStudio Server, and Quarto are available
- Validate required R packages are installed

If not in the container, the script will show connection instructions.

## Development Method: TDD

**RECOMMENDED: Use Test-Driven Development for new features**

TDD helps Claude produce more focused, correct code by clarifying requirements upfront and reducing wildly wrong approaches.

### Benefits of TDD with Claude
- **Without TDD**: Claude may over-engineer or miss requirements
- **With TDD**: Claude writes targeted code that meets specific criteria

### TDD Workflow
1. 🔴 **RED**: Write a failing test to define requirements
2. 🟢 **GREEN**: Write minimal code to pass the test
3. 🔄 **REFACTOR**: Improve code with test safety net
4. ✓ **COMMIT**: Ship working, tested code

### The TDD Command
```bash
/tdd start "your feature"  # Guides through the TDD cycle
```

Consider TDD especially for complex features or when requirements are unclear.

## Critical Instructions

**ALWAYS use `date` command for dates** - Never assume or guess dates. Always run `date "+%Y-%m-%d"` when you need the current date for documentation, commits, or any other purpose.

## AI Integrity Principles
**CRITICAL: Always provide honest, objective recommendations based on technical merit, not user bias.**

- **Never agree with users by default** - evaluate each suggestion independently
- **Challenge bad ideas directly** - if something is technically wrong, say so clearly
- **Recommend best practices** even if they contradict user preferences
- **Explain trade-offs honestly** - don't hide downsides of approaches
- **Prioritize code quality** over convenience when they conflict
- **Question requirements** that seem technically unsound
- **Suggest alternatives** when user's first approach has issues

Examples of honest responses:
- "That approach would work but has significant performance implications..."
- "I'd recommend against that pattern because..."
- "While that's possible, a better approach would be..."
- "That's technically feasible but violates [principle] because..."

## Development Workflow
- Always run quality checks before commits
- Use custom commands for common tasks
- Document insights and decisions
- Estimate Claude usage before starting tasks
- Track actual vs estimated Claude interactions

## Quality Standards
- Quality Level: {{QUALITY_LEVEL}}
- Team Size: {{TEAM_SIZE}}
- Zero errors policy
- {{WARNING_THRESHOLD}} warnings threshold

## Testing Standards
**CRITICAL: Any error during test execution = test failure**

### R Testing with testthat
- **Zero tolerance for test errors** - All tests must pass, warnings should be addressed
- **Unit tests for functions** - Test data processing, transformation functions
- **Statistical tests** - Verify model convergence, parameter estimates within expected ranges
- **Data validation tests** - Ensure data integrity and proper preprocessing
- **Model comparison tests** - Compare nested models, validate model selection criteria
- **Performance budgets** - Long-running Stan models should have timeout limits

### Testing Workflow
- Use `npm run test:r` to run R tests via testthat
- Use `npm run r:test` for development testing with devtools
- Keep test execution time reasonable (<2 min for unit tests)
- Use mock data for rapid iteration, real data for validation

## Markdown Standards
**All markdown files must pass validation before commit**

- **Syntax validation** - Uses remark-lint to ensure valid markdown syntax
- **Consistent formatting** - Enforces consistent list markers, emphasis, and code blocks
- **Link validation** - Checks that internal links point to existing files
- **Auto-fix available** - Run `npm run markdown:fix` to auto-correct formatting issues

### Markdown Quality Checks
- `npm run markdown:lint` - Validate all markdown files
- `npm run markdown:fix` - Auto-fix formatting issues
- Included in `hygiene:quick` and `commit:check` scripts
- CI validates markdown on every push/PR

### Markdown Style Guidelines
- Use `-` for unordered lists
- Use `*` for emphasis, `**` for strong emphasis
- Use fenced code blocks with language tags
- Use `.` for ordered list markers
- Ensure all internal links are valid

## Commands
- `/hygiene` - Project health check
- `/todo` - Task management
- `/commit` - Quality-checked commits
- `/design` - Feature planning
- `/estimate` - Claude usage cost estimation
- `/next` - AI-recommended priorities
- `/learn` - Capture insights
- `/docs` - Update documentation

## Architecture Principles

### R Code Standards
- Keep functions focused and testable (< 50 lines preferred)
- R script files under 400 lines
- Comprehensive error handling with informative messages
- **Prefer base R and data.table over tidyverse**
  - Use base R for core operations (subsetting, transformations)
  - Use data.table for efficient operations on large datasets (millions of rows)
  - Avoid tidyverse/dplyr unless specifically beneficial
  - Rationale: Fewer dependencies, better performance for CPS data
- Document functions with roxygen2 comments
- Avoid global state, use function parameters

#### Data Manipulation Examples
```r
# Preferred: data.table
library(data.table)
dt <- as.data.table(cps_data)
dt[EDUC == 125, .(unemp_rate = weighted.mean(EMPSTAT == 2, WTFINL)),
   by = .(YEAR, MONTH)]

# Acceptable: base R
cps_phd <- cps_data[cps_data$EDUC == 125, ]
aggregate(EMPSTAT ~ YEAR + MONTH, data = cps_phd, FUN = mean)

# Avoid: tidyverse
# library(dplyr)
# cps_data %>% filter(EDUC == 125) %>% ...
```

### Statistical Modeling Standards
- Start with simplest model, add complexity incrementally
- Always check model convergence diagnostics (Rhat, ESS)
- Validate prior choices with prior predictive checks
- Perform posterior predictive checks
- Compare models using LOO-CV or WAIC
- Document model assumptions and limitations

## Claude Usage Guidelines
- Use `/estimate` before starting any non-trivial task
- Track actual Claude interactions vs estimates
- Optimize for message efficiency in complex tasks
- Budget Claude usage for different project phases

**Typical Usage Patterns**:
- **Bug Fix**: 10-30 messages
- **Small Feature**: 30-80 messages  
- **Major Feature**: 100-300 messages
- **Architecture Change**: 200-500 messages

## Collaboration Guidelines
- Always add Claude as co-author on commits
- Run `/hygiene` before asking for help
- Use `/todo` for quick task capture
- Document learnings with `/learn`
- Regular `/reflect` sessions for insights

## Project Standards
- Test coverage: 60% minimum for R functions
- Documentation: All functions documented with roxygen2
- Error handling: Graceful failures with clear messages
- Performance: Monitor code complexity and file sizes
- Reproducibility: Use renv for R package management
- Data provenance: Document data sources and transformations
- Model artifacts: Save models with metadata (date, version, convergence stats)
- ALWAYS use atomic commits
- Use emojis judiciously
- NEVER Edit() a file before you Read() the file

## R Project Structure
- `R/` - R functions and utilities
- `tests/testthat/` - Unit tests for R functions
- `data/` - Processed data files (RDS, parquet)
- `data-raw/` - Raw data and processing scripts
- `models/` - Saved Stan models and fitted objects
- `reports/` - Quarto documents and analysis notebooks
- `stan/` - Stan model files (.stan)
- `DESCRIPTION` - R package metadata
- `NAMESPACE` - R package namespace (auto-generated)

### TDD Examples

- [🔴 test: add failing test for updateCommandCatalog isolation (TDD RED)](../../commit/00e7a22)
- [🔴 test: add failing tests for tdd.js framework detection (TDD RED)](../../commit/2ce43d1)
- [🔴 test: add failing tests for learn.js functions (TDD RED)](../../commit/8b90d58)
- [🔴 test: add failing tests for formatBytes and estimateTokens (TDD RED)](../../commit/1fdac58)
- [🔴 test: add failing tests for findBrokenLinks (TDD RED phase)](../../commit/8ec6319)
