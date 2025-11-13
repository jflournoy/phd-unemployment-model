# Parameter Recovery Report Evaluation

This document explains the evaluation criteria for parameter recovery and validation reports in this project.

## Overview

The `/evaluate-report` command provides automated quality assessment of statistical validation reports, ensuring they meet rigorous standards before being used to validate modeling approaches on real data.

## Why Report Evaluation Matters

Parameter recovery validation is critical in statistical modeling:

1. **Prevents false confidence** - Poorly validated models can produce misleading results
2. **Catches methodological errors** - Common anti-patterns (like testing on training data) invalidate findings
3. **Ensures reproducibility** - Well-documented validation enables replication
4. **Guides model selection** - Proper validation reveals when simpler models suffice

## Evaluation Criteria

### 1. Coverage Validation (Critical)

**What it checks:**
- Confidence intervals contain true parameter values in ~95% of simulations
- Tests conducted on NEW datasets, not training data
- Differences between groups tested, not just individual parameters

**Why it matters:**
- Fundamental test of statistical validity
- Miscalibrated CIs lead to wrong conclusions
- Testing on training data gives artificially perfect 100% coverage

**Common issues:**
- ❌ Testing models on their own training data
- ❌ Only testing individual parameters instead of differences
- ❌ Coverage rates outside 93-97% range

**Best practices:**
- ✅ Fit MANY models on MANY new datasets
- ✅ Check if each CI contains the true value
- ✅ Report coverage for all comparisons of interest

### 2. DGP Consistency (Critical)

**What it checks:**
- All simulations use identical data-generating process (DGP)
- Parameters match between reference data and validation runs
- Default function parameters documented explicitly

**Why it matters:**
- Parameter mismatches cause systematic bias
- Inconsistent DGP invalidates coverage calculations
- Undocumented defaults lead to confusion and errors

**Project learnings:**
- When we used `trend_slope=0.0003` in reference data but default `trend_slope=0` in validation, coverage dropped from 94% to 73%
- Fixing this single parameter alignment restored proper coverage

**Best practices:**
- ✅ Document ALL parameters with explicit values
- ✅ Use named parameters consistently: `baseline_rates = c(phd = 0.040, ...)`
- ✅ Add "What We're Comparing" section explaining DGP

### 3. Bias Quantification

**What it checks:**
- Mean estimation error reported (should be near zero)
- Precision (SD of estimates) quantified
- Bias distribution visualized
- Relative bias calculated (% of true value)

**Why it matters:**
- Small systematic bias acceptable; large bias indicates problems
- Precision shows estimation variability
- Together, bias and precision characterize estimation quality

**Best practices:**
- ✅ Report bias, precision, RMSE for all parameters
- ✅ Create bias distribution plots (should be symmetric around zero)
- ✅ Compare bias to practical significance thresholds (<5% ideally)

### 4. False Positive Control

**What it checks:**
- Type I error rate tested with null simulations
- All groups have identical parameters (no true differences)
- False positive rate ≤5% (ideally 3-7%)

**Why it matters:**
- High false positive rate means spurious findings
- Must control Type I error to trust significant results
- Different tests (baseline, trend, seasonal) may have different rates

**Best practices:**
- ✅ Run 200+ null simulations
- ✅ Test: baseline differences, trend differences, and combined
- ✅ Report rate with 95% CI (using binomial distribution)

### 5. Visualization Quality

**What it checks:**
- 3+ diagnostic plots included
- Component-wise visualization (trend, seasonal, baseline)
- Uncertainty visualization (error bars, ribbons, CI plots)
- Clear labels and legends

**Why it matters:**
- Visual inspection catches issues that summary statistics miss
- Diagnostic plots reveal patterns in bias, coverage, precision
- Good visualization aids interpretation and communication

**Best practices:**
- ✅ Raw data + fitted values overlay
- ✅ Coverage rate by parameter/comparison
- ✅ Bias distribution (histogram or density)
- ✅ CI width analysis (precision across time/space)
- ✅ Effect size plots with confidence intervals

### 6. Report Structure

**Required sections:**
- **Overview** - What's being validated and why
- **Why This Matters** - Real-world implications
- **Model Specifications** - Exact formulas for all models
- **Coverage Results** - Tables and plots showing coverage rates
- **Bias Analysis** - Mean bias, SD, RMSE by parameter type
- **False Positive Test** - Null simulation results (if applicable)
- **Interpretation** - Clear Pass/Fail with actionable insights

**Why it matters:**
- Standardized structure ensures completeness
- Readers can quickly assess validation quality
- Missing sections indicate incomplete validation

### 7. Statistical Rigor

**What it checks:**
- Sufficient simulations (≥200 for coverage, ≥100 for model selection)
- Proper CI construction (accounting for covariance between parameters)
- Multiple comparisons awareness
- Realistic parameter values (based on real data patterns)
- Parallel processing for computational efficiency

**Why it matters:**
- Too few simulations give unreliable estimates of coverage/bias
- Ignoring covariance miscalibrates CIs for differences
- Unrealistic parameters make validation irrelevant to real analysis

**Best practices:**
- ✅ Use 200-500 simulations for coverage validation
- ✅ Extract covariance matrix: `vcov(model)`
- ✅ Base simulation parameters on real CPS data ranges
- ✅ Document computational settings (cores, runtime)

### 8. Documentation Quality

**What it checks:**
- All DGP parameters stated explicitly with values
- "What We're Comparing" section explaining test procedure
- Reproducible code (seeds, package versions)
- Computational notes (runtime, parallelization)

**Why it matters:**
- Enables exact replication by others
- Clarifies what validation actually tests
- Prevents confusion about defaults and assumptions

**Best practices:**
- ✅ State ALL parameters: `baseline_rates = c(phd = 0.040, masters = 0.050, bachelors = 0.060)`
- ✅ Explain step-by-step: "1. Simulate data with known parameters, 2. Fit model, 3. Check if CI contains true value"
- ✅ Set seed: `set.seed(42)` for reproducibility
- ✅ Note software versions: R 4.4.0, mgcv 1.9-1

## Anti-Patterns to Avoid

### 1. Training Data Reuse (Critical Error)

**The mistake:**
```r
# WRONG: Testing model on its own training data
sim_data <- simulate_data(...)
model <- fit_model(sim_data)
ci <- confint(model)  # CI will contain fitted value with ~100% probability!
```

**The fix:**
```r
# CORRECT: Test on NEW data
for (i in 1:n_sims) {
  sim_data <- simulate_data(seed = i)  # NEW dataset each time
  model <- fit_model(sim_data)
  ci <- confint(model)
  covered[i] <- (true_param >= ci[1]) & (true_param <= ci[2])
}
mean(covered)  # Should be ~0.95
```

**Why this matters:**
- Testing on training data guarantees ~100% coverage
- Gives false confidence in model validity
- One of the most common validation errors

### 2. DGP Mismatch

**The mistake:**
```r
# Reference data uses trend_slope = 0.0003
ref_data <- simulate_data(trend_slope = 0.0003)

# But validation uses default trend_slope = 0
validate_coverage()  # Uses default!
```

**The fix:**
```r
# Explicitly match ALL parameters
validate_coverage(trend_slope = 0.0003)  # Matches reference data
```

### 3. Prediction Interval Confusion

**The mistake:**
Using prediction interval methodology for parameter CIs:
```r
# WRONG for parameter validation
model <- fit_model(data)
predict(model, newdata = test_data)  # This is for predictions, not parameter validation!
```

**Correct approaches:**
- **For parameter CIs:** Fit many models, check if each CI contains true value
- **For prediction intervals:** Fit ONE model, test on many new datasets

### 4. Missing Covariance

**The mistake:**
```r
# WRONG: Ignoring covariance for differences
diff <- coef[1] - coef[2]
se_diff <- sqrt(se[1]^2 + se[2]^2)  # Missing covariance term!
```

**The fix:**
```r
# CORRECT: Include covariance
V <- vcov(model)
se_diff <- sqrt(V[1,1] + V[2,2] - 2*V[1,2])
```

## Using the Evaluation Tool

There are three ways to evaluate reports:

### 1. Interactive Mode (Recommended)

The easiest way - it automatically finds and lists available reports:

```bash
# Via npm
npm run report:evaluate

# Or directly
bash scripts/evaluate-report-interactive.sh
```

**What it does:**
- Lists all reports in `reports/` (HTML, QMD, Rmd files)
- Shows file size and modification date
- Highlights the most recently modified report
- Lets you select which one to evaluate (or press Enter for most recent)

**Example output:**
```
📊 Searching for reports in reports/ directory...

Found 12 report(s):

  1) factor-smooth-parameter-recovery.qmd [Quarto Source]
     Size: 36K | Modified: 2025-11-13
     Path: reports/factor-smooth-parameter-recovery.qmd

  2) factor-smooth-parameter-recovery.html [Rendered HTML]
     Size: 296K | Modified: 2025-11-13
     Path: reports/factor-smooth-parameter-recovery.html

Most recent: factor-smooth-parameter-recovery.qmd

Select report to evaluate (1-12) or press Enter for most recent:
```

### 2. Direct File Specification

If you know which report you want to evaluate:

```bash
# Via npm
npm run report:evaluate reports/factor-smooth-parameter-recovery.html

# Via bash wrapper
bash scripts/evaluate-report-interactive.sh reports/factor-smooth-parameter-recovery.html

# Direct R script
Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.html
```

### 3. Slash Command

When using Claude Code:

```bash
/evaluate-report
```

Claude will:
1. List available reports
2. Ask which one to evaluate
3. Run the evaluation
4. Interpret results with recommendations

### Verbose Mode

For detailed diagnostics, add `--verbose`:

```bash
# Interactive
npm run report:evaluate:verbose

# Direct
bash scripts/evaluate-report-interactive.sh reports/your-report.html --verbose
```

## Interpreting Results

### Grades

- **PASS (≥80%)** - Report meets standards, validation is trustworthy
- **CONDITIONAL PASS (65-79%)** - Minor issues, address warnings before publication
- **FAIL (<65%)** - Critical issues, do not trust validation results

### Output Sections

1. **Overall Grade** - Summary pass/fail with percentage
2. **Category Scores** - Breakdown by evaluation criterion
3. **Critical Issues** - Must fix before trusting results
4. **Warnings** - Should address for robustness
5. **Strengths** - What the report does well
6. **Recommendations** - Specific actionable improvements

### Example Output

```
========================================
  Parameter Recovery Report Evaluation
========================================

Report: factor-smooth-parameter-recovery.html

===============================
  EVALUATION RESULTS
===============================

Overall Grade: PASS (95%)

Category Scores:
----------------
  Coverage:              100%  [Pass]
  Dgp_consistency:        80%  [Pass]
  Bias:                  100%  [Pass]
  False_positive:        100%  [Pass]
  Visualization:         100%  [Pass]
  Structure:             100%  [Pass]
  Rigor:                  80%  [Pass]
  Documentation:         100%  [Pass]

Warnings (Should Address):
--------------------------
  ⚠️  No discussion of DGP consistency across simulations

Strengths:
----------
  ✅ Strong coverage validation methodology
  ✅ Excellent visualization and diagnostics
  ✅ Well-documented and reproducible

Recommendations:
----------------
  • Add explicit discussion of DGP parameter consistency
  • Document default parameter values in function calls

========================================
```

## Integration with Project Workflow

### When to Evaluate

1. **After creating a new validation report** - Ensure quality before using results
2. **Before submitting for review** - Catch issues early
3. **When updating validation functions** - Verify reports still meet standards
4. **During code review** - Systematic quality check

### Continuous Improvement

The evaluation criteria are based on project learnings stored in `.claude/learnings/`:

- Coverage validation methodology insights
- DGP consistency lessons learned
- Anti-patterns discovered during development
- Best practices from statistical literature

As the project evolves, update both:
1. Evaluation criteria in `scripts/evaluate-report.R`
2. This documentation in `docs/report-evaluation.md`

## References

### Project Resources

- [`.claude/commands/evaluate-report.md`](../.claude/commands/evaluate-report.md) - Command documentation
- [`scripts/evaluate-report.R`](../scripts/evaluate-report.R) - Evaluation script
- [`R/parameter-recovery-validation.R`](../R/parameter-recovery-validation.R) - Validation functions
- [`.claude/learnings/`](../.claude/learnings/) - Project learnings archive

### Statistical Background

Key concepts in parameter recovery validation:

- **Coverage probability** - P(CI contains true parameter)
- **Bias** - E[estimate - true value]
- **Precision** - SD(estimates)
- **RMSE** - sqrt(bias² + variance)
- **Type I error** - P(reject H0 | H0 true)

For more on validation methodology, see:
- Gelman & Hill (2007) - Data Analysis Using Regression and Multilevel/Hierarchical Models
- McElreath (2020) - Statistical Rethinking, Chapter 4 (model validation)
- Wood (2017) - Generalized Additive Models: An Introduction with R, Chapter 5 (inference)

## Contributing

To improve the evaluation criteria:

1. Add learnings: `/learn <insight>`
2. Update evaluation script: `scripts/evaluate-report.R`
3. Update documentation: `docs/report-evaluation.md`
4. Add tests: `test/evaluate-report.test.js`
5. Run checks: `npm run hygiene`

Questions? Open an [issue](https://github.com/jflournoy/phd-unemployment-model/issues).
