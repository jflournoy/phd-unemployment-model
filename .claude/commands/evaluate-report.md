# Report Evaluation: Parameter Recovery Validation

Evaluate parameter recovery and validation reports for quality, completeness, and statistical rigor.

## Your Task

You are an expert statistical reviewer evaluating parameter recovery and validation reports for this PhD unemployment modeling project.

### Workflow

1. **List available reports** in the `reports/` directory:
   ```bash
   ls -lh reports/*.{html,qmd,Rmd} 2>/dev/null | grep -E "\.(html|qmd|Rmd)$"
   ```

2. **Ask the user** which report to evaluate, or evaluate the most recently modified report by default

3. **Run the evaluation**:
   ```bash
   Rscript scripts/evaluate-report.R <report-file>
   ```

4. **Present results** with clear interpretation and actionable recommendations

### Example

```bash
# Find reports
$ ls -1t reports/*.html reports/*.qmd 2>/dev/null | head -5

# Most recent: reports/factor-smooth-parameter-recovery.html

# Evaluate it
$ Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.html
```

## Evaluation Criteria

Based on project learnings and statistical best practices, evaluate reports on:

### 1. Coverage Validation (Critical)
- ✅ **Proper coverage testing**: Fit MANY models on NEW datasets, check if CIs contain true values
- ❌ **Anti-pattern**: Testing models on their own training data (gives 100% coverage)
- Target: 95% CIs should contain true value in ~93-97% of simulations
- Must test differences between groups, not just individual parameters

### 2. DGP Consistency (Critical)
- All validation simulations must use EXACT same data-generating process
- Parameters must match between reference data and validation runs
- Default parameters in functions must be documented explicitly
- Any mismatch causes systematic bias and miscalibrated coverage

### 3. Bias Quantification
- Mean estimation error should be near zero
- Bias should be small relative to true parameter values (<5% ideally)
- Bias distribution plots should be symmetric around zero
- Must report both bias and precision (SD of estimates)

### 4. False Positive Control
- Type I error rate should be ≤5% (ideally 3-7% range)
- Test with null datasets where NO differences exist between groups
- Report: baseline differences, trend differences, and combined rates

### 5. Visualization Quality
- Clear separation of observed data vs fitted values
- Component-wise plots (trend, seasonal, baseline)
- Coverage diagnostic plots (CI width, bias distribution)
- Effect size plots with confidence intervals

### 6. Report Structure
Must include these sections:
- **Overview**: Clear statement of what's being validated
- **Why This Matters**: Real-world implications
- **Model Specifications**: Exact formulas for all models tested
- **Coverage Results**: Tables + plots showing coverage rates
- **Bias Analysis**: Mean bias, SD, RMSE by parameter type
- **False Positive Test**: Null simulation results
- **Interpretation**: Clear Pass/Fail with actionable insights

### 7. Statistical Rigor
- Sufficient simulation runs (n ≥ 200 for coverage, ≥ 100 for model selection)
- Proper confidence interval construction (accounting for covariance)
- Multiple comparisons awareness (if testing many parameters)
- Realistic parameter values (based on real CPS data patterns)
- Parallel processing support for computational efficiency

### 8. Documentation Quality
- All DGP parameters explicitly stated with values
- "What We're Comparing" section explaining test procedure step-by-step
- Code is reproducible (seeds, versions, dependencies)
- Computational notes (runtime, parallelization settings)

## Common Issues to Flag

### Anti-patterns:
1. **Training data reuse**: Testing model on data used to fit it
2. **DGP mismatch**: Validation params differ from reference data params
3. **Missing defaults**: Not documenting function default parameters
4. **Prediction interval confusion**: Using wrong approach for parameter CIs

### Red flags:
- Coverage rates outside 93-97% range
- Large systematic bias (>10% of true value)
- False positive rate >8%
- Missing visualization of key diagnostics
- Vague descriptions of data-generating process

### Yellow flags:
- Coverage in 90-93% or 97-100% range (investigate further)
- Moderate bias 5-10% (may be acceptable depending on context)
- False positive rate 6-8% (borderline)
- Insufficient simulation runs (<100)

## Output Format

Provide a structured evaluation report with:

1. **Overall Grade**: Pass / Conditional Pass / Fail
2. **Critical Issues**: Must fix before trusting results
3. **Warnings**: Should address for robustness
4. **Strengths**: What the report does well
5. **Recommendations**: Specific actionable improvements
6. **Checklist**: Tick-box summary of all criteria

## Example Usage

```bash
# Evaluate current parameter recovery report
npm run report:evaluate reports/factor-smooth-parameter-recovery.html

# Evaluate with detailed diagnostics
Rscript scripts/evaluate-report.R reports/factor-smooth-parameter-recovery.qmd --verbose
```

## Notes

- Use learnings from `.claude/learnings/` to inform evaluation
- Reference validation functions in `R/parameter-recovery-validation.R`
- Check against patterns in existing reports
- Be constructive but rigorous - statistical validity is critical
- Suggest specific code improvements where applicable
