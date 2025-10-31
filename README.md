# PhD Unemployment Statistical Modeling

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue)](https://www.r-project.org/)
[![Stan](https://img.shields.io/badge/Stan-brms%2Fcmdstanr-red)](https://mc-stan.org/)
[![Commands](https://img.shields.io/badge/commands-14-brightgreen)](.claude/commands/)

Statistical modeling of PhD unemployment rates relative to sensible baselines (general unemployment, other graduate degrees) using Bayesian time series methods with Test-Driven Development.

## Overview

This project aims to model PhD unemployment patterns using advanced time series techniques to understand:

- How PhD unemployment compares to general unemployment rates
- Seasonal patterns and economic cycles in PhD employment
- Differences across degree types (PhD vs Master's vs Bachelor's)
- Long-term trends and structural changes

### Scientific Goals

- **Maximum disaggregation**: Decompose unemployment into multiple seasonal components, trends, and cycles
- **Model parsimony**: Start with simplest models, add complexity incrementally
- **Robust inference**: Use Bayesian methods for principled uncertainty quantification
- **Clear communication**: Document the TDD process and present insights through compelling visualizations

## Tech Stack

### Core Technologies

- **R** (≥ 4.0.0) - Data processing and analysis
- **Stan** - Bayesian statistical modeling via:
  - [brms](https://paul-buerkner.github.io/brms/) - High-level modeling interface
  - [cmdstanr](https://mc-stan.org/cmdstanr/) - R interface to CmdStan
- **tidyverse** - Data manipulation and visualization

### Statistical Methods

- **GAMs** (Generalized Additive Models) - Flexible trend and seasonal modeling
- **Gaussian Processes** - Non-parametric time series modeling
- **Autoregressive Models** - Temporal dependence structures
- **Seasonal Decomposition** - Multiple seasonal components

### Data Source

- [IPUMS USA](https://usa.ipums.org/usa/index.shtml) - Current Population Survey (CPS) data

## Development Approach

This project uses **Test-Driven Development (TDD)** for statistical modeling:

- Write tests for data validation and processing functions
- Test model convergence and diagnostics
- Validate statistical properties with simulated data
- Ensure reproducibility through comprehensive testing

The TDD workflow infrastructure is preserved from [rmurphey/claude-setup](https://github.com/rmurphey/claude-setup), providing professional development tools and workflows.

## Development Environment

**IMPORTANT**: All development should occur within the **RStudio Quarto Docker container**.

### Environment Check

**Always run this at the start of a session:**

```bash
npm run env:check
```

This script will verify:

- You're inside the Docker container
- R, RStudio Server, and Quarto are available
- Required R packages are installed

If you're not in the correct environment, the script provides connection instructions.

### Why Docker?

- Consistent R and package versions
- Pre-configured Stan toolchain
- Quarto for reproducible reports
- Isolated environment for reproducibility

*See [Issue #2](https://github.com/jflournoy/phd-unemployment-model/issues/2) for Docker setup documentation (coming soon)*

## Project Structure

```
phd-unemployment-model/
├── R/                  # R functions and utilities
├── tests/testthat/     # Unit tests for R functions
├── data/               # Processed data files (gitignored)
├── data-raw/           # Raw data and processing scripts
├── models/             # Fitted models and Stan code (gitignored)
├── reports/            # Quarto analysis documents
├── stan/               # Custom Stan model files
├── docs/               # Project documentation
│   ├── data-acquisition.md    # IPUMS data guide
│   ├── modeling-approach.md   # Statistical workflow
│   └── tdd-workflow.md        # TDD for statistics
├── DESCRIPTION         # R package metadata
├── CLAUDE.md           # AI development guidelines
└── README.md           # This file
```

## Getting Started

### Prerequisites

- Docker with RStudio Quarto image (recommended)
- OR: R ≥ 4.0.0, RStudio, and Stan toolchain installed locally

### Installation (within Docker container)

```r
# Install development dependencies
install.packages("devtools")
install.packages("testthat")

# Install project dependencies
devtools::install_deps()

# Load package for development
devtools::load_all()
```

### TDD Workflow Commands

This project includes powerful workflow commands via npm scripts:

```bash
# Project health check
/hygiene

# Run R tests
npm run test:r

# Task management (GitHub Issues)
/todo list
/todo add "task description"

# Learning capture
/learn "insight or learning"

# Documentation
/docs
```

See [CLAUDE.md](CLAUDE.md) for complete command reference.

### CI Monitoring

The project includes multiple ways to monitor continuous integration status:

#### Option 1: Hygiene Command

The `/hygiene` command includes CI status checks:

```bash
/hygiene
```

This displays recent GitHub Actions workflow runs and their status.

#### Option 2: Repository Monitoring

Active monitoring with desktop notifications:

```bash
npm run monitor:start        # Start monitoring
npm run monitor:check        # Check current status
npm run monitor:stop         # Stop monitoring
```

Features:
- Desktop notifications for test failures
- Sound alerts (configurable)
- Failure history tracking
- Customizable check intervals

Configure in `.monitor-config.json`:

```json
{
  "interval": 300000,
  "desktopNotifications": true,
  "sound": true,
  "testsOnly": true
}
```

#### Option 3: Pre-Push Hook

Automatically check CI status before pushing:

```bash
git push
```

The pre-push hook will:
- Check if CI is currently failing
- Block push if tests are failing on main
- Can be bypassed with `git push --no-verify` if needed

Install the hook:

```bash
npm run hooks:install
```

## Development Workflow

### 1. Data Acquisition

1. Register at [IPUMS USA](https://usa.ipums.org/usa/index.shtml)
2. Extract CPS data with employment and education variables
3. Place raw data in `data-raw/`
4. Create processing script following TDD principles

See [docs/data-acquisition.md](docs/data-acquisition.md) for detailed guide.

### 2. Data Processing

```r
# Example: Process IPUMS data
source("data-raw/01-process-ipums-cps.R")

# Processed data saved to data/
phd_unemp <- readRDS("data/phd_unemployment.rds")
```

### 3. Model Development

Follow the TDD cycle:

1. **RED** - Write failing test for model property
2. **GREEN** - Implement model to pass test
3. **REFACTOR** - Improve model with test safety net
4. **COMMIT** - Save working, tested model

See [docs/modeling-approach.md](docs/modeling-approach.md) for modeling strategy.

### 4. Analysis & Reporting

Create Quarto documents in `reports/` for:

- Exploratory data analysis
- Model comparison
- Results visualization
- Final write-up

## Documentation

Comprehensive documentation in `docs/`:

- **[data-acquisition.md](docs/data-acquisition.md)** - How to obtain and process IPUMS data
- **[modeling-approach.md](docs/modeling-approach.md)** - Statistical modeling philosophy and workflow
- **[tdd-workflow.md](docs/tdd-workflow.md)** - Applying TDD to statistical modeling
- **[CLAUDE.md](CLAUDE.md)** - AI-assisted development guidelines

## Testing

```r
# Run all tests
devtools::test()

# Or via npm
npm run test:r

# Run specific test file
testthat::test_file("tests/testthat/test-unemployment-rate.R")

# Check test coverage
covr::package_coverage()
```

See [docs/tdd-workflow.md](docs/tdd-workflow.md) for testing strategy.

## Contributing

This is a research project. For questions or suggestions, please open an [issue](https://github.com/jflournoy/phd-unemployment-model/issues).

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- TDD workflow infrastructure adapted from [rmurphey/claude-setup](https://github.com/rmurphey/claude-setup)
- Data from [IPUMS USA](https://usa.ipums.org/usa/index.shtml), University of Minnesota
- Statistical modeling powered by the [Stan](https://mc-stan.org/) ecosystem
