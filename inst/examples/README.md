# Reference Code

This folder contains the original sandbox code used to develop and test the models before packaging. These are **reference implementations** - not part of the package API.

## Purpose

- **Reference**: Examples of how the models were originally used
- **Ideas**: Code to pull from when building package functions and vignettes
- **Testing**: Original workflows that validated the model logic

## Files

- `stan_helpers.R` - Data preparation and workflow helpers (use as reference for package functions)
- `current_week_fit.R` - Current week fitting workflow
- `backtest_sequential.R` - Sequential backtesting implementation
- `compare_algorithms.R` - Algorithm comparison workflows
- `test_workflow.R` - Complete model testing workflow

## Note

When building out the package:
1. Review these scripts for patterns and ideas
2. Refactor the best parts into proper package functions (in `R/`)
3. Create vignettes that demonstrate the cleaned-up workflows
4. Keep this as reference but don't rely on it for production use