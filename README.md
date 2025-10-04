
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflendzoneModel

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**NFL Bayesian state-space models for team strength estimation and game
prediction**

`nflendzoneModel` provides pre-compiled CmdStan models for efficient
Bayesian analysis of NFL game data. The package uses the
[`instantiate`](https://wlandau.github.io/instantiate/) framework to
compile Stan models during package installation, making them immediately
available without manual compilation.

## Features

- **Pre-compiled Stan Models**: Models compile once during installation
- **State-Space Framework**: Global week-level team strength tracking
  with AR(1) dynamics
- **Home Field Advantage**: Team-specific and league-wide HFA estimation
- **Seasonal Effects**: Handles season transitions and carryover
- **Out-of-Sample Predictions**: Generate quantities block for
  forecasting

## Models

### Team Strength Fit Model

A hierarchical state-space model featuring:

- **Team Strengths**: AR(1) evolution across weeks with seasonal regime
  changes
- **Home Field Advantage**: League-level AR(1) + team-specific
  deviations
- **Sum-to-Zero Constraints**: Efficient identifiability
- **Non-Centered Parameterization**: Improved sampling efficiency

### Generated Quantities Model

Standalone model for predictions:

- **Filtered States**: Team strengths and HFA at last observed time
- **Multi-Step Forecasting**: Simulates latent states forward
- **Out-of-Sample Games**: Predictive distributions for future games

## Installation

### Prerequisites

You need CmdStan installed:

``` r
# Install cmdstanr
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Install CmdStan
cmdstanr::install_cmdstan()
```

### Install Package

``` r
# Install from local source (compiles Stan models)
install.packages(".", repos = NULL, type = "source")
```

The Stan models will compile automatically during installation.

## Usage

``` r
library(nflendzoneModel)

# Access pre-compiled models
fit_mod <- get_team_strength_fit_model()
gq_mod <- get_team_strength_gq_model()

# Fit the model (requires prepared Stan data)
fit <- fit_team_strength_model(
  stan_data = your_stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

# Generate predictions
predictions <- generate_team_predictions(
  fit = fit,
  gq_data = your_gq_data,
  parallel_chains = 4
)
```

See vignettes for detailed examples.

## Model Details

### State Evolution

**Within Season (Weekly Updates):**

    team_strength[w] = φ_week * team_strength[w-1] + innovation_week

**Season Transitions (Carryover):**

    team_strength[w] = φ_season * team_strength[w-1] + innovation_season

### Home Field Advantage

**League Level:**

    league_hfa[s] = φ_hfa * league_hfa[s-1] + innovation_hfa

**Team Level:**

    team_hfa[s][t] = league_hfa[s] + team_deviation[s][t]

### Observation Model

    result[g] ~ Normal(μ[g], σ_obs)
    μ[g] = team_strength[w][home] - team_strength[w][away] + team_hfa[s][home] * hfa[g]

## Development

Based on the [instantiate development
workflow](https://wlandau.github.io/instantiate/index.html#development).

After editing Stan models or R code:

``` r
source("dev/build.R")
```

**Important:** Do not use `pkgload::load_all()` - always install the
package properly for Stan models to compile.

See [`dev/README.md`](dev/README.md) for details.

## Stan Model Files

- [`src/stan/team_strength_fit.stan`](src/stan/team_strength_fit.stan) -
  Main fitting model
- [`src/stan/team_strength_gq.stan`](src/stan/team_strength_gq.stan) -
  Generated quantities for predictions

## License

MIT + file LICENSE

## Author

Tyler Pollard
