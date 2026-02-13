# EstimatingBiologicalVariation

> **⚠️ This project is under active development and is not yet ready for
> production use.** APIs, models, and the user interface may change without
> notice. Contributions, feedback, and bug reports are very welcome — but
> please be aware that things can break between commits.

---

## What is this?

**EstimatingBiologicalVariation** is an R package that ships a
feature-rich [Shiny](https://shiny.posit.co/) application for estimating
the three core components of biological variation:

| Component                        | Symbol | Description                                      |
| -------------------------------- | ------ | ------------------------------------------------ |
| **Analytical variation**         | CV_A   | Measurement imprecision of the laboratory method |
| **Within-individual variation**  | CV_I   | Day-to-day fluctuation within the same person    |
| **Between-individual variation** | CV_G   | Variation across different individuals           |

Reliable estimates of these components are essential for setting
analytical performance specifications, calculating reference change
values (RCVs), and evaluating the utility of population-based reference
intervals.

### Why Bayesian?

Classical ANOVA-based estimates can be unstable when the number of
subjects or replicates is small. This package fits **Bayesian
hierarchical models via [Stan](https://mc-stan.org/)** to obtain
full posterior distributions for every variance component — giving you
credible intervals instead of single point estimates, and making it
straightforward to incorporate prior knowledge.

---

## Key Features

- **Interactive Shiny UI** — a guided, wizard-style workflow with a
  custom "glass" design system (dark-themed cards, D3.js plots, and
  smooth animations).
- **Multiple Stan models** — including normal-theory (NTT) and
  heavy-tailed / degrees-of-freedom–gamma (NTTDFGAM) formulations for
  robust estimation.
- **Built-in prior elicitation** — set informative or weakly informative
  priors through an interactive panel with real-time visualisation of
  prior predictive distributions.
- **MCMC diagnostics** — trace plots, R-hat, effective sample size, and
  per-parameter convergence summaries generated automatically after
  sampling.
- **ANOVA baseline with bootstrap CIs** — classical variance-component
  estimates with bootstrap confidence intervals, for comparison with the
  Bayesian results.
- **Data exploration** — interactive scatter plots with click-to-exclude
  functionality, descriptive statistics, and automatic outlier
  highlighting.
- **Rich D3-based visualisations** — posterior densities, forest plots,
  subject-level CV_I panels, and RCV plots rendered client-side for
  responsive interaction.
- **Fast C++ back-end** — Rcpp routines for variance-component ANOVA,
  bootstrap resampling, and Stan data preparation.
- **Excel I/O** — upload `.xlsx` / `.xls` / `.csv` files and download
  result workbooks with a single click.

---

## Installation

### Prerequisites

| Requirement                         | Minimum version                                                   |
| ----------------------------------- | ----------------------------------------------------------------- |
| R                                   | ≥ 4.1.0                                                           |
| [rstan](https://mc-stan.org/rstan/) | ≥ 2.21.0                                                          |
| A C++ toolchain                     | Rtools (Windows), Xcode CLI (macOS), or `build-essential` (Linux) |

> **Tip:** If you have never used Stan before, follow the
> [RStan Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
> guide to make sure your C++ toolchain is configured correctly.

### From source (development)

```r
# Install remotes if needed
install.packages("remotes")

# Install the package (and dependencies) directly from the local source
remotes::install_local("path/to/EstimatingBiologicalVariation")

# — or, if you have the repo cloned and your working directory is the
#   package root:
devtools::install()
```

### Dependencies

Core dependencies are declared in `DESCRIPTION` and will be installed
automatically. The most important ones are:

`shiny`, `rstan`, `data.table`, `readxl`, `ggplot2`, `DT`, `writexl`,
`truncnorm`, `purrr`, `htmltools`, `Rcpp`.

---

## Quick Start

```r
library(EstimatingBiologicalVariation)
run_app()
```

This launches the Shiny application in your default browser. From there
you can:

1. **Upload** your dataset (Excel or CSV).
2. **Map columns** — the app guesses which columns correspond to
   Subject, Sample, Replicate, and Result, but you can adjust manually.
3. **Explore** your data interactively and exclude/include observations.
4. **Configure priors** for the Bayesian model.
5. **Run the analysis** — choose between the NTT or NTTDFGAM model and
   set MCMC parameters (chains, iterations, warm-up, etc.).
6. **Review results** — posterior summaries, diagnostics, RCVs, and
   downloadable reports.

---

## Project Structure

```
R/                     # All R source code
  app_core.R           #   Shiny server / UI assembly
  run_app.R            #   Entry point: run_app()
  mod_*.R              #   Shiny modules (upload, explore, model, …)
  glass*.R             #   Custom "glass" UI components
  utils_*.R            #   Helpers (analysis, D3 plots, constants, …)
  biovar_ported.R      #   Ported Stan-output processing functions
  RcppExports.R        #   Auto-generated Rcpp bindings
src/                   # C++ source files (Rcpp / RcppArmadillo)
  process_stan_data.cpp
  variance_components.cpp
  cv_transforms.cpp
inst/
  app/                 # Shiny app entry-point used by run_app()
  assets/              # CSS & JS for the glass design system + D3 plots
  stan/                # Stan model files (.stan) and pre-compiled caches
man/                   # Auto-generated documentation (roxygen2)
```

---

## Development

### Rebuild after editing R or C++ code

```r
# Re-generate Rcpp exports and roxygen docs, then reload
Rcpp::compileAttributes()
devtools::document()
devtools::load_all()
```

### Rebuild fully

```r
devtools::install()
```

### Running tests

```r
devtools::test()
```

---

## Current Status

| Area                                 | Status                      |
| ------------------------------------ | --------------------------- |
| Core Bayesian models (NTT, NTTDFGAM) | ✅ Functional               |
| ANOVA + bootstrap baseline           | ✅ Functional               |
| Shiny UI & wizard workflow           | 🚧 Under active development |
| D3-based visualisations              | 🚧 Under active development |
| Prior elicitation module             | 🚧 Under active development |
| Documentation & vignettes            | 🔜 Planned                  |
| Unit test coverage                   | 🔜 Planned                  |
| CRAN submission                      | 🔜 Not yet planned          |

---

## Contributing

This project is in an early stage and we appreciate all forms of
contribution:

- **Bug reports** — please open an
  [issue](https://github.com/pernille267/EstimatingBiologicalVariation/issues)
  with a minimal reproducible example.
- **Feature requests** — open an issue describing the use-case.
- **Pull requests** — fork, branch, and submit a PR. Please run
  `devtools::check()` before submitting.

---

## License

MIT © 2026 Pernille Fauskanger — see [LICENSE](LICENSE) for details.
