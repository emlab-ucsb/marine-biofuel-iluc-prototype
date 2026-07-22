# marine-biofuel-iluc-prototype

This repository contains a report documenting a protype proof-of-concept analysis investigating the projected indirect land use change (ILUC) implications of increasing marine biofuel demand. This prototype is being done by Environmental Markets Lab (emLab) at University of California, Santa Barbara (UCSB) for International Council on Clean Transportation (ICCT). All results presented in this analysis should be considered exploratory and preliminary.

## Live Report

The rendered analysis report is hosted on GitHub Pages:
**https://emlab-ucsb.github.io/marine-biofuel-iluc-prototype/**

The report is automatically re-rendered and redeployed whenever changes to `report.qmd` or `_quarto.yml` are pushed to the `main` branch.

## Running the analysis locally

### R package management (renv)

This project uses [`renv`](https://rstudio.github.io/renv/) to snapshot the exact R
package versions used in the analysis in a lockfile, so that everyone works from the same setup.
The first time you open the project in Positron/R/RStudio, ensure you have `renv` installed, then run the following:

```r
renv::restore()
```

This installs the required packages into a project-local library (from `renv.lock`).

A few tips:

- Restore is usually quick, since packages are pulled from a shared cache.
- If a package still isn't found, check that R/RStudio is using **R 4.6** (the
  version this project is pinned to) — `renv` keeps a separate library per R version.
- `pdftools` needs the system `poppler` library. On macOS: `brew install poppler`;
  on Ubuntu/Debian: `sudo apt-get install libpoppler-cpp-dev`.

### Rendering the report locally

To render the report locally, first install [Quarto](https://quarto.org/docs/get-started/). You can then either render the report using the built-in functionality of IDEs such as Positron or R Studio, or run this in the terminal:

```bash
quarto render
```

The output will be written to `_site/`. Note that `_site/` is not tracked in git, since GitHub renders the report automatically using GitHub Actions each time  changes to `report.qmd` or `_quarto.yml` are pushed to the `main` branch.

