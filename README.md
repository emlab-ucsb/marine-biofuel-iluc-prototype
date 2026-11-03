# marine-biofuel-iluc-prototype

This repository contains a report documenting a protype proof-of-concept analysis investigating the projected indirect land use change (ILUC) implications of increasing marine biofuel demand. This prototype is being done by Environmental Markets Lab (emLab) at University of California, Santa Barbara (UCSB) for International Council on Clean Transportation (ICCT). All results presented in this analysis should be considered exploratory and preliminary.

## Live Report

The rendered analysis report is hosted on GitHub Pages:
**https://emlab-ucsb.github.io/marine-biofuel-iluc-prototype/**

The site has two pages:

- `report.qmd`, the report for ICCT. It is the site's home page.
- `technical-notes.qmd`, technical notes for collaborators: every change we made to emLab's land-use model, the first test run, and our questions for the model's authors. Nothing in the report links to it, so share its link directly. The page is unlisted, not private: anyone with the link, or anyone browsing this public repository, can find it.

The report's emLab and UCSB logos are in `images/`, and `partials/title-block.html` places them above its title.

Both pages read their data, headline numbers and figure theme from `r/report_setup.R`. The site is automatically re-rendered and redeployed whenever changes to either page, `r/report_setup.R`, `data/`, `_quarto.yml` or the other files listed in `.github/workflows/render-report.yml` are pushed to the `main` branch.

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

The output will be written to `_site/`. Note that `_site/` is not tracked in git, since GitHub renders the site automatically using GitHub Actions each time changes are pushed to the `main` branch.

## Use of generative AI

Claude (Anthropic, model Opus 5.5) was used through Claude Code as a coding and writing assistant. It drafted and edited much of the R code, including the changes to the vendored land-use model listed in the technical notes, and much of the report prose, and it helped find and check sources. GitHub Copilot's coding agent set up the initial Quarto report and GitHub Pages workflow. We set the research question and scenarios, chose the data sources and key assumptions, and reviewed the code and text. The AI tools are not authors, and we are responsible for all code, analysis and conclusions.

The AI tools produced no data or results: every number, figure and table in the report is computed from the code in this repository when the report is rendered. Most commits made with Claude's help carry a `Co-Authored-By: Claude` trailer.
