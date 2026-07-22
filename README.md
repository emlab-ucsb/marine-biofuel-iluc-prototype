# marine-biofuel-iluc-prototype

This repository contains a report documenting a protype proof-of-concept analysis investigating the projected indirect land use change (ILUC) implications of increasing marine biofuel demand. This prototype is being done by Environmental Markets Lab (emLab) at University of California, Santa Barbara (UCSB) for International Council on Clean Transportation (ICCT). All results presented in this analysis should be considered exploratory and preliminary.

## Live Report

The rendered analysis report is hosted on GitHub Pages:
**https://emlab-ucsb.github.io/marine-biofuel-iluc-prototype/**

The report is automatically re-rendered and redeployed whenever changes to `report.qmd` or `_quarto.yml` are pushed to the `main` branch.

## Setup (one-time)

To enable GitHub Pages for this repository:

1. Go to **Settings → Pages** in the GitHub repository.
2. Under **Source**, select **GitHub Actions**.
3. Save. The next push to `main` will trigger a render and deploy.

## Local Development

To render the report locally, install [Quarto](https://quarto.org/docs/get-started/) and run:

```bash
quarto render
```

The output will be written to `_site/`.
