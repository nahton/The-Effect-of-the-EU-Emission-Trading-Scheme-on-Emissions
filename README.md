# EU ETS Research Paper Replication

This repository contains the data and R code for the first public replication package of the EU ETS research paper.

The package is focused on the finalized matched sample, the country-level sample construction behind it, and the main tables and figures generated from that sample.

The cleanup was verified with R 4.4.1.

## Quick Start

Install the required R packages once using your preferred R package manager:
`broom`, `dplyr`, `fixest`, `fuzzyjoin`, `ggplot2`, `haven`, `MatchIt`,
`purrr`, `readr`, `scales`, `sf`, `stringdist`, `stringr`, `tibble`, and
`tidyr`.

From the repository root, run the main result script:


source("scripts/reproduce_paper_results.R")


The script reads `total_data.csv`, the finalized matched sample, and writes the reproduced tables and plots to `outputs/`.

Optional appendix tables that are generated from the finalized matched sample can be reproduced with:


source("scripts/reproduce_appendix_results.R")


In RStudio, open `EU_ETS_Replication.Rproj` from the cloned repository folder. RStudio will then use the repository as the working directory, regardless of where the folder was cloned.

## Core Scope

The first-release replication workflow covers:

- the finalized matched sample in `total_data.csv`
- the country-level matching scripts for France, the United Kingdom, the Netherlands, and Norway
- Table 1: matched-sample counts
- Table 2: baseline PPML estimates
- Table 3: phase-specific PPML estimates
- Table 8: sector-specific PPML estimates
- Table 9: country-specific PPML estimates
- Table 10: country-specific OLS estimates
- Figure 3: average emissions in the matched panel
- Figure 4: yearly treatment effects
- Figure 5: indexed matched-panel emissions
- Figures 8-11: country-level matched-panel emissions

The optional appendix script reproduces Tables 5-6 from `total_data.csv`.
The harmonized sector mapping used in the paper is stored directly in `data/sector_mapping.csv`.
