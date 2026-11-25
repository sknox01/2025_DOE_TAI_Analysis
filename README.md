# Evaluation of daytime, nighttime and neural network CO₂ flux partitioning in tidal marshes

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24519166.svg)](https://doi.org/10.5281/zenodo.24519166)

Analysis code accompanying:

> Knox, S. H., et al. *Advancing Flux Partitioning Across Salinity and Elevation Gradients: Integrating Tidal Marsh Hydrological Variables into Artificial Neural Networks.* Global Change Biology (accepted). DOI: [add when available]

## Overview

This repository evaluates three methods for partitioning net ecosystem CO₂ exchange (NEE) into gross primary production (GPP) and ecosystem respiration (RECO) at eight tidal marsh sites in the United States and Canada:

- the daytime light-response method
- the nighttime temperature-response method
- physics-informed artificial neural network (ANN) partitioning

It contains the code and site data used to produce the paper's statistics, tables and figures. The ANN model, its predictions and the processed site data are in the companion repository [onur-kocer/nee-partition](https://github.com/onur-kocer/nee-partition) (Zenodo DOI: [10.5281/zenodo.24489069](https://doi.org/10.5281/zenodo.24489069)).

## Contents

- `code/results_summary.Rmd`: the main analysis. It:
  - compares modelled nighttime RECO with measured nighttime NEE, with day-block bootstrap confidence intervals
  - selects the best-performing and baseline ANN predictor sets at each site
  - builds diel composites of RECO and GPP
  - relates partitioning performance to site characteristics (Kendall's τ, Theil–Sen slopes, permutation tests)
  - compares the methods with the stable-isotope partitioning at US-EDN
  - produces the supplementary diagnostics
- `code/site_map.R`: the study site map (Figure 1)
- `data/Site characteristics final.xlsx`: site metadata (AmeriFlux site ID, coordinates, elevation, relative elevation Z\*MHW, tidal range, salinity class)
- `data/Isotope_EDN.csv`: half-hourly stable-isotope GPP and RECO estimates for US-EDN (August–September 2022). Columns: `datetime`, `GPP_isotope_night0` (GPP, with nighttime values set to zero) and `Reco_isotope` (RECO), both in µmol CO₂ m⁻² s⁻¹
- `DOE_TAI_Partitioning.Rproj`: RStudio project file; paths are resolved from the project root with `here`

Figures are exported to Global Change Biology specifications (300 dpi at printed size, with panel labels) and named by figure number (`Figure_2.png`, `Figure_S5.png`, …). Generated figures and tables are written to `figures/` and `output/`, which the scripts create.

## Requirements

R ≥ 4.4.1 with these packages:

```r
install.packages(c(
  "tidyverse", "vroom", "lubridate", "glue", "broom", "Metrics", "readxl",
  "here", "patchwork", "ggrepel", "reshape2", "corrplot", "Hmisc", "lomb",
  "sf", "rnaturalearth", "rnaturalearthdata"
))
```

## Reproducing the analysis

1. Download this repository and [nee-partition](https://doi.org/10.5281/zenodo.24489069).
2. Open `DOE_TAI_Partitioning.Rproj` in RStudio.
3. In `code/results_summary.Rmd`, set `base_dir` to your local copy of `nee-partition`. The script reads the ANN predictions from `nee-partition/model_predictions/` and the processed site data from `nee-partition/data/`.
4. Knit `code/results_summary.Rmd`, then run `code/site_map.R`.

## Data sources

Eddy covariance data for seven sites are from AmeriFlux:

| Site | Dataset |
|---|---|
| CA-DSM | Knox (2023), https://doi.org/10.17190/AMF/1964085 |
| CA-RBM | Knox (2025), https://doi.org/10.17190/AMF/2563527 |
| US-Dmg | Arias-Ortiz & Baldocchi (2025), https://doi.org/10.17190/AMF/1964086 |
| US-EDN | Oikawa (2020), https://doi.org/10.17190/AMF/1543381 |
| US-PLo | Forbrich (2026), https://doi.org/10.17190/AMF/3004320 |
| US-Srr | Bergamaschi & Windham-Myers (2018), https://doi.org/10.17190/AMF/1418685 |
| US-StJ | Vargas (2018), https://doi.org/10.17190/AMF/1480316 |

US-PLM data can be provided upon request. Water table depth and salinity data were provided by the site principal investigators. The data were quality controlled, gap-filled and partitioned with the CanFlux data pipeline ([Nesic et al., 2025](https://doi.org/10.5281/zenodo.16740100)). The processed dataset used in the paper is archived with nee-partition ([10.5281/zenodo.24489069](https://doi.org/10.5281/zenodo.24489069)).

## Citation

Please cite the paper and this archive:

> Knox, S. H., et al. (2026). *Evaluation of daytime, nighttime and neural network CO₂ flux partitioning in tidal marshes* (Version v1.0.0) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.24519166

## Licence

Code: MIT (see `LICENSE`). Data: CC BY 4.0.

## Acknowledgements

ChatGPT (OpenAI) and Claude (Anthropic) were used to help streamline parts of the R code. All code was reviewed by the authors.

## Contact

Sara H. Knox, Department of Geography, McGill University — sara.knox@mcgill.ca
