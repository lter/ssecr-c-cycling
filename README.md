# Carbon Cycling Responses Across Spatiotemporal Scales

This working group is part of the [LTER's](https://lternet.edu/) [Synthesis Skills for Early Career Researchers](https://lter.github.io/ssecr/) course

### Research questions
How does carbon cycling respond to environmental change (in experimental manipulations and to natural disturbances) and how does the duration of the disturbance influence how we model carbon cycling projections in the future across ecosystems? Further, what are the mechanisms driving these responses?

### Motivation
Experimental manipulations are utilized at multiple LTER sites to understand how ecosystems respond to disturbances across a variety of scales. These manipulations are designed to mimic future environmental stressors, and their results are generally interpreted in the context of ecosystem response to climate change. We are interested in how relevant the duration of these experiments are in their applicability and accuracy in reflecting IRL environmental change.

### Approach
We will compile data on carbon stocks, carbon fluxes, and productivity measurements across LTER sites and beyond that implement different experimental manipulations (e.g. warming, nutrients additions, etc.).

### Deliverables
- Recommended timescale of experimental manipulation
- How disturbances affect C stocks, fluxes, and productivity

### Possible environmental manipulations
- temperature (global long-term & local heat wave)
- fire
- nutrient addition
- precipitation
- storms

## Working group members
- Carla Lopez Lloreda
- Guopeng Liang
- Jon Gewirtzman
- Julie Gan
- Ricky Brokaw
- Taylor Walker
- Yiyang Xu

## Mentors
- Marcy Litvak
- Will Wieder

---

## Repository Structure

```
ssecr-c-cycling/
  R/
    pipeline/                # Data acquisition infrastructure
      download.R             # EDI + URL download functions with caching
      registry.R             # Dataset registry reader/query helpers
      manifest.R             # JSON provenance manifest read/write/verify
      preprocess_runner.R    # Orchestrator: registry → download → preprocess
    preprocess/              # One file per LTER site (18 files)
      preprocess_knz.R       # ... through preprocess_and.R
    utils_dates.R            # Date parsing (YYYY, YYYY-MM, MM/DD/YYYY, etc.)
    utils_labels.R           # Site metadata lookup and abbreviation mapping
    utils_analysis.R         # Core analysis: relative response, trend, LRR
    utils_plots.R            # Shared ggplot theme, trend color palette
  data/
    dataset_registry.csv     # Authoritative source-of-truth: EDI packages → outputs
    column_key.csv           # Maps raw column names → harmonized names
    site_metadata.csv        # Site abbreviations, ecosystem types, case-study flags
    manifests/               # JSON download provenance (git-tracked)
    raw/                     # Cached EDI downloads (.gitignored, reproducible)
    ready/                   # Processed CSVs ready for harmonization
    harmonized/              # Harmonized output (single combined dataset)
  analysis/                  # Numbered scripts (run in order)
    00_download-and-preprocess.R  # Download from EDI + run preprocessing
    00_validate-data.R       # Validate files, columns, manifests, registry
    01_harmonize.R           # Combine all datasets via ltertools::harmonize()
    02_relative-response.R   # Treatment/control ratios; per-experiment plots
    03_trend-classification.R    # Classify trends (stable/variable/inc/dec)
    04_detection-time.R      # Timepoints needed to detect significant trends
    05_lrr-analysis.R        # Log-response ratio analysis
    06_summary-stats.R       # Summary tables, manuscript outputs
  figures/                   # Generated figures
    supplemental/            # Supplemental tables and panel figures
  legacy/
    preprocessing_scripts_original/  # Archived original preprocessing scripts
  docs/                      # Conceptual figures and methods notes
```

## How to Reproduce the Analysis

### From a fresh clone (full pipeline)

1. **Clone the repository**
   ```
   git clone https://github.com/lter/ssecr-c-cycling.git
   cd ssecr-c-cycling
   ```

2. **Install R dependencies**
   ```r
   install.packages(c("tidyverse", "patchwork", "scales", "jsonlite", "digest", "httr"))
   devtools::install_github("lter/ltertools")
   install.packages("EDIutils")  # For EDI data downloads
   ```

3. **Run the full pipeline**
   ```r
   # Step 1: Download raw data from EDI and preprocess into ready/ CSVs
   source("analysis/00_download-and-preprocess.R")

   # Step 2: Validate all data files, manifests, and registry
   source("analysis/00_validate-data.R")

   # Step 3-8: Run analysis
   source("analysis/01_harmonize.R")
   source("analysis/02_relative-response.R")
   source("analysis/03_trend-classification.R")
   source("analysis/04_detection-time.R")
   source("analysis/05_lrr-analysis.R")
   source("analysis/06_summary-stats.R")
   ```

### If `data/ready/` files already exist

Skip step 1 and start from `00_validate-data.R`. The ready/ CSVs are checked into the repo.

All scripts use relative paths from the repo root directory.

## Data Dictionary (Harmonized Dataset)

The harmonized dataset (`data/harmonized/harmonized_current.csv`) contains:

| Column | Description |
|--------|-------------|
| `source` | Original processed CSV filename |
| `Date` | Observation date (typically year) |
| `Treatment` | Experimental treatment identifier |
| `Replicate` | Replicate/plot identifier |
| `Response.Variable` | Measured response value (biomass, flux, cover, etc.) |
| `site_abbr` | LTER site abbreviation (KNZ, HBR, etc.) |
| `site_type` | Ecosystem type (Grassland, Forest, Coastal, Tundra, Urban, Freshwater) |
| `experiment_type` | Type of manipulation (Fertilization, Warming, etc.) |
| `stock_or_flux` | Whether response is a Stock, Flux, or Proxy (indirect C measurement) |
| `is_case_study` | TRUE for the 7 focal case-study sites |

## Case-Study Sites

| Abbreviation | Full Name | Ecosystem | Years |
|---|---|---|---|
| KNZ | Konza Prairie | Grassland | 1986-2021 |
| HBR | Hubbard Brook | Forest | 2009-2022 |
| KBS | Kellogg Biological Station | Grassland | 1990-2022 |
| SBC | Santa Barbara Coastal | Coastal | 2008-2024 |
| HFR | Harvard Forest | Forest | 1991-2021 |
| BNZ | Bonanza Creek | Tundra | 2009-2021 |
| CAP | Central Arizona-Phoenix | Urban | 2006-2024 |

## Dataset Provenance

Every dataset is traceable from its EDI source through preprocessing to the harmonized output. The authoritative registry is `data/dataset_registry.csv`.

| Site | Dataset | EDI Package | Response | Type | Years |
|------|---------|-------------|----------|------|-------|
| AND | Tree DBH (WS06/07/08) | knb-lter-and.2742.28 | DBH (cm) | Stock | 2002-2015 |
| ARC | Tussock tundra biomass | knb-lter-arc.10004.8 | Biomass (g/m2) | Stock | 1982-2015 |
| BNZ | CiPEHR NEE | knb-lter-bnz.481.23 | NEE | Flux | 2009-2021 |
| CAP | Desert fertilization biomass | knb-lter-cap.632.17 | Biomass (g/m2) | Stock | 2006-2024 |
| CDR | BioCON biomass | knb-lter-cdr.302.13 | Biomass (g/m2) | Stock | 1998-2021 |
| GCE | Vegetation recovery cover* | DOI: 10.6073/pasta/6df40a... | Vegetation cover (%) | Proxy | multi-year |
| HBR | MELNHE litterfall | knb-lter-hbr.404.1 | Mass (g/m2) | Flux | 2009-2022 |
| HFR | Soil warming respiration | knb-lter-hfr.5.37 | soil_res | Flux | 1991-2021 |
| KBS | MCSE NPP | knb-lter-kbs.19.85 | Biomass (g/m2) | Stock | 1990-2022 |
| KNZ | Belowground plot biomass | knb-lter-knz.57.15 | Biomass (g/m2) | Stock | 1986-2021 |
| LUQ | CTE soil GHG fluxes | knb-lter-luq.164.678951 | CO2 flux | Flux | 2003-2010 |
| MCM | Stoichiometry CO2 flux | knb-lter-mcm.4014.5 | CO2 flux (µmol/m²/s) | Flux | 2003-2010 |
| NTL | Cascade bloom chlorophyll* | knb-lter-ntl.413.2 | Chlorophyll (µg/L) | Proxy | 2011-2019 |
| NWT | 3-factor ANPP | knb-lter-nwt.13.7 | mass (g/m2) | Stock | 2006-2019 |
| PIE | TIDE shoot mass | knb-lter-pie.202.5 | shoot_mass | Stock | 2004-2020 |
| SBC | Kelp removal biomass | knb-lter-sbc.119 | DRY_GM2 | Stock | 2008-2024 |
| SEV | NFert biomass | knb-lter-sev.186.208431 | Biomass (g/m2) | Stock | 2004-2023 |
| VCR | 1st inundation experiment | knb-lter-vcr.168.24 | totalMass | Stock | 1994-2014 |

*GCE and NTL are carbon proxies: vegetation percent cover (GCE) and chlorophyll (NTL) are indirect measurements of plant/algal carbon, not direct carbon stocks or fluxes.

## Excluded Datasets

The following datasets were excluded or replaced:

| Site | Dataset | Reason |
|------|---------|--------|
| AND | TP114 Entity 6 DBA (original) | DBA/DBH incompatibility; fixed by using TV010 DBH for all watersheds |
| MCM | Stoichiometry biota (4013.6) | Invertebrate abundance, not carbon; replaced by CO2 flux (4014.5) |
| CDR | sIDE percent cover | Percent cover, not carbon |
| CDR | tIDE percent cover | Percent cover, not carbon |
| CDR | Soil %C E002 (knb-lter-cdr.449.9) | One dataset per site; BioCON retained |
| CDR | sIDE biomass (knb-lter-cdr.707.2) | One dataset per site; BioCON retained |
| CDR | Small Biodiversity biomass (knb-lter-cdr.291.8) | One dataset per site; BioCON retained |
| CDR | Soil %C E001 (knb-lter-cdr.472.8) | One dataset per site; BioCON retained |
| CDR | Soil carbon flux E004 (knb-lter-cdr.590.8) | One dataset per site; BioCON retained |
| VCR | 2nd inundation experiment (knb-lter-vcr.169.24) | One dataset per site; 1st inundation retained (longer record) |

## Pipeline Architecture

```
EDI Repository                  dataset_registry.csv         data/ready/
  ┌──────────┐    download.R    ┌──────────────────┐         ┌─────────┐
  │ EDI pkg  │──────────────────│ registry entry   │────────▶│ CSV     │
  └──────────┘    ▼             └──────────────────┘  prepr. └─────────┘
              data/raw/              ▲                    │        │
              + manifest.json        │                    │        ▼
                                     │              preprocess_   column_key.csv
                              preprocess_runner.R   {site}.R     + ltertools::harmonize()
                                                                       │
                                                                       ▼
                                                              data/harmonized/
```

Each download generates a JSON manifest in `data/manifests/` recording the EDI package ID, download timestamp, and MD5 checksum. The validation script (`00_validate-data.R`) verifies manifest integrity.

## Contributing Guidelines & Style Guide

See [CONTRIBUTING.md](https://github.com/lter/ssecr-c-cycling/blob/main/CONTRIBUTING.md)
