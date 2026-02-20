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
  R/                       # Shared R utility functions
    utils_dates.R          # Date parsing (handles YYYY, YYYY-MM, MM/DD/YYYY, etc.)
    utils_labels.R         # Site metadata lookup and abbreviation mapping
    utils_analysis.R       # Core analysis: relative response, trend classification, LRR
    utils_plots.R          # Shared ggplot theme, trend color palette
  data/                    # All data files
    ready/                 # Processed CSVs ready for harmonization (one per experiment)
    harmonized/            # Harmonized output (single combined dataset)
    column_key.csv         # Maps raw column names to tidy names for harmonization
    site_metadata.csv      # Lookup: site abbreviations, ecosystem types, case-study flags
  analysis/                # Numbered analysis scripts (run in order)
    00_validate-data.R     # Check that all files and columns are present
    01_harmonize.R         # Combine all processed datasets into one harmonized file
    02_relative-response.R # Calculate treatment/control ratios; per-experiment plots
    03_trend-classification.R  # Classify trends (stable/variable/increasing/decreasing)
    04_detection-time.R    # How many timepoints to detect a significant trend
    05_lrr-analysis.R      # Log-response ratio: standard, early-vs-full, moving window
    06_summary-stats.R     # Summary tables, stat verification, manuscript outputs
  figures/                 # All generated figures
    supplemental/          # Supplemental tables and panel figures
  preprocessing/           # Site-specific data processing scripts
  docs/                    # Conceptual figures and methods notes
  defunct/                 # Archived old scripts and data (historical reference)
  Analysis/                # [LEGACY] Original analysis scripts
  Harmonizing/             # [LEGACY] Original harmonization files
  Ready_data/              # [LEGACY] Original processed data
  plots/                   # [LEGACY] Original generated plots
```

## How to Reproduce the Analysis

1. **Clone the repository**
   ```
   git clone https://github.com/lter/ssecr-c-cycling.git
   cd ssecr-c-cycling
   ```

2. **Install R dependencies**
   The project uses these key packages:
   - `tidyverse` (dplyr, ggplot2, tidyr, readr)
   - `patchwork` (plot composition)
   - `scales` (axis formatting)
   - `ltertools` (data harmonization; install via `devtools::install_github("lter/ltertools")`)
   - `SiZer`, `HERON` (SiZer analysis)

3. **Run the analysis pipeline in order:**
   ```r
   source("analysis/00_validate-data.R")  # Validate data files
   source("analysis/01_harmonize.R")      # Create harmonized dataset
   source("analysis/02_relative-response.R")  # Calculate relative responses
   source("analysis/03_trend-classification.R")  # Classify trends
   source("analysis/04_detection-time.R")  # Detection time analysis
   source("analysis/05_lrr-analysis.R")    # Log-response ratio analysis
   source("analysis/06_summary-stats.R")   # Summary statistics
   ```

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
| `stock_or_flux` | Whether response is a Stock, Flux, or Other |
| `is_case_study` | TRUE for the 8 focal case-study sites |

## Case-Study Sites

| Abbreviation | Full Name | Ecosystem | Years |
|---|---|---|---|
| KNZ | Konza Prairie | Grassland | 1986-2021 |
| HBR | Hubbard Brook | Forest | 2009-2022 |
| KBS | Kellogg Biological Station | Grassland | 1990-2022 |
| SBC | Santa Barbara Coastal | Coastal | 2008-2024 |
| GCE | Georgia Coastal Ecosystems | Coastal | 2010-2020 |
| HFR | Harvard Forest | Forest | 1991-2021 |
| BNZ | Bonanza Creek | Tundra | 2009-2021 |
| CAP | Central Arizona-Phoenix | Urban | 2006-2024 |

## CDR Dataset Note

Cedar Creek (CDR) currently has 8 datasets in the pipeline covering different experiments and response variables. A decision on which to retain for the final analysis is pending. All are included for now.

## Additional Datasets Available (Not Yet in Pipeline)

The following processed datasets in `data/ready/` are available but not yet in the column key:
- `LUQ_seedling_2003-2021_processed.csv` (seedling height/diameter)
- `PIE_NutrientAddition_PlantParameters_17Y_Multiple_processed.csv` (shoot height/mass)

Raw datasets on Google Drive that could be processed:
- GCE inundation (elevation and percent cover)
- NTL heat wave chlorophyll
- MCM CO2 fluxes
- SBC NPP measurements

## Contributing Guidelines & Style Guide

See [CONTRIBUTING.md](https://github.com/lter/ssecr-c-cycling/blob/main/CONTRIBUTING.md)
