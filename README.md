# rfaR (Development)

**This package is currently in development**

An R implementation of the U.S. Army Corps of Engineers (USACE) Risk Management Center-Reservoir Frequency Analysis (RMC-RFA) methodology/software.
This package produces reservoir stage-frequency curves with uncertainty bounds by combining deterministic flood routing (Modified Puls method) with a nested Monte Carlo framework.

## Installation
```r
# Install dependencies first
install.packages(c("zoo", "lubridate", "evd", "cli"))

# Optional: for parallel processing
install.packages(c("future", "future.apply"))

# Install from GitHub
devtools::install_github("USACE-RMC/rfaR")
```

## Features
 
- **Modified Puls Routing**: Level pool reservoir routing using the storage-indication method

## Quick Start
```r
library(rfaR)

# Run Modified Puls routing with example data
results <- mod_puls_routing(
 cc_resmodel,
 cc_inflowhydro,
 initial_elev = cc_init_elev,
 full_results = TRUE
)

head(results)
```

## Example Data

The package includes example data from Cherry Creek Dam:

- `cc_resmodel` - Reservoir elevation-storage-discharge relationship
- `cc_inflowhydro` - Example inflow hydrograph
- `cc_init_elev` - Initial reservoir elevation
- `cc_hms_results` - HEC-HMS routing results for validation
