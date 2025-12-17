# rfaR

Reservoir Flood Analysis in R - tools for hydrologic routing and flood analysis.

## Installation
```r
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
