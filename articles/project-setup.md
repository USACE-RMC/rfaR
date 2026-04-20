# Setting Up Your rfaR Project

## Setting Up Your rfaR Project

We recommend organizing your analysis as an RStudio project. This keeps
file paths relative and reproducible across machines.

### Step 1 — Create a new RStudio Project

In RStudio: **File → New Project → New Directory → New Project**

Name it something like `my_dam_rfa/`. This creates a `.Rproj` file and
sets the working directory automatically on open.

### Step 2 — Recommended directory structure

    my_dam_rfa/
    ├── my_dam_rfa.Rproj
    ├── data/
    │   ├── resmodel.csv
    │   ├── hydrographs/
    │   │   ├── event_1.csv
    │   │   └── event_2.csv
    │   └── stage_record.csv
    ├── R/
    │   └── my_dam_simulate.R
    └── output/

### Step 3 — Import your data

`rfaR` expects data in specific formats. The built-in datasets show the
required structure:

``` r
# Reservoir model: elevation, storage, outflow
head(jmd_resmodel)

# Hydrograph: time and flow columns
head(jmd_hydro_apr1999)

# Stage record: date and stage columns  
head(jmd_wy1980_stage)
```

Read your own data using
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) with relative
paths:

``` r
resmodel  <- read.csv("data/resmodel.csv")
stage_rec <- read.csv("data/stage_record.csv")
```

### Step 4 — Verify column structure

Before running
[`rfa_simulate()`](https://usace-rmc.github.io/rfaR/reference/rfa_simulate.md),
verify your imported data matches the expected format:

``` r
# Check against built-in example
str(jmd_resmodel)
str(resmodel)  # should match
```
