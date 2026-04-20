# rfaR

> [!WARNING]
> This package is currently under active development. The API may change without notice.

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![R](https://img.shields.io/badge/R-%3E%3D4.1-blue)](https://cran.r-project.org/) [![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

An R implementation of the U.S. Army Corps of Engineers (USACE) Risk Management Center-Reservoir Frequency Analysis (RMC-RFA) methodology/software. This package produces reservoir stage-frequency curves with uncertainty bounds by combining deterministic flood routing (Modified Puls method) with a nested Monte Carlo framework — providing H&H engineers the tools needed for dam safety risk assessments in accordance with USACE/RMC guidance.

## What's New

See [`NEWS.md`](NEWS.md) for a history of user-facing changes across versions.

## Installation

### For users

``` r
# Install from GitHub using remotes
install.packages("remotes")
remotes::install_github("USACE-RMC/rfaR", build_vignettes = TRUE)

# Install from GitHub using devtools
install.packages("devtools")
devtools::install_github("USACE-RMC/rfaR")
```

### For developers and contributors

If you plan to modify the package source, clone the repository and use `devtools`:

``` r
# After cloning the repo to disk (e.g., via git clone or GitHub Desktop):
# - Open an R session in the repo root (or an RStudio project at the repo root)

# For active development — loads all functions into the session without installing
devtools::load_all()

# To install your local working copy into your R library
devtools::install()
```

> [!NOTE]
> Cloning the repository alone does not make `library(rfaR)` work — the package must be either installed (`devtools::install()` or `remotes::install_github()`) or loaded into the session via `devtools::load_all()`. A cloned repo on disk is source code, not an installed package.

## Example Data

The package includes example data from a hypothetical dam, John McGraw Dam (JMD, variables as "jmd_"):

#### Reservoir Model

-   `jmd_resmodel` - Elevation-storage-discharge relationship

#### Stage & Inflow Records

-   `jmd_wy1980_stage` - Daily reservoir stage (WY 1980–2024)
-   `jmd_por_inflow` - Daily inflow for the full period of record

#### Flood Frequency

-   `jmd_bf_parameter_sets` - 10,000 LP3 parameter sets from RMC-BestFit 2.0
-   `jmd_seasonality` - Flood seasonality analysis results

#### Inflow Hydrographs

-   `jmd_hydro_pmf` - Probable Maximum Flood (PMF)
-   `jmd_hydro_sdf` - Spillway Design Flood (SDF)
-   `jmd_hydro_jun1965` - June 1965 flood event
-   `jmd_hydro_jun1965_15min` - June 1965 flood event (15-minute intervals)
-   `jmd_hydro_may1955` - May 1955 flood event
-   `jmd_hydro_apr1999` - April 1999 flood event
-   `jmd_hydro_jun1921` - June 1921 flood event

#### Empirical & Benchmark Results

-   `jmd_empirical_stage_wy1980_pt` - Observed stage-frequency with perception threshold
-   `jmd_rfa_expected` - RMC-RFA expected stage-frequency curve
-   `jmd_rfa_full` - RMC-RFA full uncertainty benchmark curve

#### Methodology Illustration

-   `example_stratified` - Stratified sampling example across Uniform, Normal, and EV1 distributions

## Quick Start

``` r
# Load the package
library(rfaR)

# Example stage timeseries data 
head(jmd_wy1980_stage)

# Example BestFit LP3 parameter sets (10,000 sets)
head(jmd_bf_parameter_sets)

# Example Flood Seasonality (from RFA)
print(jmd_seasonality)

# Example Reservoir Model
head(jmd_resmodel)

# Hydrograph Setup
jmd_hydrographs <- hydrograph_setup(jmd_hydro_pmf,
                                    jmd_hydro_jun1965_15min,
                                    jmd_hydro_may1955,
                                    jmd_hydro_jun1965,
                                    jmd_hydro_apr1999,
                                    jmd_hydro_sdf,
                                    jmd_hydro_jun1921,
                                    critical_duration = 2,
                                    routing_days = 10)
                                
# Expected Only 
jmd_expected <- rfa_simulate(sim_type        = "expected",
                              bestfit_params = jmd_bf_parameter_sets,
                              stage_ts       = jmd_wy1980_stage,
                              seasonality    = jmd_seasonality$relative_frequency,
                              hydrographs    = jmd_hydrographs,
                              resmodel       = jmd_resmodel,
                              Nbins          = 50, 
                              events_per_bin = 200,
                              sim_name       = "jmd")

# Full Uncert
jmd_fulluncert <- rfa_simulate(sim_type       = "full",
                               bestfit_params = jmd_bf_parameter_sets,
                               stage_ts       = jmd_wy1980_stage,
                               seasonality    = jmd_seasonality$relative_frequency,
                               hydrographs    = jmd_hydrographs,
                               resmodel       = jmd_resmodel,
                               Nbins          = 50, 
                               events_per_bin = 200,
                               sim_name       = "jmd",
                               Ncores         = 26)
                              
```

## Quick Start Results

```r
print(jmd_expected$stage_frequency)

```

<details>

<summary>Plotting Results</summary>

``` r
# ggplot is contained in tidyverse
library(tidyverse)
# or
library(ggplot2)

# Load Empirical Stage-Frequency & Create Z-variate field
jmd_empirical_stage_wy1980_pt$Z_var <- qnorm(1 - jmd_empirical_stage_wy1980_pt$plot_posit)

# Create z-variate field for plotting
jmd_result <- jmd_expected$stage_frequency
head(jmd_result)

jmd_result$Z_var <- qnorm(1 - jmd_result$AEP)

# Tidyverse equivalent
#jmd_result <- jmd_expected$stage_frequency |> mutate(Z_var = qnorm(1 - AEP))

# Plot Setup
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

z_limit1 <- qnorm(1 - 9.9e-1)
z_limit2 <- qnorm(1 - 1e-8)

crit_elevs <- data.frame(
  Name = c("Upper PMF", "Top of Dam", "Flood Control Pool"),
  Elev = c(3893.8,3881.8,3871.8))

crit_elevs$label_text <- paste(crit_elevs$Name,"=",crit_elevs$Elev)

# RFA Result
jmd_rfa_expected$Z_var <- qnorm(1-jmd_rfa_expected$AEP)

# Plot Results
ggplot() +
  geom_line(data = jmd_rfa_expected,
            aes(x = Z_var, y = Expected, color = "RFA"),
            linewidth = 0.8) +
  geom_line(data = jmd_result,
            aes(x = Z_var, y = Expected, color = "rfaR"),
            linewidth = 0.8) +
  # Colors
  scale_color_manual(values = c("RFA" = "#EE0000FF","rfaR" = "#3B4992FF"))+
  geom_point(data = jmd_empirical_stage_wy1980_pt, aes(x = Z_var, y = stage_ft, shape = "Obs. Stages (WY 1980-2024)"),size = 1.5,alpha = 0.7) +
  scale_shape_manual(values = c("Obs. Stages (WY 1980-2024)" = 16)) +
  # Scales
  scale_x_continuous(breaks = z_breaks,
                     minor_breaks = z_breaks_minor,
                     labels = aep_breaks) +
  scale_y_continuous(breaks = seq(3700,3910,10),
                     minor_breaks = seq(3700,3910,5),
                     labels = scales::comma) +
  # Labels
  labs(x = "AEP",
       y = "Stage (ft-NAVD88)",
       title = paste("JMD - Expected Only"),
       shape = "Observations",
       color = NULL) +
  geom_hline(yintercept = crit_elevs$Elev) +
  annotate("text",x = qnorm(1 - 0.99), y = crit_elevs$Elev,
           label = crit_elevs$label_text,
           size = 2.5, hjust = 0, vjust = -.5) +
  # Theme
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(0.95, 0.1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7, face = "bold"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"))+
  coord_cartesian(xlim = c(z_limit1, z_limit2), ylim = c(3800,3900))
```

</details>

## License

`rfaR` is released under the BSD 3-Clause License with an additional USACE government disclaimer. The license is split across two files per R packaging convention:

- **`LICENSE`** — the minimal stub required by R, naming the copyright holder and year
- **`LICENSE.note`** — the full BSD 3-Clause text along with the USACE government notice, list of conditions, and disclaimer

See both files for the complete terms.

## References

#### RMC-RFA Methodology

-   Smith, C. H. (2020). *RMC-RFA User's Guide* (v1.1). U.S. Army Corps of Engineers, Risk Management Center, Lakewood, CO.

#### Flood Frequency Analysis

-   England, J. F., Cohn, T. A., Faber, B. A., Stedinger, J. R., Thomas, W. O., Veilleux, A. G., Kiang, J. E., & Mason, R. R. (2019). *Guidelines for Determining Flood Flow Frequency — Bulletin 17C* (Techniques and Methods 4–B5). U.S. Geological Survey. <https://doi.org/10.3133/tm4B5>

#### Flood Routing

-   U.S. Army Corps of Engineers, Hydrologic Engineering Center. (n.d.). Modified Puls Model. *HEC-HMS Technical Reference Manual*. <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/channel-flow/modified-puls-model>

#### Statistical Foundations

-   Chow, V. T. (1954). The log-probability law and its engineering applications. *Proceedings of the ASCE*, 80, 1–25.
-   Efron, B. (1979). Bootstrap methods: another look at the jackknife. *The Annals of Statistics*, 7, 1–26.
-   Efron, B., & Tibshirani, R. J. (1998). *An Introduction to the Bootstrap*. CRC Press.
-   Nathan, R., et al. (2016). Estimating the exceedance probability of extreme rainfalls up to the probable maximum precipitation. *Journal of Hydrology*.
-   Nathan, R., & Weinmann, E. (2013). *Monte Carlo Simulation Techniques*. Australian Rainfall and Runoff Discussion Paper. Engineers Australia.
-   Vose, D. (2008). *Risk Analysis: A Quantitative Guide*. John Wiley & Sons, West Sussex, England.

<details>

<summary>Development References</summary>

-   Wickham, H., & Bryan, J. (2023). *R Packages* (2nd ed.). O'Reilly. <https://r-pkgs.org/>
-   Wickham, H. (2019). *Advanced R* (2nd ed.). CRC Press. <https://adv-r.hadley.nz/>

</details>
