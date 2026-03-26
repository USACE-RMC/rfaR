---
title: "MCD GEV Example"
output:
  rmarkdown::html_document:
    theme: flatly
    toc: true
    toc_float: true
    toc_depth: 3
eval: false
---

# Summary 

This article steps through an example of `rfa_simulate` (expected only) using a GEV inflow volume-frequency parameter set. The example will load the required data and display the formatting currently required to successful execute `rfa_simulate`. The example project is the Marcus Camby Dam (MCD & `mcd_`).

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

# Analysis Set Up

The libraries loaded after `rfaR` are intended to make data analysis and visualization easier.

```{r setup, echo = FALSE, warning = FALSE, message = FALSE}
library(rfaR)
library(dplyr)
library(tidyr)
library(ggplot2)
```

All of the MCD data is stored in sub-directories within this parent directory. Setting up the parent directory as an R-project can also make searching for files/directories easier. An R-project will change the working directory to the specified project location. For example, setting `parent_dir` as the R-project directory would set the default working directory (likely on the C: drive) to `parent_dir` (ex. `getwd()` would return `"D:/0.RMC/Reefer/MC_Dam"`).

```{r mcd-directory,include = FALSE, warning = FALSE}
parent_dir <- "D:/0.RMC/Reefer/MC_Dam"
```

# Load Data

Currently, the preferred method of data entry for rfaR is copying data from RMC-RFA into CSVs and saving them to a parent/project directory. Future development will prioritize loading the data from RFA sqlite files and less-rigid formatting requirements. The subsections below provide an example of reading the data and the correct formatting. Data will be presented as the R-console output as opposed to a formatted table. Note that R-Studio refers to rows as `obs.` and columns as `variables`.

## BestFit Parameter Sets

The BestFit parameter sets should be 10,000 rows and 3 columns. Each row is a set of 3 parameters (for both GEV and LP3 parameter sets). Users can include a 4th column, log-likelihood, if desired. All of the data is numeric

```{r load-bestfit-params, warning = FALSE}
bestfit_file <- file.path(parent_dir,"bestfit","mcd_bestfit_params.csv")
mcd_gev_params <- read.csv(bestfit_file)

# Datatype (class) of each column
sapply(mcd_gev_params, class)

# Display the first 5 rows of the parameter sets
head(mcd_gev_params)
```

## Seasonality

`rfaR` does not have a seasonality analysis module at this time. The seasonality analysis from RMC-RFA should be copied as is from the software. The seasonality data should have 12 rows and 4 columns. The third column, `$relative_frequency` will be used in `rfa_simulate()` to sample the seasonality.

```{r load-seasonality, warning = FALSE}
szn_file <- file.path(parent_dir,"seasonality","mcd_seasonality.csv")
mcd_seasonality <- read.csv(szn_file)

# Datatype (class) of each column
sapply(mcd_seasonality, class)

# Display the seasonality data
print(mcd_seasonality)
```

## Stage Timeseries

`rfaR` modified the starting stage sampling method from RMC-RFA. Observed stages are sampled corresponding to the sampled month (seasonality). Therefore, columns of date and time are required in the stage timeseries data. Currently, `rfa_simulate()` can only handle date time data in separate columns, as **character** data. Future development will allow the user to specify if the date & time data is already a formatted as date (`as.Date()`) and/or POSIXct (`as.POSIXct()`) objects.

The empirical stage frequency from RMC-RFA has been loaded below. This data contains 2 columns of stage and corresponding plotting-position. 

```{r load-stageTS, warning = FALSE}
stage_ts_file <- file.path(parent_dir,"stage_data","mcd_stageTS.csv")

mcd_stage_TS <- read.csv(stage_ts_file)

# Class of each column
sapply(mcd_stage_TS, class)

# Empirical Stage Frequency
mcd_empirical_stage <- read.csv(file.path(parent_dir,"stage_data","mcd_empirical_stage.csv"))

# Display the stage time series
head(mcd_stage_TS)
```

## Reservoir Model

The reservoir model is formatted identically to RMC-RFA. The model should be three columns: stage, storage, and discharge and the stage must be monotonic.

```{r load-resmodel, warning = FALSE}
resmodel_file <- file.path(parent_dir,"res_model","mcd_resmodel.csv")
mcd_resmodel <- read.csv(resmodel_file)

# Selected Rows
mcd_resmodel[c(1,10,25,50,60),]
```

## Hydrograph Shapes

The hydrographs have been copied directly from RMC-RFA, including the **Ordinate** column. The 

```{r load-hydrographs, warning = FALSE}
hydro_1993 <- read.csv(file.path(parent_dir,"hydrographs","mcd_1993flood.csv"))
hydro_1995 <- read.csv(file.path(parent_dir,"hydrographs","mcd_1995flood.csv"))
hydro_2004 <- read.csv(file.path(parent_dir,"hydrographs","mcd_2004flood.csv"))
hydro_2010 <- read.csv(file.path(parent_dir,"hydrographs","mcd_2010flood.csv"))

# First two rows of each hydrograph dataframe
bind_rows(hydro_1993[c(1:2),],
          hydro_1995[c(1:2),],
          hydro_2004[c(1:2),],
          hydro_2004[c(1:2),])

```

# Set up Hydrograph Shapes

```{r setup-hydrographs, warning = FALSE}
# Check crit and routing dir
mcd_hydrographs <- hydrograph_setup(hydro_1993,
                                    hydro_1995,
                                    hydro_2004,
                                    hydro_2010,
                                    critical_duration = 1,
                                    routing_days = 5)

```

```{r plot-hydrographs, echo = FALSE, warning = FALSE}
# Add Peak Flow and Time
for (i in seq_along(mcd_hydrographs)) {
  mcd_hydrographs[[i]] <- mcd_hydrographs[[i]] |> 
  mutate(peak_flow = max(inflow)) |> 
  mutate(peak_flow_hour = hour[which.max(inflow)])
}

# Bind rows
hydros <- bind_rows(mcd_hydrographs)

hydros <- hydros |> 
  group_by(hydrograph_num) |> 
  mutate(hour_from_peak = hour - peak_flow_hour,
         scaled_from_peak = inflow/peak_flow) |> 
  ungroup() |>
  mutate(hydrograph_name =  case_when(
    hydrograph_num == 1 ~ "1993",
    hydrograph_num == 2 ~ "1995",
    hydrograph_num == 3 ~ "2004",
    hydrograph_num == 4 ~ "2010"
  ))

# Min/Max Time
max_hr <- ceiling(max(hydros$hour_from_peak)/12)*12
min_hr <- floor(min(hydros$hour_from_peak)/12)*12
seq(min_hr,max_hr,12)

# ggplot
ggplot(hydros) + 
  geom_line(aes(x = hour_from_peak, 
                y = scaled_from_peak, 
                color = hydrograph_name),
            linewidth = 0.85,
            alpha = 0.79) + 
  labs(x = "Time (hour)", 
       y = "Normalized Inflow (cfs)",
       color = NULL,
       title = "Input Hydrographs",
       subtitle = "Centered on Peak Inflow, Normalized to Individual Peak Flow") +
  scale_x_continuous(breaks = seq(min_hr,max_hr,12),
                     minor_breaks = seq(min_hr,max_hr,6)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) + 
  ggsci::scale_color_aaas() + 
  theme_bw()+
  theme(legend.position = "inside",
        legend.justification.inside = c(0.8,0.8),
        legend.text = element_text(size = 7, face = "italic"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.subtitle = element_text(size = 9, face = "italic"),
        plot.title = element_text(size = 11, face = "bold.italic")) + 
  coord_cartesian(xlim = c(-48,60))

```

# Expected-Only Stage-Frequency Analysis


```{r mcd-expectd-only, warning = FALSE, message = FALSE}
# Expected Only 
mcd_expected <- rfa_simulate(sim_type = "expected",
                             bestfit_params = mcd_gev_params,
                             dist = "GEV",
                             stage_ts = mcd_stage_TS,
                             seasonality = mcd_seasonality$relative_frequency,
                             hydrographs = mcd_hydrographs,
                             resmodel = mcd_resmodel,
                             Nbins = 50, 
                             events_per_bin = 200,
                             sim_name = "mcd",
                             results_dir = parent_dir)

```

```{r result-plot-setup, include = FALSE}
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

z_limit1 <- qnorm(1 - 9.9e-1)
z_limit2 <- qnorm(1 - 1e-7)

theme_stage_freq <- function(){
  theme_bw() +
    theme(legend.position = "inside",
          legend.justification.inside = c(0.95, 0.1),
          legend.text = element_text(size = 7, face = "italic"),
          legend.title = element_text(size = 8, face = "bold"),
          legend.background = element_rect(fill = "white", color = "black"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.subtitle = element_text(size = 9, face = "italic"),
          plot.title = element_text(size = 11, face = "bold.italic"))
}


```

# Display Results

The results have been plotted below on a standard `ggplot2` template. Note that the AEPs of both the results and emprical stage-frequency were converted to z-variates for plotting purposes.

```{r results-expected-plot, warning=FALSE, collapse = TRUE}
mcd_expected_df <- mcd_expected$stage_frequency |> 
  mutate(z_aep = qnorm(1-AEP))

mcd_empirical_stage <- mcd_empirical_stage |> 
  mutate(z_aep = qnorm(1-plot_posit))

ggplot() +
  geom_line(data = mcd_expected_df,
            aes(x = z_aep, y = Expected),
            color = "#3B4992FF",
            linewidth = 0.85) +
  # Observed Stage points
  geom_point(data = mcd_empirical_stage, 
             aes(x = z_aep, y = stage_ft, shape = "Obs. Stages"),
             size = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c("Obs. Stages" = 16)) +
  # Scales
  scale_x_continuous(breaks = z_breaks,
                     minor_breaks = z_breaks_minor,
                     labels = aep_breaks) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels = scales::label_comma()) +
  # Labels
  labs(x = "AEP",
       y = "Stage (ft-NAVD88)",
       title = paste("Marcus Camby Dam"),
       shape = NULL,
       color = NULL) +
  # Theme
  theme_stage_freq()


```
