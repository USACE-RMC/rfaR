## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# call tidy 
library(tidyverse)
library(gt)
theme_set(theme_bw())

## ----validation metrics, echo = F, warning = F--------------------------------
# Define thresholds and create metrics table
validation_metrics <- tibble(
  Metric = c("Mean % Difference", "RMSE", "Nash-Sutcliffe Efficiency (NSE)"),
  Excellent = c("< 0.1%", "< 1.0", "> 0.95"),
  Good = c("0.1% - 1%", "1.0 - 5.0", "0.90 - 0.95"),
  Acceptable = c("1% - 5%", "5.0 - 10.0", "0.75 - 0.90"),
  Poor = c("> 5%", "> 10.0", "< 0.75")
)

# Create gt table
metrics_table <- validation_metrics |> 
  gt() |> 
  # Header
  tab_header(
    title = "Validation Metrics and Performance Criteria",
    subtitle = "Thresholds for Assessing Modified Puls Routing Accuracy"
  ) |> 
  # Column labels
  cols_label(
    Metric = "Performance Metric",
    Excellent = "Excellent",
    Good = "Good",
    Acceptable = "Acceptable",
    Poor = "Poor"
  ) |> 
  # Color coding - Excellent (green)
  tab_style(
    style = list(
      cell_fill(color = "#2E7D32"),  # Dark green
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Excellent)
  ) |> 
  # Color coding - Good (light green/yellow-green)
  tab_style(
    style = list(
      cell_fill(color = "#7CB342"),  # Yellow-green
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Good)
  ) |> 
  # Color coding - Acceptable (orange)
  tab_style(
    style = list(
      cell_fill(color = "#F57C00"),  # Orange
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Acceptable)
  ) |> 
  # Color coding - Poor (red)
  tab_style(
    style = list(
      cell_fill(color = "#C62828"),  # Dark red
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Poor)
  ) |> 
  # Bold metric names
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Metric)
  ) |> 
  # Center align all columns except Metric
  cols_align(
    align = "center",
    columns = c(Excellent, Good, Acceptable, Poor)
  ) |> 
  cols_align(
    align = "left",
    columns = Metric
  ) |> 
  # Table styling
  tab_options(
    table.font.size = px(11),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#4a5f4a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#4a5f4a",
    heading.background.color = "#4a5f4a",
    heading.align = "left",
    column_labels.background.color = "#e8e8e8",
    data_row.padding = px(8),
    column_labels.padding = px(8)
  )

metrics_table

## ----setup, warning = F-------------------------------------------------------
#library(rfaR)
devtools::load_all()

## ----cherry creek routing-----------------------------------------------------
cherry_creek_routing <- mod_puls_routing(resmodel_df = cc_resmodel, 
                                      inflow_df = cc_inflowhydro, 
                                      initial_elev = cc_init_elev, 
                                      full_results = TRUE)

## ----CC percent difference, echo = F, warning = F, message = F----------------
# Add Source Field
cherry_creek_routing$source <- "rfaR"
cc_hms <- cc_hms_results
cc_hms$source <- "HMS"

# Bind Rows
cc_comp_df_long <- rbind(cherry_creek_routing,cc_hms)

# Pivot Wider for Validation
cc_comp_df_wide <- pivot_wider(cc_comp_df_long, 
            names_from = source, 
            values_from = c(inflow_cfs, elevation_ft, storage_acft, outflow_cfs))

cc_perdiff <- cc_comp_df_wide |> 
  mutate(delta_elev = elevation_ft_rfaR - elevation_ft_HMS,
         delta_outflow = outflow_cfs_rfaR - outflow_cfs_HMS,
         pdiff_elev = ((delta_elev)/elevation_ft_HMS)*100,
         pdiff_outflow = ((delta_outflow)/outflow_cfs_HMS)*100) |> 
  summarize(mean_pdiff_elev = mean(pdiff_elev),
            mean_pdiff_outflow = mean(pdiff_outflow),
            sd_pdiff_elev = sd(pdiff_elev),
            sd_pdiff_outflow = sd(pdiff_outflow)) |> 
  gt() |> 
  # Header
  tab_header(
    title = "Cherry Creek Dam Mod-Puls Routing Results",
    subtitle = "Summary of Percent Difference between rfaR and HMS Routing"
  ) |>
  # Columns
  tab_spanner(
    label = "Elevation (ft-NAVD88)",
    columns = c(mean_pdiff_elev, sd_pdiff_elev)
  ) |> 
  tab_spanner(
    label = "Outflow (cfs)",
    columns = c(mean_pdiff_outflow, sd_pdiff_outflow)
  ) |> 
  cols_label(
    mean_pdiff_elev = "Mean %Diff",
    sd_pdiff_elev = "SD %Diff",
    mean_pdiff_outflow = "Mean %Diff",
    sd_pdiff_outflow = "SD %Diff"
  ) |> 
  # Format
  fmt_scientific(
    columns = c(mean_pdiff_elev,sd_pdiff_elev,mean_pdiff_outflow,sd_pdiff_outflow),
    decimals = 2,
    exp_style = "E"
  ) |> 
  # Alignment
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  # Make column headers bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(everything())
  ) |> 
  # Table styling
  tab_options(
    table.font.size = px(11),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#4a5f4a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#4a5f4a",
    heading.background.color = "#4a5f4a",
    heading.align = "left",
    column_labels.background.color = "#e8e8e8",
    data_row.padding = px(8),
    column_labels.padding = px(8)
  )

cc_perdiff  

## ----CC validation summary, echo = F, warning = F-----------------------------
# Peak Flow Comparison
peak_cc_rfaR <- max(cc_comp_df_wide$outflow_cfs_rfaR)
peak_cc_HMS <- max(cc_comp_df_wide$outflow_cfs_HMS)
peak_cc_diff_pct <- ((peak_cc_rfaR - peak_cc_HMS) / peak_cc_HMS) * 100

# Peak Elevation Comparison
peak_elev_cc_rfaR <- max(cc_comp_df_wide$elevation_ft_rfaR)
peak_elev_cc_HMS <- max(cc_comp_df_wide$elevation_ft_HMS)
peak_elev_cc_diff_pct <- ((peak_elev_cc_rfaR - peak_elev_cc_HMS) / peak_elev_cc_HMS) * 100

# Root Mean Square Error (RMSE)
rmse_cc_outflow <- sqrt(mean((cc_comp_df_wide$outflow_cfs_rfaR - cc_comp_df_wide$outflow_cfs_HMS)^2))
rmse_cc_elevation <- sqrt(mean((cc_comp_df_wide$elevation_ft_rfaR - cc_comp_df_wide$elevation_ft_HMS)^2))

# Nash-Sutcliffe Efficiency (NSE)
nse_cc_outflow <- 1 - sum((cc_comp_df_wide$outflow_cfs_HMS - cc_comp_df_wide$outflow_cfs_rfaR)^2) / 
                   sum((cc_comp_df_wide$outflow_cfs_HMS - mean(cc_comp_df_wide$outflow_cfs_HMS))^2)

# Maximum Absolute Difference
max_abs_diff_cc_outflow <- max(abs(cc_comp_df_wide$outflow_cfs_rfaR - cc_comp_df_wide$outflow_cfs_HMS))
max_abs_diff_cc_elev <- max(abs(cc_comp_df_wide$elevation_ft_rfaR - cc_comp_df_wide$elevation_ft_HMS))


cc_validation_summary <- data.frame(
  Metric = c("Peak Outflow (cfs)", 
             "Peak Elevation (ft)", 
             "RMSE Outflow (cfs)", 
             "RMSE Elevation (ft)",
             "NSE Outflow",
             "Max Abs Diff Outflow (cfs)",
             "Max Abs Diff Elevation (ft)"),
  rfaR = c(peak_cc_rfaR, peak_elev_cc_rfaR, rmse_cc_outflow, rmse_cc_elevation, 
           nse_cc_outflow, max_abs_diff_cc_outflow, max_abs_diff_cc_elev),
  HMS = c(peak_cc_HMS, peak_elev_cc_HMS, NA, NA, NA, NA, NA),
  Difference = c(peak_cc_rfaR - peak_cc_HMS, peak_elev_cc_rfaR - peak_elev_cc_HMS,
                 NA, NA, NA, NA, NA),
  Pct_Diff = c(peak_cc_diff_pct, peak_elev_cc_diff_pct, NA, NA, NA, NA, NA)) |> 
  gt() |> 
  # Header
  tab_header(
    title = "Summary of Validation Metrics for Cherry Creek Dam Routing",
    subtitle = "Validation of rfaR Mod-Puls Routing"
  ) |>
  # Columns
  cols_label(
    Metric = "Metric",
    rfaR = "rfaR",
    HMS = "HMS",
    Difference  = "Difference",
    Pct_Diff = "%Diff"
  ) |> 
  # Format
  fmt_scientific(
    columns = c(Difference,Pct_Diff),
    decimals = 2,
    exp_style = "E"
  ) |> 
  fmt_number(
    columns = c(rfaR,HMS),
    decimals = 2,
    use_seps = T
  ) |> 
  # Alignment
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  # Make column headers bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |> 
  # Table styling
  tab_options(
    table.font.size = px(11),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#4a5f4a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#4a5f4a",
    heading.background.color = "#4a5f4a",
    heading.align = "left",
    column_labels.background.color = "#e8e8e8",
    data_row.padding = px(8),
    column_labels.padding = px(8)
  )

cc_validation_summary

## ----CC Results Figure, echo = F, warning = F, fig.cap = "Comparison of inflow and outflow hydrographs between rfaR and HEC-HMS Modified Puls routing for Cherry Creek Dam validation.", fig.width = 8, fig.height = 5----
cc_routing <- ggplot(cc_comp_df_long) + 
  geom_line(aes(x = time_hr,
                y = inflow_cfs,
                color = "Inflow"),
            linewidth = 0.75) +
    geom_line(aes(x = time_hr,
                y = outflow_cfs,
                color = "Outflow"),
            linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,960,24)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Inflow" = "#3B4992FF",
                                "Outflow" = "#EE0000FF"),
                     breaks = c("Inflow", "Outflow"))+
  facet_wrap(vars(source)) +
  theme(legend.position = "bottom") +
  labs(x = "Time (hr)", 
       y = "Flow (cfs)",
       title = "Cherry Creek Dam - Validation Routing", 
       color = NULL) +
  coord_cartesian(xlim = c(0,144))

## ----JMD routing--------------------------------------------------------------
jmd_routing <- mod_puls_routing(resmodel_df = jmd_resmodel, 
                                      inflow_df = jmd_inflowhydro, 
                                      initial_elev = jmd_init_elev, 
                                      full_results = TRUE)

## ----JMD percent diff, echo = F, warning = F----------------------------------
# Add Source Field
jmd_routing$source <- "rfaR"
jmd_hms <- jmd_hms_results
jmd_hms$source <- "HMS"

# Bind Rows
jmd_comp_df_long <- rbind(jmd_routing,jmd_hms)

# Pivot Wider for Validation
jmd_comp_df_wide <- pivot_wider(jmd_comp_df_long, 
            names_from = source, 
            values_from = c(inflow_cfs, elevation_ft, storage_acft, outflow_cfs))

jmd_validation <- jmd_comp_df_wide |> 
  mutate(delta_elev = elevation_ft_rfaR - elevation_ft_HMS,
         delta_outflow = outflow_cfs_rfaR - outflow_cfs_HMS,
         pdiff_elev = ((delta_elev)/elevation_ft_HMS)*100,
         pdiff_outflow = ((delta_outflow)/outflow_cfs_HMS)*100) |> 
  summarize(mean_pdiff_elev = mean(pdiff_elev,na.rm = T),
            mean_pdiff_outflow = mean(pdiff_outflow,na.rm = T),
            sd_pdiff_elev = sd(pdiff_elev,na.rm = T),
            sd_pdiff_outflow = sd(pdiff_outflow,na.rm = T)) |> 
  gt() |> 
  # Header
  tab_header(
    title = "John Martin Dam Mod-Puls Routing Results",
    subtitle = "Summary of Percent Difference between rfaR and HMS Routing"
  ) |>
  # Columns
  tab_spanner(
    label = "Elevation (ft-NAVD88)",
    columns = c(mean_pdiff_elev, sd_pdiff_elev)
  ) |> 
  tab_spanner(
    label = "Outflow (cfs)",
    columns = c(mean_pdiff_outflow, sd_pdiff_outflow)
  ) |> 
  cols_label(
    mean_pdiff_elev = "Mean %Diff",
    sd_pdiff_elev = "SD %Diff",
    mean_pdiff_outflow = "Mean %Diff",
    sd_pdiff_outflow = "SD %Diff"
  ) |> 
  # Format
  fmt_scientific(
    columns = c(mean_pdiff_elev,sd_pdiff_elev,mean_pdiff_outflow,sd_pdiff_outflow),
    decimals = 2,
    exp_style = "E"
  ) |> 
  # Alignment
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  # Make column headers bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners(everything())
  ) |> 
  # Table styling
  tab_options(
    table.font.size = px(11),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#4a5f4a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#4a5f4a",
    heading.background.color = "#4a5f4a",
    heading.align = "left",
    column_labels.background.color = "#e8e8e8",
    data_row.padding = px(8),
    column_labels.padding = px(8)
  )

jmd_validation  

## ----JMD validation metrics, echo = F, warning = F----------------------------
# Peak Flow Comparison
peak_jmd_rfaR <- max(jmd_comp_df_wide$outflow_cfs_rfaR)
peak_jmd_HMS <- max(jmd_comp_df_wide$outflow_cfs_HMS)
peak_jmd_diff_pct <- ((peak_jmd_rfaR - peak_jmd_HMS) / peak_jmd_HMS) * 100

# Peak Elevation Comparison
peak_elev_jmd_rfaR <- max(jmd_comp_df_wide$elevation_ft_rfaR)
peak_elev_jmd_HMS <- max(jmd_comp_df_wide$elevation_ft_HMS)
peak_elev_jmd_diff_pct <- ((peak_elev_jmd_rfaR - peak_elev_jmd_HMS) / peak_elev_jmd_HMS) * 100

# Root Mean Square Error (RMSE)
rmse_jmd_outflow <- sqrt(mean((jmd_comp_df_wide$outflow_cfs_rfaR - jmd_comp_df_wide$outflow_cfs_HMS)^2))
rmse_jmd_elevation <- sqrt(mean((jmd_comp_df_wide$elevation_ft_rfaR - jmd_comp_df_wide$elevation_ft_HMS)^2))

# Nash-Sutcliffe Efficiency (NSE)
nse_jmd_outflow <- 1 - sum((jmd_comp_df_wide$outflow_cfs_HMS - jmd_comp_df_wide$outflow_cfs_rfaR)^2) / 
                   sum((jmd_comp_df_wide$outflow_cfs_HMS - mean(jmd_comp_df_wide$outflow_cfs_HMS))^2)

# Maximum Absolute Difference
max_abs_diff_jmd_outflow <- max(abs(jmd_comp_df_wide$outflow_cfs_rfaR - jmd_comp_df_wide$outflow_cfs_HMS))
max_abs_diff_jmd_elev <- max(abs(jmd_comp_df_wide$elevation_ft_rfaR - jmd_comp_df_wide$elevation_ft_HMS))


jmd_validation_summary <- data.frame(
  Metric = c("Peak Outflow (cfs)", 
             "Peak Elevation (ft)", 
             "RMSE Outflow (cfs)", 
             "RMSE Elevation (ft)",
             "NSE Outflow",
             "Max Abs Diff Outflow (cfs)",
             "Max Abs Diff Elevation (ft)"),
  rfaR = c(peak_jmd_rfaR, peak_elev_jmd_rfaR, rmse_jmd_outflow, rmse_jmd_elevation, 
           nse_jmd_outflow, max_abs_diff_jmd_outflow, max_abs_diff_jmd_elev),
  HMS = c(peak_jmd_HMS, peak_elev_cc_HMS, NA, NA, NA, NA, NA),
  Difference = c(peak_jmd_rfaR - peak_jmd_HMS, peak_elev_jmd_rfaR - peak_elev_jmd_HMS,
                 NA, NA, NA, NA, NA),
  Pct_Diff = c(peak_jmd_diff_pct, peak_elev_jmd_diff_pct, NA, NA, NA, NA, NA)) |> 
  gt() |> 
  # Header
  tab_header(
    title = "Summary of Validation Metrics for John Martin Dam Routing",
    subtitle = "Validation of rfaR Mod-Puls Routing"
  ) |>
  # Columns
  cols_label(
    Metric = "Metric",
    rfaR = "rfaR",
    HMS = "HMS",
    Difference  = "Difference",
    Pct_Diff = "%Diff"
  ) |> 
  # Format
  fmt_scientific(
    columns = c(Difference,Pct_Diff),
    decimals = 2,
    exp_style = "E"
  ) |> 
  fmt_number(
    columns = c(rfaR,HMS),
    decimals = 2,
    use_seps = T
  ) |> 
  # Alignment
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  # Make column headers bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |> 
  # Table styling
  tab_options(
    table.font.size = px(11),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#4a5f4a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#4a5f4a",
    heading.background.color = "#4a5f4a",
    heading.align = "left",
    column_labels.background.color = "#e8e8e8",
    data_row.padding = px(8),
    column_labels.padding = px(8)
  )

jmd_validation_summary

## ----JMD Results Figure, echo = F, warning = F, fig.cap = "Comparison of inflow and outflow hydrographs between rfaR and HEC-HMS Modified Puls routing for John Martin Dam validation.", fig.width = 8, fig.height = 5----
jmd_routing <- ggplot(jmd_comp_df_long) + 
  geom_line(aes(x = time_hr,
                y = inflow_cfs,
                color = "Inflow"),
            linewidth = 0.75) +
    geom_line(aes(x = time_hr,
                y = outflow_cfs,
                color = "Outflow"),
            linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,960,24)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Inflow" = "#3B4992FF",
                                "Outflow" = "#EE0000FF"),
                     breaks = c("Inflow", "Outflow"))+
  facet_wrap(vars(source)) +
  theme(legend.position = "bottom") +
  labs(x = "Time (hr)", 
       y = "Flow (cfs)",
       title = "John Martin Dam - Validation Routing", 
       color = NULL) +
  coord_cartesian(xlim = c(0,144))

