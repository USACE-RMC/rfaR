# Mod-Puls Routing Routine for rfaR
# This will use the Cherry Creek Example from the excel sheet as the example
# Secondary Example Could be JMD
# Validate against HMS Results
#
# DM - 12/11/2025

# Package Management -----------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# If these packages aren't installed, pacman automatically installs them from CRAN before loading them.
# https://www.r-bloggers.com/2016/04/let-pacman-eat-up-library-and-require-2/

pacman::p_load(tidyverse, scales,
               patchwork, fs, ggsci,
               lubridate, zoo, janitor, gt)

devtools::load_all()

theme_set(theme_bw())

# Load Data --------------------------------------------------------------------
inflow <- dir_ls("data/mod_puls/",glob = "*cherry_creek_inflow.csv",recurse = T) %>%
  read_csv()

res_model <- dir_ls("data/mod_puls/",glob = "*cherry_creek_stage_stor_dis.csv", recurse = T) %>%
  read_csv()

hms_results <- dir_ls("data/mod_puls/",glob = "*cherry_creek_hms_results.csv",recurse = T) %>%
  read_csv()

# Use the function -------------------------------------------------------------
cherry_creek_simple <- mod_puls_routing(res_model, inflow, 5565)
cherry_creek_full <- mod_puls_routing(res_model, inflow, 5565, full_results = TRUE)

# =============================================================================
# COMPARE RESULTS WITH HMS
# =============================================================================

routing_results <- bind_rows(hms_results |> mutate(Source = "HMS"),
                             cherry_creek_full |> select(-inflow_cfs) |> mutate(Source = "rfaR")) |>
  mutate(time_hour = ifelse(is.na(time_hour),time_hr,time_hour),
         elevation_ft = ifelse(is.na(elevation_ft),elev_ft,elevation_ft)) |>
  select(c(time_hour,elevation_ft,storage_acft,outflow_cfs, Source))

colors_i_like <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF","#5F559BFF","#A20056FF","#808180FF","#1B1919FF")

ggplot(routing_results) +
  geom_line(aes(x = time_hour,
                y = outflow_cfs,
                color = Source,
                linetype = Source),
            linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,960,24)) +
  scale_color_manual(values = c("HMS" = "#008B45FF",
                                "rfaR" = "#3B4992FF"),
                     breaks = c("HMS", "rfaR"))+
  coord_cartesian(xlim = c(0,144))

ggplot(routing_results) +
  geom_line(aes(x = time_hour,
                y = elevation_ft,
                color = Source,
                linetype = Source),
            linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,960,24)) +
  scale_color_manual(values = c("HMS" = "#008B45FF",
                                "rfaR" = "#3B4992FF"),
                     breaks = c("HMS", "rfaR"))+
  coord_cartesian(xlim = c(0,144))

# =============================================================================
# COMPUTE SUMMARY STATS
# =============================================================================
metrics <- routing_results |>
  pivot_wider(
    names_from = Source,
    values_from = c(elevation_ft, storage_acft, outflow_cfs)
  )|>
  summarise(
    # Elevation metrics
    elev_SSE = sum((elevation_ft_HMS - elevation_ft_rfaR)^2, na.rm = TRUE),
    elev_RMSE = sqrt(mean((elevation_ft_HMS - elevation_ft_rfaR)^2, na.rm = TRUE)),
    elev_MAE = mean(abs(elevation_ft_HMS - elevation_ft_rfaR), na.rm = TRUE),

    # Storage metrics
    storage_SSE = sum((storage_acft_HMS - storage_acft_rfaR)^2, na.rm = TRUE),
    storage_RMSE = sqrt(mean((storage_acft_HMS - storage_acft_rfaR)^2, na.rm = TRUE)),
    storage_MAE = mean(abs(storage_acft_HMS - storage_acft_rfaR), na.rm = TRUE),

    # Outflow metrics
    outflow_SSE = sum((outflow_cfs_HMS - outflow_cfs_rfaR)^2, na.rm = TRUE),
    outflow_RMSE = sqrt(mean((outflow_cfs_HMS - outflow_cfs_rfaR)^2, na.rm = TRUE)),
    outflow_MAE = mean(abs(outflow_cfs_HMS - outflow_cfs_rfaR), na.rm = TRUE)
  )


# =============================================================================
# JMD
# =============================================================================

jmd_routing <- mod_puls_routing(resmodel_df = jmd_resmodel,
                                         inflow_df = jmd_inflowhydro,
                                         initial_elev = jmd_init_elev,
                                         full_results = TRUE)

# Add Source Field
jmd_routing$source <- "rfaR"
jmd_hms <- jmd_hms_results
jmd_hms$source <- "HMS"

# Bind Rows
jmd_comp_df_long <- rbind(jmd_routing,jmd_hms)

ggplot(jmd_comp_df_long) +
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
       title = "Cherry Creek Validation Routing",
       color = NULL) +
  coord_cartesian(xlim = c(0,144))


jmd_comp_df_wide <- pivot_wider(jmd_comp_df_long,
                               names_from = source,
                               values_from = c(inflow_cfs, elevation_ft, storage_acft, outflow_cfs))

jmd_comp_df_wide |>
  mutate(delta_elev = elevation_ft_rfaR - elevation_ft_HMS,
         delta_outflow = outflow_cfs_rfaR - outflow_cfs_HMS,
         pdiff_elev = ((delta_elev)/elevation_ft_HMS)*100,
         pdiff_outflow = ((delta_outflow)/outflow_cfs_HMS)*100) |>
  summarize(mean_pdiff_elev = mean(pdiff_elev),
            mean_pdiff_outflow = mean(pdiff_outflow),
            sd_pdiff_elev = sd(pdiff_elev),
            sd_pdiff_outflow = sd(pdiff_outflow))

jmd_comp_df_wide <- jmd_comp_df_wide |>
  mutate(delta_elev = elevation_ft_rfaR - elevation_ft_HMS,
         delta_outflow = outflow_cfs_rfaR - outflow_cfs_HMS,
         pdiff_elev = ((delta_elev)/elevation_ft_HMS)*100,
         pdiff_outflow = ((delta_outflow)/outflow_cfs_HMS)*100)
