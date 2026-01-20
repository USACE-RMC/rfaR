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
# inflow <- dir_ls("data/mod_puls/",glob = "*cherry_creek_inflow.csv",recurse = T) %>%
#   read_csv()
cc_inflowhydro
# res_model <- dir_ls("data/mod_puls/",glob = "*cherry_creek_stage_stor_dis.csv", recurse = T) %>%
#   read_csv()
cc_resmodel
# hms_results <- dir_ls("data/mod_puls/",glob = "*cherry_creek_hms_results.csv",recurse = T) %>%
#   read_csv()
cc_hms_results

# Use the function -------------------------------------------------------------
cherry_creek_simple <- mod_puls_routing(cc_resmodel, cc_inflowhydro, 5565)
cherry_creek_full <- mod_puls_routing(cc_resmodel, cc_inflowhydro, 5565, full_results = TRUE)

# =============================================================================
# COMPARE RESULTS WITH HMS
# =============================================================================
stage_stor <- approxfun(x = cc_resmodel$stor_acft, y = cc_resmodel$elev_ft)

cherry_creek_full <- cherry_creek_full |> mutate(Stage_ft = stage_stor(storage_acft))

routing_results <- bind_rows(cc_hms_results |> mutate(Source = "HMS"),
                             cherry_creek_full |> select(-inflow_cfs) |> mutate(Source = "rfaR")) |>
  mutate(Stage_ft = stage_stor(storage_acft))

colors_i_like <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF","#5F559BFF","#A20056FF","#808180FF","#1B1919FF")

# "#FF9900"
# "#F28E2B"
# "#0072B2"
# "#0000FF"

cc_flow <- ggplot(cherry_creek_full) +
  geom_line(aes(x = time_hr,
                y = outflow_cfs,
                color = "Discharge (cfs)"),
            linewidth = 0.75) +
  geom_line(aes(x = time_hr,
                y = inflow_cfs,
                color = "Inflow (cfs)"),
            linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,960,12)) +
  scale_color_manual(values = c("Inflow (cfs)" = "#BB0021FF",
                                "Discharge (cfs)" = "#3B4992FF"),
                     breaks = c("Inflow (cfs)", "Discharge (cfs)")) +
  scale_y_continuous(breaks = seq(0,100000,10000),
                     minor_breaks = seq(0,100000,2500),
                     label = scales::comma) +
  coord_cartesian(xlim = c(0,96)) +
  labs(x = "Time (hours)",
       y = "Flow (cfs)",
       color = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85,0.8))

cc_stage <- ggplot(cherry_creek_full) +
  geom_line(aes(x = time_hr,
                y = Stage_ft,
                color = "Stage (ft-NAVD88)"),
            linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,960,12)) +
  scale_color_manual(values = c("Stage (ft-NAVD88)" = "#008B45FF"),
                     breaks = c("Stage (ft-NAVD88)")) +
  scale_y_continuous(breaks = seq(5550,5580,10),
                     minor_breaks = seq(5550,5580,2),
                     label = scales::comma) +
  coord_cartesian(xlim = c(0,96)) +
  labs(x = "Time (hours)",
       y = "Flow (cfs)",
       color = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85,0.25))

combo_routing_plot <- cc_flow / cc_stage
ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/cc_routing.png",combo_routing_plot, width = 7, height = 7, dpi = 400)

# COMP
ggplot(routing_results) +
  geom_line(aes(x = time_hr,
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

# JMD rfaR routing ===
# Flow and Stage
jmd_flow <- jmd_comp_df_long |>
  filter(source == "rfaR") |>
  ggplot() +
  geom_line(aes(x = time_hr,
                y = inflow_cfs,
                color = "Inflow"),
            linewidth = 0.75) +
  geom_line(aes(x = time_hr,
                y = outflow_cfs,
                color = "Outflow"),
            linewidth = 1) +
  scale_x_continuous(breaks = seq(0,960,24)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                     labels = scales::scientific) +
  scale_color_manual(values = c("Inflow" = "#3B4992FF",
                                "Outflow" = "#EE0000FF"),
                     breaks = c("Inflow", "Outflow"))+
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.8)) +
  labs(x = NULL,
       y = "Flow (cfs)",
       #title = "JMD Validation Routing",
       color = NULL) +
  coord_cartesian(xlim = c(0,144))

jmd_stage <- jmd_comp_df_long |>
  filter(source == "rfaR") |>
  ggplot() +
  geom_line(aes(x = time_hr,
                y = elevation_ft,
                color = "Stage"),
            linewidth = 1.25) +
  scale_x_continuous(breaks = seq(0,960,24)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                     labels = scales::comma) +
  scale_color_manual(values = c("Stage" = "#008B45FF"))+
  theme(legend.position = "none") +
  labs(x = "Time (hr)",
       y = "Stage (ft-NAVD88)",
       #title = "JMD Validation Routing",
       color = NULL) +
  coord_cartesian(xlim = c(0,144))

# Combined plot
jmd_comb_routing <- (jmd_flow/jmd_stage) +
  plot_annotation(title = "PMF Routing",
                  subtitle = "Example Data from JMD",
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/JMD_Routing.png",jmd_comb_routing, width = 8, height = 5, dpi = 400)

# Try to show 100 example routings
random_scales <- sample(runif(100, min = 0.1, max = 1.5), 100, replace = T)

routing_list <- list()
for (i in 1:length(random_scales)){
  # Scale
  scaled_hydro <- tibble(Time_hr = example_time,
                         Inflow = jmd_inflowhydro$inflow_cfs*random_scales[i])
  # Route
  scaled_routing <- mod_puls_routing(resmodel_df = jmd_resmodel,
                                    inflow_df = scaled_hydro,
                                    initial_elev = jmd_init_elev,
                                    full_results = TRUE)
  # Add ID
  scaled_routing$ID <- i

  # Save Results
  routing_list[[i]] <- scaled_routing
}

routing_df <- bind_rows(routing_list)

# max points
max_points <- routing_df |>
  group_by(ID) |>
  summarize(maxstage = max(elevation_ft),
            maxstage_time = routing_df$time_hr[which.max(elevation_ft)])

ggplot() +
  geom_line(data = jmd_routing,
            aes(x = time_hr,
                y = elevation_ft,
                color = "JMD PMF"),
            linewidth = 0.8,
            alpha = 0.8) +
  geom_line(data = routing_df,
            aes(x = time_hr,
                y = elevation_ft,
                group = ID,
                color = "Scaled Inflow Hydrograph"),
            linewidth = 0.5,
            alpha = 0.2) +
  geom_point(data = max_points,
             aes(x = maxstage_time,
                 y = maxstage),
             size = 1.2,
             alpha = 0.75) +
  scale_color_manual(values = c("JMD PMF" = "#008B45FF",
                                "Scaled Inflow Hydrograph" = "#808180FF"))+
  scale_x_continuous(breaks = seq(0,24*10,24),
                     minor_breaks = seq(0,24*10,6)) +
  scale_y_continuous(breaks = seq(3800,3900,10),labels = scales::comma_format())+
  labs(x = "Time (hour)",
       y = "Stage (ft-NAVD88)",
       color = NULL) +
  theme(legend.position = "bottom")
ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/JMD_MC_Routing_Example.png", width = 7, height = 5, dpi = 400)
