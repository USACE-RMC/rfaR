## Flood Seasonality Sample
library(tidyverse)
colors_i_like <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF","#5F559BFF","#A20056FF","#808180FF","#1B1919FF")

# Flood Szn Distributions ======================================================
jmd_seasonality <- jmd_seasonality |>
  mutate(month_abbr = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                            levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

seasonality_rfa <- ggplot(jmd_seasonality) +
  geom_col(aes(x = month_abbr, y = frequency),
           fill = "#3B4992FF",
           color = "#808180FF") +
  labs(x = "Month",
       y = "Frequency",
       title = "Flood Seasonality Distribution",
       subtitle = "Example Data from JMD") +
  geom_text(data =jmd_seasonality |> filter(frequency >0), aes(x = month_abbr, y = frequency, label = relative_frequency),
            vjust = -.5,
            size = 4.5) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/RFA_Seasonality.png",seasonality_rfa, width = 10, height = 7, dpi = 400)

# Reservoir Stage ==============================================================
# From rfaR
# Number of simulations
Nsims <- 10000 * 1000

# Initialize vector of sampled months
InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = jmd_seasonality$relative_frequency)

# Initialize vector of sampled stages
InitStages <- numeric(Nsims)

# Use vector of sampled months to create sample of starting stages
stage_ts <- jmd_wy1980_stage
stage_ts$months <- lubridate::month(lubridate::mdy(jmd_wy1980_stage$date))

# Extract unique months
UniqMonths <- sort(unique(InitMonths))

for (i in 1:length(UniqMonths)) {
  sampleID <- which(InitMonths == UniqMonths[i])
  InitStages[sampleID] <- sample(stage_ts$stage[stage_ts$months %in% UniqMonths[i]],
                                 size = sum(InitMonths == UniqMonths[i]), replace = TRUE)
}
# Histogram of Monthly Sampling ====
month_df <- tibble(Month_num = 1:12,
                   Month_fact = factor(Month_num,levels = 1:12),
                   Month_name = month.name,
                   Month_abbr = month.abb)

month_sample <- tibble(Months = InitMonths)

# Summarize
month_sample_sum <- month_sample |>
  group_by(Months) |>
  summarize(Count = length(Months)) |>
  ungroup() |>
  left_join(month_df, join_by(Months == Month_num)) |>
  mutate(Rel_Freq = round(Count/sum(Count),3),
         Cume_Freq = round(cumsum(Rel_Freq),3))

# plot
sampled_seasonality <- ggplot(month_sample_sum) +
  geom_col(aes(x = Months, y = Count),
           fill = "#008280FF",
           color = "#808180FF") +
  scale_x_continuous(breaks = seq(4,9,1),
                     labels = month_sample_sum$Month_abbr) +
  geom_text(aes(x = Months, y = Count, label = Rel_Freq),
            vjust=-.5,
            size = 4.5) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                     labels = scales::scientific) +
  labs(x = "Month",
       y = "Frequency",
       title = "Sampled Flood Seasonality Distribution",
       subtitle = "Example Data from JMD") +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Sampled_Seasonality.png",sampled_seasonality, width = 10, height = 7, dpi = 400)

# Histogram of Stage Sampling =====
month_df <- month_df |>
  mutate(Month_fact = factor(Month_abbr,levels = Month_abbr))

stages_df <- tibble(stages = InitStages,
                    sampled_month = InitMonths) |>
  left_join(month_df, join_by(sampled_month == Month_num))


# stages_df_sum <- stages_df |>
#   group_by(Month_fact) |>
#   summarize(Count = length())

# Hist Facet
sampled_starting_stage <- ggplot(stages_df) +
  geom_histogram(aes(x = stages,
                     group = Month_abbr,
                     fill = Month_abbr),
                 binwidth = 5,
                 color = "#808180FF") +
  ggsci::scale_fill_aaas()+
  labs(x = "Starting Reservoir Stage (NAVD88)",
       y = "Frequency(1000s)",
       title = "Sampled Starting Stage Distributions",
       subtitle = "Separeted by Sampled Month") +
  scale_x_continuous(breaks = seq(3700,3900,20), minor_breaks = seq(3700,3900,5), limits = c(3780,3870)) +
  scale_y_continuous(breaks = seq(0,1e6,2e5),
                     limits = c(0,6e5),
                     labels = seq(0,1e6,2e5)/1e3) +
  facet_grid(vars(Month_fact))+
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Sampled_Starting_Stage.png",sampled_starting_stage, width = 10, height = 7, dpi = 400)

# Starting Stage without season
ggplot(stages_df) +
  geom_histogram(aes(x = stages),
                 binwidth = 5,
                 fill = "#A20056FF",
                 color = "#808180FF") +
  labs(x = "Starting Reservoir Stage (NAVD88)",
       y = "Frequency",
       title = "Sampled Starting Stage Distributions",
       subtitle = "Separeted by Sampled Month") +
  scale_x_continuous(breaks = seq(3700,3900,20), minor_breaks = seq(3700,3900,5), limits = c(3780,3870)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                     labels = scientific) +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Sampled_Starting_Stage_no_szn.png", width = 10, height = 7, dpi = 400)
