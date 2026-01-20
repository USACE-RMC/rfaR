## Hydrograph shape sample
library(tidyverse)
colors_i_like <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF","#5F559BFF","#A20056FF","#808180FF","#1B1919FF")

Nsims <- 10000 * 1000

critical_dur = 2
routing_dur = 10

# Hydrosetup
hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                                jmd_hydro_jun1921,
                                jmd_hydro_jun1965,
                                # jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                jmd_hydro_pmf,
                                jmd_hydro_sdf,
                                critical_duration = critical_dur,
                                routing_days = routing_dur)
# Sample Order
hydro_samp <- tibble(hydro_samp = sample(1:length(hydrographs), size = Nsims, replace = TRUE))
hydro_samp_sum <- hydro_samp |>
  count(hydro_samp) |>
  mutate(rel_freq = round(n/sum(n),4))

# Histogram
hydro_samples <- ggplot(hydro_samp_sum) +
  geom_col(aes(x = hydro_samp, y = n),
           fill = "#5F559BFF",
           color = "#808180FF") +
  scale_x_continuous(breaks = seq(1,6,1)) +
  geom_text(aes(x = hydro_samp, y = n, label = rel_freq),
            vjust=-.5,
            size = 5) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                     labels = scales::label_comma()) +
  labs(x = "Month",
       y = "Frequency",
       title = "Sampled Hydrograph Distribution",
       subtitle = "Example Data from JMD") +
  coord_cartesian(ylim = c(floor(min(hydro_samp_sum$n)/100)*100,
                           ceiling(max(hydro_samp_sum$n)/100)*100)) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Sampled_Hydrograph.png", hydro_samples, width = 10, height = 7, dpi = 400)

# full plot
ggplot(hydro_samp_sum) +
  geom_col(aes(x = hydro_samp, y = n),
           fill = "#5F559BFF",
           color = "#808180FF") +
  scale_x_continuous(breaks = seq(1,6,1)) +
  geom_text(aes(x = hydro_samp, y = n, label = rel_freq),
            vjust=-.5,
            size = 5) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                     labels = scales::label_comma()) +
  labs(x = "Month",
       y = "Frequency",
       title = "Sampled Hydrograph Distribution",
       subtitle = "Example Data from JMD")+
  theme(plot.title = element_text(face = "bold", size = 14))
