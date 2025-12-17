# Mod-Puls Routing Routine for rfaR
# This will use the Cherry Creek Example from the excel sheet as the example
# Secondary Example Could be Thurmond (use PMF)
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

theme_set(theme_bw())

# Load Data --------------------------------------------------------------------
inflow <- dir_ls("data/mod_puls/",glob = "*cherry_creek_inflow.csv",recurse = T) %>%
  read_csv()

res_model <- dir_ls("data/mod_puls/",glob = "*cherry_creek_stage_stor_dis.csv", recurse = T) %>%
  read_csv()

hms_results <- dir_ls("data/mod_puls/",glob = "*cherry_creek_hms_results.csv",recurse = T) %>%
  read_csv()

# Constants --------------------------------------------------------------------
# Assume One Hour - convert to seconds for CFS
dt <- 1*60*60

# Storage - convert acre-ft to ft^s
sqft_acre <- 43560

# Starting Elevation
h_1 <- 5565

# Functions --------------------------------------------------------------------
# modpuls <- function(dt)


# Storage Indicator ------------------------------------------------------------
res_model |> mutate(stor_sqft = stor_acft*sqft_acre,
                    storage_indicator = (2*stor_sqft/dt)+outflow_cfs) -> res_model

# Set up Spreadsheet first - improve later ------
modpuls <- inflow |> mutate(I_avg = rollsum(flow_cfs,k=2, align = "right",fill = NA),
                            `2St1/dt - Ot1` = 0,
                            `2St2/dt + Ot2` = 0, 
                            O_t = 0,
                            S_t = 0,
                            Elev_t = 0)

for (i in 1:nrow(modpuls)){
  
  # Timestep 1
  if (i < 2){
    modpuls$`2St1/dt - Ot1`[i] = 0
    modpuls$`2St2/dt + Ot2`[i] = 0
    modpuls$Elev_t[i] = h_1
    
    # Use elevation to get storage
    modpuls$S_t[i] = approx(res_model$elev_ft,res_model$stor_sqft,xout = modpuls$Elev_t[i])$y
    # Use storage to get outflow
    modpuls$O_t[i] = approx(res_model$stor_sqft,res_model$outflow_cfs,xout = modpuls$S_t[i])$y
    
  } else{
    modpuls$`2St1/dt - Ot1`[i] = 2*modpuls$S_t[i-1]/dt - modpuls$O_t[i-1]
    modpuls$`2St2/dt + Ot2`[i] = modpuls$`2St1/dt - Ot1`[i] + modpuls$I_avg[i]
    
    # Get outflow
    modpuls$O_t[i] = approx(res_model$storage_indicator,res_model$outflow_cfs,xout = modpuls$`2St2/dt + Ot2`[i])$y
    
    # Get Storage
    modpuls$S_t[i] = approx(res_model$storage_indicator,res_model$stor_sqft,xout = modpuls$`2St2/dt + Ot2`[i])$y
    
    # Get Elevation
    modpuls$Elev_t[i] = approx(res_model$stor_sqft,res_model$elev_ft,xout = modpuls$S_t[i])$y
  }
}


# Results ----------------------------------------------------------------------
# Do the answers make sense?
colors_i_like <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF","#5F559BFF","#A20056FF","#808180FF","#1B1919FF")
# colour_vector <- palettes::as_color(pal_aaas("default")(10))


ggplot(modpuls) + 
  geom_line(aes(x = time_hour, y = flow_cfs),color = "#3B4992FF", linewidth = 0.75) + 
  geom_line(aes(x = time_hour, y = O_t),color = "#EE0000FF", linewidth = 0.75) + 
  scale_x_continuous(breaks = seq(0,960,24)) + 
  coord_cartesian(xlim = c(0,144))

ggplot(modpuls) + 
  geom_line(aes(x = time_hour, y = Elev_t),color = "#008B45FF", linewidth = 0.75) + 
  scale_x_continuous(breaks = seq(0,960,24)) + 
  coord_cartesian(xlim = c(0,500))
