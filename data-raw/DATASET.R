# CHERRY CREEK EXAMPLE DATA ====================================================
# Cherry Creek Reservoir Model ("reservoir geometry")
cc_resmodel <- read.csv("data-raw/cherry_creek/cherry_creek_resmodel.csv")

# Inflow Hydrograph
cc_inflowhydro <- read.csv("data-raw/cherry_creek/cherry_creek_inflow.csv")

# CC Initial Elevation
cc_init_elev <- 5565

# Mod-Puls Routing Results from HMS - starting elev = 5565
cc_hms_results  <- read.csv("data-raw/cherry_creek/cherry_creek_hms_results.csv")

# Save to package
usethis::use_data(cc_resmodel, overwrite = TRUE)
usethis::use_data(cc_inflowhydro, overwrite = TRUE)
usethis::use_data(cc_init_elev, overwrite = TRUE)
usethis::use_data(cc_hms_results, overwrite = TRUE)

# JOHN MARTIN DAM (JMD) CORE EXAMPLE DATA ======================================
# JMD Res Model
jmd_resmodel <- read.csv("data-raw/JMD/res_model/jmd_resmodel_best_est.csv")

usethis::use_data(jmd_resmodel, overwrite = TRUE)

# JOHN MARTIN DAM - DISCHARGE GAGE =============================================
jmd_por_inflow <- read.csv("data-raw/JMD/discharge_gage/jmd_por_inflow.csv")

usethis::use_data(jmd_por_inflow, overwrite = TRUE)

# JOHN MARTIN DAM - EMPIRICAL FREQUENCY ========================================
jmd_empirical_stage_wy1980 <- read.csv("data-raw/JMD/empirical_freq/empirical_stage_wy1980-2024.csv")
jmd_empirical_stage_wy1980_pt <- read.csv("data-raw/JMD/empirical_freq/empirical_stage_wy1980-2024_PT.csv")

usethis::use_data(jmd_empirical_stage_wy1980, overwrite = TRUE)
usethis::use_data(jmd_empirical_stage_wy1980_pt, overwrite = TRUE)

# JOHN MARTIN DAM - HYDROGRAPHS ================================================
jmd_hydro_apr1999 <- read.csv("data-raw/JMD/hydrographs/April_1999.csv")
jmd_hydro_jun1921 <- read.csv("data-raw/JMD/hydrographs/June_1921.csv")
jmd_hydro_jun1965 <- read.csv("data-raw/JMD/hydrographs/June_1965_HMS.csv")
jmd_hydro_jun1965_15min <- read.csv("data-raw/JMD/hydrographs/June_1965_15min.csv")
jmd_hydro_may1955 <- read.csv("data-raw/JMD/hydrographs/May_1955.csv")
jmd_hydro_pmf <- read.csv("data-raw/JMD/hydrographs/PMF.csv")
jmd_hydro_sdf <- read.csv("data-raw/JMD/hydrographs/SDF.csv")

usethis::use_data(jmd_hydro_apr1999, overwrite = TRUE)
usethis::use_data(jmd_hydro_jun1921, overwrite = TRUE)
usethis::use_data(jmd_hydro_jun1965, overwrite = TRUE)
usethis::use_data(jmd_hydro_jun1965_15min, overwrite = TRUE)
usethis::use_data(jmd_hydro_may1955, overwrite = TRUE)
usethis::use_data(jmd_hydro_pmf, overwrite = TRUE)
usethis::use_data(jmd_hydro_sdf, overwrite = TRUE)

# JOHN MARTIN DAM - SEASONALITY ================================================
jmd_seasonality <- read.csv("data-raw/JMD/seasonality/seasonality_full_por.csv")

usethis::use_data(jmd_seasonality, overwrite = TRUE)

# JOHN MARTIN DAM - STAGE DURATION =============================================
jmd_stage_duration <- read.csv("data-raw/JMD/stage_duration/stage_duration_monthly_1980-2024.csv")

usethis::use_data(jmd_stage_duration, overwrite = TRUE)

# JOHN MARTIN DAM - STAGE GAGE =================================================
jmd_wy1980_stage <- read.csv("data-raw/JMD/stage_gage/jmd_wy1980-2024_stage.csv")

usethis::use_data(jmd_wy1980_stage, overwrite = TRUE)

# JOHN MARTIN DAM - VOLUME FREQUENCY CURVE =====================================
jmd_vfc_parameters <- read.csv("data-raw/JMD/vfc/final_2day_parameters.csv")
jmd_vfc <- read.csv("data-raw/JMD/vfc/final_2day_vfc.csv")
jmd_bf_parameter_sets <- read.csv("data-raw/JMD/vfc/parameter_sets.csv")

usethis::use_data(jmd_vfc_parameters, overwrite = TRUE)
usethis::use_data(jmd_vfc, overwrite = TRUE)
usethis::use_data(jmd_bf_parameter_sets, overwrite = TRUE)

# JOHN MARTIN DAM - RFA RESULTS EXPECTED ONLY ==================================
jmd_rfa_expected <- read.csv("data-raw/JMD/rfa_results/jmd_rfa_expected.csv")

usethis::use_data(jmd_rfa_expected, overwrite = TRUE)

# JOHN MARTIN DAM - RFA RESULTS MEDIAN ONLY ==================================
jmd_rfa_median <- read.csv("data-raw/JMD/rfa_results/jmd_rfa_median.csv")

usethis::use_data(jmd_rfa_median, overwrite = TRUE)

# JOHN MARTIN DAM - RFA RESULTS FULL UNCERT ====================================
jmd_rfa_full <- read.csv("data-raw/JMD/rfa_results/jmd_rfa_full.csv")

usethis::use_data(jmd_rfa_full, overwrite = TRUE)

# JOHN MARTIN DAM - RFA EXPECTED TABULAR RESULTS ===============================
jmd_rfa_expected_tabular <- read.csv("data-raw/JMD/rfa_results/jmd_rfa_expected_tabular_result.csv")

usethis::use_data(jmd_rfa_expected_tabular, overwrite = TRUE)

# STRATIFIED SAMPLING EXAMPLE BINS =============================================
example_stratified <- read.csv("data-raw/stratified_sampling/stratified_bins_example.csv")

usethis::use_data(example_stratified, overwrite = TRUE)

# MOD-PULS VALIDATION - JMD MAY 1955 ===========================================
hms_scaled_may1955 <- read.csv("data-raw/JMD/hms_routing/ModPuls_Validation_May1955.csv")

usethis::use_data(hms_scaled_may1955, overwrite = TRUE)

# THINK!!!! ====================================================================
think <- r"{
  _____   _   _   ___   _   _   _  __
 |_   _| | | | | |_ _| | \ | | | |/ /
   | |   | |_| |  | |  |  \| | | ' /
   | |   |  _  |  | |  | |\  | | . \
   |_|   |_| |_| |___| |_| \_| |_|\_\

}"

usethis::use_data(think, overwrite = TRUE)

# USACE - RMC CASTLE ===========================================================
castle <- r"{
  _______________________________________
 /                                       \
/   _   _   _                 _   _   _   \
|  | |_| |_| |   _   _   _   | |_| |_| |  |
|   \   _   /   | |_| |_| |   \   _   /   |
|    | | | |     \       /     | | | |    |
|    | |_| |______|     |______| |_| |    |
|    |              ___              |    |
|    |  _    _    (     )    _    _  |    |
|    | | |  |_|  (       )  |_|  | | |    |
|    | |_|       |       |       |_| |    |
|   /            |_______|            \   |
|  |___________________________________|  |
\       USACE RISK MANAGEMENT CENTER      /
 \_______________________________________/
}"
usethis::use_data(castle, overwrite = TRUE)
