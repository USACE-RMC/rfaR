# ==============================================================================
# CHERRY CREEK EXAMPLE DATA
# ==============================================================================
# Cherry Creek Reservoir Model ("reservoir geometry")
cc_resmodel <- read.csv("data-raw/cherry_creek/cherry_creek_resmodel.csv")

# Inflow Hydrograph
cc_inflowhydro <- read.csv("data-raw/cherry_creek/cherry_creek_inflow.csv")
cc_init_elev <- 5565

# Mod-Puls Routing Results from HMS - starting elev = 5565
cc_hms_results  <- read.csv("data-raw/cherry_creek/cherry_creek_hms_results.csv")

# Save to package
usethis::use_data(cc_resmodel, overwrite = TRUE)
usethis::use_data(cc_inflowhydro, overwrite = TRUE)
usethis::use_data(cc_init_elev, overwrite = TRUE)
usethis::use_data(cc_hms_results, overwrite = TRUE)

# ==============================================================================
# JOHN MARTIN DAM (JMD) EXAMPLE DATA
# ==============================================================================

# ==============================================================================
# J.STROM THURMOND (JST) DAM EXAMPLE DATA
# ==============================================================================
