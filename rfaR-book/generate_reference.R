# manual/generate_reference.R
library(tools)
library(purrr)

rd_db <- Rd_db("rfaR")

fn_groups <- list(
  "Execute Reservoir Frequency Analysis" = c("rfa_simulate"),
  "Primary RFA Modules" = c(
    "stratified_sampler",
    "flow_frequency_sampler",
    "flow_frequency_sampler_expected",
    "hydrograph_setup",
    "scale_hydrograph",
    "mod_puls_routing",
    "stage_frequency_curve"
  ),
  "Rejection Sampling" = c(
    "pmf_stage_lognormal",
    "rejection_sampling_stage",
    "rejection_plotter"
  ),
  "Additional Utilities" = c(
    "bootstrap_vfc",
    "qp3",
    "aep2stage",
    "stage2aep",
    "interpolate_aep_matrix",
    "interpolate_stage_matrix",
    "power_function",
    "theme_rfar_conceptual"
  )
)

out <- file("05-function-reference.qmd", "w")
writeLines("# Function Reference\n", out)

iwalk(fn_groups, function(fns, group_name) {
  writeLines(paste0("## ", group_name, "\n"), out)
  walk(fns, function(fn) {
    rd_file <- rd_db[[paste0(fn, ".Rd")]]
    if (!is.null(rd_file)) {
      tmp <- tempfile(fileext = ".txt")
      Rd2txt(rd_file, out = tmp)
      raw <- readLines(tmp)
      clean <- gsub("(.)\\x08", "", raw, perl = TRUE)
      writeLines(c(paste0("### `", fn, "()`\n"), "```", clean, "```", ""), out)
    }
  })
})

close(out)
