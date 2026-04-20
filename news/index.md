# Changelog

## rfaR 0.5.1

### Summary

Fixes a bug reported by a tester where
[`hydrograph_setup()`](https://usace-rmc.github.io/rfaR/reference/hydrograph_setup.md)
produced `NA` for `obs_vol` when the input CSV contained trailing empty
rows — a common artifact of
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) on
Excel-exported files.

### Root cause

[`read.csv()`](https://rdrr.io/r/utils/read.table.html) reads the full
padded row range from Excel exports, producing trailing rows with `NA`
in the Flow column and `""` in Date/Time. These propagated through
[`zoo::rollmeanr()`](https://rdrr.io/pkg/zoo/man/rollmean.html) as `NA`,
poisoning `obs_vol`, and broke the routing extension’s time sequence.

### Fix

Truncate each input hydrograph at the last row with a valid (non-NA)
flow value. Interior NAs are preserved so malformed inputs fail loudly
downstream, rather than being silently dropped. Emits a
[`cli::cli_inform`](https://cli.r-lib.org/reference/cli_abort.html) per
hydrograph noting how many trailing rows were dropped.

### Testing

Added `tests/testthat/test-hydrograph_setup.R` with two tests:

1.  Confirms `obs_vol` is finite when input has trailing empty rows, and
    matches the result from clean input
2.  Confirms the `cli_inform` message fires with the correct row count

Both pass locally under `devtools::test()` and `devtools::check()`.

## rfaR 0.5.0

- Created news file. This feels like a big step

### Major Changes

- Added validation vignettes and user guide/manual.

### Bug Fixes

- There will be many blind spots and bugs. All questions, bug reports,
  and suggestions are welcome!
