# rfaR: RMC Reservoir Frequency Analysis in R

An R implementation of the U.S. Army Corps of Engineers (USACE) Risk
Management Center-Reservoir Frequency Analysis (RMC-RFA)
methodology/software. This package produces reservoir stage-frequency
curves with uncertainty bounds by combining deterministic flood routing
(Modified Puls method) with a nested Monte Carlo framework. The outer
loop quantifies knowledge uncertainty in inflow volume frequency
distributions, while the inner loop simulates natural variability across
thousands of flood events. Uncertain variables include inflow volume,
flood hydrograph shape, seasonal occurrence, and antecedent reservoir
stage. The resulting stage-frequency curves provide critical hydrologic
hazard assessment data for dam safety risk analyses, enabling evaluation
of loading parameters (stage, discharge, duration) and their associated
annual exceedance probabilities.

## See also

Useful links:

- <https://github.com/USACE-RMC/rfaR>

- <https://ideal-broccoli-1q9y47z.pages.github.io/>

- Report bugs at <https://github.com/USACE-RMC/rfaR/issues>

## Author

**Maintainer**: Daniel McGraw <daniel.e.mcgraw@usace.army.mil> (P.E.,
USACE RMC)

Authors:

- Andrew Verdin <andrew.p.verdin@usace.army.mil> (PhD, P.H., USACE RMC)

- C. Haden Smith <cole.h.smith@usace.army.mil> (P.E., USACE RMC)

Other contributors:

- Allen Avance <allen.avance@usace.army.mil> (P.E., USACE RMC)
  \[contributor\]

- Samantha Hartke <samantha.h.hartke@usace.army.mil> (PhD, USACE RMC)
  \[contributor\]

- Julian Gonzalez <julian.t.gonzalez@usace.army.mil> (USACE RMC)
  \[contributor\]
