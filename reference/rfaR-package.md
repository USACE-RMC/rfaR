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

- <https://usace-rmc.github.io/rfaR/>

- Report bugs at <https://github.com/USACE-RMC/rfaR/issues>

## Author

**Maintainer**: Daniel McGraw <daniel.e.mcgraw@usace.army.mil>
([ORCID](https://orcid.org/0009-0006-4859-0179)) (P.E., USACE RMC)

Authors:

- Andrew Verdin <andrew.p.verdin@usace.army.mil>
  ([ORCID](https://orcid.org/0000-0003-1967-9626)) (PhD, P.H., USACE
  RMC)

- C. Haden Smith <cole.h.smith@usace.army.mil>
  ([ORCID](https://orcid.org/0000-0002-4651-9890)) (P.E., USACE RMC)

Other contributors:

- Allen Avance <allen.avance@usace.army.mil> (P.E., USACE RMC)
  \[contributor\]

- Samantha Hartke <samantha.h.hartke@usace.army.mil>
  ([ORCID](https://orcid.org/0000-0002-0239-4723)) (PhD, USACE RMC)
  \[contributor\]

- Julian Gonzalez <julian.t.gonzalez@usace.army.mil>
  ([ORCID](https://orcid.org/0009-0009-9058-7653)) (USACE RMC)
  \[contributor\]

- Sadie Niblett <sadie.s.niblett@usace.army.mil>
  ([ORCID](https://orcid.org/0009-0008-8588-4816)) (USACE RMC)
  \[reviewer\]

- Reuben Sasaki <reuben.a.sasaki@usace.army.mil> (P.E., USACE RMC)
  \[reviewer\]

- Bryan Robinson <bryan.j.robinson@usace.army.mil> (P.E., USACE RMC)
  \[reviewer\]
