# rfaR ggplot2 Theme for Conceptual Plots

A custom ggplot2 theme used in the rfaR conceptual realization vignette.
Based on
[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html) with
tightened text sizes, italic titles, and the legend suppressed. Intended
for figures that emphasize curves over annotation density.

## Usage

``` r
theme_rfar_conceptual()
```

## Value

A ggplot2 theme object that can be added to a `ggplot` via `+`.

## See also

[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

## Examples

``` r
library(ggplot2)
ggplot(jmd_rfa_expected, aes(x = AEP, y = Expected)) +
  geom_line() +
  scale_x_continuous(transform = c("log10", "reverse")) +
  labs(title = "JMD Expected Stage-Frequency",
       subtitle = "Demonstrating theme_rfar_conceptual()",
       x = "AEP", y = "Stage (ft)") +
  theme_rfar_conceptual()
```
