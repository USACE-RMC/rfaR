#' rfaR ggplot2 Theme for Conceptual Plots
#'
#' A custom \pkg{ggplot2} theme used in the rfaR conceptual realization
#' vignette. Based on \code{\link[ggplot2]{theme_bw}} with tightened text
#' sizes, italic titles, and the legend suppressed. Intended for figures
#' that emphasize curves over annotation density.
#'
#' @return A \pkg{ggplot2} theme object that can be added to a \code{ggplot}
#'   via \code{+}.
#'
#' @export
#'
#' @seealso \code{\link[ggplot2]{theme_bw}}
#'
#' @examples
#' library(ggplot2)
#' ggplot(jmd_rfa_expected, aes(x = AEP, y = Expected)) +
#'   geom_line() +
#'   scale_x_continuous(transform = c("log10", "reverse")) +
#'   labs(title = "JMD Expected Stage-Frequency",
#'        subtitle = "Demonstrating theme_rfar_conceptual()",
#'        x = "AEP", y = "Stage (ft)") +
#'   theme_rfar_conceptual()
theme_rfar_conceptual <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(size = 11, face = "bold.italic"),
      plot.subtitle = ggplot2::element_text(size = 8, face = "italic"),
      axis.title = ggplot2::element_text(size = 9, face = "bold"),
      axis.text = ggplot2::element_text(size = 8)
    )
}

#' AEP-Labelled Normal-Variate X Scale
#'
#' Creates a continuous \pkg{ggplot2} x-axis positioned on the standard-normal
#' variate scale while displaying labels as annual exceedance probabilities
#' (AEPs). This is useful for stage-frequency, flow-frequency, and similar
#' conceptual plots where values are modeled or transformed in normal-variate
#' space but should be interpreted as probabilities.
#'
#' Major breaks are specified as AEP values and transformed using
#' \code{qnorm(1 - AEP)}. Minor breaks are generated from the supplied
#' multipliers across AEP decades from \eqn{10^{-1}} through \eqn{10^{-10}}.
#'
#' @param aep_breaks Numeric vector of annual exceedance probabilities to use
#'   as major axis breaks. Values must be greater than 0 and less than 1.
#'   Defaults range from 0.99 through \eqn{10^{-10}}.
#' @param minor_aep Numeric vector of multipliers used to generate minor AEP
#'   breaks within each decade. The default, \code{2:9}, produces minor breaks
#'   such as 0.2 through 0.9, 0.02 through 0.09, and so on.
#'
#' @return A \pkg{ggplot2} continuous x-scale object that can be added to a
#'   \code{ggplot} via \code{+}.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' example_data <- data.frame(
#'   AEP = c(0.99, 0.5, 0.1, 0.01, 0.001),
#'   Stage = c(10.2, 11.5, 14.0, 17.3, 20.1)
#' )
#'
#' ggplot(example_data, aes(x = qnorm(1 - AEP), y = Stage)) +
#'   geom_line() +
#'   scale_x_aep() +
#'   labs(
#'     title = "Example Stage-Frequency Curve",
#'     x = "Annual Exceedance Probability",
#'     y = "Stage (ft)"
#'   )
scale_x_aep <- function(
  aep_breaks = c(
    9.9e-1,
    9e-1,
    5e-1,
    1e-1,
    1e-2,
    1e-3,
    1e-4,
    1e-5,
    1e-6,
    1e-7,
    1e-8,
    1e-9,
    1e-10
  ),
  minor_aep = 2:9
) {
  minor_aep_breaks <- unlist(
    lapply(minor_aep, function(i) i * 10^-(1:10))
  )

  z_breaks <- stats::qnorm(1 - aep_breaks)
  z_breaks_minor <- stats::qnorm(1 - minor_aep_breaks)

  ggplot2::scale_x_continuous(
    breaks = z_breaks,
    minor_breaks = z_breaks_minor,
    labels = aep_breaks
  )
}
