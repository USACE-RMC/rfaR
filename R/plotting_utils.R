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
theme_rfar_conceptual <- function(){
  ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = 11, face = "bold.italic"),
                   plot.subtitle = ggplot2::element_text(size = 8, face = "italic"),
                   axis.title = ggplot2::element_text(size = 9, face = "bold"),
                   axis.text = ggplot2::element_text(size = 8))
}
