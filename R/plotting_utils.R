#' rfaR ggplot theme for conceptual example
#'
#' custom ggplot2 theme for rfaR-Realization-Conceptual.Rmd
#' @export
theme_rfar_conceptual <- function(){
  ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = 11, face = "bold.italic"),
                   plot.subtitle = ggplot2::element_text(size = 8, face = "italic"),
                   axis.title = ggplot2::element_text(size = 9, face = "bold"),
                   axis.text = ggplot2::element_text(size = 8))
}
