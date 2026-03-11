# Define the palette outside the function so it's accessible
okabe_ito_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "Pass" = "#0072B2", "Fail" = "#D55E00", "#CC79A7")

#' @title NanoQuRe Professional Theme
#' @export
nanoqure_theme <- function() {
  # Use + to combine, not separate lines
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") + 
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "right"
    )
}

#' @title NanoQuRe Color Scale
#' @export
scale_fill_nanoqure <- function(...) {
  ggplot2::scale_fill_manual(values = okabe_ito_palette, ...)
}