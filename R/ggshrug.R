#' Shruggie plot
#'
#' Creates a linerange plot formatted to look like a shrug emoticon.
#'
#' @param df data frame containing parameters 
#' @param x grouping variable
#' @param y measurement variable
#' @param ymin minimum y value (lower bound of shrug)
#' @param ymax maximum y value (upper bound of shrug)
#'
#' @return A ggplot object.
#'
#' 
#' @export
#' @import ggplot2
#' @author Erin Jonaitis, \email{emjonaitis@@gmail.com}
#' 
ggshrug <- function(df, x, y, ymin, ymax, xlab='group', ylab='quantity') {
  right <- '\u002F\u00AF'
  left <- '\u00AF\u005C'
  ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) + 
    ggplot2::geom_linerange(ggplot2::aes(ymin=ymin, 
                       ymax=ymax),
                   lty=5,
                   position=ggplot2::position_nudge(x=-0.03)) +
    ggplot2::geom_label(label='\u30C4', family='Arial Unicode MS',
               fill='white', label.size=NA) + 
    ggplot2::geom_label(ggplot2::aes(y=ymin), label=left, fill='white', 
               label.padding=ggplot2::unit(0.1, 'lines'), label.size=NA) +
    ggplot2::geom_label(ggplot2::aes(y=ymax), label=right, fill='white', 
               label.padding=ggplot2::unit(0.1, 'lines'), label.size=NA) +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()
}
