#' Create heatmap
#'
#' Create heatmap
#'
#' @param x odf object
#' @param log logarithmic scale?
#' @param n number of classes (if !log)
#' @param legend.title legend title
#' @import ggplot2
#' @export
odf_heatmap <- function(x, log = FALSE, n = 5, legend.title = "Flow") {

  x <- odf_remove_via(odf_remove_type(x))


  od <- x$od
  p <- x$points

  od <- od %>%
    mutate(orig = factor(orig, levels = p$id, labels = paste(p$name, "-", p$id)),
           dest = factor(dest, levels = p$id, labels = p$id))

  if (log) {
    mx <- floor(log10(max(od$flow, na.rm = TRUE)))
    breaks <- 10^(c(-Inf, 0:mx))
  } else {
    breaks <- pretty(od$flow, n = n)
  }

  breaks_format <- format(breaks, scientific = FALSE, trim = TRUE, big.mark = ",")
  labels <- sapply(1:(length(breaks)-1), function(i) {
    paste0(breaks_format[i], " to ", breaks_format[i+1])
  })
  od$flow <- cut(od$flow, breaks = breaks, labels = labels)

  g <- ggplot(od, aes(x=dest, y=orig, fill=flow)) +
    geom_tile() +
    scale_fill_viridis_d(legend.title, option = "B", direction = -1, na.translate = FALSE, drop=FALSE) +
    scale_x_discrete("Origin") +
    scale_y_discrete("Destination") +
    theme(panel.background = element_rect(fill = "grey70"))

  g
}
