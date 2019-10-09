#' Create heatmap
#'
#' Create heatmap
#'
#' @param x odf object
#' @param log logarithmic scale?
#' @param n number of classes (if !log)
#' @param legend.title legend title
#' @param base_size base size
#' @param sort should the levels of o/d be sorted by total flow
#' @import ggplot2
#' @export
odf_heatmap <- function(x, log = FALSE, n = 5, legend.title = "Flow", base_size = 11, sort = FALSE) {

  x <- odf_remove_via(odf_remove_type(x))


  od <- x$od
  p <- x$points


  # reorder
  if (sort) {
    od_agg_o <- od %>%
      group_by(orig, .drop = FALSE) %>%
      summarise(flow = sum(flow, na.rm = TRUE))

    od_agg_d <- od %>%
      group_by(dest, .drop = FALSE) %>%
      summarise(flow = sum(flow, na.rm = TRUE))

    if (is.factor(od$dest)) {
      flows <- od_agg_o$flow + od_agg_d$flow
    } else {
      m_o <- match(od_agg_o$orig, p$id)
      m_d <- match(od_agg_d$dest, p$id)
      flows <- rep(0, nrow(p))
      flows[m_o] <- od_agg_o$flow
      flows[m_d] <- flows[m_d] + od_agg_d$flow
    }
    ord <- order(flows, decreasing = TRUE)
  } else {
    ord <- 1:nrow(p)
  }

  od <- od %>%
    mutate(orig = factor(orig, levels = p$id[ord], labels = paste(p$name, "-", p$id)[ord]),
           dest = factor(dest, levels = p$id[ord], labels = p$id[ord]))


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

  #sort

  g <- ggplot(od, aes(x=dest, y=orig, fill=flow)) +
    geom_tile() +
    scale_fill_viridis_d(legend.title, option = "B", direction = -1, na.translate = FALSE, drop=FALSE) +
    scale_x_discrete("Origin") +
    scale_y_discrete("Destination") +
    theme_gray(base_size = base_size) +
    theme(panel.background = element_rect(fill = "grey70"))

  g
}
