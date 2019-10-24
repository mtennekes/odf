#' Get Euclidean distances
#'
#' Get Euclidean distances
#'
#' @param x x
#' @export
get_eucl_distances <- function(x) {
  p <- x$points

  d <- as.matrix(dist(st_coordinates(p)))
  dimnames(d) <- list(p$id, p$id)
  d
}

#' Get flows as matrix
#'
#' Get flows as matrix
#'
#' @param x x
#' @param directional directional
#' @export
#' @import tidyr
get_flows <- function(x, directional = FALSE) {
  p <- x$points
  m <- matrix(0, nrow = nrow(p), ncol = nrow(p), dimnames = list(p$id, p$id))

  o <- x$od
  o2 <- o %>%
    group_by(orig,dest) %>%
    summarize(flow = sum(flow, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(orig, flow, drop = FALSE, fill = 0)

  m <- as.matrix(o2[, -1])
  rownames(m) <- as.character(o2$dest)

  if (directional) {
    m <- m + t(m)
  }
  m
}


stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)
