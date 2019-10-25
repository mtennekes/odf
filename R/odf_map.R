#' Create map from odf object
#'
#' Create map from odf object
#'
#' @param x odf object
#' @param by_type create flows for different type separately
#' @param by_via create flows for different via routes separately
#' @param incl_total include total (in case \code{by_type = TRUE})
#' @export
#' @rdname odf_flows
#' @name odf_flows
odf_flows <- function(x, by_type = TRUE, by_via = TRUE, incl_total = FALSE, ...) {
  if (!by_type) {
    x <- odf_remove_type(x)
  } else {
    x <- odf_add_total(x)
  }
  if (!by_via) x <- odf_remove_via(x)
  if (is.null(x$routes)) x <- odf_add_lines(x, ...)

  od <- x$od
  p <- x$points
  r <- x$routes

  #p$name <- factor(p$name, levels = p$name)

  od <- add_odvia(od)

  via_labels <- odf_add_place_names(od, p)

  od2 <- od %>%
    mutate(label = sapply(via_labels, function(v) {
              if (length(v) == 2) {
                paste0(v[1], " to ", v[2])
              } else {
                paste0(v[1], paste0(" via ", v[2:(length(v)-1)]), " to ", v[length(v)])
              }
            }),
           VIA = via2chr(via)) %>%
    left_join(r %>% mutate(VIA = via2chr(via), via = NULL), by = c("orig" = "orig", "dest" = "dest", "VIA" = "VIA")) %>%
    mutate(via = VIA, VIA = NULL) %>%
    st_as_sf()

  if ("type" %in% names(od2)) {
    od3 <- od2 %>% select(label, orig, dest, via, flow, type, geometry)
  } else {
    od3 <- od2 %>% select(label, orig, dest, via, flow, geometry)
  }

  od3
}

#' @rdname odf_flows
#' @name odf_points
odf_points <- function(x) {
  p <- x$points
  select(p, name, id, geometry)
}
