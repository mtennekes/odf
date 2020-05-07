#' Create map from odf object
#'
#' Create map from odf object
#'
#' @param x odf object
#' @param ... arguments passed on to od_add_lines
#' @export
#' @rdname odf_flows
#' @name odf_flows
od_flows <- function(x, ...) {
  if (is.null(x$routes)) x <- odf_add_lines(x, via = by_via,...)

  od <- x$od
  p <- x$points
  r <- x$routes

  #p$name <- factor(p$name, levels = p$name)

  if (any(od$orig == od$dest)) {
    warning("od data contains values on diagonal, i.e. orig=dest. These are ignored")
    od <- od %>% filter(orig!=dest)
  }



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

  orig <- x$od %>%
    select(orig, flow) %>%
    group_by(orig) %>%
    summarize(flow_from = sum(flow))

  dest <- x$od %>%
    select(dest, flow) %>%
    group_by(dest) %>%
    summarize(flow_to = sum(flow))


  suppressWarnings({
    select(p, name, id, geometry) %>%
    left_join(orig, by = c("id" = "orig")) %>%
    left_join(dest, by = c("id" = "dest")) %>%
    replace_na(list(flow_to = 0, flow_from = 0))
  })
}
