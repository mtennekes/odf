#' Remove type grouping and aggregate odf accordingly
#'
#' Remove type grouping and aggregate odf accordingly
#'
#' @param x odf object
#' @param na.rm na.rm
#' @export
#' @import dplyr
odf_remove_type <- function(x, na.rm = FALSE) {
  od <- x$od

  od2 <- od %>%
    mutate(VIA = via2chr(via)) %>%
    group_by(orig, dest, VIA) %>%
    summarize(flow = sum(flow, na.rm = na.rm)) %>%
    ungroup() %>%
    mutate(via = chr2via(VIA)) %>%
    select(orig, dest, via, flow)
  x$od <- od2
  x
}

#' Remove via points and aggregate odf accordingly
#'
#' Remove via points and aggregate odf accordingly
#'
#' @param x odf object
#' @param na.rm na.rm
#' @export
#' @import dplyr
odf_remove_via <- function(x, na.rm = FALSE) {
  od <- x$od

  od$via <- odf_via(lapply(1:nrow(od), function(i) return(NULL)))

  if ("type" %in% names(od)) {
    od2 <- od %>%
      group_by(orig, dest, type) %>%
      summarize(flow = sum(flow, na.rm = na.rm)) %>%
      ungroup()
    od3 <- od2 %>%
      mutate(via = odf_via(lapply(1:nrow(od2), function(i) return(NULL)))) %>%
      select(orig, dest, via, flow, type)
  } else {
    od2 <- od %>%
      group_by(orig, dest) %>%
      summarize(flow = sum(flow, na.rm = na.rm)) %>%
      ungroup()
    od3 <- od2 %>%
      mutate(via = odf_via(lapply(1:nrow(od2), function(i) return(NULL)))) %>%
      select(orig, dest, via, flow)
  }
  x$od <- od3
  x
}
