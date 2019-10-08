#' Create origin-destination flows (odf) object
#'
#' Create origin-destination flows (odf) object, which is a list consisting of a data.frame with the od data, an sf object with spatial points and optionally, an sf object with spatial (poly)lines of the routes
#'
#' @param od od
#' @param points points
#' @param routes routes
#' @param col_orig col_orig
#' @param col_dest col_dest
#' @param col_flow col_flow
#' @param col_type col_type
#' @param col_time col_time
#' @export
#' @import sf
odf <- function(od, points, routes = NULL, col_orig = 1, col_dest = 2, col_flow = 3, col_via = NULL, col_type = NULL, col_time = NULL) {

  # check od
  if (!is.data.frame(od)) stop("od is not a data.frame")
  nms <- names(od)
  has_type <- !missing(col_type)
  has_time <- !missing(col_time)
  has_via <- !missing(col_via)



  # check od columns
  if (is.numeric(col_orig)) col_orig <- nms[col_orig]
  if (is.numeric(col_dest)) col_dest <- nms[col_dest]
  if (is.numeric(col_via)) col_via <- nms[col_via]
  if (is.numeric(col_flow)) col_flow <- nms[col_flow]
  if (has_type) {
    if (is.numeric(col_type)) col_type <- nms[col_type]
    if (!(col_type %in% nms)) stop("column ", col_type, " not found")
    if (!inherits(od[[col_type]], c("numeric", "factor"))) stop("the column type should be an integer/numeric or factor")
  }
  if (has_time) {
    warning("time column not supported yet")
    #if (is.numeric(col_time)) col_time <- nms[col_time]
  }

  if (!(col_orig %in% nms)) stop("column ", col_orig, " not found")
  if (!(col_dest %in% nms)) stop("column ", col_dest, " not found")
  if (!(col_via %in% nms)) stop("column ", col_via, " not found")
  if (!(col_flow %in% nms)) stop("column ", col_flow, " not found")

  if (!inherits(od[[col_orig]], c("numeric", "integer", "factor"))) stop("the column orig should be an integer/numeric or factor")
  if (!inherits(od[[col_dest]], c("numeric", "integer", "factor"))) stop("the column dest should be an integer/numeric or factor")
  if (!inherits(od[[col_flow]], "numeric")) stop("the column flow should be an integer/numeric")

  if (has_via) {
    if (!is.list(od[[col_via]])) stop("the column via should be a list")
    lapply(od[[col_via]], function(x) {
      if (!is.null(x) && !inherits(x, c("numeric", "integer", "factor"))) stop("the column via contains non-numeric/factor values")
      NULL
    })
    col_via_name <- col_via
  } else {
    od$VIA__ <- lapply(1:nrow(od), function(i) return(NULL))
    col_via <- "VIA__"
    col_via_name <- "<unspecified>"
  }
  od[[col_via]] <- odf_via(od[[col_via]])

  if (has_type) {
    od <- od[, c(col_orig, col_dest, col_via, col_flow, col_type)]
    names(od) <- c("orig", "dest", "via", "flow", "type")
    attr(od, "original_names") <- c(col_orig, col_dest, col_via_name, col_flow, col_type)
  } else {
    od <- od[, c(col_orig, col_dest, col_via, col_flow)]
    names(od) <- c("orig", "dest", "via", "flow")
    attr(od, "original_names") <- c(col_orig, col_dest, col_via_name, col_flow)
  }


  # check points
  if (!inherits(points, "sf")) stop("points should be an sf object")
  if (!all(st_geometry_type(points) == "POINT")) stop("geometry type of points should be POINT")

  points_cols <- setdiff(names(points), attr(points, "sf_column"))[1:2]
  points <- points[, points_cols]
  names(points) <- c("id", "name", "geometry")
  attr(points, "sf_column") <- "geometry"

  # check if all ids in orig/dest are in points
  if (!all(od$orig %in% points$id)) stop("not all orig ids in od are contains in points")
  if (!all(od$dest %in% points$id)) stop("not all dest ids in od are contains in points")

  # check if all ids in via are in points
  vias <- unlist(od$via)
  if (!is.null(vias)) {
    lapply(vias, function(v) {
      if (!all(v %in% points$id)) stop("not all via ids in od are contains in points")
    })
  }

  # check routes
  has_routes <- !missing(routes)

  if (has_routes) {
    if (!inherits(routes, "sf")) stop("points should be an sf object")
    if (!all(st_geometry_type(routes) %in% c("LINESTRING", "MULTILINESTRING"))) stop("geometry type of points should be LINESTRING or MULTILINESTRING")

    routes_cols <- setdiff(names(routes), attr(routes, "sf_column"))[1:2]
    routes <- routes[, routes_cols]
    names(routes) <- c("orig", "dest", "geometry")
    attr(routes, "sf_column") <- "geometry"
  }

  structure(list(od = od, points = points, routes = routes), class = "odf")
}

# odf_via <- function(...) {
#   args <- list(...)
#   x <- do.call(c, args)
#
#   structure(x, class = "odf_via")
# }

