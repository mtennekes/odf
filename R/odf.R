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
#' @param col_via col_via
#' @param col_type col_type
#' @param col_time col_time
#' @export
#' @import sf
odf <- function(od, points, routes = NULL, col_orig = 1, col_dest = 2, col_flow = 3, col_via = NULL, col_type = NULL, col_time = NULL) {


  # check points
  if (!inherits(points, "sf")) stop("points should be an sf object")
  if (!all(st_geometry_type(points) == "POINT")) stop("geometry type of points should be POINT")

  points_cols <- setdiff(names(points), attr(points, "sf_column"))[1:2]
  points <- points[, points_cols]
  names(points) <- c("id", "name", "geometry")
  attr(points, "sf_column") <- "geometry"
  if (is.factor(points$id) || !inherits(points$id, c("character", "numeric", "integer"))) {
    points$id <- as.character(points$id)
  }
  points_num <- is.numeric(points$id)



  # check od
  if (!is.data.frame(od)) stop("od is not a data.frame")
  nms <- names(od)
  has_type <- !missing(col_type)
  has_time <- !missing(col_time)
  has_via <- !missing(col_via)

  # check od columns
  if (is.numeric(col_orig)) col_orig <- nms[col_orig]
  if (is.numeric(col_dest)) col_dest <- nms[col_dest]
  if (is.numeric(col_flow)) col_flow <- nms[col_flow]

  if (has_type) {
    if (is.numeric(col_type)) col_type <- nms[col_type]
    if (!(col_type %in% nms)) stop("column ", col_type, " not found")

    if (!is.factor(od[[col_type]])) {
      od[[col_type]] <- as.factor(od[[col_type]])
    }
  }
  if (has_time) warning("time column not supported yet")

  if (!(col_orig %in% nms)) stop("column ", col_orig, " not found")
  if (!(col_dest %in% nms)) stop("column ", col_dest, " not found")
  if (!(col_flow %in% nms)) stop("column ", col_flow, " not found")



  check_od_col <- function(col, name) {
    if (is.null(col)) return(NULL)
    if (points_num) {
      if (!is.numeric(col)) stop("the column ", name, " should be integer/numeric (like the points id column)")
    } else {
      if (!is.factor(col)) {
        col <- factor(col, levels = points$id)
      } else if (!all(levels(col) == points$id)) {
        stop("levels of column ", name, " do not correspond to the points id column")
      }
    }
    col
  }

  od[[col_orig]] <- check_od_col(od[[col_orig]], "orig")
  od[[col_dest]] <- check_od_col(od[[col_dest]], "dest")


  if (!inherits(od[[col_flow]], "numeric")) stop("the column flow should be an integer/numeric")

  if (has_via) {
    if (!(col_via %in% nms)) stop("column ", col_via, " not found")
    if (is.numeric(col_via)) col_via <- nms[col_via]

    if (!is.list(od[[col_via]])) stop("the column via should be a list")
    od[[col_via]] <- lapply(od[[col_via]], function(x) {
      check_od_col(x, "via")
    })
    col_via_name <- col_via
  } else {
    od$VIA__ <- lapply(1:nrow(od), function(i) return(NULL))
    col_via <- "VIA__"
    col_via_name <- "<unspecified>"
  }
  od[[col_via]] <- List(od[[col_via]])

  if (has_type) {
    od <- od[, c(col_orig, col_dest, col_via, col_flow, col_type)]
    names(od) <- c("orig", "dest", "via", "flow", "type")
    attr(od, "original_names") <- c(col_orig, col_dest, col_via_name, col_flow, col_type)
  } else {
    od <- od[, c(col_orig, col_dest, col_via, col_flow)]
    names(od) <- c("orig", "dest", "via", "flow")
    attr(od, "original_names") <- c(col_orig, col_dest, col_via_name, col_flow)
  }

  # check if all ids in orig/dest are in points
  if (points_num) {
    if (!all(od$orig %in% points$id)) stop("not all orig ids in od are contains in points")
    if (!all(od$dest %in% points$id)) stop("not all dest ids in od are contains in points")
  } else {
    if (!all(levels(od$orig) %in% points$id)) stop("not all orig ids in od are contains in points")
    if (!all(levels(od$dest) %in% points$id)) stop("not all dest ids in od are contains in points")
  }

  # check if all ids in via are in points
  vias <- unlist(od$via)
  if (!is.null(vias)) {
    if (points_num) {
      lapply(vias, function(v) {
        if (!all(v %in% points$id)) stop("not all via ids in od are contains in points")
      })
    } else {
      lapply(vias, function(v) {
        if (!all(levels(v) %in% points$id)) stop("not all via ids in od are contains in points")
      })

    }
  }

  # remove o=d points
  if (any(od$orig == od$dest)) {
    warning("OD data contains inner flows (e.g. orig=dest), which are ignored")
    od <- od[!(od$orig == od$dest), ]
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

    if (points_num) {
      if (!all(routes$orig %in% points$id)) stop("not all orig ids in od are contains in routes")
      if (!all(routes$dest %in% points$id)) stop("not all dest ids in od are contains in routes")
    } else {
      if (!all(levels(routes$orig) %in% points$id)) stop("not all orig ids in od are contains in routes")
      if (!all(levels(routes$dest) %in% points$id)) stop("not all dest ids in od are contains in routes")
    }
  }

  structure(list(od = od, points = points, routes = routes), class = "odf")
}

