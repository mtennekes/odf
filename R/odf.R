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
#' @param remove_diag remove_diag
#' @export
#' @import sf
od <- function(E, U = NULL, col_orig = 1, col_dest = 2, col_flow = NULL, col_type = NULL, col_id = 1, remove_diag = FALSE) {

  E <- precheck_E(E, col_orig = col_orig, col_dest = col_dest, col_flow = col_flow, col_type = col_type)

  if (inherits(E, "sf")) {
    if (!all(st_geometry_type(E) %in% c("LINESTRING", "MULTILINESTRING"))) stop("geometry type of points should be (MULTI)LINESTRING")
    if (is.null(U)) U <- get_od_endpoints(E, U)
  } else {
    if (is.null(U)) stop("Either specify U, or let E be an sf object with lines.")
  }


  U <- check_U(U, col_id)


  E <- check_E(E, U)




  if (!("id" %in% names(points))) {
    points$id <- 1L:nrow(points)
  } else {
    if (!inherits(points$id, c("Factor", "character", "numeric", "integer"))) {
      stop("\"id\" column of points should be a Factor, character, or integer)")
    }
    if (is.character(points$id)) {
      points$id <- as.factor(points$id)
    } else if (is.numeric(points$id) && !is.integer(points$id)) {
      points$id <- as.integer(points$id)
    }
    if (anyDuplicated(points$id)) {
      stop("\"id\" column contains duplicated values")
    }
  }
  points_num <- is.integer(points$id)


  check_od_col <- function(col, name) {
    if (is.null(col)) return(NULL)

    if (points_num) {
      if (!is.numeric(col)) stop("the column ", name, " should be integer/numeric (like the points id column)")
    } else {
      if (!is.factor(col)) {
        col <- factor(col, levels = levels(points$id))
      } else if (!all(levels(col) == levels(points$id))) {
        stop("levels of column ", name, " do not correspond to the levels of points id column")
      }
    }
    col
  }

  od[[col_orig]] <- check_od_col(od[[col_orig]], "orig")
  od[[col_dest]] <- check_od_col(od[[col_dest]], "dest")




  # check if all ids in orig/dest are in points
  if (!all(od$orig %in% points$id)) stop("not all orig ids in od are contains in points")
  if (!all(od$dest %in% points$id)) stop("not all dest ids in od are contains in points")

  # remove o=d points
  if (any(od$orig == od$dest) && remove_diag) {
    warning("OD data contains inner flows (e.g. orig=dest), which are ignored")
    od <- od[!(od$orig == od$dest), ]
  }

  # # check routes
  # has_routes <- !missing(routes)
  # if (has_routes) {
  #   if (!inherits(routes, "sf")) stop("points should be an sf object")
  #   if (!all(st_geometry_type(routes) %in% c("LINESTRING", "MULTILINESTRING"))) stop("geometry type of points should be LINESTRING or MULTILINESTRING")
  #
  #
  #   routes[[col_orig]] <- check_od_col(od[[col_orig]], "orig")
  #   od[[col_dest]] <- check_od_col(od[[col_dest]], "dest")
  #
  #
  #
  #   routes_cols <- setdiff(names(routes), attr(routes, "sf_column"))[1:2]
  #   routes <- routes[, routes_cols]
  #   names(routes) <- c("orig", "dest", "geometry")
  #   attr(routes, "sf_column") <- "geometry"
  #
  #   if (points_num) {
  #     if (!all(routes$orig %in% points$id)) stop("not all orig ids in od are contains in routes")
  #     if (!all(routes$dest %in% points$id)) stop("not all dest ids in od are contains in routes")
  #   } else {
  #     if (!all(levels(routes$orig) %in% points$id)) stop("not all orig ids in od are contains in routes")
  #     if (!all(levels(routes$dest) %in% points$id)) stop("not all dest ids in od are contains in routes")
  #   }
  # }

  structure(list(od = od, points = points), class = "od")
}

precheck_E <- function(E, col_orig, col_dest, col_flow, col_type) {
  # check E
  if (!is.data.frame(E)) stop("E is not a data.frame")
  nms <- names(E)

  # check E columns
  if (is.numeric(col_orig)) col_orig <- nms[col_orig]
  if (is.numeric(col_dest)) col_dest <- nms[col_dest]

  has_flow <- !is.null(col_flow)
  has_type <- !is.null(col_type)

  if (has_flow) {
    if (is.numeric(col_flow)) col_flow <- nms[col_flow]
    if (!(col_flow %in% nms)) stop("column ", col_flow, " not found")
    if (!is.numeric(E[[col_flow]])) stop("the column flow should be an integer/numeric")
  }

  if (has_type) {
    if (is.numeric(col_type)) col_type <- nms[col_type]
    if (!(col_type %in% nms)) stop("column ", col_type, " not found")
    if (!is.factor(E[[col_type]])) E[[col_type]] <- as.factor(E[[col_type]])
  }

  if (!(col_orig %in% nms)) stop("column ", col_orig, " not found")
  if (!(col_dest %in% nms)) stop("column ", col_dest, " not found")

  attr(E, "od_orig") <- col_orig
  attr(E, "od_dest") <- col_dest
  if (has_flow) attr(E, "od_flow") <- col_flow
  if (has_type) attr(E, "od_type") <- col_type

  E
}

check_U <- function(U, col_id) {
  if (!inherits(U, "sf")) stop("U should be an sf object")
  if (!all(st_geometry_type(U) == "POINT")) stop("The geometry type of U should be POINT")
  nms <- names(U)

  # check U columns
  if (is.numeric(col_id)) col_id <- nms[col_id]

  if (!inherits(U[[col_id]], c("Factor", "character", "numeric", "integer"))) {
    stop("col_id column of points should be a Factor, character, or integer)")
  }
  if (is.character(U[[col_id]])) {
    U[[col_id]] <- as.factor(U[[col_id]])
  } else if (is.numeric(U[[col_id]]) && !is.integer(U[[col_id]])) {
    U[[col_id]] <- as.integer(U[[col_id]])
  }
  if (anyDuplicated(U[[col_id]])) {
    stop("\"id\" column contains duplicated values")
  }
  attr(U, "od_id") <- cold_id
  U
}

check_E <- function(E, U) {

}


od_id <- function(od) attr(od, "od_id")
od_o <- function(od) attr(od, "od_orig")
od_d <- function(od) attr(od, "od_dest")
od_f <- function(od) attr(od, "od_flow")
od_t <- function(od) attr(od, "od_type")


