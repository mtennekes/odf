#' Create origin-destination flows (odf) object
#'
#' Create origin-destination flows (odf) object, which is a list consisting of a data.frame with the od data, an sf object with spatial points and optionally, an sf object with spatial (poly)lines of the routes
#'
#' @param E `data.frame` or `sf` object of edges. If it is an `sf`, it should contain (MULTI)LINESTRING geometries
#' @param U `sf` object of nodes. It should contain `POINT` geometry.
#' @param col_orig column of E (integer or name) that specifies the origin
#' @param col_dest column of E (integer or name) that specifies the destination
#' @param col_id column of U (integer or name) that specifies the id
#' @export
#' @import sf
od <- function(E, U = NULL, col_orig = 1, col_dest = 2, col_id = 1) {

  E <- precheck_E(E, col_orig = col_orig, col_dest = col_dest)
  if (is.null(U)) U <- get_od_endpoints(E, U)

  U <- check_U(U, col_id)
  check_E(E, U)

  col_id <- od_id(U)
  col_dest

  # move attributes from U and E to od object
  col_orig <- od_o(E)
  col_dest <- od_d(E)
  col_id <- od_id(U)

  attr(E, "od_orig") <- NULL
  attr(E, "od_dest") <- NULL
  attr(U, "od_id") <- NULL

  od <- structure(list(U = U, E = E), class = "od", od_id = col_id, od_orig = col_orig, od_dest = col_dest)
}

precheck_E <- function(E, col_orig, col_dest) {
  # check E
  if (!is.data.frame(E)) stop("E is not a data.frame")
  if (inherits(E, "sf") && !all(st_geometry_type(E) %in% c("LINESTRING", "MULTILINESTRING"))) stop("geometry type of E should be (MULTI)LINESTRING")

  nms <- names(E)

  # check E columns
  if (is.numeric(col_orig)) col_orig <- nms[col_orig]
  if (is.numeric(col_dest)) col_dest <- nms[col_dest]

  if (!(col_orig %in% nms)) stop("column ", col_orig, " not found")
  if (!(col_dest %in% nms)) stop("column ", col_dest, " not found")
  if (!acc_cls(E[[col_orig]])) stop("col_orig column of E should be a Factor, character, or integer)")
  if (!acc_cls(E[[col_dest]])) stop("col_dest column of E should be a Factor, character, or integer)")

  E[[col_orig]] <- num_to_int(E[[col_orig]])
  E[[col_dest]] <- num_to_int(E[[col_dest]])
  if (!comp_cols(E[[col_orig]], E[[col_dest]])) stop("col_orig and col_dest columns of E should be the same class and have the same levels (if they are factors))")

  attr(E, "od_orig") <- col_orig
  attr(E, "od_dest") <- col_dest

  E
}

check_U <- function(U, col_id) {
  if (!inherits(U, "sf")) stop("U should be an sf object")
  if (!all(st_geometry_type(U) %in% c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON"))) stop("The geometry type of U should be (MULTI)POINT or (MULTIPOLYGON")
  nms <- names(U)

  if (is.numeric(col_id)) col_id <- nms[col_id]
  if (!acc_cls(U[[col_id]])) stop("col_id column of points should be a Factor, character, or integer)")
  U[[col_id]] <- num_to_int(U[[col_id]])
  if (!check_unique(U[[col_id]])) stop("column col_id of U contains duplicated values")

  attr(U, "od_id") <- col_id
  U
}

check_E <- function(E, U) {
  col_orig <- attr(E, "od_orig")
  col_dest <- attr(E, "od_dest")
  col_id <- attr(U, "od_id")
  if (!comp_cols(E[[col_orig]], U[[col_id]])) stop("columns col_orig/col_dest of E and column col_id of U should be the same class and have the same levels (if they are factors)")

  if (!col_subset(E[[col_orig]], U[[col_id]])) stop("column col_orig of E contains values that are not in col_id of U")
  if (!col_subset(E[[col_dest]], U[[col_id]])) stop("column col_dest of E contains values that are not in col_id of U")
}

comp_cols <- function(x, y) {
  if (!identical(class(x), class(y))) {
    FALSE
  } else if (is.factor(x) && !identical(levels(x), levels(y))) {
    FALSE
  } else TRUE
}
acc_cls <- function(x) inherits(x, c("Factor", "character", "integer")) || (inherits(x, "numeric") && all(x %% 1 == 0))

num_to_int <- function(x, name = NULL) {
  if (inherits(x, "numeric")) {
    if (!is.null(name)) message(name, " has been cast from numeric to integer")
    as.integer(x)
  } else {
    x
  }
}


check_unique <- function(...) {
  args <- list(...)
  x <- do.call(paste, args)
  anyDuplicated(x) == 0L
}

col_subset <- function(x, y) all(x %in% y)

od_id <- function(x) attr(x, "od_id")
od_o <- function(x) attr(x, "od_orig")
od_d <- function(x) attr(x, "od_dest")


