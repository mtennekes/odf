#' Checck validity of od objects
#'
#' Checks whether an od object is valid. See details.
#'
#' A valid `od` object should: contain two list items, `U` and `E`, and three attributes `od_id`, `od_orig`, and `od_dest`. The values of these attributes are column names of `U` (`od_id`) and `E` (`od_orig` and `od_dest`). `U` should be an `sf` object (points or polygons), and `E` a data.frame (optionally an sf object (lines).
#'
#' @param x od object
#' @param verbose should messages be returned about possible invalidations? TRUE by default.
#' @export
od_is_valid <- function(x, verbose = TRUE) {
  name <- deparse(substitute(x))[1]

  if (!inherits(x, "od")) {
    if (verbose) message(name, " is not an od object")
    FALSE
  } else if (!all(c("E", "U") %in% names(x))) {
    if (verbose) message("E and/or U are missing")
    FALSE
  } else if (!all(c("od_id", "od_orig", "od_dest") %in% names(attributes(x)))) {
    if (verbose) message("not all required attributes are found: \"od_id\", \"od_orig\", \"od_dest\"")
    FALSE
  } else {
    U <- x$U
    E <- x$E

    col_id <- od_id(x)
    col_orig <- od_o(x)
    col_dest <- od_d(x)

    if (!inherits(U, "sf")) {
      if (verbose) message("U is not an sf object")
      FALSE
    } else if (!all(st_geometry_type(U) %in% c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON"))) {
      if (verbose) message("geometry type of U should be (MULTI)POINT or (MULTI)POLYGON")
      FALSE
    } else if (!inherits(E, "data.frame")) {
      if (verbose) message("E is not a data.frame")
      FALSE
    } else if (inherits(E, "sf") && !all(st_geometry_type(E) %in% c("LINESTRING", "MULTILINESTRING"))) {
      if (verbose) message("geometry type of E should be (MULTI)LINESTRING")
      FALSE
    } else if (!col_id %in% names(U)) {
      if (verbose) message(col_id, " is not a column in U")
      FALSE
    } else if (!col_orig %in% names(E)) {
      if (verbose) message(col_orig, " is not a column in E")
      FALSE
    } else if (!col_dest %in% names(E)) {
      if (verbose) message(col_dest, " is not a column in E")
      FALSE
    } else if (!acc_cls(U[[col_id]])) {
      if (verbose) message(col_id, " in U is not a factor, character or integer")
      FALSE
    } else if (!acc_cls(E[[col_orig]])) {
      if (verbose) message(col_orig, " in E is not a factor, character or integer")
      FALSE
    } else if (!acc_cls(E[[col_dest]])) {
      if (verbose) message(col_dest, " in E is not a factor, character or integer")
      FALSE
    } else if (!comp_cols(E[[col_orig]], E[[col_dest]])) {
      if (verbose) message("col_orig and col_dest columns of E should be the same class and have the same levels (if they are factors))")
      FALSE
    } else if (!comp_cols(U[[col_id]], E[[col_orig]])) {
      if (verbose) message("columns col_orig/col_dest of E and column col_id of U should be the same class and have the same levels (if they are factors)")
      FALSE
    } else if (!col_subset(E[[col_orig]], U[[col_id]])) {
      if (verbose) message("column col_orig of E contains values that are not in col_id of U")
      FALSE
    } else if (!col_subset(E[[col_dest]], U[[col_id]])) {
      if (verbose) message("column col_dest of E contains values that are not in col_id of U")
      FALSE
    } else {
      TRUE
    }
  }
}
