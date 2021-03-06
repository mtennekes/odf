od_sum <- function(x, value, direction = c("in", "out", "stay"), name = paste(value, direction, sep = "_")) {

  E <- x$E
  if (inherits(E, "sf")) E <- sf::st_drop_geometry(E)
  U <- x$U

  col_i <- od_id(x)
  col_d <- od_d(x)
  col_o <- od_o(x)

  if ("in" %in% direction) {
    name_in <- name[match("in", direction)]
    Ein <- E %>%
      filter(!!sym(col_o) != !!sym(col_d)) %>%
      group_by_at(vars(col_d)) %>%
      summarize(!!name_in := sum(!!sym(value))) %>%
      ungroup()
  }


  if ("out" %in% direction) {
    name_out <- name[match("out", direction)]
    Eout <- E %>%
      filter(!!sym(col_o) != !!sym(col_d)) %>%
      group_by_at(vars(col_o)) %>%
      summarize(!!name_out := sum(!!sym(value))) %>%
      ungroup()
  }

  if ("stay" %in% direction) {
    name_stay <- name[match("stay", direction)]
    Estay <- E %>%
      filter(!!sym(col_o) == !!sym(col_d)) %>%
      rename(!!name_stay := sym(value)) %>%
      select(sym(col_o), sym(name_stay))
  }



  if ("in" %in% direction) {
    U <- U %>%
      left_join(Ein, by = setNames(col_d, col_i)) %>%
      mutate(!!name_in := replace_na(!!sym(name_in), 0))
  }

  if ("out" %in% direction) {
    U <- U %>%
      left_join(Eout, by = setNames(col_o, col_i)) %>%
      mutate(!!name_out := replace_na(!!sym(name_out), 0))
  }

  if ("stay" %in% direction) {
    U <- U %>%
      left_join(Estay, by = setNames(col_o, col_i)) %>%
      mutate(!!name_stay := replace_na(!!sym(name_stay), 0))
  }

  x$U <- U
  x
}



#' Sum of OD flows
#'
#' Sum of OD flows.
#'
#' @param x \code{od} object
#' @param value names of the variable in x$E that contain the values to be summed
#' @param name name of the newly created variable of sums in x$U
#' @name od_sum_in
#' @rdname od_sum
#' @export
od_sum_in <- function(x, value, name = paste(value, "in", sep = "_")) {
  od_sum(x = x, value = value, direction = "in", name = name)
}

#' @name od_sum_out
#' @rdname od_sum
#' @export
od_sum_out <- function(x, value, name = paste(value, "out", sep = "_")) {
  od_sum(x = x, value = value, direction = "out", name = name)
}

#' @name od_sum_stay
#' @rdname od_sum
#' @export
od_sum_stay <- function(x, value, name = paste(value, "stay", sep = "_")) {
  od_sum(x = x, value = value, direction = "stay", name = name)
}
