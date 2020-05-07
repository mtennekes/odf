od_sum <- function(x, value, direction = c("in", "out"), name = paste(value, direction, sep = "_")) {
  E <- x$E
  U <- x$U

  col_i <- od_id(x)
  col_o <- od_d(x)
  col_d <- od_o(x)

  if ("in" %in% direction) {
    name_in <- name[match("in", direction)]
    Ein <- E %>%
      sf::st_drop_geometry() %>%
      group_by_at(vars(col_o)) %>%
      summarize(!!name_in := sum(!!sym(value))) %>%
      ungroup()
  }


  if ("out" %in% direction) {
    name_out <- name[match("out", direction)]
    Eout <- E %>%
      sf::st_drop_geometry() %>%
      group_by_at(vars(col_d)) %>%
      summarize(!!name_out := sum(!!sym(value))) %>%
      ungroup()
  }

  if ("in" %in% direction) {
    U <- U %>%
      left_join(Ein, by = setNames(col_o, col_i)) %>%
      mutate(!!name_in := replace_na(!!sym(name_in), 0))
  }

  if ("out" %in% direction) {
    U <- U %>%
      left_join(Eout, by = setNames(col_d, col_i)) %>%
      mutate(!!name_out := replace_na(!!sym(name_out), 0))
  }

  x$U <- U
  x
}




od_sum_in <- function(x, value, name = paste(value, "in", sep = "_")) {
  od_sum(x = x, value = value, direction = "in", name = name)
}

od_sum_out <- function(x, value, name = paste(value, "out", sep = "_")) {
  od_sum(x = x, value = value, direction = "out", name = name)
}
