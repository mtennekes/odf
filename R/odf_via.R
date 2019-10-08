#' @export
odf_via <- function(lat) {
  as_odf_via(lat)
}

#' @export
as_odf_via <- function(x) {
  structure(x, class = "odf_via")
}

#' @export
c.odf_via <- function(x, ...) {
  as_odf_via(NextMethod())
}

#' @export
`[.odf_via` <- function(x, i) {
  as_odf_via(NextMethod())
}

#' @export
format.odf_via <- function(x, ...) {
  via2chr(x)
}


via2chr <- function(x) {
  unlist(lapply(x, function(y) ifelse(is.null(y), "", paste(format(y), collapse = ","))))
}

chr2via <- function(x) {
  res <- lapply(x, function(v) {
    if (v == "") {
      NULL
    } else {
      as.integer(unlist(strsplit(v, split = ",", fixed = TRUE)))
    }
  })
  odf_via(res)
}

add_odvia <- function(od) {
  od$odvia <- odf_via(mapply(c, od$orig, od$via, od$dest, SIMPLIFY = FALSE))
  od
}

odf_add_place_names <- function(od, points) {
  odvia <- odf_via(mapply(c, od$orig, od$via, od$dest, SIMPLIFY = FALSE))
  lapply(odvia, function(v) {
    points$name[match(v, points$id)]
  })
}



#' @export
print.odf_via <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

