# @export
List <- function(lat) {
  as_List(lat)
}

# @export
as_List <- function(x) {
  structure(x, class = "List")
}

# @export
c.List <- function(x, ...) {
  as_List(NextMethod())
}

# @export
`[.List` <- function(x, i) {
  as_List(NextMethod())
}

# @export
format.List <- function(x, ...) {
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
  List(res)
}

add_odvia <- function(od) {
  od$odvia <- List(mapply(function(...) {
    args <- list(...)
    if (is.factor(args[[1]])) {
      lvls <- levels(args[[1]])
      x <- factor(do.call(c, args), levels = 1:length(lvls), labels = lvls)
    } else {
      x <- do.call(c, args)
    }
    x
  }, od$orig, od$via, od$dest, SIMPLIFY = FALSE))
  od
}

odf_add_place_names <- function(od, points) {
  odvia <- List(mapply(c, od$orig, od$via, od$dest, SIMPLIFY = FALSE))
  lapply(odvia, function(v) {
    if (is.factor(od$orig)) {
      points$name[v]
    } else {
      points$name[match(v, points$id)]
    }
  })
}



#' @export
print.List <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

