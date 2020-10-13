print.od <- function(x, ...) {
  cat("Origin-destination (od) data object, which is a list of two elements:\n")

  printdf(x$U, "$ U: the nodes")
  cat(paste0("node id column: \"", od_id(x), "\"\n"))

  printdf(x$E, "$ E: the edges")
  cat(paste0("origin column: \"", od_o(x), "\"; destination column: \"", od_d(x), "\"\n"))
}

printdf <- function(x, y) {
  if (inherits(x, "sf")) {
    crs <- st_crs(x)
    crs_text <- if (is.na(crs)) {
      "NA"
    } else if (!is.na(crs$epsg)) {
      paste0("EPSG ", crs$epsg)
    } else if (!is.na(crs$proj4string)) {
      paste0("proj4string \"", crs$proj4string, "\"")
    } else {
      crs$wkt
    }
    prj <- paste("CRS:", crs_text)
    x <- as.data.frame(x)
  } else {
    x <- as.data.frame(x)
    prj <- NA
  }

  if (nrow(x) > 6) {
    cat(paste0("\n", y, " (", nrow(x), " rows, printing first 6 rows)\n"))
    print(x[1:6,])
  } else {
    cat(paste0("\n", y, "\n"))
    print(x)
  }
  if (!is.na(prj)) cat(paste0(prj, "\n"))
}
