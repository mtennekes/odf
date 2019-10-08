print.odf <- function(x, ...) {
  cat("Origin-destination flows (odflows) object\n")

  od <- x$od
  #od$via <- odf_via(od$via)
  p <- x$points
  r <- x$routes

  printdf(od, "Data")
  printdf(p, "Points")
  printdf(r, "Routes")
}

printdf <- function(x, y) {
  if (inherits(x, "sf")) {
    crs <- st_crs(x)
    epsg <- ifelse(is.na(crs$epsg), "", paste0("epsg ", crs$epsg, ", "))
    proj <- crs$proj4string

    prj <- paste0("CRS: ", epsg, "proj4 \"", proj, "\"\n")
    x <- as.data.frame(x)
  } else {
    prj <- NA
  }

  if (is.null(x)) {
    cat(paste0("\n", y, ": not specified\n"))
  } else if (nrow(x) > 10) {
    cat(paste0("\n", y, " (", nrow(x), " rows, printing first 10 rows)\n"))
    print(x[1:10,])
  } else {
    cat(paste0("\n", y, "\n"))
    print(x)
  }
  if (!is.na(prj)) cat(prj)
}


