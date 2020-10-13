#' Add lines (curved or straight) to an odf object
#'
#' Add lines (curved or straight) to an odf object
#'
#' @param x od object
#' @param angle angle
#' @param points_per_line points_per_line
#' @param range range
#' @param trunc trunc
#' @param min_trunc_dist = min_trunc_dist
#' @param overwrite.geometry overwrite.geometry
#' @param points_per_line points_per_line
#' @export
od_add_lines <- function(x, angle = 1/24*pi, points_per_line = ifelse(angle == 0, 2, 100), range = c(0, 1), trunc = units::set_units(c(0, 0), "m"), min_trunc_dist = units::set_units(5000, "m"), overwrite.geometry = FALSE) {

  od_is_valid(x)

  E <- x$E
  U <- x$U

  col_i <- od_id(x)
  col_o <- od_o(x)
  col_d <- od_d(x)

  crs <- sf::st_crs(U)

  if (inherits(E, "sf")) {
    if (!overwrite.geometry) stop("E already has a sf column. Set overwrite.geometry = TRUE to overwrite it.")
    sfcol <- attr(E, "sf_column")
    E <- sf::st_drop_geometry(E)
  } else {
    sfcol <- "geometry"
  }

  len <- range[2] - range[1]
  #points_per_line <- round(points_per_line / len)

  pO <- st_geometry(U)[match(E[[col_o]], U[[col_i]])]
  pD <- st_geometry(U)[match(E[[col_d]], U[[col_i]])]

  mpoints <- mapply(c, pO, pD, SIMPLIFY = FALSE)

  sfc <- sf::st_sfc(lapply(mpoints, create_lines, angle, points_per_line), crs = crs)

  #od$geometry <- sf::st_sfc(lapply(od$odvia, create_lines, p, angle, points_per_line, via), crs = st_crs(p))

  if (range[1] != 0 || range[2] != 0) {
    if (!all(sf::st_geometry_type(sfc) == "LINESTRING")) {
      warning("range other than c(0, 1) only supports single lines (so no multiline-routes)")
    } else {
      lns <- sf::st_length(sfc)

      d1 <- range[1] * lns
      d2 <- range[2] * lns

      delta <- d2 - d1

      d1[delta > (min_trunc_dist + sum(trunc))] <- d1[delta > (min_trunc_dist + sum(trunc))] + trunc[1]
      d2[delta > (min_trunc_dist + sum(trunc))] <- d2[delta > (min_trunc_dist + sum(trunc))] - trunc[2]


      range1 <- as.numeric(d1 / lns)
      range2 <- as.numeric(d2 / lns)

      sfc <- sf::st_sfc(mapply(function(l, r1, r2) {
        lwgeom::st_linesubstring(l, r1, r2)
      }, sfc, range1, range2, SIMPLIFY = FALSE), crs = crs)
    }
  }

  E[[sfcol]] <- sfc

  if (!inherits(E, "sf")) E <- sf::st_as_sf(E)

  x$E <- E
  x
}


create_lines <- function(mp, angle, points_per_line) {
  co <- st_coordinates(mp)

  nr <- nrow(co)

  1:(nr-1L)


  res <- lapply(1:(nr-1L), function(i) {
    co <- create_line(co[i, ], co[i+1, ], angle, points_per_line)
  })

  if (length(res)==1) {
    st_linestring(res[[1]])
  } else {
    st_multilinestring(res)
  }
}

create_line <- function(p1, p2, angle, points_per_line) {
  x1 <- p1[1]
  y1 <- p1[2]

  x2 <- p2[1]
  y2 <- p2[2]

  m <- if (abs(angle) < 1e-6) {
    # straight lines
    fs <- seq(0, 1, length.out = points_per_line)

    x <- sapply(fs, function(f) {
      x1 + (x2 - x1) * f
    })
    y <- sapply(fs, function(f) {
      y1 + (y2 - y1) * f
    })

    matrix(c(x,y), ncol = 2, byrow = FALSE)

  } else {



    dir <- atan2((x2-x1), (y2-y1))
    dir2 <- dir-.5*pi + angle
    diff <- sqrt((y2-y1)^2 + (x2-x1)^2)
    r <- (diff/2) / cos(.5*pi - angle)
    xc <- x1 + sin(dir2) * r
    yc <- y1 + cos(dir2) * r

    angles <- seq(angle, -angle, length.out = points_per_line)
    #m <- as.matrix(y3[, c("xc", "yc", "dir", "r")])
    #res <- apply(m, MARGIN = 1, FUN = function(r) {
    xs <- sapply(angles, function(a) {
      xc + r * sin(dir + .5*pi + a)
    })
    ys <- sapply(angles, function(a) {
      yc + r * cos(dir + .5*pi + a)
    })
    matrix(c(xs,ys), ncol = 2, byrow = FALSE)
  }

  # id1 <- max(1, floor(points_per_line * range[1]))
  # id2 <- min(points_per_line, ceiling(points_per_line * range[2]))
  #
  # m[id1:id2, ]
}

