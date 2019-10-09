#' Add lines (curved or straight) to an odf object
#'
#' Add lines (curved or straight) to an odf object
#'
#' @param x odf object
#' @param via use via routes
#' @param angle angle
#' @param crs crs
#' @param points_per_line points_per_line
odf_add_lines <- function(x, via = TRUE, angle = 1/24*pi, crs = 3857, points_per_line = 100) {
  od_original <- x$od
  p_original <- x$points
  crs_original <- st_crs(p_original)

  x <- odf_remove_type(x) # since we do not distinguish between types

  od <- x$od

  p <- st_transform(x$points, crs = crs)

  od <- add_odvia(od)

  od$geometry <- st_sfc(lapply(od$odvia, create_lines, p, angle, points_per_line, via), crs = st_crs(p))

  od <- st_as_sf(od[, c("orig", "dest", "via", "geometry")])

  routes <- st_transform(od, crs = crs_original)

  structure(list(od = od_original, points = p_original, routes = routes), class = "odf")
}


create_lines <- function(s, p, angle, points_per_line, via) {
  l <- length(s)
  ls <- if (via) {
    1:(l-1)
  } else {
    c(1, l-1)
  }
  res <- lapply(ls, function(i) {
    co <- create_line(s[i], s[i+1], p, angle, points_per_line)
  })
  st_multilinestring(res)
}

create_line <- function(i, j, p, angle, points_per_line) {
  p1 <- st_coordinates(p$geometry[p$id == i])
  p2 <- st_coordinates(p$geometry[p$id == j])

  x1 <- p1[1]
  y1 <- p1[2]

  x2 <- p2[1]
  y2 <- p2[2]

  if (abs(angle) < 1e-6) {
    # straight lines
    fs <- seq(0, 1, length.out = points_per_line)

    x <- sapply(fs, function(f) {
      (x1 * (f-1) + x2 * f) / 2
    })
    y <- sapply(fs, function(f) {
      (y1 * (f-1) + y2 * f) / 2
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

}

