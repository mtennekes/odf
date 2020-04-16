devtools::load_all()
library(tidyverse)
library(sf)
library(tmap)
library(treemap)

# test hierarchical coloring & tree colors

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od_with_diag.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")

polys <-readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_polys.rds")


# Create odf object
x <- odf(od, points, col_flow = "value", col_type = "mode")

# Create hex grid

g <- sf::st_make_grid(polys, square = FALSE, cellsize = 1000)

isec <- st_intersects(st_centroid(g), polys)
wisec <- (sapply(isec, length) > 0)

g <- g[wisec]

