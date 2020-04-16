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

pnts <- odf_points(x)
flws <- odf_flows(x)

f <- get_flows(x)

flws2 <- flws %>%
  filter(orig == "GM0935")

tmap_mode("view")

tm_shape(flws2) +
  tm_lines(lwd = "flow", scale = 10)

pnts$flow <- f["GM0935",]


tm_shape(pnts) +
  tm_bubbles(size = "flow", scale = 1)

