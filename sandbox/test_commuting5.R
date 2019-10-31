## test one row perspective

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")

# Create odf object
x <- odf(od, points, col_flow = "value", col_type = "mode")

library(tidyverse)
library(sf)
library(tmap)
library(igraph)

# extract spatial points (sf) object from odf object
map_points <- odf_points(x)

# extract spatial lines (sf) objects, disregarding type
map_flows <- odf_flows(x, by_type = FALSE, by_via = TRUE)

m <- map_flows %>%
  filter(orig == "GM0935")

m_x <- st_coordinates(map_points$geometry[map_points$id == "GM0935"])[1]
m_y <- st_coordinates(map_points$geometry[map_points$id == "GM0935"])[2]

mdf <- m %>%
  mutate(geometry = map_points$geometry[match(dest, map_points$id)],
         label = map_points$name[match(dest, map_points$id)],
         orig = NULL)
mdf2 <- mdf %>%
  bind_cols(as.data.frame(st_coordinates(mdf))) %>%
  #st_set_geometry(NULL) %>%
  mutate(dist = sqrt((m_x - X) ^ 2 + (m_y - Y) ^ 2),
         angle = atan2(X - m_x, Y - m_y),
         dist_log = log10(dist),
         sina = sin(angle),
         cosa = cos(angle))


md <- scale(st_drop_geometry(mdf2[, c("dist_log", "sina", "cosa")])) #
md <- scale(st_drop_geometry(mdf2[, c("X", "Y", "sina", "cosa")])) #



# determine number of clusters
library(NbClust)
res <- NbClust(data = md, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")

mdf2$clustering <- kmeans(md, centers = 11)$cluster

tm_shape(mdf2) +
  tm_bubbles(size = "flow", col = "clustering", style = "cat", palette = "cat")


f <- odf::get_flows(x)
cs <- colSums(f)
rs <- rowSums(f)

plot(cs, rs)


map_points$ratio <- rs / cs
map_points$ave <- (cs + rs) / 2
map_points$rs <- rs
map_points$cs <- cs

tm_shape(map_points) +
  tm_bubbles(col = "ratio", size = "ave", style = "kmeans")

tm_shape(map_points) +
  tm_bubbles(size = "rs", style = "kmeans")

tm_shape(map_points) +
  tm_bubbles(size = "cs", style = "kmeans")

