## test clustering on od matrix

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od_with_diag.rds")
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

f <- get_flows(x, directional = TRUE)

cs <- colSums(f)
rs <- rowSums(f)

top100_dest <- order(cs, decreasing = TRUE)[1:200]
top100_orig <- order(rs, decreasing = TRUE)[1:200]

dest_rel <- f / rs
orig_rel <- t(f) / cs

f2 <- cbind(dest_rel[, top100_dest], orig_rel[, top100_orig])

f3 <- cbind(f2, st_coordinates(map_points))
f4 <- scale(f3)
f4[,401:402] <- f4[,401:402] * 10


# determine number of clusters
library(NbClust)
res <- NbClust(data = f2, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 30, method = "kmeans")

set.seed(1)

map_points$clustering <- kmeans(f4, centers = 10)$cluster
map_points$flow <- cs + rs

tm_shape(map_points) +
  tm_bubbles(size = "flow", col = "clustering", style = "cat", palette = "cat", stretch.palette = FALSE)


map_flows2 <- map_flows %>%
  mutate(clustering = map_points$clustering[match(map_flows$dest, map_points$id)]) %>%
  filter(flow >= 150)


tm_shape(map_points) +
  tm_bubbles(size = "flow", col = "clustering", style = "cat", palette = "Dark2", stretch.palette = FALSE) +
tm_shape(map_flows2) +
  tm_lines(col = "clustering", lwd = "flow", scale = 20, style = "cat", palette = "Dark2", stretch.palette = FALSE)


## Hierarchical clustering
f4 <- scale(f3)
f4[,401:402] <- f4[,401:402] * 100
res <- hclust(dist(f4))
plot(res)


map_points$clustering5 <- cutree(res, 7)
map_points$clustering25 <- cutree(res, 25)

map_points$flow <- cs + rs

tmap_mode("view")
tm_shape(map_points) +
  tm_bubbles(size = "flow", col = "clustering25", style = "cat", palette = "cat", stretch.palette = FALSE)
