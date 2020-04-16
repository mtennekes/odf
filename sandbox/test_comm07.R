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

# Clustering
f <- get_flows(x, directional = FALSE)
frel <- f/rowSums(f)


map_points <- odf_points(x)
#frel2 <- cbind(frel, scale(st_coordinates(map_points)) * 10)
frel2 <- scale(st_coordinates(map_points))

d <- dist(frel2)
dm <- as.matrix(d)

hc <- hclust(d, method = "ward.D2", members = rowSums(f))

#
map_points$cl1 <- cutree(hc, k = 4)
map_points$cl2 <- cutree(hc, k = 20)
map_points$cl3 <- cutree(hc, k = 390)

# Tree colors
if (FALSE) {
    tp <- treepalette(map_points, index = c("cl1", "cl2", "cl3"), palette.HCL.options = list(hue_fraction = .4)) %>%
    mutate(cl1=as.integer(cl1),
           cl2=as.integer(cl2),
           cl3=as.integer(cl3))

  map_points2 <- map_points %>%
    left_join(tp, by = c("cl1", "cl2", "cl3")) %>%
    select(name, id, cl1, cl2, cl3, HCL.color)

  tm_shape(map_points2) +
    tm_dots(col = "HCL.color", size = .03)
}


# create pdf of k = 3...20
if (FALSE) {
  tms <- lapply(3:20, function(k) {
    map_points$clk <- cutree(hc, k = k)
    tm_shape(map_points) +
      tm_dots(col = "clk", size = .05, style = "cat", palette = "Dark2", title = paste0("k=", k))
  })

  tmap_save(tmap_arrange(tms), filename = "clustering2.pdf", width = 10, height = 10)

}

# show map
if (FALSE) {
  map_points$clk <- cutree(hc, k = 20)
  map_points$flow <- rowSums(f)
  tm_shape(map_points) +
    tm_bubbles(col = "clk", size = "flow", style = "cat", palette = "Dark2", title.col = paste0("k=", 20))
}

####
map_flows <- odf_flows(x)

map_flows2 <- map_flows %>%
  mutate(orig_cl = map_points$cl2[as.integer(orig)],
         dest_cl = map_points$cl2[as.integer(dest)],
         same = (orig_cl == dest_cl))


map_flows_inner <- map_flows2 %>%
  filter(same, flow >= 150)

map_flows_outer <- map_flows2 %>%
  filter(!same, flow >= 150)


tm_shape(map_flows_outer) +
  tm_lines(lwd = "flow", col = "dest_cl", style = "cat", palette = "Set2", scale = 15) +
tm_shape(map_flows_inner) +
  tm_lines(lwd = "flow", col = "dest_cl", style = "cat", palette = "Dark2", scale = 15)

