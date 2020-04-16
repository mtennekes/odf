## test clustering

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


eu <- get_eucl_distances(x)
fl <- get_flows(x)

alpha <- .8
m <- alpha * eu/max(eu) + (1-alpha) * (fl/max(fl))


library(cluster)

plot(sapply(2:40, function(k) {
  pam(m, k=k)$silinfo$avg.width
}), type = "l")

res <- pam(m, k=28)


map_points$cl <- res$clustering

# show map with flows
tm_shape(map_flows %>% filter(flow >= 150)) +
  tm_lines(lwd = "flow", scale = 10, popup.vars = TRUE) +
  tm_shape(map_points) +
  tm_dots("cl", style = "cat", palette = "Dark2", stretch.palette=FALSE)



library(WeightedCluster)
cl <- hclust(as.dist(m), method = "ward", members = rowSums(m))

cl <- wcKMedoids(as.dist(m), 28, weights = rowSums(m))

map_points$cl <- cl$clustering

# show map with flows
tm_shape(map_flows %>% filter(flow >= 150)) +
  tm_lines(lwd = "flow", scale = 10, popup.vars = TRUE) +
  tm_shape(map_points) +
  tm_dots("cl", style = "cat", palette = "Dark2", stretch.palette=FALSE)




# top x cities
topX <- map_flows %>%
  sf::st_set_geometry(NULL) %>%
  group_by(orig) %>%
  summarise(flow = sum(flow)) %>%
  ungroup() %>%
  arrange(desc(flow)) %>%
  head(10)

map_flows2 <- map_flows %>%
  filter(orig %in% topX$orig, dest %in% topX$orig)

map_points2 <- map_points %>%
  filter(id %in% topX$orig)

tmap_mode("view")
tm_shape(map_flows2) +
  tm_lines(lwd = "flow", scale = 10, col = "blue", popup.vars = TRUE, palette = "Dark2") +
  tm_shape(map_points2) +
  tm_dots()





edge2 <- map_flows2 %>%
  sf::st_set_geometry(NULL) %>%
  select(orig, dest) %>%
  as.matrix() %>%
  t() %>%
  as.vector()

g2 <- igraph::make_undirected_graph(edge2)
g2




co <- st_coordinates(map_points3)
igraph::plot.igraph(g3, layout = co)

mst3 <- mst(g3, weights = as.numeric(sf::st_length(map_flows3)))

plot(mst3, layout = co)
