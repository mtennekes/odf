# test coloured cities and igraph

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")

# Create odf object
x <- odf(od, points, col_flow = "value", col_type = "mode")


# load tmap and set options
library(tmap)
tmap_mode("view")
tmap_options(limits = c(facets.view = 6))



# extract spatial points (sf) object from odf object
map_points <- odf_points(x)

# extract spatial lines (sf) objects, disregarding type
map_flows <- odf_flows(x, by_type = FALSE, by_via = TRUE)

map_flows2 <- map_flows %>%
  mutate(orig_cls = ifelse(orig == "GM0363", "Amsterdam",
               ifelse(orig == "GM0599", "Rotterdam",
               ifelse(orig == "GM0518", "Den Haag",
               ifelse(orig == "GM0344", "Utrecht", "Other")))),
         dest_cls = ifelse(dest == "GM0363", "Amsterdam",
                    ifelse(dest == "GM0599", "Rotterdam",
                    ifelse(dest == "GM0518", "Den Haag",
                    ifelse(dest == "GM0344", "Utrecht", "Other")))))

map_flows_sel <- map_flows2 %>% filter(flow >= 150)

# show map with flows, and colors for the big 4
tm_shape(map_flows_sel) +
  tm_lines(lwd = "flow", scale = 10, col = "dest_cls", popup.vars = TRUE, palette = "Dark2", title.col = "Destination") +
  tm_shape(map_points) +
  tm_dots()




# show only the big 4
map_flows_sel2 <- map_flows_sel %>%
  filter(orig %in% c("GM0363", "GM0599", "GM0518", "GM0344") & dest %in% c("GM0363", "GM0599", "GM0518", "GM0344"))

tm_shape(map_flows_sel2) +
  tm_lines(lwd = "flow", scale = 20, col = "dest_cls", popup.vars = TRUE, palette = "Dark2") +
  tm_shape(map_points) +
  tm_dots()

# hex grid
data(NLD_muni)
h <- sf::st_make_grid(NLD_muni, square = FALSE)

# width powered
map_flows_sel$flow2 <- map_flows_sel$flow ^ 2
tm_shape(map_flows_sel) +
  tm_lines(lwd = "flow2", scale = 10, col = "dest_cls", popup.vars = TRUE, palette = "Dark2") +
  tm_shape(map_points) +
  tm_dots()



# top x cities
topX <- map_flows2 %>%
  sf::st_set_geometry(NULL) %>%
  group_by(orig) %>%
  summarise(flow = sum(flow)) %>%
  ungroup() %>%
  arrange(desc(flow)) %>%
  head(70)

map_flows3 <- map_flows2 %>%
  filter(orig %in% topX$orig, dest %in% topX$orig)

map_points3 <- map_points %>%
  filter(id %in% topX$orig)

tm_shape(map_flows3) +
  tm_lines(lwd = "flow", scale = 10, col = "blue", popup.vars = TRUE, palette = "Dark2") +
  tm_shape(map_points3) +
  tm_dots()

library(igraph)


edge3 <- map_flows3 %>%
  sf::st_set_geometry(NULL) %>%
  select(orig, dest) %>%
  as.matrix() %>%
  t() %>%
  as.vector()

g3 <- igraph::make_undirected_graph(edge3)
g3

co <- st_coordinates(map_points3)
igraph::plot.igraph(g3, layout = co)

mst3 <- mst(g3, weights = as.numeric(sf::st_length(map_flows3)))

plot(mst3, layout = co)
