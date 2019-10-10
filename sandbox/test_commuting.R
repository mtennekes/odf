# Read goods data (from Chris de Blois)
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

map_flows_sel <- map_flows %>% filter(flow >= 200)


# show map with flows
tm_shape(map_flows_sel) +
  tm_lines(lwd = "flow", scale = 20, popup.vars = TRUE) +
  tm_shape(map_points) +
  tm_dots()

# create heatmap
g <- odf_heatmap(x, log = TRUE, base_size = 8, sort = TRUE)
ggplot2::ggsave("sandbox/commuting_od.pdf", width = 20, height = 20)


