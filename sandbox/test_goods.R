# Read goods data (from Chris de Blois)
od <- readRDS("~/pCloudDrive/flows_goods/data/throughput/OD_via.rds")
points <- readRDS("~/pCloudDrive/flows_goods/data/throughput/centroids.rds")

# Create odf object
x <- odf(od, points, col_via = "via", col_flow = "value", col_type = "mode")




# load tmap and set options
library(tmap)
tmap_mode("view")
tmap_options(limits = c(facets.view = 6))



# extract spatial points (sf) object from odf object
map_points <- odf_points(x)

# extract spatial lines (sf) objects, disregarding type
map_flows <- odf_flows(x, by_type = FALSE, by_via = TRUE)

# create custom binary variable (Import/export from the Netherlands)
map_flows <- map_flows %>%
  mutate(direction = factor(orig == 0, levels = c(FALSE, TRUE), labels = c("Import", "Export")),
         flow_sqrt = sqrt(flow))

# show map with flows
tm_shape(map_flows) +
  tm_lines(lwd = "flow_sqrt", col = "direction", scale = 10, popup.vars = TRUE, title.col = "Type", palette = "Dark2") +
  tm_shape(map_points) +
  tm_dots()

# create heatmap
odf_heatmap(x, log = TRUE)


library(ggplot2)
g <- ggplot(heatmap, aes(y = orig, x = dest, fill=flow)) +
  geom_tile() +
  scale_fill_viridis_c(option = "B", direction = -1) +
  scale_x_discrete("Destination") +
  scale_y_discrete("Origin") +
  theme(panel.background = element_rect(fill = "grey70"))


