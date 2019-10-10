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
  mutate(orig_cls = ifelse(orig == "GM0363", "Adam",
               ifelse(orig == "GM0599", "Rotterdam",
               ifelse(orig == "GM0518", "Den Haag",
               ifelse(orig == "GM0344", "Utrecht", "Overig")))),
         dest_cls = ifelse(dest == "GM0363", "Adam",
                    ifelse(dest == "GM0599", "Rotterdam",
                    ifelse(dest == "GM0518", "Den Haag",
                    ifelse(dest == "GM0344", "Utrecht", "Overig")))))

map_flows_sel <- map_flows2 %>% filter(flow >= 150)

# show map with flows, and colors for the big 4
tm_shape(map_flows_sel) +
  tm_lines(lwd = "flow", scale = 10, col = "dest_cls", popup.vars = TRUE, palette = "Dark2") +
  tm_shape(map_points) +
  tm_dots()

# show only the big 4
map_flows_sel2 <- map_flows_sel %>%
  filter(orig %in% c("GM0363", "GM0599", "GM0518", "GM0344") & dest %in% c("GM0363", "GM0599", "GM0518", "GM0344"))

tm_shape(map_flows_sel2) +
  tm_lines(lwd = "flow", scale = 20, col = "dest_cls", popup.vars = TRUE, palette = "Dark2") +
  tm_shape(map_points) +
  tm_dots()

