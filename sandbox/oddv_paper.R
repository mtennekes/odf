library(sf)
devtools::load_all("../tmaptools/")
devtools::load_all("../tmap/")
devtools::load_all()
tmap_mode("view")


# Read commuting data (from Marko Roos / Yvonne Gootzen)
#od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od_with_diag.rds")
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")


points$geometry[points$id == "GM0599"] <- st_point(c(91437.09, 437561.29))


# Create odf object
x <- odf(od, points, col_flow = "value")



oddv_save <- function(tm, file) {
  tm <- tm + tm_view(set.view = c(4.721206+.025, 52.134765+.02, 10))
  tmpf <- tempfile(fileext = ".html")
  #browser()
  tmap_save(tm, filename = tmpf, selfcontained = FALSE)
  webshot::webshot(tmpf, file, zoom = 2, vwidth = 1000, vheight = 1000)
}

add_city_class <- function(mf) {
  mf %>%
    mutate(orig_cls = factor(ifelse(orig == "GM0363", "Amsterdam",
                                    ifelse(orig == "GM0599", "Rotterdam",
                                           ifelse(orig == "GM0518", "Den Haag",
                                                  ifelse(orig == "GM0344", "Utrecht", "other")))), levels = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")),
           dest_cls = factor(ifelse(dest == "GM0363", "Amsterdam",
                                    ifelse(dest == "GM0599", "Rotterdam",
                                           ifelse(dest == "GM0518", "Den Haag",
                                                  ifelse(dest == "GM0344", "Utrecht", "other")))), levels = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")))
  # ifelse(dest == "GM0772", "Eindhoven",
  # ifelse(dest == "GM0935", "Maastricht",
  # ifelse(dest == "GM0202", "Arnhem",
  # ifelse(dest == "GM0193", "Zwolle",
  # ifelse(dest == "GM0153", "Enschede",
  # ifelse(dest == "GM0106", "Assen",
  # ifelse(dest == "GM0080", "Leeuwarden",
  # ifelse(dest == "GM0014", "Groningen", "Other")))))))))))))
}


pal <- tmaptools::get_brewer_pal("Dark2", plot = F)[c(2,4,1,5,3)]
pal <- tmaptools::get_brewer_pal("Set1", plot = F)[c(1,4,5,3,2)]
pal <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]
demoplot(pal, "bar")
names(pal) <- c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")



############## map 1
## raw data (aggregates on muni level)

map_points <- odf_points(x)

# extract spatial lines (sf) objects, disregarding type
map_flows <- odf_flows(x, by_type = FALSE, by_via = FALSE)

tm1 <- tm_shape(map_flows) +
  tm_lines(lwd = "flow", scale = 10) +
  tm_shape(map_points) +
  tm_dots()

oddv_save(tm1, "commuting_ODDV_map1.png")

############### map 2A
## filter: edges >= 500

x2 <- x
x2$od <- x2$od %>%
  filter(flow >= 500)
x2$points <- x2$points %>%
  filter(id %in% x2$od$orig | id %in% x2$od$dest)

map_flows2 <- odf_flows(x2, by_type = FALSE, by_via = FALSE) %>%
  add_city_class()
map_points2 <- odf_points(x2)

tm2 <- tm_shape(map_flows2) +
  tm_lines(lwd = "flow", scale = 10) +
  tm_shape(map_points2) +
  tm_dots()

oddv_save(tm2, "commuting_ODDV_map2.png")


############### map 2B
## filter: nodes outflow >= 5000

large_muni <- map_flows2 %>%
  st_drop_geometry() %>%
  group_by(dest) %>%
  summarize(flow = sum(flow)) %>%
  filter(flow >= 5000)

x3 <- x2
x3$od <- x3$od %>%
  filter(dest %in% large_muni$dest)
x3$points <- x3$points %>%
  filter(id %in% x3$od$orig | id %in% x3$od$dest)


map_flows3 <- odf_flows(x3, by_type = FALSE, by_via = TRUE)
map_points3 <- odf_points(x3)
  #filter(id %in% large_muni$dest)

tm_shape(map_flows3) +
  tm_lines(lwd = "flow", scale = 10) +
  tm_shape(map_points3) +
  tm_dots()

############## map 3



x4 <- x2
# x4$od <- x4$od %>%
#   filter(flow >= 500)

map_flows4 <- odf_flows(x4, by_type = FALSE, by_via = TRUE, angle = 0, range = c(.5, 1), trunc = units::set_units(c(500, 0), "m"), min_trunc_dist = units::set_units(1000, "m")) %>%
  add_city_class()

map_points4 <- odf_points(x4)

tm_shape(map_flows4) +
  tm_lines(lwd = "flow", scale = 10, col = "dest_cls", popup.vars = TRUE, palette = pal, title.col = "Municipality") +
tm_shape(map_points4) +
  tm_bubbles(size = "flow_to", scale = .5, popup.vars = TRUE)


###### pie chart

df <- x4$od %>%
  add_city_class()

##### TESTING
# id <- "GM0003"
# id <- "GM0363"
# df2 <- df %>%
#   filter(orig == id) %>%
#   group_by(dest_cls) %>%
#   summarize(flow = sum(flow)) %>%
#   ungroup()
# ggplot(df2, aes(x=2, y=flow, fill = dest_cls)) +
#   geom_bar(stat="identity", width=1, size = ifelse(nrow(df2) == 1, 0, 2), color = "pink", show.legend = FALSE) +
#   geom_vline(xintercept = 2.5, color = "orange", size = 5 ) +
#   geom_rect(xmin = 0, xmax = .75, ymin = 0, ymax = sum(df2$flow), size = 0, color = "yellow", fill = "blue") +
#   geom_vline(xintercept = 1.5, color = "orange", size = 5 ) +
#   scale_fill_manual(values = pal) +
#   coord_polar("y", start=0) +
#   xlim(.75, 2.5) +
#   theme_void()

grobs <- lapply(map_points4$id, function(id) {
  df2 <- df %>%
    filter(orig == id) %>%
    group_by(dest_cls) %>%
    summarize(flow = sum(flow)) %>%
    ungroup()

  # #### PIE chart
  # ggplotGrob(ggplot(df2, aes(x="", y=flow, fill = dest_cls)) +
  #   geom_bar(stat="identity", width=1, size = 2, color = "white", show.legend = FALSE) +
  #   geom_vline(xintercept = -.5, color = "white", size = 5 ) +
  #   scale_fill_manual(values = pal) +
  #   coord_polar("y", start=0) +
  # theme_void())

  ggplotGrob(ggplot(df2, aes(x=2, y=flow, fill = dest_cls)) +
    geom_bar(stat="identity", width=1, size = ifelse(nrow(df2) == 1, 0, 2), color = "white", show.legend = FALSE) +
    geom_vline(xintercept = 2.5, color = "white", size = 5 ) +
    geom_rect(xmin = 0, xmax = .75, ymin = 0, ymax = sum(df2$flow), size = 0, color = "white", fill = "grey90") +
    geom_vline(xintercept = 1.5, color = "white", size = 5 ) +
    scale_fill_manual(values = pal) +
    coord_polar("y", start=0) +
    xlim(.75, 2.5) +
    theme_void())
})
names(grobs) <- map_points4$id
map_points4 <- map_points4 %>%
  mutate(size = pmax(10000, flow_from))

#save(map_flows4, map_points4, pal, grobs, file="~/git/tmap/local/transparent_grobs.rdata")

tm4 <- tm_shape(map_flows4) +
  tm_lines(lwd = "flow", scale = 10, col = "dest_cls", popup.vars = TRUE, palette = pal, title.col = "Municipality") +
  tm_shape(map_points4) +
  tm_symbols(size = "size", scale = 1, popup.vars = TRUE, shape = "id", shapes = grobs, legend.shape.show = FALSE)


oddv_save(tm4, "commuting_ODDV_map3.png")
