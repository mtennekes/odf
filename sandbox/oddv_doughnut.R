library(sf)
devtools::load_all("../tmaptools/")
devtools::load_all("../tmap/")
devtools::load_all()
tmap_mode("view")

###########################################################################################
#### misc functions
###########################################################################################
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


###########################################################################################
#### misc functions
###########################################################################################

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")
points$geometry[points$id == "GM0599"] <- st_point(c(91437.09, 437561.29)) # edit Rotterdam

# Create odf object
#x <- odf(od, points, col_flow = "value")
x <- odf(od, points, col_flow = "value", col_orig = "gm_to", col_dest = "gm_from")

x$od <- x$od %>%
  filter(flow >= 500)

# Create spatial objects
map_flows <- odf_flows(x, by_type = FALSE, by_via = TRUE, angle = 0, range = c(.5, 1), trunc = units::set_units(c(500, 0), "m"), min_trunc_dist = units::set_units(1000, "m")) %>%
  add_city_class()
map_points <- odf_points(x)

# Define palette
pal <- tmaptools::get_brewer_pal("Dark2", plot = F)[c(2,4,1,5,3)]
pal <- tmaptools::get_brewer_pal("Set1", plot = F)[c(1,4,5,3,2)]
pal <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]
#colorspace::demoplot(pal, "bar")
names(pal) <- c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")


###########################################################################################
#### grobs
###########################################################################################


df <- x$od %>%
  add_city_class()
grobs <- lapply(map_points$id, function(id) {
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
names(grobs) <- map_points$id
map_points <- map_points %>%
  mutate(size = pmax(10000, flow_from))

#save(map_flows4, map_points4, pal, grobs, file="~/git/tmap/local/transparent_grobs.rdata")

tm <- tm_shape(map_flows) +
  tm_lines(lwd = "flow", scale = 10, col = "dest_cls", popup.vars = TRUE, palette = pal, title.col = "Municipality") +
  tm_shape(map_points) +
  tm_symbols(size = "size", scale = 1, popup.vars = TRUE, shape = "id", shapes = grobs, legend.shape.show = FALSE) +
  tm_tiles(paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"), group = "Labels")


oddv_save(tm4, "commuting_ODDV_map3.png")
