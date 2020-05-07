library(sf)
devtools::load_all("../tmaptools/")
devtools::load_all("../tmap/")
devtools::load_all()
tmap_mode("view")

add_city_class <- function(mf) {
  mf %>%
    mutate(gm_from_cls = factor(ifelse(gm_from == "GM0363", "Amsterdam",
                             ifelse(gm_from == "GM0599", "Rotterdam",
                             ifelse(gm_from == "GM0518", "Den Haag",
                             ifelse(gm_from == "GM0344", "Utrecht", "other")))), levels = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")),
           gm_to_cls = factor(ifelse(gm_to == "GM0363", "Amsterdam",
                             ifelse(gm_to == "GM0599", "Rotterdam",
                             ifelse(gm_to == "GM0518", "Den Haag",
                             ifelse(gm_to == "GM0344", "Utrecht", "other")))), levels = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")))
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
x <- od(od, points, col_orig = "gm_to", col_dest = "gm_from")

x$U <- x$U %>%
  sf::st_transform(3857) %>%
  select(name, everything())

x$E <- x$E %>%
  group_by(gm_from, gm_to) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  filter(value >= 500) %>%
  mutate(label = paste(x$U$name[match(gm_from, x$U$id)], "to", x$U$name[match(gm_to, x$U$id)])) %>%
  add_city_class() %>%
  select(label, everything())

x <- od_add_lines(x, angle = 0, range = c(.5, 1), trunc = units::set_units(c(500, 0), "m"), min_trunc_dist = units::set_units(1000, "m"))


x <- od_sum_out(x, "value")
x <- od_sum_in(x, "value")



# Define palette
pal <- tmaptools::get_brewer_pal("Dark2", plot = F)[c(2,4,1,5,3)]
pal <- tmaptools::get_brewer_pal("Set1", plot = F)[c(1,4,5,3,2)]
pal <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]
#colorspace::demoplot(pal, "bar")
names(pal) <- c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht", "other")


###########################################################################################
#### grobs
###########################################################################################

create_grobs <- function(x)


df <- x$od %>%
  add_city_class()
grobs <- lapply(map_points$id, function(id) {
  df2 <- df %>%
    filter(orig == id) %>%
    group_by(dest_cls) %>%
    summarize(flow = sum(flow)) %>%
    ungroup()


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
