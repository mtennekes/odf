library(sf)
devtools::load_all("../tmaptools/")
devtools::load_all("../tmap/")
devtools::load_all()
tmap_mode("view")


###########################################################################################
#### misc functions
###########################################################################################

add_city_class <- function(col, levels, other) {
  x <- match(col, levels)
  x[is.na(x)] <- length(levels) + 1L

  factor(x, levels = 1:(length(levels) + 1L), labels = c(levels, other))
}
create_grobs <- function(x, id = "name", by = "name_from", flow = "value", class = "class_to", pal) {
  U <- x$U
  E <- x$E
  if (inherits(E, "sf")) E <- sf::st_drop_geometry(E)

  ids <- U[[id]]

  grobs <- lapply(ids, function(i) {
    df <- E %>%
      filter(!!sym(by) == i)

    if (nrow(df) == 0L) warning(i, " has ", flow, " 0")
    suppressWarnings({
      df <- df %>%
        group_by(!!sym(class)) %>%
        summarize(!!flow := sum(!!sym(flow))) %>%
        ungroup()
    })

    ggplotGrob(ggplot(df, aes(x=2, y=!!sym(flow), fill = !!sym(class))) +
                 geom_bar(stat="identity", width=1, size = ifelse(nrow(df) == 1, 0, 2), color = "white", show.legend = FALSE) +
                 geom_vline(xintercept = 2.5, color = "white", size = 5 ) +
                 geom_rect(xmin = 0, xmax = .75, ymin = 0, ymax = sum(df[[flow]]), size = 0, color = "white", fill = "grey90") +
                 geom_vline(xintercept = 1.5, color = "white", size = 5 ) +
                 scale_fill_manual(values = pal) +
                 coord_polar("y", start=0) +
                 xlim(.75, 2.5) +
                 theme_void())
  })
  names(grobs) <- ids
  grobs
}


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
  mutate(name_from = x$U$name[match(gm_from, x$U$id)],
         name_to = x$U$name[match(gm_to, x$U$id)],
         label = paste(name_from, "to", name_to),
         class_to = add_city_class(name_to, c("Amsterdam", "Rotterdam", "'s-Gravenhage", "Utrecht"), "other")) %>%
  select(label, everything())

x <- od_sum_out(x, "value")
x <- od_sum_in(x, "value")

x$U <- x$U %>% mutate(size = pmax(10000, value_out))

# Define palette
pal <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]
names(pal) <- levels(x$E$class_to)
grobs <- create_grobs(x, id = "name", by = "name_from", flow = "value", class = "class_to", pal)

x$E <- x$E %>%
  filter(value >= 500)

x <- od_add_lines(x, angle = 0, range = c(.5, 1), trunc = units::set_units(c(500, 0), "m"), min_trunc_dist = units::set_units(1000, "m"))




###########################################################################################
#### grobs
###########################################################################################


#save(map_flows4, map_points4, pal, grobs, file="~/git/tmap/local/transparent_grobs.rdata")

tm <- tm_shape(x$E) +
  tm_lines(lwd = "value", scale = 10, col = "class_to", popup.vars = TRUE, palette = pal, title.col = "Municipality") +
  tm_shape(x$U) +
  tm_symbols(size = "size", scale = 1, popup.vars = TRUE, shape = "name", shapes = grobs, legend.shape.show = FALSE) +
  tm_tiles(paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"), group = "Labels")


#oddv_save(tm4, "commuting_ODDV_map3.png")




x$E %>%
  filter(name_from == "Wassenaar") %>%
  group_by(class_to) %>%
  summarize(value = sum(value))

