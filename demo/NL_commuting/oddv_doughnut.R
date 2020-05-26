library(sf)
devtools::load_all("../tmaptools/")
devtools::load_all("../tmap/")
devtools::load_all()
tmap_mode("view")
source("sandbox/create_oddv_doughnuts.R")


###########################################################################################
#### read data
###########################################################################################

# Read commuting data
data(NL_commuting)
data(NL_muni_poly)
data(NL_muni_point)

# Create odf object
x <- od(NL_commuting, NL_muni_point, col_orig = "muni_from", col_dest = "muni_to", col_id = "id")


###########################################################################################
#### process data
###########################################################################################


highlighted_big4 <- c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht")
highlighted_brabant <- c("Eindhoven", "Tilburg", "Breda", "Den Bosch")
highlighted_limburg <- c("Maastricht", "Heerlen", "Sittard-Geleen", "Outside Limburg")
#highlighted_noord <- c("Groningen", "Leeuwarden", "Assen", "Heerenveen", "Emmen")
highlighted_east <- c("Zwolle", "Enschede", "Apeldoorn", "Arnhem", "Nijmegen")
highlighted_noord <- c("Groningen", "Leeuwarden", "Assen", "West and south Netherlands")

# Define palette
pal5 <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]
pal6 <- colorspace::qualitative_hcl(6, palette = "Dark3", c = 100, l = 50, h1 = 35)[c(1,3,2,5,6,4)]

# align palettes on blue
if (FALSE) {
targetcol <- as.vector(col2rgb(pal5[5]))
gdiff <- 100
ga <- ""
gi <- 0
for (i in 1:360) {
  a <- lapply(colorspace::qualitative_hcl(6, palette = "Dark3", c = 100, l = 50,h1=i), function(col) as.vector(col2rgb(col)))
  dff <- abs(sapply(a, function(ai) {
    sum(ai-targetcol)
  }))
  if (min(dff) < gdiff) {
    ga <- a[which.min(dff)]
    gdiff <- min(dff)
    gi <- i
  }
}
}


x$U$show <- TRUE
x$E$show <- TRUE


if (FALSE) {
  # total inflow
  x2 <- x
  x2$E <- x2$E %>%
    filter(muni_from != muni_to) %>%
    group_by(muni_from, muni_to) %>%
    summarize(value = sum(value)) %>%
    ungroup()
  x2 <- od_sum_in(x2, value = "value")


  tm_shape(x2$U) + tm_bubbles(size = "value_in", id = "name")
}



## Netherlands
tmNL_in_out <- create_oddv_doughnuts(x, highlighted_big4, pal = pal5, size_min = 5000, size_max = 250000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = list(set.view = c(4.746, 52.155, 10), set.zoom.limits = c(9,12)))


## NN
x_NN <- filter_Noord(x, NL_muni_poly)
NN_poly <- NL_muni_poly %>% filter(NUTS2_name %in% c("Groningen", "Friesland (NL)", "Drenthe")) %>% st_union() %>% st_sf(x=1) %>% tmaptools::simplify_shape() %>% st_cast("POLYGON") %>% head(1)

labels <- sf::st_sf(labels = c("From west and south Netherlands", "From east Netherlands"), geometry = sf::st_sfc(list(sf::st_point(c(5.608, 52.478)), sf::st_point(c(6.491, 52.478))), crs = 4326))

clip_rect <- tmaptools::bb_poly(bb(c(xmin = 3.5, ymin = 52.5, xmax = 8, ymax = 54.5)))
tmNN_in_out <- tm_shape(NN_poly) + tm_borders(col = "black", group = NULL) + create_oddv_doughnuts(x_NN, highlighted_noord, pal = pal5, size_min = 1000, size_max = 30000, doughnut_scale = 1, flow_min = 300, flow_max = 5000, flow_scale = 10, view_args = list(set.view = c(6.2, 53, 10), set.zoom.limits = c(9,12)), clip = clip_rect) + tm_shape(labels) + tm_text("labels", size = 1.2, group = NULL)

## Oost
tmNO_in_out <- create_oddv_doughnuts(x, highlighted_east, pal = pal6, size_min = 5000, size_max = 35000, doughnut_scale = 1.5, flow_min = 500, flow_max = 5000, flow_scale = 10, view_args = list(set.view = c(6.24, 52.07, 10), set.zoom.limits = c(9,12)))


## Brabant
tmNB_in_out <- create_oddv_doughnuts(x, highlighted_brabant, pal = pal5, size_min = 5000, size_max = 35000, doughnut_scale = 1.5, flow_min = 500, flow_max = 5000, flow_scale = 10, view_args = list(set.view = c(5.2, 51.6, 10), set.zoom.limits = c(9,12)))


## Limburg
x_LI <- filter_Limburg(x, NL_muni_poly)
LI_poly <- NL_muni_poly %>% filter(NUTS3_name == "Zuid-Limburg")
LI_poly <- sf::st_union(LI_poly)


labels <- sf::st_sf(labels = c("From outside Limburg", "From North and Central Limburg"), geometry = sf::st_sfc(list(sf::st_point(c(5.673, 51.124)), sf::st_point(c(6.03, 51.124))), crs = 4326))

clip_rect <- tmaptools::bb_poly(bb(c(xmin = 5.5, ymin = 50.7, xmax = 6.2, ymax = 51.12)))

tmL_in_out <- tm_shape(LI_poly) + tm_borders(col = "black", group = NULL) + create_oddv_doughnuts(x_LI, highlighted_limburg, pal = pal5, size_min = 1000, size_max = 30000, doughnut_scale = 1.5, flow_min = 200, flow_max = 5000, flow_scale = 15, view_args = list(set.view = c(5.87, 50.95, 11), set.zoom.limits = c(10,12)), clip = clip_rect) + tm_shape(labels) + tm_text("labels", size = 1.2, group = NULL)




# save
tmap_save(tmNL_in_out, file = "local/Dutch_commuting_v3/indexNL.html", in.iframe = TRUE, libdir = "index_files")
tmap_save(tmNB_in_out, file = "local/Dutch_commuting_v3/indexNB.html", in.iframe = TRUE, libdir = "index_files")
tmap_save(tmL_in_out, file = "local/Dutch_commuting_v3/indexLI.html", in.iframe = TRUE, libdir = "index_files")
tmap_save(tmNN_in_out, file = "local/Dutch_commuting_v3/indexNN.html", in.iframe = TRUE, libdir = "index_files")
tmap_save(tmNO_in_out, file = "local/Dutch_commuting_v3/indexNO.html", in.iframe = TRUE, libdir = "index_files")





