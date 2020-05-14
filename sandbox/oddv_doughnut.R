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

# Define palette
pal <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]


x$U$show <- TRUE
x$E$show <- TRUE

## Netherlands
(tmNL_in_out <- create_oddv_doughnuts(x, highlighted_big4, pal = pal, size_min = 5000, size_max = 250000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = list(set.view = c(4.746, 52.155, 10))))

## Brabant
(tmNB_in_out <- create_oddv_doughnuts(x, highlighted_brabant, pal = pal, size_min = 5000, size_max = 40000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = list(set.view = c(5.2, 51.6, 10))))

## Limburg
x_LI <- filter_Limburg(x, NL_muni_poly)
LI_poly <- NL_muni_poly %>% filter(NUTS3_name == "Zuid-Limburg")

labels <- sf::st_sf(labels = c("From outside Limburg", "From North and Central Limburg"), geometry = sf::st_sfc(list(sf::st_point(c(175000, 350000)), sf::st_point(c(200000, 350000))), crs = 28992))

clip_rect <- tmaptools::bb_poly(bb(c(xmin = 5.5, ymin = 50.7, xmax = 6.2, ymax = 51.12)))

(tmL_in_out <- tm_shape(LI_poly) + tm_borders(col = "black") + create_oddv_doughnuts(x_LI, highlighted_limburg, pal = pal, size_min = 1000, size_max = 30000, doughnut_scale = 1.5, flow_min = 200, flow_max = 5000, flow_scale = 20, view_args = list(set.view = c(5.87, 50.95, 10)), clip = clip_rect) + tm_shape(labels) + tm_text("labels", size = 1.2))


lfNL <- tmap_leaflet(tmNL_in_out)
lfNB <- tmap_leaflet(tmNB_in_out)
lfLI <- tmap_leaflet(tmL_in_out)

lfs <- leafsync::sync(lfNL, lfNB, lfLI, sync = "none")

htmlwidgets::saveWidget(lfNL, file = "indexNL.html", selfcontained = FALSE)
htmlwidgets::saveWidget(lfNB, file = "indexNB.html", selfcontained = FALSE)
htmlwidgets::saveWidget(lfLI, file = "indexLI.html", selfcontained = FALSE)

tmps
setwd("local/Dutch_commuting_v1/")
tmap_save(tmps, filename = "./index4.html", selfcontained = FALSE)
setwd("../..")
