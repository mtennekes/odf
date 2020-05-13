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


x_LI <- filter_Limburg(x, NL_muni_poly)


LI_poly <- NL_muni_poly %>% filter(NUTS3_name == "Zuid-Limburg")

(tmNL_in_out <- create_oddv_doughnuts(x, highlighted_big4, pal = pal, size_min = 5000, size_max = 250000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = list(set.view = c(4.746, 52.155, 10))))


(tmNB_in_out <- create_oddv_doughnuts(x, highlighted_brabant, pal = pal, size_min = 5000, size_max = 250000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = list(set.view = c(4.746, 52.155, 10))))

(tmL_in_out <- tm_shape(LI_poly) + tm_borders(col = "black") + create_oddv_doughnuts(x_LI, highlighted_limburg, pal = pal, size_min = 5000, size_max = 30000, doughnut_scale = 1.5, flow_min = 200, flow_max = 5000, flow_scale = 20, view_args = list(set.view = c(5.772, 50.830, 10), set.bounds = c(5.638, 50.749, 6.096, 51.073), set.zoom.limits = c(10, 12))))

(tmL_in_in <- tm_shape(LI_poly) + tm_borders(col = "black") + create_oddv_doughnuts(x_LI, highlighted_limburg, edges_direction = "in", doughnut_type = "in", pal = pal, size_min = 5000, size_max = 30000, doughnut_scale = 1.5, flow_min = 100, flow_max = 5000, flow_scale = 20, view_args = list(set.view = c(5.772, 50.830, 10), set.bounds = c(5.638, 50.749, 6.096, 51.073), set.zoom.limits = c(10, 12))))

(tmL_out_in <- tm_shape(LI_poly) + tm_borders(col = "black") + create_oddv_doughnuts(x_LI, highlighted_limburg, edges_direction = "out", doughnut_type = "in", pal = pal, size_min = 5000, size_max = 30000, doughnut_scale = 1.5, flow_min = 500, flow_max = 5000, flow_scale = 20, view_args = list(set.view = c(5.772, 50.830, 10), set.bounds = c(5.638, 50.749, 6.096, 51.073), set.zoom.limits = c(10, 12))))

(tmL_out_out <- tm_shape(LI_poly) + tm_borders(col = "black") + create_oddv_doughnuts(x_LI, highlighted_limburg, edges_direction = "out", doughnut_type = "out", pal = pal, size_min = 5000, size_max = 30000, doughnut_scale = 1.5, flow_min = 100, flow_max = 5000, flow_scale = 20, view_args = list(set.view = c(5.772, 50.830, 10), set.bounds = c(5.638, 50.749, 6.096, 51.073), set.zoom.limits = c(10, 12))))




setwd("local/Dutch_commuting_v1/")
tmap_save(tm, filename = "./index3.html", selfcontained = FALSE)
setwd("../..")
