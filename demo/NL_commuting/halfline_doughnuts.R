library(odf)
library(sf)
library(tmap)
library(dplyr)

tmap_mode("view")
source("demo/NL_commuting/create_halfline_doughnuts.R")


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

# Define palette
pal5 <- colorspace::qualitative_hcl(5, palette = "Dark3", c = 100, l = 50)[c(1,3,2,5,4)]


## Netherlands
tmNL_in_out <- create_halfline_doughnuts(x, highlighted_big4, pal = pal5, size_min = 5000, size_max = 250000, doughnut_scale = 1.5, flow_min = 500, flow_max = 20000, flow_scale = 10, view_args = list(set.view = c(4.746, 52.155, 10), set.zoom.limits = c(9,12)))
