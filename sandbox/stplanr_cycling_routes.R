library(stplanr)

from <- c(-1.55, 53.80) # geo_code("leeds")
to <- c(-1.76, 53.80) # geo_code("bradford uk")
json_output <- route_cyclestreet(from = from, to = to, plan = "quietest", save_raw = TRUE)

#API b6f58a32a788b75b
str(json_output) # what does cyclestreets give you?
rf_lb <- route_cyclestreet(from, to, plan = "quietest")
rf_lb@data
plot(rf_lb)
(rf_lb$length / (1000 * 1.61)) / # distance in miles
  (rf_lb$time / (60 * 60)) # time in hours - average speed here: ~8mph
# Plan a 'balanced' route from Pedaller's Arms to the University of Leeds
rb_pa <- route_cyclestreet("Pedaller's Arms, Leeds", "University of Leeds, UK", "balanced")

geo_code("Miradorplein, Maastricht, Netherlands")

from <- c(5.716896, 50.865712)
to <- c(5.970638, 50.893712)
