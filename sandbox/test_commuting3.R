## test multiline

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")

# Create odf object
x <- odf(od, points, col_flow = "value", col_type = "mode")

library(tidyverse)
library(sf)
library(tmap)

# extract spatial points (sf) object from odf object
map_points <- odf_points(x)

# extract spatial lines (sf) objects, disregarding type
map_flows <- odf_flows(x, by_type = FALSE, by_via = TRUE)


# show map with flows, and colors for the big 4
tm_shape(map_flows %>% filter(flow >= 150)) +
  tm_lines(lwd = "flow", scale = 10, popup.vars = TRUE) +
  tm_shape(map_points) +
  tm_dots()


AMS_MST <- c("GM0363", "GM0344", "GM0796", "GM0772", "GM0988", "GM0957", "GM1883", "GM0935")

## AMS to EIN

f <- map_flows %>%
  filter(orig %in% AMS_MST[1:4], dest %in% AMS_MST[1:4])

gg <- map_flows %>%
  filter(orig %in% AMS_MST[1:4], dest %in% AMS_MST[1:4]) %>%
  st_set_geometry(NULL) %>%
  mutate(oid = match(orig, AMS_MST),
         did = match(dest, AMS_MST))

gg$flow[3] <- gg$flow[3] + gg$flow[2]
gg$flow[4] <- gg$flow[4] + gg$flow[5] + gg$flow[6]
gg$flow[6] <- gg$flow[6] + gg$flow[5]

gg$flow[10] <- gg$flow[10] + gg$flow[11]
gg$flow[9] <- gg$flow[9] + gg$flow[7] + gg$flow[8]
gg$flow[7] <- gg$flow[7] + gg$flow[8]

#gg <- gg[c(1,3,4,9,10,12),]

gg <- gg %>%
  mutate(y1 = oid - .5,
         y2 = oid + .5,
         x1 = oid,
         x2 = did)
gg$x1 <- c(2,3,2,1,3,2,3,2,4,3,2,3)

ggplot(gg, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = flow)) +
  geom_rect()


####
## AMS to MST

f2 <- map_flows %>%
  filter(orig %in% AMS_MST, dest %in% AMS_MST)

gg <- map_flows %>%
  filter(orig %in% AMS_MST, dest %in% AMS_MST) %>%
  st_set_geometry(NULL) %>%
  mutate(oid = match(orig, AMS_MST),
         did = match(dest, AMS_MST))

for (i in 1:6) {
  for (j in (i+1):7) {
    gg$flow[gg$oid == i & gg$did == j] <- sum(gg$flow[gg$oid == i & gg$did >= j])
  }
}
for (i in 8:3) {
  for (j in (i-1):2) {
    gg$flow[gg$oid == i & gg$did == j] <- sum(gg$flow[gg$oid == i & gg$did <= j])
  }
}

library(RColorBrewer)

gg <- gg %>%
  mutate(y1 = oid - .5,
         y2 = oid + .5,
         x1 = ifelse(did > oid, did - 1, did + 1),
         x2 = did,
         palette = rownames(brewer.pal.info[brewer.pal.info$category == "seq",])[oid],
         pos = round((flow/(max(flow)+1)) * 9 + .5))

gg$fill <- mapply(function(pal, pos) RColorBrewer::brewer.pal(9, pal)[pos], gg$palette, gg$pos)

ggplot(gg, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = I(fill))) +
  geom_rect()


