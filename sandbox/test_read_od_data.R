# goods data
od <- readRDS("~/pCloudDrive/flows_goods/data/throughput/OD_value.rds")
points <- readRDS("~/pCloudDrive/flows_goods/data/throughput/centroids.rds")

od <- od[od$orig != 36 & od$dest != 36, ]

# Create odf object
x <- od(od, points, col_orig = "orig", col_dest = "dest")

od_is_valid(x)

# Dutch commuting
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")


x <- od(od, points)

od_is_valid(x)

x$E$a <- round(runif(147936) * 100)
x$E$b <- round(runif(147936) * 1000)
x$E$c <- round(runif(147936) * 10000)

library(tidyverse)

x$E <- x$E %>%
  group_by(gm_from, gm_to, mode) %>%
  summarize(value = sum(value)) %>%
  ungroup()

od_is_valid(x)



# test lines
data(rivers, package = "tmap")

rivers$o <- sample(25, 1617, replace = TRUE)
rivers$d <- sample(25, 1617, replace = TRUE)
rivers$f <- as.integer(rivers$strokelwd * 100)

x <- od(rivers, col_orig = "o", col_dest = "d")


x$E <- x$E %>%
  group_by(o, d) %>%
  summarize(f = sum(f)) %>%
  ungroup()

od_is_valid(x)
