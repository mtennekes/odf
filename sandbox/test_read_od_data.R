# goods data
od <- readRDS("~/pCloudDrive/flows_goods/data/throughput/OD_value.rds")
points <- readRDS("~/pCloudDrive/flows_goods/data/throughput/centroids.rds")



# Create odf object
x <- od(od, points, col_flow = "Waarde")



# Dutch commuting
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")


# test lines
data(rivers, package = "tmap")

rivers$o <- 1:1617
rivers$d <- (1:1617) + 1617
rivers$f <- as.integer(rivers$strokelwd * 100)

x <- od(rivers, col_orig = "o", col_dest = "d", col_flow = "f")
