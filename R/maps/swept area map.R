library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)


year <- 2021
s <- read.scsset(year = year, survey = "regular", valid = 1) # Tow data.
map.new()
ix <- s$swept.area.method == "model"
points(lon(s)[ix], lat(s)[ix], cex = 0.0005 * s$swept.area[ix], pch = 21, bg = "grey")
ix <- s$swept.area.method == "average"
points(lon(s)[ix], lat(s)[ix], cex = 0.0005 * s$swept.area[ix], pch = 21, bg = NA)
map("coast")
mtext(year, 3, 1.56, cex = 1.5)

s <- read.scsset(year = 2014:2021, survey = "regular", valid = 1) # Tow data.

t <- table(year(s), s$swept.area.method)
