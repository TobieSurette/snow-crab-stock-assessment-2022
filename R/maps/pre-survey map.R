library(gulf.data)
library(gulf.spatial)

s <- read.scsset(2021, valid = 1)
s <- s[substr(s$tow.id, 1, 2) == "XP", ]

clg()
tiff(file = paste0("results/maps/pre-survey map.tiff"), compression = "lzw", units = "in", res = 300, height = 5, width = 7)

map.new(xlim = c(-61.52, -60.95), ylim = c(46.45, 46.666667));

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")
map("bathymetry")
map("coast")

points(lon(s), lat(s), pch = 21, bg = "grey", col = "grey30")

points(-61.01007, 46.62429, pch = 21, bg = "grey20")

text(-61.01007, 46.62429, "Cheticamp", pos = 2, cex = 0.75)
text(-61.01930, 46.58324, "Point Cross", pos = 2, cex = 0.70)
text(-60.96608, 46.64970, "Petit Etang", pos = 2, cex = 0.70)
text(-61.04254, 46.54736, "Grand Etang", pos = 2, cex = 0.75)
text(-61.07527, 46.49790, "Cap LeMoine", pos = 2, cex = 0.75)

points(-61.01007, 46.62429, pch = 21, bg = "grey20")
points(-61.01930, 46.58324, pch = 21, bg = "grey20")
points(-60.96608, 46.64970, pch = 21, bg = "grey20")
points(-61.04254, 46.54736, pch = 21, bg = "grey20")
points(-61.07527, 46.49790, pch = 21, bg = "grey20")

box()

map.axis(1:2)
scale.bar(pos = 4, len = 6, number = 3, cex = 0.5)

dev.off()

