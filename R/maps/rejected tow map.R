library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

years <- 2000:2021
s <- read.scsset(years, survey = "regular")

# Proportion of bad tows per year:
gdevice("pdf", file = "results/figures/scs rejected tow rate - barplot", height = 5, width = 7)
t <- table(year(s), s$valid)
plot(range(years), c(0, 25), type = "n", xlab = "", ylab = "", yaxs = "i")
grid()
gbarplot(100 * t[, 1] / apply(t, 1, sum), add = TRUE)
mtext("Percentage of rejected tows", 2, 2.5, cex = 1.25)
mtext("Year", 1, 2.75, cex = 1.25)
dev.off()

# Map of bad tow occurence:
years <- 1990:2021
s <- read.scsset(years, survey = "regular")

clg()
file <- paste0("results/maps/rejected tow map 1990-2021")
gdevice("pdf", file = file, height = 8.5, width = 8.5)

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")
map("bathymetry")

# Plot survey grids:
mif <- read.gulf.spatial(layer = c("scs", "grid", "mif"))
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude <- tmp$latitude
}

n <- unlist(lapply(mif, function(x) return(x$n)))
for (i in 1:length(mif)) lines(mif[[i]]$longitude, mif[[i]]$latitude, col = "grey50")

map("coast")

# Plot fishing zones:
x <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
x <- x[x@data$label %in% c("12", "12E", "12F", "19"), ]
plot(x, add = TRUE, lwd = 0.8)

map("coast")
map.axis(1:4)

# Plot survey points:
points(lon(s[s$valid == 1, ]), lat(s[s$valid == 1, ]), pch = 21, bg = "grey70", col = "grey50", lwd = 0.1, cex = 0.3)
points(lon(s[s$valid == 0, ]), lat(s[s$valid == 0, ]), pch = 21, bg = "red", col = "red", lwd = 0.1, cex = 0.375)

# Provinces and regions:
text(-63.45, 46.38, "Prince Edward Island", srt = -18, cex = 0.75, font = 2)
text(-60.9, 46.38, "Cape Breton", srt = 58, cex = 0.85, font = 2)
text(-63.5, 45.65, "Nova Scotia", srt = 0, cex = 1.0, font = 2)
text(-65.65, 46.75, "New Brunswick", srt = 0, cex = 1.0, font = 2)
text(-65.5, 48.65, "Québec", srt = 0, cex = 1.0, font = 2)

# Fishing zones:
text(-63, 47.5, "Area 12", srt = 0, cex = 1.0, font = 2)
text(-62.4, 48.48, "Area 12E", srt = -25, cex = 0.9, font = 2)
text(-60.75, 47.685, "Area 12F", srt = -30, cex = 0.9, font = 2)
text(-60.9, 47.0, "Area 19", srt = 60, cex = 0.9, font = 2)

box()
wind.rose()

dev.off()
