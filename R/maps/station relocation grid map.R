library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

# Calculate number of times a station has moved:
years <- 2013:2021
data <- read.scsset(year = years, survey = "regular")

data$tow.id <- gsub("A1R", "A1", data$tow.id)
data$tow.id <- gsub("FA1", "A1", data$tow.id)

# Relabel tows:
ix <- which(year(data) == 2021 & data$station.type == "fixed")
data$tow.id[ix] <- gsub("A", "R", data$tow.id[ix])

radius <- 0.75
data$station <- station(lon(data), lat(data), distance.tol = radius*2)

# Plot survey grids:
mif <- read.gulf.spatial(layer = c("scs", "grid", "mif"))
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude <- tmp$latitude

   p <- as.polygon(mif[[i]]$longitude, mif[[i]]$latitude)
   ix <- in.polygon(p, lon(data), lat(data))
   mif[[i]]$n <- length(unique(data$station[ix])) - 1
}

clg()
file <- paste0("results/maps/station relocation map 2013-2021")
gdevice("pdf", file = file, height = 8.5, width = 8.5)

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")

map("bathymetry")

map("coast")
n <- unlist(lapply(mif, function(x) return(x$n)))
for (i in 1:length(mif)){
   polygon(mif[[i]]$longitude, mif[[i]]$latitude, col = grey(1-n[i]/max(n)))
   if (n[i] > 7) col <- "white" else col <- "black"
   text(mean(mif[[i]]$longitude[1:4]), mean(mif[[i]]$latitude[1:4]), n[i], cex = 0.5, col = col)
}

# Plot fishing zones:
x <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
x <- x[x@data$label %in% c("12", "12E", "12F", "19"), ]
plot(x, add = TRUE, lwd = 0.8)

map("coast")
map.axis(1:4)

# Provinces and regions:
text(-63.45, 46.38, "Prince Edward Island", srt = -18, cex = 0.75, font = 2)
text(-60.9, 46.38, "Cape Breton", srt = 58, cex = 0.85, font = 2)
text(-63.5, 45.65, "Nova Scotia", srt = 0, cex = 1.0, font = 2)
text(-65.65, 46.75, "New Brunswick", srt = 0, cex = 1.0, font = 2)
text(-65.5, 48.65, "Québec", srt = 0, cex = 1.0, font = 2)

# Fishing zones:
#text(-63, 47.5, "Area 12", srt = 0, cex = 1.0, font = 2)
#text(-62.4, 48.48, "Area 12E", srt = -25, cex = 0.9, font = 2)
#text(-60.75, 47.685, "Area 12F", srt = -30, cex = 0.9, font = 2)
#text(-60.9, 47.0, "Area 19", srt = 60, cex = 0.9, font = 2)

box()
wind.rose()

dev.off()

# Relocation table:


