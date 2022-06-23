library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

# Calculate number of times a station has moved:
years <- 2013:2021
data <- read.scsset(year = years, survey = "regular")

data$tow.id <- gsub("A1R", "A1", data$tow.id)
data$tow.id <- gsub("FA1", "A1", data$tow.id)
data <- data[!is.na(lon(data)) & !is.na(lat(data)), ]
data <- cbind(data, deg2km(lon(data), lat(data)))
data <- scsset(data)

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

which(n >= 6)

# Plot grid:
i <- 236  # 38  45  49  79  80  98  99 116 117 140 141 163 204 209 226 227 236 312

clg()
tiff(file = paste0("results/maps/station relocation example grid ", i, ".tiff"), compression = "lzw", units = "in", res = 300, height = 7, width = 7)

x <- mif[[i]]$longitude
y <- mif[[i]]$latitude
map.new(range(x), range(y))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")

map("bathymetry")
grid()
#polygon(x, y)
p <- as.polygon(x, y)
ix <- which(in.polygon(p, lon(data), lat(data)))
u <- unique(data$station[ix])
cols <- rainbow(length(u))
for (j in 2:length(ix)){
   arrows(lon(data)[ix[j-1]], lat(data)[ix[j-1]],
          lon(data)[ix[j]], lat(data)[ix[j]],
          length = 0.1, col = "grey60")
}
points(lon(data)[ix[data$valid[ix] == 0]], lat(data)[ix[data$valid[ix] == 0]],
       pch = 4, col = cols[match(data$station[ix[data$valid[ix] == 0]], u)], lwd = 2)
points(lon(data)[ix[data$valid[ix] == 1]], lat(data)[ix[data$valid[ix] == 1]],
       pch = 21, bg = cols[match(data$station[ix[data$valid[ix] == 1]], u)])
stations <- unique(data$station[ix])
for (j in 1:length(stations)){
   iy <- which(data$station[ix] == stations[j])
   if (length(iy) == 1) text(mean(lon(data[ix,])[iy]), mean(lat(data[ix,])[iy]), j, pos = 2)
   if (length(iy) > 1) text(mean(lon(data[ix,])[iy]), mean(lat(data[ix,])[iy]), j)
}
map("coast")
mtext(paste0("Grid #", i), 3, 0.75, cex = 1.25)
mtext("x (km)", 1, 2.5, cex = 1.25)
mtext("y (km)", 2, 2.5, cex = 1.25)

u <- deg2km(par("usr")[1:2], par("usr")[3:4])
ux <- seq(u[1, 1], u[2,1], by = 1)
uy <- seq(u[1, 2], u[2,2], by = 1)
ux <- ux[1:min(c(length(ux), length(uy)))]
uy <- uy[1:min(c(length(ux), length(uy)))]

u <- km2deg(ux, uy)

vline(u$longitude, lty = "dashed", lwd = 1, col = "grey70")
hline(u$latitude, lty = "dashed", lwd = 1, col = "grey70")

text(par("usr")[1] + 0.14 * diff(par("usr"))[1:2],
     par("usr")[3] + 0.94 * diff(par("usr"))[3:4],
     paste0(length(unique(data$station[ix]))-1, " relocations"),
     cex = 1.25)

axis(1, at = u$longitude, labels = 0:(length(ux)-1))
axis(2, at = u$latitude, labels = 0:(length(ux)-1))

box()

dev.off()

