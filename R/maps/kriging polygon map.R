library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

clg()
language <- language("en")
height <- 7
width <- 8.5

qc.str <- "Québec"
if (language == "english"){
   pei.str  <- "Prince Edward Island"
   cb.str   <- "Cape Breton"
   ns.str   <- "Nova Scotia"
   nb.str   <- "New\nBrunswick"

   # Fishing zones:
   zone12.str <- "Area 12"
   zoneE.str  <- "Area 12E"
   zoneF.str  <- "Area 12F"
   zone19.str <- "Area 19"
}

if (language == "french"){
   pei.str  <- "Ile-du-Prince-Edouard"
   cb.str   <- "Cap-Breton"
   ns.str   <- "Nouvelle-Ecosse"
   nb.str   <- "Nouveau\nBrunswick"

   # Fishing zones:
   zone12.str <- "Zone 12"
   zoneE.str  <- "Zone 12E"
   zoneF.str  <- "Zone 12F"
   zone19.str <- "Zone 19"
}


tiff(file = paste0("results/maps/kriging polygon map - ", language, ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")

#map("bathymetry")

# Plot survey grids:
x <- read.gulf.spatial("kriging revised")

# Show fishing zone kriging polygons:
cols <- colorRamp(c("khaki1", "darkolivegreen3"))((2:5)/5) / 255
cols <- rgb(cols[,1], cols[,2], cols[,3])

border <- "grey50"
polygon(x$zone12$longitude, x$zone12$latitude, border = border, col = cols[1], lwd = 0.8)
polygon(x$zoneE$longitude, x$zoneE$latitude, border = border, col = cols[2], lwd = 0.8)
polygon(x$zoneF$longitude, x$zoneF$latitude, border = border, col = cols[3], lwd = 0.8)
polygon(x$zone19$longitude, x$zone19$latitude, border = border, col = cols[4], lwd = 0.8)

#polygon(x$gulf$longitude, x$gulf$latitude, border = "grey30", lwd = 2.8)


# Buffer zones:
polygon(x$zoneEF_unassigned$longitude, x$zoneEF_unassigned$latitude, border = border, col = "coral1", lwd = 0.8)
polygon(x$zone19_F_buffer$longitude, x$zone19_F_buffer$latitude, border = border, col = "coral2", lwd = 0.8)
polygon(x$zone19_12_buffer$longitude, x$zone19_12_buffer$latitude, border = border, col = "coral3", lwd = 0.8)

# Plot fishing zones:
x <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
x <- x[x@data$label %in% c("12", "12E", "12F", "19"), ]
plot(x, add = TRUE, lwd = 0.6)

grid(col = "grey60")
map("coast")
map.axis(1:4)

# Provinces and regions:
text(-63.45, 46.38, pei.str, srt = -18, cex = 0.75, font = 2)
text(-60.9, 46.38,  cb.str,  srt = 58, cex = 0.85, font = 2)
text(-63.5, 45.65,  ns.str,  srt = 0, cex = 1.0, font = 2)
text(-65.65, 46.75, nb.str,  srt = 0, cex = 1.0, font = 2)
text(-65.5, 48.65,  qc.str,  srt = 0, cex = 1.0, font = 2)

# Fishing zones:
text(-63, 47.5,      zone12.str, srt = 0, cex = 1.0, font = 2)
text(-62.4, 48.48,   zoneE.str, srt = -25, cex = 0.9, font = 2)
text(-60.75, 47.685, zoneF.str, srt = -30, cex = 0.9, font = 2)
text(-60.9, 47.0,    zone19.str, srt = 60, cex = 0.9, font = 2)

# Buffer zones:
text(-61.05, 48.09, "A", srt = 0, cex = 1.1, font = 2)
text(-60.5, 47.40,  "B", srt = 0, cex = 1.1, font = 2)
text(-61.33, 46.49, "C", srt = 0, cex = 1.1, font = 2)

box()
wind.rose()
scale.bar(length = 80)

dev.off()

