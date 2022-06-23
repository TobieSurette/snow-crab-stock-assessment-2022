library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

clg()

year <- 2021
language <- language("en")

qc.str  <- "Québec"
if (language == "english"){
   pei.str  <- "Prince Edward Island"
   cb.str   <- "Cape Breton"
   ns.str   <- "Nova Scotia"
   nb.str   <- "New\nBrunswick"
   legend.str = c("Good tow", "Repeat Tow", "Alternate tow", "Abandonned tow")

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
   nb.str   <- "Nouveau-\nBrunswick"
   legend.str = c("Bon trait", "Trait répété", "Station alternative", "Trait abandonné")

   # Fishing zones:
   zone12.str <- "Zone 12"
   zoneE.str  <- "Zone 12E"
   zoneF.str  <- "Zone 12F"
   zone19.str <- "Zone 19"
}

if (language == "bilingual"){
   pei.str  <- "PEI / IPE"
   cb.str   <- "Cap-Breton"
   ns.str   <- "Nova Scotia\nNouvelle-Ecosse"
   nb.str   <- "New Brunswick\nNouveau-Brunswick"
   legend.str = c("Good tow / Bon trait", "Repeat / Répété", "Alternate / Alternative", "Abandoned / Abandonné")

   # Fishing zones:
   zone12.str <- "Area / Zone 12"
   zoneE.str  <- "Area / Zone 12E"
   zoneF.str  <- "Area / Zone 12F"
   zone19.str <- "Area / Zone 19"
}

height <- 7
width <- 8.5

# Draw map:
clg()
tiff(file = paste0("results/maps/survey tow map ", year, " - ", language, ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

# Read complete set of tows:
s <- read.scsset(year, survey = "regular")

# Relabel tows:
ix <- which(year(s) == 2021 & s$station.type == "fixed")
s$tow.id[ix] <- gsub("A", "R", s$tow.id[ix])

# Determine set of abandonned tows:
t <- table(substr(s$tow.id, 3,5), s$valid)
abandonned <- rownames(t)[t[, 2] == 0]
a <- s[(substr(s$tow.id, 6, 6) == "F") & (substr(s$tow.id, 3, 5) %in% abandonned), ]

# Remove invalid tows:
s <- s[s$valid == 1, ]

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")

#map("bathymetry")

# Plot survey grids:
grids <- read.gulf.spatial("scs grids")
for (i in 1:length(grids)){
   z <- km2deg(grids[[i]]$x, grids[[i]]$y)
   polygon(z$longitude, z$latitude, border = "grey40", lwd = 0.5)

}

# Plot fishing zones:
x <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
x <- x[x@data$label %in% c("12", "12E", "12F", "19"), ]
plot(x, add = TRUE, lwd = 0.8)


# Plot survey stations:
ix <- substr(s$tow.id, 6, 6) == "F"
points(lon(s)[ix], lat(s)[ix], pch = 21, bg = "grey", col = "grey50", cex = 0.8)

ix <- substr(s$tow.id, 6, 6) == "A"
points(lon(s)[ix], lat(s)[ix], pch = 22, bg = "green", col = "grey50", cex = 1.2)

ix <- substr(s$tow.id, 6, 6) == "R"
points(lon(s)[ix], lat(s)[ix], pch = 23, bg = "blue", col = "grey50", cex = 1.2)

points(lon(a), lat(a), pch = 24, bg = "red", col = "grey50", cex = 1.2)

map("coast")
map.axis(1:4)

# Provinces and regions:
text(-63.45, 46.38, pei.str, srt = -18, cex = 0.75, font = 1)
text(-60.9, 46.38, cb.str, srt = 58, cex = 0.85, font = 1)
text(-63.5, 45.65, ns.str, srt = 0, cex = 1.0, font = 1 )
text(-65.60, 46.75, nb.str, srt = 0, cex = 1.0, font = 1)
text(-65.5, 48.60, qc.str, srt = 0, cex = 1.0, font = 1)

# Fishing zones:
text(-63, 47.5, zone12.str, srt = 0, cex = 1.0, font = 2)
text(-62.4, 48.48, zoneE.str, srt = -25, cex = 0.9, font = 2)
text(-60.75, 47.685, zoneF.str, srt = -30, cex = 0.9, font = 2)
text(-60.9, 47.0, zone19.str, srt = 60, cex = 0.9, font = 2)

box()
wind.rose()

# Legend:
legend("bottomleft",
       legend = legend.str,
       pch = c(21, 23, 22, 24),
       pt.cex = 1.5,
       pt.bg = c("grey", "blue", "green", "red"),
       col = "grey50",
       cex = 1.15,
       bg = "white")

dev.off()
