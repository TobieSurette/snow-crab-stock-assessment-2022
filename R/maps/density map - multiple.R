library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")["gulf"]

var <- "COM"  # "COM" # "MIGE56" #"COMSC345" #
output <- "tiff" # jpeg <- TRUE
weight <- FALSE

# Uncorrected variables:
res <- list()
m <- list()
years <- 2011:2021
for (i in 1:length(years)){
   print(years[i])
   # Read three years of data (for variogram averaging):
   s <- read.scsset(year = (years[i]-2):years[i], survey = "regular", valid = 1) # Tow data.
   b <- read.scsbio(year = (years[i]-2):years[i], survey = "regular")            # Biological data.
   b$tow.id <- tow.id(b)

   # Import catch data:
   import(s, fill = 0) <- catch(b, category = var, weight = weight, as.hard.shelled = TRUE, units = "t") # Merge catches.
   s[var] <- 1000000 * s[var] / repvec(s$swept.area, ncol = length(var))   # Convert to tonnes per km2.

   m[[i]] <- ked(s, variables = var, variogram.average = 3)

   res[[i]] <- summary(m[[i]], polygon = p)
}
names(res) <- years

# Display density maps:
library(akima)

# Prepare plot:
height <- 11
width <- 8.5

# Draw map:
clg()

tiff(file = paste0("results/maps/density.map.", var, ".", years[1], "-", years[length(years)], ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

k <- kronecker(matrix(1:12, ncol = 3, byrow = TRUE), matrix(1, nrow = 3, ncol = 5))
k <- rbind(0, cbind(0, k, 0), 0)
layout(k)
par(mar = c(0, 0, 0, 0))
r <- NULL
for (i in 1:length(years)){
   if (i %in% c(1))   axis.side <- c(2,3)
   if (i %in% c(10))  axis.side <- c(1,2)
   if (i %in% c(11))  axis.side <- c(1)
   if (i %in% c(4,7)) axis.side <- c(2)
   if (i %in% c(2))   axis.side <- c(3)
   if (i %in% c(3))   axis.side <- c(3,4)
   if (i %in% c(5,8)) axis.side <- NULL
   if (i %in% c(6,9)) axis.side <- c(4)

   # Background map:
   map.new(xlim = c(-66.5, -60-1/6), ylim = c(45.5, 49+1/6))
   map("coast")

   mu <- m[[i]]$map[,,1]
   lon <- m[[i]]$map.longitude
   lat <- m[[i]]$map.latitude
   index <- !is.na(lon) & !is.na(lat) & !is.na(mu)
   mu <- mu[index]
   lon <- lon[index]
   lat <- lat[index]

   xx <- seq(-66.5, -60, len = 400)
   yy <- seq(45, 49, len = 400)
   zz <- interp(x = lon, y = lat, z = mu, xo = xx, yo = yy,  linear = TRUE, extrap = TRUE, duplicate = "mean")$z

   xxx <- repvec(xx, nrow = length(yy))
   yyy <- repvec(yy, ncol = ncol(xxx))
   xxx <- t(xxx)
   yyy <- t(yyy)
   index <- in.polygon(as.polygon(p$gulf$longitude, p$gulf$latitude), xxx, yyy)
   dim(index) <- dim(zz)
   zz[!index] <- NA

   cols <- colorRampPalette(c("blue4", "blue", "mediumturquoise", "yellow", "orange", "red", "darkred"))

   #image(xx, yy, 1000 * zz, add = TRUE, col = cols(100), breaks = c(seq(0, 3000, len = 100), 10000))

   if (var %in% c("COM", "COMSC12", "COMSC345")){
      breaks = c(seq(0, 5000, by = 500), 15000)
      if (weight) zz <- 1000 * zz
   }
   if (var == "MIGE56"){
      breaks = c(seq(0, 15000, by = 1500), 100000)
   }

   image(xx, yy, zz, add = TRUE, col = cols(length(breaks)-1), breaks = breaks)

   # Fishing zones:
   v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
   v <- subset(v, label %in% c("12", "12E", "12F", "19"))
   plot(v, add = TRUE)

   lines(p[[1]]$longitude, p[[1]]$latitude, lwd = 0.5)

   text(-61, 48.7, years[i], cex = 1.4)

   if (length(axis.side) > 0) map.axis(axis.side)

   box(lwd = 1)

   # Store results:
   ix <- !is.na(zz)
   xxx <- as.numeric(repvec(xx, ncol = ncol(zz))[ix])
   yyy <- as.numeric(repvec(yy, nrow = ncol(zz))[ix])
   zzz <- zz[ix]
   if (i == 1) r <- data.frame(year = years[i], x = xxx, y = yyy, z = zzz) else r <- rbind(r, data.frame(year = years[i], x = xxx, y = yyy, z = zzz))
}

#colors <- colorRampPalette(c("blue", "yellow", "orange", "red", "darkred"))
str <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
str[length(str)] <- paste(breaks[length(breaks)-1], "+")

plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
legend("center",
       legend = rev(str),
       pt.bg = rev(cols(length(breaks))),
       pch = 22,
       pt.cex = 3,
       cex = 0.95,
       bty = "n",
       title = ifelse(weight, "kg / km2", "# / km2"))

dev.off()
