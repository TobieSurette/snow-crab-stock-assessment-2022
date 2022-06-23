library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

year <- 2017:2021
var <- "effort"  # "cpue", "effort", or "landings"
output <- "tiff"  # "pdf", "jpg", "tiff"
language <- language("fr")

grid.summary.logbook <- function(x, resolution = 0.05, by = NULL){
   # Grid coordinates:
   x$lon = round(x$longitude / resolution) * resolution
   x$lat = round(x$latitude / resolution) * resolution

   # Calculate grid statistics:
   grid <- aggregate(list(landings = x[, "slip.prop.day"]), by = x[c("lon", "lat", by)], sum, na.rm = TRUE)
   grid$effort <- aggregate(list(effort = x[, "trap.day"]), by = x[c("lon", "lat", by)], sum, na.rm = TRUE)$effort
   grid$cpue <- grid$landings / grid$effort

   # Calculate grid areas:
   grid$area <- NA
   lats <- sort(unique(grid$lat))
   for (i in 1:length(lats)){
      yy <- c(lats[i] - resolution/2, lats[i] + resolution/2)
      xx <- c(-64 - resolution/2, -64 + resolution/2) # Area does not change with longitude.
      xx <- c(xx[1], xx[1], xx[2], xx[2])
      yy <- c(yy[1], yy[2], yy[2], yy[1])
      tmp <- deg2km(xx, yy)
      p <- as.polygon(tmp$x, tmp$y)
      grid$area[grid$lat == lats[i]] <- area(p)
   }

   return(grid)
}

map.fishery <- function(year, variable, resolution = 0.03, bathymetry = TRUE, legend = TRUE, language = "en", cex = 1){
   language <- language(language)

   var <- variable

   # Define text annotations:
   qc.str  <- "Québec"
   landings.str <- paste(expression("t / km2"))
   cpue.str <- paste(expression("kg /trap"))
   if (language == "english"){
      pei.str <- "Prince Edward Island"
      cb.str  <- "Cape Breton"
      ns.str  <- "Nova Scotia"
      nb.str  <- "New\nBrunswick"
      effort.str <- paste(expression("traps / km2"))
   }
   if (language == "french"){
      pei.str <- "Ile-du-Prince-Edouard"
      cb.str  <- "Cap-Breton"
      ns.str  <- "Nouvelle-Ecosse"
      nb.str  <- "Nouveau\nBrunswick"
      effort.str <- paste(expression("trappes / km2"))
   }

   # Read fishery logbook data:
   x <- read.sc.logbook(year)
   x$flag[which(x$flag == "")] <- NA
   x <- x[is.na(x$flag), ]

   # Generate grid summary statistics:
   grid <- grid.summary.logbook(x, resolution = resolution)

   # Define variable to de diplayed:
   if (var == "landings") grid$value <- 0.001 * grid[, var] / grid$area
   if (var == "effort") grid$value <- grid[, var] / grid$area
   if (var == "cpue") grid$value <- grid$cpue

   # Transform grid to square form:
   lons <- seq(min(grid$lon), max(grid$lon), by = resolution)
   lats <- seq(min(grid$lat), max(grid$lat), by = resolution)
   tmp <- expand.grid(lons, lats)
   names(tmp) <- c("lon", "lat")
   tmp$value <- NA
   grid$x <- NA
   grid$y <- NA
   for (i in 1:nrow(grid)){
      ix <- which.min((grid$lon[i] - tmp$lon)^2 + (grid$lat[i] - tmp$lat)^2)
      tmp$value[ix] <- grid$value[i]
      grid$x[i] <- match(tmp$lon[i], tmp$lon)
      grid$y[i] <- match(tmp$lat[i], tmp$lat)
   }
   z <- tmp$value
   dim(z) <- c(length(lons), length(lats))

   # Remove single isolated grids:
   ix <- 2:(nrow(z)-1)
   iy <- 2:(ncol(z)-1)
   iz <- !is.na(z[ix, iy]) & is.na(z[ix-1, iy]) & is.na(z[ix+1, iy]) & is.na(z[ix, iy-1]) & is.na(z[ix, iy+1])
   iz <- iz & is.na(z[ix-1, iy-1]) & is.na(z[ix+1, iy+1]) & is.na(z[ix+1, iy-1]) & is.na(z[ix-1, iy+1])
   iz <- rbind(FALSE, cbind(FALSE, iz, FALSE), FALSE)
   iz[!iz] <- NA

   # Draw map:
   if (var == "landings") scale <- seq(0, 5, len = 11)  # Tonnes per square kilometer.
   if (var == "effort")   scale <- seq(0, 80, len = 9)  # Traps per square kilometer.
   if (var == "cpue")     scale <- seq(0, 150, by = 15) # kg per trap.

   breaks <- c(scale, max(grid$value[!is.na(grid$value) & is.finite(grid$value)]))
   cols <- rev(grey(scale / max(scale)))

   map.new(xlim = c(-66, -60.25), ylim = c(45.5, 49))
   rect(par("usr")[1] + 1/6, par("usr")[3], par("usr")[2], par("usr")[4]-1/6, border = "skyblue4", lwd = 2)

   if (bathymetry) map("bathymetry")

   image(lons, lats, z, add = TRUE, breaks = breaks, col = cols)
   #points(grid$lon, grid$lat, cex = 0.5 * sqrt(grid$value))

   vline(seq(-66, -60), lty = "dotted", col = "grey70")
   hline(seq(46, 49), lty = "dotted", col = "grey70")

   static <- NULL
   if (year == 2018) static <- read.gulf.spatial("kriging revised")$static_closure_2018
   if (year == 2019) static <- read.gulf.spatial("kriging revised")$static_closure_2019
   if (!is.null(static)){
      polygon(static$longitude, static$latitude, col = "grey30", border = "grey30", lwd = 0.50, density = 24)
      lines(static$longitude, static$latitude, lwd = 1.0, col = "grey30")
   }

   map("coast")
   box()

   # Map fishing zones:
   f <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
   f <- f[f@data$label %in% c("12", "12E", "12F", "19"), ]
   plot(f, add = TRUE, lwd = 0.8)

   # Provinces and regions:
   text(-63.45, 46.38, pei.str, srt = -18, cex = cex * 0.75, font = 1)
   text(-60.9, 46.38, cb.str, srt = 58, cex = cex * 0.85, font = 1)
   text(-63.5, 45.65, ns.str, srt = 0, cex = cex * 1.0, font = 1 )
   if (!legend) text(-65.50, 46.75, nb.str, srt = 0, cex = cex * 1.0, font = 1)
   text(-65.5, 48.60, qc.str, srt = 0, cex = cex * 1.0, font = 1)

   if (legend){
      if (var == "landings") title <- landings.str
      if (var == "effort")   title <- effort.str
      if (var == "cpue")     title <- cpue.str
      legend("bottomleft",
             legend = paste0(scale[-length(scale)], " - ", scale[-1]),
             pch = 22,
             pt.bg = cols,
             title = title,
             pt.lwd = 0.5,
             pt.cex = 4, cex = 1.25, bg = "white")
   }
}

if (length(year) == 1) year.str <- as.character(year)
if (length(year) > 1) year.str <- paste0(min(year), "-", max(year))

height <- 7
width <- 8.5
if (length(year) > 1){
   height = 11
   width = 8.5
}

if (output == "pdf") pdf(file = paste0("results/maps/logbook ", var, " map ", year.str, " - ", language, ".pdf"), height = height, width = width)
if (output == "jpg") jpeg(file = paste0("results/maps/logbook ", var, " map ", year.str, " - ", language, ".jpg"), quality = 100, res = 400, height = height * 100, width = width * 100)
if (output == "tiff") tiff(file = paste0("results/maps/logbook ", var, " map ", year.str, " - ", language, ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

if (length(year) == 5){
   m <- kronecker(t(matrix(c(1:4, rep(5, 4)), nrow = 2)), matrix(1, nrow = 6, ncol = 6))
   m <- rbind(0, cbind(0, m, 0), 0)
   layout(m)
   par(mar = c(0,0,0,0))
}

for (i in 1:length(year)){
   if (i == length(year)){
      map.fishery(year[i], var = var, bathymetry = TRUE, legend = TRUE, language = language, cex = 1.5)
   }else{
      map.fishery(year[i], var = var, bathymetry = TRUE, legend = FALSE, language = language)
   }
   text(par("usr")[1] + 0.9 * diff(par("usr")[1:2]),
        par("usr")[3] + 0.9 * diff(par("usr")[3:4]),
        year[i], font = 2, cex = 1.5)
   if (length(year) == 1){
      axis(1:2)
   }else{
      if (i == 1) map.axis(2:3)
      if (i == 2) map.axis(3:4)
      if (i == 3) map.axis(2)
      if (i == 4) map.axis(4)
      if (i == 5) map.axis(c(1,2,4))
   }
}

if (output %in% c("pdf", "jpg", "tiff")) dev.off()

# Calculate fishing area by percentiles:
#sum(grid$landings)

# Calculate proportion of variable with no spatial coordinates:
#ix <- which(is.na(x$longitude) | is.na(x$latitude))
#print(1-sum(grid$landings) / sum(x[, "slip.prop.day"]))

