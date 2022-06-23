library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

year <- 2021
output <- "tiff"  # "pdf", "jpg", "tiff"
language <- language("fr")
bathymetry <- TRUE

# Define text annotations:
qc.str  <- "Québec"
if (language == "english"){
   pei.str  <- "Prince Edward Island"
   cb.str   <- "Cape Breton"
   ns.str   <- "Nova Scotia"
   nb.str   <- "New\nBrunswick"
   soft.str <- "Soft/white crab closure"
   narw.str <- "NARW closure"
}
if (language == "french"){
   pei.str  <- "Ile-du-Prince-Edouard"
   cb.str   <- "Cap-Breton"
   ns.str   <- "Nouvelle-Ecosse"
   nb.str   <- "Nouveau-\nBrunswick"
   soft.str <- "Fermeture crabe mou/blanc"
   narw.str <- "Fermeture baleine noire"
}

# Read grid closures:
x <- read.csv(locate(package = "gulf.data", keywords = c("narw", "closure", "csv")))  # NARW closures.
s <- read.csv(locate(package = "gulf.data", keywords = c("soft", "closure", "csv")))  # Soft crab closures.
ix <- which(substr(s$name, 1, 1) %in% c("G", "H", "I"))
s$name[ix] <- toupper(substr(s$name[ix], 1, 4))
sectors <- s[grep("sector", tolower(s$name)), ]
s <- s[ix, ]
s <- s[year(s$date.closed) %in% year, ]
if (nrow(s) > 0){
   s$date.closed <- as.POSIXct(s$date.closed)
   s$date.open <- as.POSIXct(paste0(year(s$date.closed), "-12-31"))
   s$type <- "soft crab"
}
static.2018 <- read.gulf.spatial("kriging revised")$static_closure_2018
static.2019 <- read.gulf.spatial("kriging revised")$static_closure_2019

# Reformat dates:
field <- function(x, i = 1) as.numeric(unlist(lapply(strsplit(x, "/"), function(x) x[i])))
x$date.closed <- as.POSIXct(x$date.closed)
x$date.open[x$date.open == ""] <- NA
x$date.open <- as.POSIXct(x$date.open)
x$type <- "NARW"
x <- x[year(x$date.closed) == year, ]
s <- s[setdiff(names(s), "comment")]
if (nrow(x) == 0){
   names(s) <- gsub("name", "grid", names(s))
   x <- s
}else{
   x$grid <- gsub(" ", "", x$grid)
   x <- aggregate(x["date.open"], by = x[c("grid", "date.closed", "type")], max) # Remove redundancies.
   if (nrow(s) > 0){
      names(s) <- gsub("name", "grid", names(s))
      x <- rbind(x[names(s)], s)
   }
}

# Read fishery start dates:
dates <- read.csv(locate(package = "gulf.data", keywords = c("season", "csv")))
dates <- dates[year(as.POSIXct(dates$start.date)) == year, ]

# Tabulate number of closed days during fishery:
x$zone <- fishing.zone((grid2deg(x$grid)[, 1] + grid2deg(x$grid)[, 3]) / 2, (grid2deg(x$grid)[, 2] + grid2deg(x$grid)[, 4]) / 2, species = 2526)
x$zone[is.na(x$zone)] <- "12"
x <- x[which(!(x$zone %in% c("12A", "16", "12B", "17"))), ]
x$date.open[is.na(x$date.open)] <- max(x$date.open, na.rm = TRUE)

zones <- sort(unique(x$zone))
x$days <- 0

start.date <- min(as.POSIXct(dates$start.date))
end.date <- max(as.POSIXct(dates$end.date))
len <- as.numeric(difftime(end.date, start.date, units = "days"))
len <- as.character(start.date + (60*60*24)*(0:len))
m <- matrix(0, nrow = nrow(x), ncol = length(len))
colnames(m) <- len
for (i in 1:length(zones)){
   start.date <- as.POSIXct(dates$start.date[dates$zone == zones[i]])
   end.date   <- as.POSIXct(dates$end.date[dates$zone == zones[i]])

   len <- as.numeric(difftime(end.date, start.date, units = "days"))
   len <- as.character(start.date + (60*60*24)*(0:len))

   # Generate dates that span season:
   for (j in 1:nrow(x)){
      xx <- grid2deg(x$grid[j])
      yy <- mean(as.numeric(xx[1,c(2, 4)]))
      xx <- mean(as.numeric(xx[1,c(1, 3)]))
      if (!is.na(x$zone[j]) & (x$zone[j] == zones[i])){
         days <- round(as.numeric(difftime(x$date.open[j], x$date.closed[j], units = "days")))
         days <- substr(as.character(x$date.closed[j] + (60*60*24)*(0:days)), 1, 10)
         days <- days[days %in% len]
         if (length(days) > 0){
            m[j, days] <- 1
            x$days[j] <- length(days) # Total days closed during season.
         }
      }
   }
}

height <- 7
width <- 8.5

# Draw map:
clg()
if (output == "pdf")  pdf(file = paste0("results/maps/area closures ", year, " - ", language, ".pdf"), height = height, width = width)
if (output == "jpg")  jpeg(file = paste0("results/maps/area closures ", year, " - ", language, ".jpg"), quality = 100, res = 400, height = height * 100, width = width * 100)
if (output == "tiff") tiff(file = paste0("results/maps/area closures ", year, " - ", language, ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")#map("bathymetry")

if (bathymetry) map("bathymetry")

vline(-66:-60, col = "grey50", lty = "dashed")
hline(44:49, col = "grey50", lty = "dashed")

if (year == 2021){
   z <- read.csv("R/sector1.csv")
   polygon(z$longitude, z$latitude, col = "grey50", lwd = 0.50, border = "grey30")
   #if (language == "english") text(-65.35, 47.95, "Closed June 16th", cex = 0.75)
   #if (language == "french")  text(-65.35, 47.95, "Fermé 16 Juin", cex = 0.75)

   z <- read.gulf.spatial("sector")
   names(z) <- gsub("x", "longitude", names(z))
   names(z) <- gsub("y", "latitude", names(z))
   sectors <- sectors[sectors$name %in% setdiff(sectors$name, "Sector 1"), ]
   sectors <- gsub("Sector ", "", sectors$name)
   for (i in 1:length(sectors)){
      ix <- which(z$sector == sectors[i])
      polygon(z$longitude[ix], z$latitude[ix], col = "grey50", lwd = 0.50, border = "grey30")
   }
}

# Display closed grids:
plot.grid(unique(x$grid[(x$days > 1) & (x$type == "soft crab")]), col = "grey50", lwd = 0.50, border = "grey30")
if (year > 2018) plot.grid(unique(x$grid[(x$days > 1) & (x$type == "NARW")]), col = "grey30", lwd = 0.50, border = "grey30", density = 24)

# Display number of days closed:
r <- aggregate(m, by = x["grid"], sum)
r[,-1] <- as.numeric(r[,-1] > 0)
r$total <- apply(r[, -1], 1, sum)
r <- r[r$total > 1, ]
r$x <- (grid2deg(r$grid)[, 1] + grid2deg(r$grid)[, 3]) / 2
r$y <- (grid2deg(r$grid)[, 2] + grid2deg(r$grid)[, 4]) / 2
r <- r[c("grid", "x", "y", "total")]

text(r$x, r$y, r$total, cex = 0.75, col = "white", font = 2)
text(r$x, r$y, r$total, cex = 0.65, font = 2)
map("coast")

# Fishing zones:
v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
v <- subset(v, label %in% c("12", "12E", "12F", "19"))
plot(v, add = TRUE, lwd = 0.5)

if (year == 2018){
   polygon(static.2018$longitude, static.2018$latitude, col = "grey30", border = "grey30", lwd = 0.50, density = 24)
   lines(static.2018$longitude, static.2018$latitude, lwd = 1.5, col = "grey30")
}
if (year == 2019){
   polygon(static.2019$longitude, static.2019$latitude, col = "grey30", border = "grey30", lwd = 0.50, density = 24)
   lines(static.2019$longitude, static.2019$latitude, lwd = 1.5, col = "grey30")
}

# Provinces and regions:
text(-63.45, 46.38, pei.str, srt = -18, cex = 0.75, font = 1)
text(-60.9, 46.38, cb.str, srt = 58, cex = 0.85, font = 1)
text(-63.5, 45.65, ns.str, srt = 0, cex = 1.0, font = 1 )
text(-65.60, 46.75, nb.str, srt = 0, cex = 1.0, font = 1)
text(-65.5, 48.60, qc.str, srt = 0, cex = 1.0, font = 1)

# Display side grids:
plot.grid("GQ22", col = "white", border = "white")
str <- paste0("GQ", 23:60)
plot.grid(str, col = "papayawhip")
tmp <- grid2deg(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(str, 3, 4), cex = 0.60)
str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)), 22)
str <- str[which(substr(str, 1, 2) == "GR"):which(substr(str, 1, 2) == "HN")]
plot.grid(str, col = "papayawhip")
tmp <- grid2deg(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(str, 1, 2), cex = 0.60)

# Display legend:
plot.grid("HJ24", col = "white", lwd = 0.50, border = "grey30", density = 24)
plot.grid("HK24", col = "grey50", lwd = 0.50, border = "grey30")
text(-66, 46 + 1/12, narw.str, pos = 4, cex = 1.00)
text(-66, 46 - 1/12, soft.str, pos = 4, cex = 1.00)

wind.rose(-60.5, 48.6)

map.axis(1:2)
box(col = "grey50")

if (output %in% c("pdf", "jpg", "tiff")) dev.off()



