library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

year <- 2019
output <- "" # "pdf"

# Read grid closures:
x <- read.csv(locate(package = "gulf.data", keywords = c("narw", "closure", "csv")))  # NARW closures.
s <- read.csv(locate(package = "gulf.data", keywords = c("soft", "closure", "csv")))  # Soft crab closures.
static.2018 <- read.gulf.spatial("kriging revised")$static_closure_2018
static.2019 <- read.gulf.spatial("kriging revised")$static_closure_2019

# Reformat date:
field <- function(x, i = 1) as.numeric(unlist(lapply(strsplit(x, "/"), function(x) x[i])))
x$date.closed <- as.POSIXct(paste0(field(x$date.closed, 3), "-", field(x$date.closed, 1), "-", field(x$date.closed, 2)))
x$date.open   <- paste0(field(x$date.open, 3), "-", field(x$date.open, 1), "-", field(x$date.open, 2))
x$date.open[grep("NA", x$date.open)] <- as.character(NA)
x$date.open <- as.POSIXct(x$date.open)
x <- x[year(x$date.closed) == year, ]
x$grid <- gsub(" ", "", x$grid)
x <- aggregate(x["date.open"], by = x[c("grid", "date.closed")], max) # Remove rendundancies.

# Read fishery start dates:
dates <- read.csv(locate(package = "gulf.data", keywords = c("season", "csv")))
dates <- dates[year(as.POSIXct(dates$start.date)) == year, ]

# Tabulate number of closed days during fishery:
x$zone <- fishing.zone((grid2deg(x$grid)[, 1] + grid2deg(x$grid)[, 3]) / 2, (grid2deg(x$grid)[, 2] + grid2deg(x$grid)[, 4]) / 2, species = 2526)
x$zone[is.na(x$zone)] <- "12"
x$zone[x$zone == "16"] <- "12"

zones <- unique(x$zone)
x$days <- NA
for (i in 1:length(zones)){
   start.date <- as.POSIXct(dates$start.date[dates$zone == zone])
   end.date   <- as.POSIXct(dates$end.date[dates$zone == zone])

   len <- as.numeric(difftime(end.date, start.date, units = "days"))
   len <- as.character(start.date + (60*60*24)*(0:len))

   m <- matrix(0, nrow = nrow(x), ncol = length(len))
   dimnames(m) <- list(x$grid, len)

   # Generate dates that span season:
   for (j in 1:nrow(x)){
      xx <- grid2deg(x$grid[j])
      yy <- mean(as.numeric(xx[1,c(2, 4)]))
      xx <- mean(as.numeric(xx[1,c(1, 3)]))
      if (!is.na(x$zone[j]) & (x$zone[j] == zone)){
         days <- as.numeric(difftime(x$date.open[j], x$date.closed[j], units = "days"))
         days <- as.character(x$date.closed[j] + (60*60*24)*(0:days))
         days <- days[days %in% colnames(m)]
         m[j, days] <- 1
      }
      x$days[j] <- sum(m[j, days]) # Total days closed during season.
   }
}

#
clg()
if (output == "pdf") pdf(file = paste0("results/maps/NARW closures ", year, ".pdf"), width = 8.5, height = 11)

m <- kronecker(matrix(1:8, ncol = 2), matrix(1, ncol = 6, nrow = 6))
m <- rbind(0,0, cbind(0, 0, m, 0, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))
weeks <- 1:8

date.str <- function(x){
   return(paste0(month.abb[month(x)], " ", day(x)))
}

map.new()
#map("bathymetry")

plot.grid(x$grid[ix], col = "khaki1", lwd = 0.5)

map("coast")

# Fishing zones:
v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
v <- subset(v, label %in% c("12", "12E", "12F", "19"))
plot(v, add = TRUE, lwd = 0.5)

box(col = "grey50")

text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.93 * diff(par("usr")[3:4]),
     paste0("week ", i, ": ", date.str(start), " to ", date.str(end)))

if (i %in% 1:4) map.axis(2)
if (i %in% c(4, 8)) map.axis(1)


if (output == "pdf") dev.off()
