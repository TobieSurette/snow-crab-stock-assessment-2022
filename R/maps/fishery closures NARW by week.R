library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)

year <- 2019
output <- "pdf"

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

# Read fishery start dates:
dates <- read.csv(locate(package = "gulf.data", keywords = c("season", "csv")))
dates <- dates[year(as.POSIXct(dates$start.date)) == year, ]
start.date <- unique(as.POSIXct(dates$start.date[dates$zone == "12"]))

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

for (i in 1:length(weeks)){
   map.new()
   map("bathymetry")

   if (i == 1) mtext(year, 3, 1.0, at = par("usr")[2], cex = 1.25)

   start <- start.date + (weeks[i]-1) * 7 * 24 * 60 * 60
   end   <- start.date + weeks[i] * 7 * 24 * 60 * 60

   ix <- which((x$date.closed < end) & ((x$date.open > start) | is.na(x$date.open)))

   if (length(ix) > 0){
      plot.grid(x$grid[ix], col = "khaki1", lwd = 0.5)
   }
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
}

if (output == "pdf") dev.off()

