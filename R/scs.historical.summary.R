library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

years <- 1988:2021

radius <- 0.75

data <- read.scsset(year = years, valid = 1, survey = "regular")
data$station <- station(lon(data), lat(data), distance.tol = radius*2)

M <- matrix(NA, nrow = max(data$station), ncol = length(years))
rownames(M) <- 1:max(data$station)
colnames(M) <- years
for (i in 1:length(years)){
   t <- table(data$station[year(data) == years[i]])
   M[names(t), i] <- 1
}

# Survey station history figure:
tiff(file = paste0("results/figures/survey station history ", min(years), "-", max(years), ".tiff"), compression = "lzw", units = "in", res = 300, height = 7, width = 7)
image(years, as.numeric(rownames(M)), z = t(M), xlab = "", ylab = "", ylim = c(0, nrow(M)), yaxs = "i", col = "grey30")
grid()
mtext("Year", 1, 2.5, cex = 1.5)
mtext("Station number", 2, 2.5, cex = 1.5)
box()
text(2011.9, 1330, "Survey redesign 2012 -", pos = 2, cex = 0.75)
text(2012.9, 1650, "Survey redesign 2013 -", pos = 2, cex = 0.75)
text(2005.9, 1135, "Partial redistribution 2006 -", pos = 2, cex = 0.75)
dev.off()

# Where the bad tows occurred during random shuffle years:
tiff(file = paste0("results/maps/tow quality ", min(years), "-", max(years), ".tiff"), compression = "lzw", units = "in", res = 300, height = 5.5, width = 7)
bad <- read.scsset(year = years, valid = 0, survey = "regular")
map.new()
map("bathymetry")
points(longitude(bad), latitude(bad), pch  = 21, bg = "red", cex = 0.4, col = "red")
points(longitude(data), latitude(data), pch  = 21, bg = "black", cex = 0.25, col = "black")
legend("topright", legend = c("Successful", "Rejected"), bg = "white", pch = 21, pt.bg = c("black", "red"), col = c("black", "red"), pt.cex = 1.5, cex = 1.1)
x <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
x <- x[x@data$label %in% c("12", "12E", "12F", "19"), ]
plot(x, add = TRUE, lwd = 0.8)
map("coast")
map.axis(1:2)
box()
dev.off()

# Proportion of survey trawlable area:
years <- 2007:2021
data <- read.scsset(year = years, survey = "regular")
ix <- substr(data$tow.id, 6, 10) == "F"
t <- table(year(date(data))[ix], data$valid[ix])

tiff(file = paste0("results/figures/proportion successful tows ", min(years), "-", max(years), ".tiff"), compression = "lzw", units = "in", res = 300, height = 7, width = 7)
p <- t[, 1] / apply(t, 1, sum)
plot(c(min(years)-0.5, max(years)+0.5), c(0, 0.25), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()
gbarplot(p, width = 1, col = "grey90", add = TRUE)
p[!(names(p) %in% as.character(2012:2013))] <- 0
gbarplot(p, width = 1, add = TRUE, col = "grey70")

mtext("Year", 1, 2.5, cex = 1.4)
mtext("Probability", 2, 2.5, cex = 1.4)
legend("topright", legend = c("Fixed & new stations", "All new stations"), pch = 22, pt.cex = 3, pt.bg = c("grey90", "grey70"))
dev.off()

# Calculate number of times a station has moved:
years <- 2013:2021
data <- read.scsset(year = years, survey = "regular")
data$tow.id <- gsub("A1R", "A1", data$tow.id)
data$tow.id <- gsub("FA1", "A1", data$tow.id)
data$year <- year(date(data))
M <- matrix(NA, nrow = 355, ncol = length(years))
rownames(M) <- 1:355
colnames(M) <- years
for (i in 1:length(years)){
    x <- data[data$year == years[i] & data$valid == 1, ]
    x$order <- NA
    x$order[substr(x$tow.id, 6, 10) == "F"] <- 0
    x$order[substr(x$tow.id, 6, 10) == "FR"] <- 0
    x$order[substr(x$tow.id, 6, 10) == "A1"] <- 1
    x$order[substr(x$tow.id, 6, 10) == "A2"] <- 2
    x$order[substr(x$tow.id, 6, 10) == "A3"] <- 3
    index <- as.numeric(substr(x$tow.id, 3, 5))
    print(sort(index))
    M[index, i] <- x$order

    y <- data[data$year == years[i] & as.numeric(substr(data$tow.id, 3, 5)) %in% which(is.na(M[,i])), ]
    y <- y[substr(y$tow.id, 6, 10) != "FR", ]
    t <- table(substr(y$tow.id, 3, 5))
    M[as.numeric(names(t)), i] <- t-1
}
n <- apply(M, 1, sum)
gbarplot(table(n), width = 1)
mtext("Number of station changes", 1, 2.5, cex = 1.4)
mtext("Frequency", 2, 2.5, cex = 1.4)

