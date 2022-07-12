library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Survey year:
years <- 1997:2021
output <- "results/tables/"
categories <- c("MIGE34L45",                                           # Instar VIII recruitment.
                "MMGE95SC12", "MMGE95SC3", "MMGE95SC4", "MMGE95SC5",   # Commercial by shell condition.
                "MM", "MMGE95", "MML95",                               # Adult males.
                "FI", "FIGNO", "FM", "FP", "FMULT",                    # Females.
                "MIGE56L69", "MIGE69L83", "MIGE83",                    # R-4, R-3, R-2, R-1 fishery recruitment.
                "MIGE83L98SC345")                                      # Skip-moulters.

categories <- c("MIGE83L98SC345")
categories <- "FM"

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")["gulf"]

# Read three years of data (for variogram averaging):
s <- read.scsset(year = (min(years)-2):max(years), survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = (min(years)-2):max(years), survey = "regular")            # Biological data.
b$tow.id <- tow.id(b)

# Import catch data:
import(s, fill = 0) <- catch(b, category = categories) # Merge catches.
s[categories] <- s[categories] / repvec(s$swept.area, ncol = length(categories)) # Standardize by swept area.

res <- NULL
m <- list()
for (i in 1:length(years)){
   # Perform kriging with external drift:
   ix <- (year(s) >= (years[i]-2)) & (year(s) <= years[i])
   m[[i]] <- ked(s[ix, ], variables = categories, variogram.average = 3, lag = 3, max.distance = 75)
}

r <- array(NA, dim = c(dim(m[[1]]$map)[1:2], length(years)))
for (i in 1:length(years)){
   r[,,i] <- m[[i]]$map[,,1]
}

image(apply(r, c(1,2), mean))

clg()
breaks <- seq(0, 6, by = 0.05)
cols <- c(colorRampPalette(c("blue", "white"))(sum(breaks <= 1)), colorRampPalette(c("white", "red"))(sum(breaks > 1)))
cols <- cols[-length(cols)]
for (i in 1:length(years)){
   image(r[,,i] / apply(r, c(1,2), mean), breaks = breaks, col = cols)
                                 
   mtext(years[i], 3)
}




