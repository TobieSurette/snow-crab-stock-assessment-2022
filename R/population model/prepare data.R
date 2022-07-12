library(gulf.data)
library(gulf.stats)
library(gulf.graphics)

years <- 2015:2021

# Read data:
s <- read.scsset(year = years, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = years, survey = "regular", sex = 1)   # Biological data.
b$tow.id <- tow.id(b)

# Determine morphometric maturity:
for (i in 1:length(years)){
   print(years[i])
   ix <- year(b) == years[i]
   bb <- b[ix, ]
   z <- rep(NA, nrow(bb))
   z[which(bb$carapace.width < 30)] <- 0 # Crab smaller than 30mm are considered immature.
   theta <- fit.morphometry.scsbio(bb$carapace.width, bb$chela.height, z, sex = 1, discrete = years[i] < 1998) # Fit morphometric model.
   v <- morphometry.scsbio(bb$carapace.width, bb$chela.height, theta = theta, discrete = years[i] < 1998)$p_mature_posterior
   b$maturity[ix] <- v > 0.5
}
b$maturity <- b$maturity == "TRUE"

# Attach size-frequencies:
groups <- c("immature", "skip", "recruit", "residual")
res <- list()
for (i in 1:length(groups)) res[i] <- list(NULL)
names(res) <- groups

# Immatures:
tmp <- s
import(tmp, fill = 0) <- freq(b[which(!b$maturity & is.new.shell(b)),], by = c("date", "tow.id"))
res$immature <- tmp

# Skip:
tmp <- s
import(tmp, fill = 0) <- freq(b[which(!b$maturity & !is.new.shell(b)),], by = c("date", "tow.id"))
res$skip <- tmp

# Recruit:
tmp <- s
import(tmp, fill = 0) <- freq(b[which(b$maturity & is.new.shell(b)),], by = c("date", "tow.id"))
res$recruit <- tmp

# Residuals:
tmp <- s
import(tmp, fill = 0) <- freq(b[which(b$maturity & !is.new.shell(b)),], by = c("date", "tow.id"))
res$residual <- tmp

# Standardize variables:
for (i in 1:length(res)){
   fvars <- names(res[[i]])[gsub("[0-9]", "", names(res[[i]])) == ""]
   res[[i]][fvars] <- 10^6 * res[[i]][fvars] / repvec(res[[i]]$swept.area, ncol = length(fvars))
}

# Frequency variables:
fvars <- unique(unlist(lapply(res, names)))
fvars <- fvars[gsub("[0-9]", "", fvars) == ""]

# Buffer frequencies with zeroes:
for (i in 1:length(res)) res[[i]][setdiff(fvars, names(res[[i]]))] <- 0

# Calculate annual size-frequencies:
f <- array(NA, dim = c(length(years), length(fvars), length(groups)))
dimnames(f) <- list(year = years, size = fvars, group = groups)
for (i in 1:length(groups)){
   tmp <- aggregate(res[[i]][fvars], by = list(year = year(res[[i]])), mean)
   rownames(tmp) <- tmp$year
   f[,,i] <- as.matrix(tmp[, -1])
}

