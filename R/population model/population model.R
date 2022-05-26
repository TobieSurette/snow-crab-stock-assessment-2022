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

# Define growth model:
growth.matrix <- function(x, theta, ymax, ...){
   ux <- sort(unique(x))
   xmax <- max(ux)
   dx <- min(diff(ux))
   x0 <- seq(min(ux), max(ux), by = dx)
   if (length(x0) > 1000) stop("Growth matrix dimensions exceed 1000x1000.")

   # Define growth means and standard errors:
   m <- growth(theta = theta)(x0)
   s <- growth(theta = theta, error = TRUE)(x0)

   # Calculate corresponding gamma parameters:
   phi <- s^2 / m # Scale parameter.
   k <- m^2 / s^2 # Shape parameter.

   # Define growth output vector:
   if (missing(ymax)) ymax <- xmax + max(m + 3 * s)

   # Map growth increments onto growth matrix:
   y0 <- seq(min(x0), ymax, by = dx)
   ymax <- y0[length(y0)]
   G <- matrix(0, nrow = length(x0), ncol = length(y0))
   dimnames(G) <- list(x = x0, y = y0)
   for (i in 1:length(x0)){
      z <- seq(x0[i], as.numeric(y0[length(y0)-1]), by = dx)
      G[i,as.character(z)] <- pgamma(z-x0[i]+dx/2, k[i], 1/phi[i]) - pgamma(z - x0[i] - dx/2, k[i], 1/phi[i])
      G[i,as.character(y0[length(y0)])] <- 1 - pgamma(y0[length(y0)] - x0[i] - dx/2, k[i], 1/phi[i])
    }

   return(G)
}

pmat <- function(x, xp = 95, log_w = 0, p_mix = 0.5){
   if ((length(xp) == 2) & (length(log_w) == 1)) log_w <- rep(log_w, 2)
   if (length(xp) == 1) p <- 1 / (1+exp(-exp(log_w) * (x - xp[1])))
   if (length(xp) == 2){
      p1 <- 1 / (1+exp(-exp(log_w[1]) * (x - xp[1])))
      p2 <- 1 / (1+exp(-exp(log_w[2]) * (x - xp[2])))
      p <- (1-p_mix) * p1 + p_mix * p2
   }
   return(p)
}

pskip <- function(x, xp = 70, log_w = -1.6, p_max = 0.5) return( p_max / (1+exp(-exp(log_w) * (x - xp))) )

sel <- function(x, xp = 50, log_w = -2, delta = 1){
   p <- 1 / ((1+exp(-exp(log_w) * (x - xp[1])))^delta)
   return(p)
}

# Define size-based processes:
theta <- c(intercept = 0.276, transition = 38.2, slope = c(0.32, 0.100), window = 1.6, sigma = 0.135)
G <- growth.matrix(as.numeric(fvars), theta)
S <- sel(as.numeric(fvars), xp = 30, log_w = -1.5, delta = 1.5)   # Size-selectivity.
catchability <- rep(0.7, length(years)+1)
catchability[years %in% c(2019, 2020)] <- 1
catchability[years %in% c(2021)] <- 0.5
catchability[years %in% c(2022)] <- 0.5
names(catchability) <- c(years, years[length(years)]+1)

#S <- rep(1, length(fvars))
P_mat <- pmat(as.numeric(fvars), xp = c(55, 105), log_w = c(-1.5, -0.1), p_mix = 0.7) # Maturation probability.
P_skip <- pskip(as.numeric(fvars), xp = 70, log_w = -1.6, p_max = 0.5) # Skip-moulting probability.
S <- repvec(catchability, ncol = length(fvars)) * repvec(S, nrow = length(catchability))      # Size-selectivity.
P_mat <- repvec(P_mat, nrow = dim(f)[1])      # Size-selectivity.
P_skip <- repvec(P_skip, nrow = dim(f)[1])      # Size-selectivity.
dimnames(S) <- list(year = names(catchability), size = fvars)
dimnames(P_mat) <- list(year = years, size = fvars)
dimnames(P_skip) <- list(year = years, size = fvars)

# Mortality parameters:
M_recruit <- 0.30
M_residual <- 0.30
M_max <- 0.4
M_exploitation <- M_max / (1+exp(-exp(-1) * (as.numeric(fvars) - 98)))
M_exploitation <- repvec(M_exploitation, nrow = dim(f)[1])

# Selectivity scaling:
F <- f
F[,,"immature"] <- (1/S[as.character(years), ]) * f[,,"immature"]
F[,,"skip"]     <- (1/S[as.character(years), ]) * f[,,"skip"]
F[,,"recruit"]  <- (1/S[as.character(years), ]) * f[,,"recruit"]
F[,,"residual"] <- (1/S[as.character(years), ]) * f[,,"residual"]

# Population processes:
mu <- F
mu[,,"immature"] <- (1-P_mat) * (((1-P_skip) * F[,,"immature"]) %*% G)[,fvars]
mu[,,"skip"]     <- P_skip * F[,,"immature"]
mu[,,"recruit"]  <- (1-M_recruit) * P_mat * (((1-P_skip) * F[,,"immature"] + F[,,"skip"]) %*% G)[, fvars]
mu[,,"residual"] <- (1-M_exploitation) * ((1-M_recruit) * F[,,"recruit"] + (1-M_residual) * F[,,"residual"])
dimnames(mu)[[1]] <- years + 1  # Increment years.


# Calculate commercial landings:
catch <- M_exploitation * ((1-M_recruit) * f[,,"recruit"] + (1-M_residual) * f[,,"residual"])
catch <- 57842.8 * catch * repvec((2.665 * 10^-4 * as.numeric(colnames(catch)) ^ 3.098) / 1000000, nrow = nrow(catch))
catch <- apply(catch, 1, sum)

Mu <- mu # Save population to Mu.

# Rescale to survey:
mu[,,"immature"] <- S[dimnames(mu)$year,] * mu[,,"immature"]
mu[,,"skip"]     <- S[dimnames(mu)$year,] *  mu[,,"skip"]
mu[,,"recruit"]  <- S[dimnames(mu)$year,] *  mu[,,"recruit"]
mu[,,"residual"] <- S[dimnames(mu)$year,] *  mu[,,"residual"]

clg()
m <- kronecker(1:2, matrix(1, ncol = 5, nrow = 5))
m <- rbind(0, cbind(0, m, 0), 0, 0)
par(mar= c(0,0,0,0))
for (i in 2:length(years)){
   layout(m)

   # Immature plot:
   z <- data.frame(f[as.character(years[i]), , "skip"], f[as.character(years[i]), , "immature"])
   rownames(z) <- fvars
   colnames(z) <- c("skip", "immature")
   gbarplot(z, grid = TRUE, xlim = c(0, 140), xaxt = "n",
            col = c("grey30", "grey"),
            border = c("grey50", "grey50"),
            xaxs = "i", ylim = c(0, 600), yaxs = "i", legend = FALSE)

   # Draw predicted values:
   lines(as.numeric(fvars), mu[as.character(years[i]), , "immature"] + mu[as.character(years[i]), , "skip"], lwd = 2, col = "blue")
   lines(as.numeric(fvars), mu[as.character(years[i]), , "skip"], lwd = 2, col = "blue")

   mtext("Density (#/km2)", 2, 3, cex = 1.5, at = 0)
   mtext(years[i], 3, 1.0, cex = 1.5)
   mtext("Immature", 4, 1, las = 3, cex = 1.5)

   # Mature plot:
   z <- data.frame(f[as.character(years[i]), , "residual"], f[as.character(years[i]), , "recruit"])
   rownames(z) <- fvars
   colnames(z) <- c("residual", "recruit")
   gbarplot(z, grid = TRUE, xlim = c(0, 140), xaxt = "n",
            col = c("grey30", "grey"),
            border = c("grey50", "grey50"),
            xaxs = "i", ylim = c(0, 200), yaxs = "i", legend = FALSE)

   # Draw predicted values:
   lines(as.numeric(fvars), mu[as.character(years[i]), , "recruit"] + mu[as.character(years[i]), , "residual"], lwd = 2, col = "blue")
   lines(as.numeric(fvars), mu[as.character(years[i]), , "residual"], lwd = 2, col = "blue")

   mtext("Capapace width (mm)", 1, 3.0, cex = 1.5)
   mtext("Mature", 4, 1, srt = 180, cex = 1.5)
   axis(1)
}


