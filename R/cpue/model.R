library(TMB)
library(gulf.data)
library(gulf.spatial)
library(gulf.graphics)
library(gulf.stats)

# Read data:
load(paste0(options("gulf.path")[[1]]$snow.crab$root, "/Databases/Fishery Logbooks/r/logbook2021corrected.rdata"))

# Fill-in missing soak times:
problems <- sort(unique(x$cfvn[is.na(x$soak.time) | ((x$soak.time == 0) & (x$slip.prop.day > 100))]))
ix <- which(x$cfvn %in% problems)
res <- aggregate(x$soak.time[ix], by = x[ix, "cfvn", drop = FALSE], median, na.rm = TRUE)
res <- cbind(res, aggregate(list(sigma = x$soak.time[ix]), by = x[ix, "cfvn", drop = FALSE], sd, na.rm = TRUE)["sigma"])
res <- cbind(res, aggregate(list(unique = x$soak.time[ix]), by = x[ix, "cfvn", drop = FALSE], function(x) sort(unique(x)))["unique"])
for (i in 1:nrow(res)){
   ix <- which((x$cfvn == res$cfvn[i]) & is.na(x$soak.time))
   x$soak.time[ix] <- res$x[i]
}

x <- x[which(x$trap.day > 0), ]
x <- x[which(x$slip.prop.day > 10), ]
x$zone <- gsub("12E[*]", "12E", x$zone)
x$zone <- gsub("12F[*]", "12F", x$zone)
x$allocation <- allocation()$english[match(x$allocation.code, allocation()$code)]

# Read fishery start dates:
dates <- read.csv(locate(package = "gulf.data", keywords = c("season", "csv")))
dates <- dates[year(as.POSIXct(dates$start.date)) == 2021, ]

zones <- unique(x$zone)
x$julian <- NA
for (i in 1:length(zones)){
   ix <- which(x$zone == zones[i])
   x$julian[ix] <-time2day(date(x$date.caught[ix]), date(dates[dates$zone == zones[i], "start.date"]))
}

# Determine first trips:
first <- aggregate(list(date.caught = date(x$date.caught)), by = x[c("cfvn", "allocation")], min)
first$date.caught <- as.character(first$date.caught)
ix <- match(x[names(first)], first)
x$first <- !is.na(ix)

x <- x[-which(is.na(x$soak.time) | x$soak.time <= 0), ]


#data <- list(landings    = exp(rnorm(1000, 6, 3)))
data <- list(landings    = x$slip.prop.day,
             trap_number = x$trap.day,
             day = x$julian,
             setting = ((x$julian %in% c(0,1)) | x$first) + 1 - 1,
             soak_time = x$soak.time,
             zone = match(x$zone, zones)-1)
data$landings[data$landings <= 1] <- 1

setwd("R/cpue")

clc()
compile("model.cpp")
dyn.load(dynlib("model"))

# Define initial parameters:
parameters <- list(alpha_zone = c(5.1192791085, 6.2948735164, 4.9486955406, 5.2387046350),
                   beta_day_0 = c( -0.0381678000, -0.1331469070, -0.0411964846, -0.0543094755),
                   beta_day_1 = c(0.0003486279, 0.0027190059, 0.0004598026, 0.0005410397 ),
                   beta_setting = 0.0920456312,
                   log_scale_soak_time = log(c(3, 3, 3, 3)),
                   log_rate_soak_time = log(c(0.3, 0.3, 0.3, 0.3)),
                   logit_eps = 2.07,
                   log_sigma_eps = c( -0.9128591525, 0.2362310120))

# Estimate initial abundance parameters:
obj <- MakeADFun(data = data,
                 parameters = parameters,
                 DLL = "model")

for (i in 1:10) obj$par <- optim(obj$par, obj$fn, control = list(trace = 3, maxit = 1000))$par
obj$par <- optim(obj$par, obj$fn, control = list(trace = 3, maxit = 5000))$par

# Residual plots:
r <- obj$report()[[1]]

plot(r, cex = 0.3)
grid()
ix <- which(x$flag == "1")
points((1:nrow(x))[ix], r[ix], col = "red")
hline(0, lty = "dashed", col = "red", lwd = 2)

boxplot(r ~ I(round(x$soak.time/5)*5), ylim = c(-2, 2))
hline(0, lty = "dashed", col = "red", lwd = 2)

boxplot(r ~ x$julian, ylim = c(-2, 2))
hline(0, lty = "dashed", col = "red", lwd = 2)

boxplot(r ~ x$cfvn, ylim = c(-2, 2))
hline(0, lty = "dashed", col = "red", lwd = 2)

boxplot(r ~ x$zone, ylim = c(-2, 2))
hline(0, lty = "dashed", col = "red", lwd = 2)

boxplot(r ~ round(x$depth), ylim = c(-2, 2))
hline(0, lty = "dashed", col = "red", lwd = 2)

boxplot(r ~ x$allocation, ylim = c(-2, 2), xaxt = "n")
hline(0, lty = "dashed", col = "red", lwd = 2)

# Soak time relation:
theta <- obj$par
s <- seq(0, 240, len = 10000)
log_scale_soak_time <- theta[grep("log_scale_soak_time", names(theta))]
log_rate_soak_time <- theta[grep("log_rate_soak_time", names(theta))]

plot(s, pgamma(s, exp(log_scale_soak_time[1]), exp(log_rate_soak_time[1])), type = "l", lwd = 2, xlim = c(0, 24))
for (i in 2:4){
   lines(s, pgamma(s, exp(log_scale_soak_time[i]), exp(log_rate_soak_time[i])))
}


beta_day_0 <- theta[grep("beta_day_0", names(theta))]
beta_day_1 <- theta[grep("beta_day_1", names(theta))]
s <- seq(0, 30, len = 1000)
plot(s, exp(s * beta_day_0[1] + beta_day_1[1] * s^2), ylim = c(0, 1))
for (i in 2:4){
   lines(s, exp(s * beta_day_0[i] + beta_day_1[i] * s^2))
}

