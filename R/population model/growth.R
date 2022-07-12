library(gulf.data)
library(gulf.graphics)
library(TMB)
source("U:/TMB/TMB utilities.R")

setwd("C:/Users/SuretteTJ/Desktop/github/snow-crab-stock-assessment-2022/R/population model")
clc(); compile("growth.cpp")
dyn.load(dynlib("growth"))

# Initialize model parameter values:
parameters <- list(intercept_growth = 0.276,       # Growth intercept parameter.
                   xp_growth = 38.2,               # Growth pivot point parameter.
                   log_window_growth = 1.6,        # Growth transition window parameter.
                   slope_growth = c(0.32, 0.100),  # Growth slope parameters.
                   log_sigma_growth = log(0.135))  # Growth error parameter.


# Define model data:
data <- list(x = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
                   42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
                   80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,
                   113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,
                   142,143,144),
             f = c(0,0,0,0,0,0,0,1,3,16,15,11,46,97,78,46,20,41,114,143,207,133,100,74,82,144,184,291,288,272,159,124,93,71,90,113,127,135,
                   133,137,158,126,115,105,90,94,105,107,112,136,127,145,146,132,121,136,123,102,111,121,120,118,106,125,130,121,95,119,122,
                   94,106,130,99,94,96,120,107,110,117,104,99,117,102,108,76,91,89,93,85,71,67,76,75,74,47,56,54,45,31,30,23,30,11,16,13,11,
                   8,5,3,6,1,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

# Create TMB model object:
obj <- MakeADFun(data = data, parameters = parameters, DLL = "growth")

# Plot growth model:
plot(data$x, obj$report()$mu, type = "l", lwd = 2, col = "blue")
lines(data$x, obj$report()$mu - obj$report()$sigma, lty = "dashed", lwd = 2, col = "red")
lines(data$x, obj$report()$mu + obj$report()$sigma, lty = "dashed", lwd = 2, col = "red")


phi = obj$report()$phi_growth
k = obj$report()$k_growth

G <- obj$report()$G
apply(G, 1, sum)  # Should all sum to 1.
image(G)

# Check projected growth:
gbarplot(data$f, data$x, grid = TRUE, xlim = c(0, 40))
lines(data$x, obj$report()$fp, lwd = 2, col = "red")
sum(obj$report()$fp)
sum(data$f)
box(col = "grey50")


