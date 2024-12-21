#install.packages("devtools")
#devtools::install_github("https://github.com/tgoodbody/sgsR")
library(sgsR)
#library(raster)
library(terra)

landcover = rast("C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/4 Predict CC (2018)/1 Inputs/Land cover/TIF/Landcover_2018_14_NoWater.tif")
plot(landcover)
barplot(landcover, maxcell = 1000000000)


cc = rast("C:/Users/mitch/Google Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/4 Predict CC (2018)/3 Outputs/RF/CC/DHPVHR/TIF/ccdhpvhr_2018_rf_1.tif")
plot(cc)
hist(cc, breaks = seq(0,100,10), maxcell = 1000000000)

# Test simple random sample
#test = sample_srs(raster = cc, nSamp = 200, plot = TRUE)

# Calculate appropriate sample size
calculate_sampsize(mraster = cc, rse = 0.07, start = 0.01, end = 0.1, increment = 0.01, plot = TRUE)
# 200 = approx 0.07 rse

# Define strata for cc (10% CC bins)
cc_breaks = c(seq(10, 90, 10))

# Perform strat using user-defined breaks
#values = terra::values(cc)
cc_strat = strat_breaks(mraster = cc, breaks = cc_breaks, plot = TRUE)
plot(cc_strat)

# Define strata for land cover (all classes in seperate bins)
lc_breaks = c(seq(1, 8, 1))
landcover1 = strat_breaks(mraster = landcover, breaks = lc_breaks, plot = TRUE)

# Combine LC and CC strata for double stratification
lc_cc_strat = strat_map(sraster = cc_strat, sraster2 = landcover1, plot = TRUE)

# Calculate allocation
# Proportional
calculate_allocation(sraster = lc_cc_strat, nSamp = 200, allocation = "prop")

# Proportional stratification - done faster (just getting allocation) with calculate_allocation
#prop1 = sample_strat(sraster = lc_cc_strat, nSamp = 200, allocation = "prop", plot = TRUE)

# Calculate representation - see number of samples per strata
#prop1_rep = calculate_representation(sraster = lc_cc_strat, existing = prop1, plot = TRUE)
#print(prop1_rep$representation, n = 100)

# Manual (with provided weights to increase rare classes at expense of Agriculture)
weights = read.csv("C:/Users/mitch/Google Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/5 CC Accuacy (2018)/5 Independent Validation Sample/CreateStratSample/weights.csv")
sum(weights) #1
typeof(weights) # List, needs to be a numeric vector
weights1 = as.numeric(unlist(weights))

calculate_allocation(sraster = lc_cc_strat, nSamp = 214, allocation = "manual", weights = weights1) # Good

man_strat = sample_strat(sraster = lc_cc_strat, nSamp = 214, allocation = "manual", weights = weights1, plot = TRUE)
typeof(man_strat)

write.csv(man_strat, "val_sample_214.csv")
