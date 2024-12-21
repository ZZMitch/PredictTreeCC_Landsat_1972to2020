##### This code finds the "area of applicability" (where model efficently learns about relation) #####
### AoA: "Where we enabled the model to learn about relationships based on the training data, and where estimated cross-validation performance holds."
### DOI: 10.1111/2041-210X.13650 (Meyer and Pebesma, 2021)
### Aoa R code: https://rdrr.io/cran/CAST/man/aoa.html
### Created by Mitchell T. Bonney, Mar. 4, 2022

##### Load and Organize Initial Data #####
### Load Libraries ###
library(CAST)
library(sf)
library(randomForest)
library(caret)
library(raster)
library(viridis)

### Set Working Directory ###
setwd("C:/Users/mitch/Google Drive/PHD/Formal Exams + Progress Reports/External Thesis Defense/RevisionsFromInternal/AreaofApplicability")

### VHR Imagery Interpretation Reference Data ###
# Load Initial VHR data and subset to only 952 useful sites
vhr_data = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/LandsatVI_VHR_2018.csv")
vhr_data = subset(vhr_data, Include == 1) # Remove bad sites (water, agriculture harvest etc.)

# Load VHR data locations
Vhr_loc = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation/AP Sites/VHR_loc1.csv")

# Merge VHR data and locations
vhr_data = merge(vhr_data, Vhr_loc, by.x = "Site")

# Convert to sf object (point geometry)
vhr_data_pts = st_as_sf(vhr_data, coords = c("Eastings", "Northings"))

# Visualize spatially
plot(vhr_data_pts[,1], col = "black")

# Vars to factor
vhr_data$LandCover = as.factor(vhr_data$LandCover)
vhr_data$CC_10 = as.factor(vhr_data$CC_10)

# Convert %VC and %CC to proportion
vhr_data$pCC_1 = vhr_data$pCC / 100

### DHP-based Forest Reference Data ###
# Load Initial DHP data and locations
dhp_data = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/LandsatVI_DHP_2018.csv")
dhp_loc = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation/Forest Sites/DHP_loc1.csv")

# Merge DHP data and locations
dhp_data = merge(dhp_data, dhp_loc, by.x = "Site")

# Convert to sf object (point geometry)
dhp_data_pts = st_as_sf(dhp_data, coords = c("Eastings", "Northings"))

# Visualize spatially
plot(dhp_data_pts[,1], col = "black")

# Add columns
dhp_data$LandCover2018 = "Forest"
dhp_data$CC_10 = 2
dhp_data$Include = 1
dhp_data$LandCover = "Forest"
dhp_data$pCC_1 = dhp_data$pCC / 100

### Merge
vhrdhp_data = rbind(vhr_data, dhp_data) # Merge tables
#####

rf_seed = 724762
# set the seed so that you get the same random forest model each time 

##### CC VHR-DHP RF 2018 #####
### Predictor Rasters 2018
# TCA_mn
TCA_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCAfit_mn_2018.tif")
names(TCA_mn) = "TCA_mn"
#plot(TCA_mn)

# TCB_mn
TCB_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCBfit_mn_2018.tif")
names(TCB_mn) = "TCB_mn"
#plot(TCB_mn)

# TCG_mn
TCG_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCGfit_mn_2018.tif")
names(TCG_mn) = "TCG_mn"
#plot(TCG_mn)

# TCW_mn
TCW_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCWfit_mn_2018.tif")
names(TCW_mn) = "TCW_mn"
#plot(TCW_mn)

# TCA_sd
TCA_sd = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCAfit_sd_2018.tif")
names(TCA_sd) = "TCA_sd"
#plot(TCA_sd)

# CC_10
CC_10 = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/CC10_2018.tif")
names(CC_10) = "CC_10"
#plot(CC_10)

### Run original Random Forest model (see vogeler_rf_cover_modeling_VC_VHR.R) ###
set.seed(rf_seed) 
cover = as.numeric(vhrdhp_data$pCC_1)
ntree = 300 # where curve plateaus
cc_rf_2018 = randomForest(cover ~ TCA_mn + TCB_mn + TCG_mn + TCW_mn + TCA_sd + CC_10, ntree = ntree, 
                            data = vhrdhp_data, importance  = TRUE)
cc_rf_2018 # R2 = 0.89

### Run Random Forest model with caret ###
set.seed(rf_seed) 
variables = c("TCA_mn", "TCB_mn", "TCG_mn", "TCW_mn", "TCA_sd", "CC_10")
cc_rf1_2018 = train(vhrdhp_data[,which(names(vhrdhp_data)%in%variables)], vhrdhp_data$pCC_1, method = "rf", 
                      importance = TRUE, tuneLength = 1, 
                      trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)) 
# LOOCV takes a while (10+ mins)
cc_rf1_2018 # R2 = 0.90, RMSE = 0.103 (10.3% CC)

### Create RasterBrick of Predictors ###
cc_preds_2018 = brick(c(TCA_mn, TCB_mn, TCG_mn, TCW_mn, TCA_sd, CC_10))
#plot(cc_preds)

### Predicted model ###
# From example code:
# prediction <- predict(studyArea,model)
# Don't need to do...

### Calculate and Plot AoA
# AOA function
cc_aoa_2018 = aoa(cc_preds_2018, cc_rf1_2018) # Needed to use k-fold cross-validation here (in place of oob)
# Takes a while (10+ mins)

# Clip out consistent non-forest
mask = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/Spatiotemporal canopy change/MaskAlwaysNonForest/always_nonforest10_nowater.tif")
mask = reclassify(mask, c(0,1,NA))
#plot(mask)

cc_aoa_2018 = mask(cc_aoa_2018, mask)

# Plot
spplot(cc_aoa_2018$DI, col.regions = viridis(100), main = "Dissimilarity Index")
#writeRaster(cc_aoa_2018$DI, "cc_rf_2018_di.tif")

spplot(cc_aoa_2018$AOA, main = "Area of Applicability")
#writeRaster(cc_aoa_2018$AOA, "cc_rf_2018_aoa.tif")

# % in AOA
aoa_freq = freq(cc_aoa_2018$AOA) 
npixels = aoa_freq[1,2] + aoa_freq[2,2]
pAOA = aoa_freq[2,2] / npixels * 100 # 98.2% in AOA
#####

##### CC VHR-DHP RF 2000 #####
### Predictor Rasters 2000
# TCA_mn
TCA_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2000/Predictors/tca_mn_2000.tif")
names(TCA_mn) = "TCA_mn"
#plot(TCA_mn)

# TCB_mn
TCB_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2000/Predictors/tcb_mn_2000.tif")
names(TCB_mn) = "TCB_mn"
#plot(TCB_mn)

# TCG_mn
TCG_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2000/Predictors/tcg_mn_2000.tif")
names(TCG_mn) = "TCG_mn"
#plot(TCG_mn)

# TCW_mn
TCW_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2000/Predictors/tcw_mn_2000.tif")
names(TCW_mn) = "TCW_mn"
#plot(TCW_mn)

# TCA_sd
TCA_sd = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2000/Predictors/tca_sd_2000.tif")
names(TCA_sd) = "TCA_sd"
#plot(TCA_sd)

# CC_10
CC_10 = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2000/Predictors/CC10_2000.tif")
names(CC_10) = "CC_10"
#plot(CC_10)

### Run original Random Forest model (see vogeler_rf_cover_modeling_VC_VHR.R) ###
# Same as 2018

### Run Random Forest model with caret ###
# Same as 2018

### Create RasterBrick of Predictors ###
cc_preds_2000 = brick(c(TCA_mn, TCB_mn, TCG_mn, TCW_mn, TCA_sd, CC_10))
#plot(cc_preds)

### Predicted model ###
# From example code:
# prediction <- predict(studyArea,model)
# Don't need to do...

### Calculate and Plot AoA
# AOA function
cc_aoa_2000 = aoa(cc_preds_2000, cc_rf1_2018) # Needed to use k-fold cross-validation here (in place of oob)
# Takes a while (10+ mins)

# Clip out consistent non-forest
mask = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/Spatiotemporal canopy change/MaskAlwaysNonForest/always_nonforest10_nowater.tif")
mask = reclassify(mask, c(0,1,NA))
#plot(mask)

cc_aoa_2000 = mask(cc_aoa_2000, mask)

# Plot
spplot(cc_aoa_2000$DI, col.regions = viridis(100), main = "Dissimilarity Index")
#writeRaster(cc_aoa_2000$DI, "cc_rf_2000_di.tif")

spplot(cc_aoa_2000$AOA, main = "Area of Applicability")
#writeRaster(cc_aoa_2000$AOA, "cc_rf_2000_aoa.tif")

# % in AOA
aoa_freq = freq(cc_aoa_2000$AOA) 
npixels = aoa_freq[1,2] + aoa_freq[2,2]
pAOA = aoa_freq[2,2] / npixels * 100 # 98.7% in AOA
#####

##### CC VHR-DHP RF 1972 #####
### Predictor Rasters 1972
# TCA_mn
TCA_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tca_mn_1972.tif")
names(TCA_mn) = "TCA_mn"
#plot(TCA_mn)

# TCB_mn
TCB_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tcb_mn_1972.tif")
names(TCB_mn) = "TCB_mn"
#plot(TCB_mn)

# TCG_mn
TCG_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tcg_mn_1972.tif")
names(TCG_mn) = "TCG_mn"
#plot(TCG_mn)

# TCW_mn
TCW_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tcw_mn_1972.tif")
names(TCW_mn) = "TCW_mn"
#plot(TCW_mn)

# TCA_sd
TCA_sd = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tca_sd_1972.tif")
names(TCA_sd) = "TCA_sd"
#plot(TCA_sd)

# CC_10
CC_10 = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/CC10_1972.tif")
names(CC_10) = "CC_10"
#plot(CC_10)

### Run original Random Forest model (see vogeler_rf_cover_modeling_VC_VHR.R) ###
# Same as 2018

### Run Random Forest model with caret ###
# Same as 2018

### Create RasterBrick of Predictors ###
cc_preds_1972 = brick(c(TCA_mn, TCB_mn, TCG_mn, TCW_mn, TCA_sd, CC_10))
#plot(cc_preds)

### Predicted model ###
# From example code:
# prediction <- predict(studyArea,model)
# Don't need to do...

### Calculate and Plot AoA
# AOA function
cc_aoa_1972 = aoa(cc_preds_1972, cc_rf1_2018) # Needed to use k-fold cross-validation here (in place of oob)
# Takes a while (10+ mins)

# Clip out consistent non-forest
mask = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/Spatiotemporal canopy change/MaskAlwaysNonForest/always_nonforest10_nowater.tif")
mask = reclassify(mask, c(0,1,NA))
#plot(mask)

cc_aoa_1972 = mask(cc_aoa_1972, mask)

# Plot
spplot(cc_aoa_1972$DI, col.regions = viridis(100), main = "Dissimilarity Index")
writeRaster(cc_aoa_1972$DI, "cc_rf_1972_di.tif")

spplot(cc_aoa_1972$AOA, main = "Area of Applicability")
writeRaster(cc_aoa_1972$AOA, "cc_rf_1972_aoa.tif")

# % in AOA
aoa_freq = freq(cc_aoa_1972$AOA) 
npixels = aoa_freq[1,2] + aoa_freq[2,2]
pAOA = aoa_freq[2,2] / npixels * 100 # 95.2% in AOA
#####