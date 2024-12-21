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
vhr_data = subset(vhr_data, LandCover2018 == "Residential") # Only residential land cover

# Vars to factor
vhr_data$LandCover = as.factor(vhr_data$LandCover)
vhr_data$CC_10 = as.factor(vhr_data$CC_10)

# Convert %VC and %CC to proportion
vhr_data$pCC_1 = vhr_data$pCC / 100

rf_seed = 724762
# set the seed so that you get the same random forest model each time 

##### CC VHR Residential RF 2018 #####
### Predictor Rasters 2018
# TCA_mn
TCA_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCAfit_mn_2018.tif")
names(TCA_mn) = "TCA_mn"
#plot(TCA_mn)

# TCG_mn
TCG_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCGfit_mn_2018.tif")
names(TCG_mn) = "TCG_mn"
#plot(TCG_mn)

# TCW_mn
TCW_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCWfit_mn_2018.tif")
names(TCW_mn) = "TCW_mn"
#plot(TCW_mn)

# TCB_sd
TCB_sd = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/TCBfit_sd_2018.tif")
names(TCB_sd) = "TCB_sd"
#plot(TCB_sd)

# Elev_mn
Elev_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/Topo/Elev_mn.tif")
names(Elev_mn) = "Elev_mn"
#plot(Elev_mn)

# CC_10
CC_10 = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/2018/Predictors/CC10_2018.tif")
names(CC_10) = "CC_10"
#plot(CC_10)

### Run original Random Forest model (see vogeler_rf_cover_modeling_VC_VHR.R) ###
set.seed(rf_seed) 
cover = as.numeric(vhr_data$pCC_1)
ntree = 500 # where curve plateaus
cc_rf_res_2018 = randomForest(cover ~ TCA_mn + TCG_mn + TCW_mn + TCB_sd + Elev_mn + CC_10, ntree = ntree, 
                          data = vhr_data, importance  = TRUE)
cc_rf_res_2018 # R2 = 0.83

### Run Random Forest model with caret ###
set.seed(rf_seed) 
variables = c("TCA_mn", "TCG_mn", "TCW_mn", "TCB_sd", "Elev_mn", "CC_10")
cc_rf1_res_2018 = train(vhr_data[,which(names(vhr_data)%in%variables)], vhr_data$pCC_1, method = "rf", 
                    importance = TRUE, tuneLength = 1, 
                    trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE))
                    #trControl = trainControl(method = "LOOCV", savePredictions = TRUE))
# LOOCV takes a while (10+ mins)
cc_rf1_res_2018 # R2 = 0.83, RMSE = 0.059 (5.9% CC) with LOOCV, 0.86 / 0.055 with 10-fold

### Create RasterBrick of Predictors ###
cc_preds_res_2018 = brick(c(TCA_mn, TCG_mn, TCW_mn, TCB_sd, Elev_mn, CC_10))
#plot(cc_preds)

### Calculate and Plot AoA
# AOA function
cc_aoa_res_2018 = aoa(cc_preds_res_2018, cc_rf1_res_2018) 
# Takes a while (10+ mins)

# Plot
spplot(cc_aoa_res_2018$DI, col.regions = viridis(100), main = "Dissimilarity Index")
#writeRaster(cc_aoa_res_2018$DI, "cc_rf_res_2018_di.tif")

spplot(cc_aoa_res_2018$AOA, main = "Area of Applicability")
#writeRaster(cc_aoa_res_2018$AOA, "cc_rf_res2018_aoa.tif")
#####

##### CC VHR Residential RF 1972 #####
### Predictor Rasters 1972
# TCA_mn
TCA_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tca_mn_1972.tif")
names(TCA_mn) = "TCA_mn"
plot(TCA_mn)

# TCG_mn
TCG_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tcg_mn_1972.tif")
names(TCG_mn) = "TCG_mn"
#plot(TCG_mn)

# TCW_mn
TCW_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tcw_mn_1972.tif")
names(TCW_mn) = "TCW_mn"
#plot(TCW_mn)

# TCB_sd
TCB_sd = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/tcb_sd_1972.tif")
names(TCB_sd) = "TCB_sd"
#plot(TCB_sd)

# Elev_mn
Elev_mn = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/Topo/Elev_mn.tif")
names(Elev_mn) = "Elev_mn"
#plot(Elev_mn)

# CC_10
CC_10 = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/1972/Predictors/CC10_1972.tif")
names(CC_10) = "CC_10"
#plot(CC_10)

### Run original Random Forest model (see vogeler_rf_cover_modeling_VC_VHR.R) ###
set.seed(rf_seed) 
cover = as.numeric(vhr_data$pCC_1)
ntree = 500 # where curve plateaus
cc_rf_res_1972 = randomForest(cover ~ TCA_mn + TCG_mn + TCW_mn + TCB_sd + Elev_mn + CC_10, ntree = ntree, 
                              data = vhr_data, importance  = TRUE)
cc_rf_res_1972 # R2 = 0.83

### Run Random Forest model with caret ###
set.seed(rf_seed) 
variables = c("TCA_mn", "TCG_mn", "TCW_mn", "TCB_sd", "Elev_mn", "CC_10")
cc_rf1_res_1972 = train(vhr_data[,which(names(vhr_data)%in%variables)], vhr_data$pCC_1, method = "rf", 
                        importance = TRUE, tuneLength = 1, 
                        trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE))
                        #trControl = trainControl(method = "LOOCV", savePredictions = TRUE))
# LOOCV takes a while (10+ mins)
cc_rf1_res_1972 # R2 = 0.83, RMSE = 0.059 (5.9% CC) with LOOCV, 0.86 / 0.055 with 10-fold

### Create RasterBrick of Predictors ###
cc_preds_res_1972 = brick(c(TCA_mn, TCG_mn, TCW_mn, TCB_sd, Elev_mn, CC_10))
#plot(cc_preds)

### Calculate and Plot AoA
# AOA function
cc_aoa_res_1972 = aoa(cc_preds_res_1972, cc_rf1_res_1972) 
# Takes a while (10+ mins)

# Plot
spplot(cc_aoa_res_1972$DI, col.regions = viridis(100), main = "Dissimilarity Index")
#writeRaster(cc_aoa_res_1972$DI, "cc_rf_res_1972_di.tif")

spplot(cc_aoa_res_1972$AOA, main = "Area of Applicability")
#writeRaster(cc_aoa_res_1972$AOA, "cc_rf_res1972_aoa.tif")
#####