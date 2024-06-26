#######################################################################################################
#######################################################################################################
## CREATION OF A RANDOM FOREST CANOPY COVER MODEL AND MAP
##      Purpose: Create a random forest regression model and prediction map for canopy cover 
##      Inputs: Canopy cover values estimated from 100 point grids using NAIP imagery for 3x3 Landsat pixel window plots.
##           LandTrendr tasseled cap predictor metrics are output from LandTrendr segmentation using median Landsat composites (growing season images only).
## Author: Jody Vogeler jody.vogeler@colostate.edu
## Date first created: 9/15/2016
##      Date last modified: 03/23/2021
#######################################################################################################
#######################################################################################################
# NOTES:
#   I created these models quite a few years ago. I pulled together the pieces of the code I think you need, 
#     but I have not tested them on a data file, so you may need to do some debugging. 
#     Let me know if you have trouble figuring that all out. 
##########################################################
#      READ IN DATA FILE AND LOAD PACKAGES
##########################################################

# I think these are all of the libraries you need, but if you get errors related to packages, let me know and i can see if I missed one. 
library(randomForest)
library(rfUtilities)
library(yaImpute)
library(caret)

# Change this to the path to the csv table that includes the response column (where you have converted percent cover to a proportion) and the associated predictors extracted
# VHR interpretation sites
#data_vhr = read.csv(file="C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation/LandsatVI_VHR_2018.csv", header=TRUE, sep=",")
data_vhr = read.csv(file="C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/4 Predict CC (2018)/1 Inputs/LandsatVI_VHR_2018.csv", header=TRUE, sep=",")
attach(data_vhr)
names(data_vhr)

data_vhr = subset(data_vhr, Include == 1) # Remove bad sites
data_vhr = subset(data_vhr, select = -c(pVC, Include)) # Remove unneeded columns

# DHP sites
data_dhp = read.csv(file = "C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/4 Predict CC (2018)/1 Inputs/LandsatVI_DHP_2018.csv", header=TRUE, sep=",")
data_dhp$LandCover = "Forest"
data_dhp$LC_num = 4 # Forest LC_num
data_dhp$CC_10 = 2 # Above 10% CC

# Combine into one table, remove overlap from VHR
#data_vhr = subset(data_vhr, pCC < 75) # Remove VHR sites with %CC 75 or more
#data_vhr = subset(data_vhr, pCC < min(data_dhp$pCC)) # Remove VHR sites with %CC more than minimum DHP %CC (77.8)
#data_vhr = subset(data_vhr, pCC < 80) # Remove VHR sites with %CC 80 or more
#data_vhr = subset(data_vhr, pCC < 85) # Remove VHR sites with %CC 85 or more, fills early gap in DHP model
#data_vhr = subset(data_vhr, pCC < 90) # Remove VHR sites with %CC 90 or more
#data_vhr = subset(data_vhr, pCC < 95) # Remove VHR sites with %CC 95 or more
#data_vhr = subset(data_vhr, pCC < 75 | LandCover != "Forest") # Remove VHR forest sites with %CC 75 or more
#data_vhr = subset(data_vhr, pCC < min(data_dhp$pCC)| LandCover != "Forest") # Remove VHR forest sites with %CC more than minimum DHP %CC (77.8)
#data_vhr = subset(data_vhr, pCC < 80 | LandCover != "Forest") # Remove VHR forest sites with %CC 80 or more
#data_vhr = subset(data_vhr, pCC < 85 | LandCover != "Forest") # Remove VHR forest sites with %CC 85 or more, , fills early gap in DHP model
#data_vhr = subset(data_vhr, pCC < 90 | LandCover != "Forest") # Remove VHR forest sites with %CC 90 or more
#data_vhr = subset(data_vhr, pCC < 95 | LandCover != "Forest") # Remove VHR forest sites with %CC 95 or more
#data_vhr = subset(data_vhr,  LandCover != "Forest")
#data_vhr = subset(data_vhr, pCC < 85 | pCC > 95 | LandCover != "Forest")
#data_vhr = subset(data_vhr, pCC < 85 | pCC > 95)

data = rbind(data_vhr, data_dhp) # Merge tables

# Get linear model results
train.control = trainControl(method = "LOOCV")

#VI_CC.lm = lm(pCC ~ NDVI_mn, data)
#summary(VI_CC.lm)
#plot(pCC ~ NDVI_mn, data)
#abline(VI_CC.lm)
#VI_CC.cv = train(pCC ~ NDVI_mn, data = data, method = "lm", trControl = train.control) 
#print(VI_CC.cv) 
# NDVI: R2 = 0.47****, RMSE: 23.9, y = 90.563x - 24.092
# TCA: R2 = 0.45****, RMSE: 24.1

# Vars to factor
#data$LandCover = as.factor(data$LandCover)
#levels(data$LandCover)
#str(data$LandCover)
data$LC_num = as.factor(data$LC_num)
levels(data$LC_num)
#data$CC_10 = as.factor(data$CC_10)
#levels(data$CC_10)

# Convert %VC and %CC to proportion
data$pCC_1 = data$pCC / 100

##########################################################
## EXPLORE A GLOBAL RANDOM FOREST MODEL
##########################################################
# find a random number to serve as the seed
sample(1:1000000,1)
# insert that random number here
rf_seed = 724762
# set the seed so that you get the same random forest model each time 
# (otherwise random forest will take different random bootstraps/branches each time so each time you reran the model it migth be slightly different)

ntree = 300 # where curve plateaus

# %CC
set.seed(rf_seed)
cover = as.numeric(data$pCC_1) # cover as a proportion
# I go ahead and run a global random forest model (all predictors retained), and then work on variable reduction in the next code section
# NOTE: change the predictor names to those from your data table
# There are other parameters you can control within the random forest model, but here I just used most of the defaults for exploratory purposes. 
#cover_rf = randomForest(cover~tca_mn+tca_sd+tcb_mn+tcb_sd+tcg_mn+tcg_sd+tcw_mn+tcw_sd+elev+slope+aspect+ssina+scosa, ntree=500, data=data)
#cover_rf
cover_rf = randomForest(cover ~ NDVI_mn + TCB_mn + TCG_mn + TCW_mn + TCA_mn + NDVI_sd + TCB_sd + TCG_sd + 
                          TCW_sd + TCA_sd + Elev_mn + Slope_mn + Aspect_mn + SSINA_mn + SCOSA_mn + Elev_sd + 
                          Slope_sd + Aspect_sd + SSINA_sd + SCOSA_sd + LC_num, ntree = ntree, 
                          data = data, importance  = TRUE) # No CC_10
cover_rf

plot(cover_rf)  # You can look at this plot and identify the number of trees around where the curve platues and then set the ntree to that in future models (or keep at default of 500)

varImpPlot(cover_rf)
importance(cover_rf)
# Model Improvement Ratio (MIR)
imp = cover_rf$importance[,"%IncMSE"]
round(imp/max(imp), 2)

pve = round(cover_rf$rsq[ntree] * 100, 2) # % Variation explained: Psuedo R2
print(pve)

rmse = round(sqrt(cover_rf$mse[ntree]), 4) # RMSE (scales based on size of response variable)
print(rmse)
prmse = round((rmse/mean(data$pCC_1))*100,1) # %RMSE - equalized for size of response variable
print(prmse)

# Cannot have both CC_10 and LC_num, pick one...

#############################################################
## Random Forest Variable Selection Procedure
#############################################################
set.seed(rf_seed) # make sure to set the seed again prior to each time you run a model
# This is the procedure for variable reduction and selecting the top model
rfsel_1 = rf.modelSel(data[,(4:24)], # change these column numbers to those that correspond to all of the predictors in the data file (so in this example, my predictors were in rows 14-27 in my data csv)
                      as.numeric(data[,"pCC_1"]), #or whatever your cover proportion column is called
                      r = c(seq(1:21)/(21)), # change this 14 to the number of predictors you have included in your data set (in the defined column range above)
                      parsimony = 0.10) # you can try different parsimony levels, but I have found 0.10 to work well
rfsel_1
vars = rfsel_1$selvars# This will spit out the final predictors from the variable reduction above for the top selected model
vars

set.seed(rf_seed) # make sure to set the seed again prior to each time you run a model
# Insert the final selected variables from the selvars above
rf.fit1 = randomForest(cover ~ NDVI_mn + TCB_mn + TCG_mn + TCW_mn + TCA_mn + LC_num, ntree = ntree, 
                       data = data, importance  = TRUE) # TCB_sd

rf.fit1

plot(rf.fit1)  

varImpPlot(rf.fit1)
importance(rf.fit1)
# Model Improvement Ratio (MIR)
imp = rf.fit1$importance[,"%IncMSE"]
round(imp/max(imp), 2)

pve = round(rf.fit1$rsq[ntree] * 100, 2) # % Variation explained: Psuedo R2
print(pve)

rmse = round(sqrt(rf.fit1$mse[ntree]), 4) # RMSE (scales based on size of response variable)
print(rmse)

# OOB - test
summary(lm(data$pCC_1 ~ rf.fit1$predicted)) # R2 approx. equal to RF pve

test = data.frame(matrix(NA, nrow = nrow(data), ncol = 0))
test$pCC = data$pCC_1 * 100
test$predicted = rf.fit1$predicted * 100
test$LandCover = data$LandCover
#write.csv(test, "CC_VHRDHP_obspred.csv")

#train(pCC ~ predicted, data = test, method = "lm", trControl = train.control)

plot(test$predicted, test$pCC) # predicted based on OOB samples...
abline(0,1, lwd = 3)
abline(lm(data$pCC_1 ~ rf.fit1$predicted), lty = 3, lwd = 3, col = "red")

prmse = round((rmse/mean(data$pCC_1))*100, 1) # %RMSE - equalized for size of response variable
print(prmse)

model = rf.fit1 # set this as the model for predicting in below section

###############################################################
## APPLY THE SELECTED MODEL TO THE PREDICTOR GRIDS
###############################################################

# Set some base paths
BASEPATH = "C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation/rf/" # set a base path here for your processing
YEAR = 2018 # set this to the year of your predictive mapping (or set a list and turn the below into a loop to predict for multiple years)

asc_base = paste0(BASEPATH,'predictors/') # change this to the path with the predictor grids
cover_base = paste0(BASEPATH,'cc_dhpvhr/') # change this to the output path for predicted cover maps

# update these to the paths to the predictor grids included in the final selected model
# Those grids will need to be converted to ascii format first. 
# NOTE: make sure that all of these ascii grids are snapped and match exactly in extent, otherwise you will get errors when you go to apply the model
NDVI_mn = (paste0(asc_base,'ndvi_mn_',YEAR,'.asc'))
TCB_mn = (paste0(asc_base,'tcb_mn_',YEAR,'.asc'))
TCG_mn = (paste0(asc_base,'tcg_mn_',YEAR,'.asc'))
TCW_mn = (paste0(asc_base,'tcw_mn_',YEAR,'.asc'))
TCA_mn = (paste0(asc_base,'tca_mn_',YEAR,'.asc'))
TCB_sd = (paste0(asc_base,'tcb_sd_',YEAR,'.asc'))
LC_num = (paste0(asc_base,'landcover_',YEAR,'_1.asc')) # Fixed landcover errors

xlist =  list(NDVI_mn = NDVI_mn, TCB_mn = TCB_mn, TCG_mn = TCG_mn, TCW_mn = TCW_mn, TCA_mn = TCA_mn, 
              TCB_sd = TCB_sd, LC_num = LC_num) # TCB_sd = TCB_sd,
xtypes = list(NDVI_mn = "numeric", TCB_mn = "numeric", TCG_mn = "numeric", TCW_mn = "numeric", TCA_mn = "numeric", 
              TCB_sd = "numeric", LC_num = "character") # TCB_sd = "numeric", 

# output asc predictive map grid file path
output_file = paste0(cover_base,'ccdhpvhr_8095_',YEAR,'.asc')

# This prediction can take some time depending on the size of your prediction study area
AsciiGridPredict(model, xlist, output_file, xtypes)

# NEXT STEP: CONVERT ASCII TO TIFS IN PYTHON CODE

# TEST ASCII
#library(raster)

#test_ndvi_mn = raster((paste0(asc_base,'ndvi_mn_',YEAR,'.asc')))
#test_tcg_mn = read.csv((paste0(asc_base,'tcg_mn_',YEAR,'.asc')))
#test_tcw_mn = read.csv((paste0(asc_base,'tcw_mn_',YEAR,'.asc')))
#test_tca_mn = read.csv((paste0(asc_base,'tca_mn_',YEAR,'.asc')))
#test_ndvi_sd = read.csv((paste0(asc_base,'ndvi_sd_',YEAR,'.asc')))
#test_LC_num = read.csv((paste0(asc_base,'landcover_',YEAR,'.asc')))
