#######################################################################################################
#######################################################################################################
## CREATION OF A RANDOM FOREST CANOPY COVER MODEL AND MAP
##      Purpose: Create a random forest regression model and prediction map for canopy cover 
##      Inputs: Canopy cover values estimated from 100 point grids using NAIP imagery for 3x3 Landsat pixel window plots.
##           LandTrendr tasseled cap predictor metrics are output from LandTrendr segmentation using median Landsat composites (growing season images only).
## Author: Jody Vogeler jody.vogeler@colostate.edu / Edits by: Mitchell Bonney (mitchell.bonney@mail.utoronto.ca)
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
data = read.csv(file="C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/LandsatVI_VHR_2018.csv", header=TRUE, sep=",")
attach(data)
names(data)

data = subset(data, Include == 1) # Remove bad sites
data = subset(data, LandCover2018 == "Residential") # Only residential land cover

# Vars to factor
#data$LandCover = as.factor(data$LandCover)
#levels(data$LandCover)
#str(data$LandCover)
data$CC_10 = as.factor(data$CC_10)
levels(data$CC_10)

# Convert %CC to proportion
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

ntree = 500 # where curve plateaus

# %CC
set.seed(rf_seed)
cover = as.numeric(data$pCC_1) # cover as a proportion
# I go ahead and run a global random forest model (all predictors retained), and then work on variable reduction in the next code section
# NOTE: change the predictor names to those from your data table
# There are other parameters you can control within the random forest model, but here I just used most of the defaults for exploratory purposes. 
#cover_rf = randomForest(cover~tca_mn+tca_sd+tcb_mn+tcb_sd+tcg_mn+tcg_sd+tcw_mn+tcw_sd+elev+slope+aspect+ssina+scosa, ntree=500, data=data)
#cover_rf
cover_rf = randomForest(cover ~ TCA_mn + TCB_mn + TCG_mn + TCW_mn + TCA_sd + TCB_sd + TCG_sd + TCW_sd + 
                          Elev_mn + Slope_mn + Aspect_mn + SSINA_mn + SCOSA_mn + Elev_sd + 
                          Slope_sd + Aspect_sd + SSINA_sd + SCOSA_sd + CC_10, ntree = ntree, 
                          data = data, importance  = TRUE) 
cover_rf

plot(cover_rf)  # You can look at this plot and identify the number of trees around where the curve platues and then set the ntree to that in future models (or keep at default of 500)

varImpPlot(cover_rf)
importance(cover_rf)
# Model Improvement Ratio (MIR)
imp = cover_rf$importance[,"%IncMSE"]
round(imp/max(imp), 2)

pve = round(cover_rf$rsq[ntree] * 100, 2) # % Variation explained: Psuedo R2
print(pve) # 81

rmse = round(sqrt(cover_rf$mse[ntree]), 4) # RMSE (scales based on size of response variable)
print(rmse) # 6.3
prmse = round((rmse/mean(data$pCC_1))*100,1) # %RMSE - equalized for size of response variable
print(prmse) # 31

#############################################################
## Random Forest Variable Selection Procedure
#############################################################
set.seed(rf_seed) # make sure to set the seed again prior to each time you run a model
# This is the procedure for variable reduction and selecting the top model
rfsel_1 = rf.modelSel(data[,(4:22)], # change these column numbers to those that correspond to all of the predictors in the data file (so in this example, my predictors were in rows 14-27 in my data csv)
                      as.numeric(data[,"pCC_1"]), #or whatever your cover proportion column is called
                      r = c(seq(1:19)/(19)), # change this 14 to the number of predictors you have included in your data set (in the defined column range above)
                      parsimony = 0.10) # you can try different parsimony levels, but I have found 0.10 to work well
rfsel_1
vars = rfsel_1$selvars# This will spit out the final predictors from the variable reduction above for the top selected model
vars

set.seed(rf_seed) # make sure to set the seed again prior to each time you run a model
# Insert the final selected variables from the selvars above
rf.fit1 = randomForest(cover ~ TCA_mn + TCG_mn + TCW_mn + TCB_sd + Elev_mn + CC_10, ntree = ntree, 
                       data = data, importance = TRUE)

rf.fit1

plot(rf.fit1)  

varImpPlot(rf.fit1) # scale = TRUE
importance(rf.fit1) # Scaled to 1
# Model Improvement Ratio (MIR)
imp = rf.fit1$importance[,"%IncMSE"]
round(imp/max(imp), 2)

pve = round(rf.fit1$rsq[ntree] * 100, 2) # % Variation explained: Psuedo R2
print(pve) # 83

rmse = round(sqrt(rf.fit1$mse[ntree]), 4) # RMSE (scales based on size of response variable)
print(rmse) # 5.88

# OOB - test
summary(lm(data$pCC_1 ~ rf.fit1$predicted)) # R2 approx. equal to RF pve

test = data.frame(matrix(NA, nrow = nrow(data), ncol = 0))
test$pCC = data$pCC_1 * 100
test$predicted = rf.fit1$predicted * 100
#test$LandCover = data$LandCover
#write.csv(test, "CC_VHR_obspred_res.csv")

#train(pCC ~ predicted, data = test, method = "lm", trControl = train.control)

plot(test$predicted, test$pCC) # predicted based on OOB samples...
abline(0,1, lwd = 3)
abline(lm(data$pCC_1 ~ rf.fit1$predicted), lty = 3, lwd = 3, col = "red")

prmse = round((rmse/mean(data$pCC_1))*100, 1) # %RMSE - equalized for size of response variable
print(prmse) # 28.5

model = rf.fit1 # set this as the model for predicting in below section

###############################################################
## APPLY THE SELECTED MODEL TO THE PREDICTOR GRIDS
###############################################################

# Set some base paths
#YEAR = 1972  # set this to the year of your predictive mapping 
# (or set a list and turn the below into a loop to predict for multiple years)
BASEPATH = "C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/Data/"
# set a base path here for your processing
#YEARPATH = (paste0(BASEPATH, YEAR, "/Predictors/"))

for (YEAR in 1974:2020){
  
  asc_base = (paste0(BASEPATH, YEAR, "/Predictors/")) # change this to the path with the predictor grids
  cover_base = paste0(BASEPATH, YEAR, "/") # change this to the output path for predicted cover maps

# update these to the paths to the predictor grids included in the final selected model
# Those grids will need to be converted to ascii format first. 
# NOTE: make sure that all of these ascii grids are snapped and match exactly in extent, otherwise you will get errors when you go to apply the model
TCA_mn = (paste0(asc_base,'tca_mn_',YEAR,'.asc'))
TCG_mn = (paste0(asc_base,'tcg_mn_',YEAR,'.asc'))
TCW_mn = (paste0(asc_base,'tcw_mn_',YEAR,'.asc'))
TCB_sd = (paste0(asc_base,'tcb_sd_',YEAR,'.asc'))
Elev_mn = (paste0(BASEPATH,'elev_mn.asc'))
CC_10 = (paste0(asc_base,'cc10_',YEAR,'_res.asc')) # MAKE SURE TO CHANGE NAME FOR EACH MODEL CHANGE

xlist =  list(TCA_mn = TCA_mn, TCG_mn = TCG_mn, TCW_mn = TCW_mn, TCB_sd = TCB_sd, Elev_mn = Elev_mn, CC_10 = CC_10) 
xtypes = list(TCA_mn = "numeric", TCG_mn = "numeric", TCW_mn = "numeric", TCB_sd = "numeric", Elev_mn = "numeric",
              CC_10 = "character") 

# output asc predictive map grid file path
output_file = paste0(cover_base,'cc_',YEAR,'_res.asc')

# This prediction can take some time depending on the size of your prediction study area
AsciiGridPredict(model, xlist, output_file, xtypes)

}

# NEXT STEP: CONVERT ASCII TO TIFS IN PYTHON CODE

# TEST ASCII
#library(raster)

#test_ndvi_mn = raster((paste0(asc_base,'ndvi_mn_',YEAR,'.asc')))
#test_tcg_mn = read.csv((paste0(asc_base,'tcg_mn_',YEAR,'.asc')))
#test_tcw_mn = read.csv((paste0(asc_base,'tcw_mn_',YEAR,'.asc')))
#test_tca_mn = read.csv((paste0(asc_base,'tca_mn_',YEAR,'.asc')))
#test_ndvi_sd = read.csv((paste0(asc_base,'ndvi_sd_',YEAR,'.asc')))
#test_LC_num = read.csv((paste0(asc_base,'landcover_',YEAR,'.asc')))
