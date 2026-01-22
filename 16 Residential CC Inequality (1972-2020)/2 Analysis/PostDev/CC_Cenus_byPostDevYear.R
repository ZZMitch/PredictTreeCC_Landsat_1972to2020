##### This code compares CC data by post-development year with Census data from the closest year across Peel DAs #####

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

library(ggplot2) # Plotting
library(patchwork)
library(Hmisc) # weighted sd
#citation("Hmisc")
library(weights) # weighted t-test
library(dplyr) # % in column

#citation("stats")

##### Bring in Data #####
### Base DA Data ### - for spatial models (later), bring in as shapefile?
da = read.csv("DA_data1.csv")
da = subset(da, Transition > 1970) # Only DAs that developed since 1972
da$Municipality = factor(da$Municipality, levels = c("Caledon", "Brampton", "Mississauga"))

### Adjusted CC Data ###
cc_dev0 = read.csv("Dev0cc.csv", check.names = FALSE) # CC by post-development year (begins at 0)
cc_dev0 = subset(cc_dev0, DAUID %in% da$DAUID) # Only DAs that developed since 1972

### Census Data ###
census1971 = read.csv("Census Data/Census1971_Peel_final.csv")
census1971$CensusYear = 1971
census1981 = read.csv("Census Data/Census1981_Peel_final.csv")
census1981$CensusYear = 1981
census1986 = read.csv("Census Data/Census1986_Peel_final.csv")
census1986$CensusYear = 1986
census1991 = read.csv("Census Data/Census1991_Peel_final.csv")
census1991$CensusYear = 1991
census1996 = read.csv("Census Data/Census1996_Peel_final.csv")
census1996$CensusYear = 1996
census2001 = read.csv("Census Data/Census2001_Peel_final.csv")
census2001$CensusYear = 2001
census2006 = read.csv("Census Data/Census2006_Peel_final.csv")
census2006$CensusYear = 2006
census2011 = read.csv("Census Data/Census2011_Peel_final.csv") # Missing variables
census2011$CensusYear = 2011
census2016 = read.csv("Census Data/Census2016_Peel_final.csv")
census2016$CensusYear = 2016

### Add 2015 Inflation-corrected MedInc to Census tables ###
# See CorrectInflation Excel file for details
census1971$MedInc = census1971$MedInc * 6.209756
census1981$MedInc = census1981$MedInc * 2.847875
census1986$MedInc = census1986$MedInc * 2.01106
census1991$MedInc = census1991$MedInc * 1.619593
census1996$MedInc = census1996$MedInc * 1.451539
census2001$MedInc = census2001$MedInc * 1.33020
census2006$MedInc = census2006$MedInc * 1.184186
census2011$MedInc = census2011$MedInc # No Data
census2016$MedInc = census2016$MedInc

census = rbind(census1971, census1981, census1986, census1991, census1996, census2001, census2006, census2011, census2016)
# Be wary of NAs for some variables in 2010 year...
#####

##### Create Table CC and Pop at 5 year intervals #####
cc_dev0_pop5 = as.data.frame(matrix(ncol = 32, nrow = nrow(cc_dev0)))
colnames(cc_dev0_pop5) = c("DAUID", "Mun", "yr0_cc", "yr0_yr", "yr0_pop", "yr5_cc", "yr5_yr", "yr5_pop", "yr10_cc", "yr10_yr", "yr10_pop", 
                           "yr15_cc", "yr15_yr", "yr15_pop", "yr20_cc", "yr20_yr", "yr20_pop", "yr25_cc", "yr25_yr", "yr25_pop",
                           "yr30_cc", "yr30_yr", "yr30_pop", "yr35_cc", "yr35_yr", "yr35_pop", "yr40_cc", "yr40_yr", "yr40_pop",
                           "yr45_cc", "yr45_yr", "yr45_pop")
cc_dev0_pop5$DAUID = cc_dev0$DAUID
cc_dev0_pop5$Mun = da$Municipality

cc_dev0_pop5$yr0_cc = cc_dev0$`0`
cc_dev0_pop5$yr5_cc = cc_dev0$`5`
cc_dev0_pop5$yr10_cc = cc_dev0$`10`
cc_dev0_pop5$yr15_cc = cc_dev0$`15`
cc_dev0_pop5$yr20_cc = cc_dev0$`20`
cc_dev0_pop5$yr25_cc = cc_dev0$`25`
cc_dev0_pop5$yr30_cc = cc_dev0$`30`
cc_dev0_pop5$yr35_cc = cc_dev0$`35`
cc_dev0_pop5$yr40_cc = cc_dev0$`40`
cc_dev0_pop5$yr45_cc = cc_dev0$`45`

cc_dev0_pop5$yr0_yr = da$Transition
cc_dev0_pop5$yr5_yr = da$Transition + 5
cc_dev0_pop5$yr10_yr = da$Transition + 10
cc_dev0_pop5$yr15_yr = da$Transition + 15
cc_dev0_pop5$yr20_yr = da$Transition + 20
cc_dev0_pop5$yr25_yr = da$Transition + 25
cc_dev0_pop5$yr30_yr = da$Transition + 30
cc_dev0_pop5$yr35_yr = da$Transition + 35
cc_dev0_pop5$yr40_yr = da$Transition + 40
cc_dev0_pop5$yr45_yr = da$Transition + 45

cc_dev0_pop5 = subset(cc_dev0_pop5, yr0_yr <= 2016) # Remove DAs with no census data (i.e., transition year after 2016)

for (i in 1:nrow(cc_dev0_pop5)) { # For all DAs with a transition year
  s1_pop0 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop0 = subset(s1_pop0, CensusYear == CensusYear[which.min(abs(s1_pop0$CensusYear - cc_dev0_pop5$yr0_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr0_pop[i] = s2_pop0$Pop # Extract population from that census
  
  s1_pop5 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop5 = subset(s1_pop5, CensusYear == CensusYear[which.min(abs(s1_pop5$CensusYear - cc_dev0_pop5$yr5_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr5_pop[i] = s2_pop5$Pop # Extract population from that census
  
  s1_pop10 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop10 = subset(s1_pop10, CensusYear == CensusYear[which.min(abs(s1_pop10$CensusYear - cc_dev0_pop5$yr10_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr10_pop[i] = s2_pop10$Pop # Extract population from that census
  
  s1_pop15 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop15 = subset(s1_pop15, CensusYear == CensusYear[which.min(abs(s1_pop15$CensusYear - cc_dev0_pop5$yr15_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr15_pop[i] = s2_pop15$Pop # Extract population from that census
  
  s1_pop20 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop20 = subset(s1_pop20, CensusYear == CensusYear[which.min(abs(s1_pop20$CensusYear - cc_dev0_pop5$yr20_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr20_pop[i] = s2_pop20$Pop # Extract population from that census
  
  s1_pop25 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop25 = subset(s1_pop25, CensusYear == CensusYear[which.min(abs(s1_pop25$CensusYear - cc_dev0_pop5$yr25_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr25_pop[i] = s2_pop25$Pop # Extract population from that census
  
  s1_pop30 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop30 = subset(s1_pop30, CensusYear == CensusYear[which.min(abs(s1_pop30$CensusYear - cc_dev0_pop5$yr30_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr30_pop[i] = s2_pop30$Pop # Extract population from that census
  
  s1_pop35 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop35 = subset(s1_pop35, CensusYear == CensusYear[which.min(abs(s1_pop35$CensusYear - cc_dev0_pop5$yr35_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr35_pop[i] = s2_pop35$Pop # Extract population from that census
  
  s1_pop40 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop40 = subset(s1_pop40, CensusYear == CensusYear[which.min(abs(s1_pop40$CensusYear - cc_dev0_pop5$yr40_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr40_pop[i] = s2_pop40$Pop # Extract population from that census
  
  s1_pop45 = subset(census, DAUID == cc_dev0_pop5$DAUID[i]) # Subset census years that match the selected DA
  s2_pop45 = subset(s1_pop45, CensusYear == CensusYear[which.min(abs(s1_pop45$CensusYear - cc_dev0_pop5$yr45_yr[i]))]) # Find census year closest to selected year
  cc_dev0_pop5$yr45_pop[i] = s2_pop45$Pop # Extract population from that census
} 

# NOTE: some data where there should be NAs (e.g., years after 2020 have NA cc, but have population and transition year numbers)
# Should not impact plots (since NAs in cc will remove those from results, but keep in mind just in case)
#####

cols = c("DAUID", "CC", "Trans_yr", "X_yr", "Mun", 
         "Pop", "PopDen", "pDet", "pAtt", "pApt", "MC_DwTy", "Cen_yr_bf", 
         "MedInc", "pImm", "pImm_Am", "pImm_Eu", "pImm_Af", "pImm_As", "pImm_notEu", "pMin", "NWEur", "Cen_yr_oth")
# DAUID, Canopy Cover, Transition Year, Post-transition year X, Municipality'
# Population, Residential Population Density, % Detached, % Attached, % Apartment, Most Common Dwelling Type, Census year of built form variables (in 2011 census)
# Inflation-adjusted Median Income, % Immigrants (Americas/Europe/Africa/Asia/not Europe), % Minorities, % with origins in NW Europe, Census year from other variables (no 2011)

##### High vs. Low CC DAs #####
##### PostDev Year 0 #####
### ID High and Low CC DAs ###
yr0_mn = weighted.mean(x = cc_dev0_pop5$yr0_cc, w = cc_dev0_pop5$yr0_pop, na.rm = TRUE) 
yr0_mn # Mean: weighted by population
yr0_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr0_cc, w = cc_dev0_pop5$yr0_pop, na.rm = TRUE))
yr0_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr0_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr0_cc > yr0_mn + yr0_sd))))
colnames(cc_yr0_hi) = cols # Create data frame to fill

cc_yr0_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr0_cc > yr0_mn + yr0_sd) # DAUIDs with high CC 
cc_yr0_hi$CC = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$yr0_cc > yr0_mn + yr0_sd) 
cc_yr0_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr0_hi$DAUID) 
cc_yr0_hi$X_yr = cc_yr0_hi$Trans_yr # Year X post-transition
cc_yr0_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr0_hi$DAUID) 

# 1 SD below mean
cc_yr0_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr0_cc < yr0_mn - yr0_sd))))
colnames(cc_yr0_lo) = cols # Create data frame to fill

cc_yr0_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr0_cc < yr0_mn - yr0_sd) # DAUIDs with high CC 
cc_yr0_lo$CC = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$yr0_cc < yr0_mn - yr0_sd) 
cc_yr0_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr0_lo$DAUID) 
cc_yr0_lo$X_yr = cc_yr0_lo$Trans_yr # Year X post-transition
cc_yr0_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr0_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr0_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr0_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr0_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr0_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr0_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr0_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr0_hi$X_yr[i] - cc_yr0_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr0_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr0_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr0_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr0_hi[i,13:ncol(cc_yr0_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr0_hi$X_yr[i] - cc_yr0_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr0_hi[i,13:ncol(cc_yr0_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr0_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr0_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr0_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr0_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr0_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr0_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr0_lo$X_yr[i] - cc_yr0_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr0_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr0_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr0_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr0_lo[i,13:ncol(cc_yr0_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr0_lo$X_yr[i] - cc_yr0_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr0_lo[i,13:ncol(cc_yr0_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr0_hi = cc_yr0_hi[rowSums(is.na(cc_yr0_hi)) <= 10, ]
cc_yr0_lo = cc_yr0_lo[rowSums(is.na(cc_yr0_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr0_hi$cc_cat = "High"
cc_yr0_lo$cc_cat = "Low"
cc_yr0 = rbind(cc_yr0_hi, cc_yr0_lo)
cc_yr0$cc_cat = factor(cc_yr0$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 5 #####
### ID High and Low CC DAs ###
yr5_mn = weighted.mean(x = cc_dev0_pop5$yr5_cc, w = cc_dev0_pop5$yr5_pop, na.rm = TRUE) 
yr5_mn # Mean: weighted by population
yr5_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr5_cc, w = cc_dev0_pop5$yr5_pop, na.rm = TRUE))
yr5_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr5_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr5_cc > yr5_mn + yr5_sd))))
colnames(cc_yr5_hi) = cols # Create data frame to fill

cc_yr5_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr5_cc > yr5_mn + yr5_sd) # DAUIDs with high CC 
cc_yr5_hi$CC = subset(cc_dev0_pop5$yr5_cc, cc_dev0_pop5$yr5_cc > yr5_mn + yr5_sd) 
cc_yr5_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr5_hi$DAUID) 
cc_yr5_hi$X_yr = cc_yr5_hi$Trans_yr + 5 # Year X post-transition
cc_yr5_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr5_hi$DAUID) 

# 1 SD below mean
cc_yr5_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr5_cc < yr5_mn - yr5_sd))))
colnames(cc_yr5_lo) = cols # Create data frame to fill

cc_yr5_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr5_cc < yr5_mn - yr5_sd) # DAUIDs with high CC 
cc_yr5_lo$CC = subset(cc_dev0_pop5$yr5_cc, cc_dev0_pop5$yr5_cc < yr5_mn - yr5_sd) 
cc_yr5_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr5_lo$DAUID) 
cc_yr5_lo$X_yr = cc_yr5_lo$Trans_yr + 5 # Year X post-transition
cc_yr5_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr5_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr5_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr5_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr5_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr5_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr5_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr5_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr5_hi$X_yr[i] - cc_yr5_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr5_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr5_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr5_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr5_hi[i,13:ncol(cc_yr5_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr5_hi$X_yr[i] - cc_yr5_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr5_hi[i,13:ncol(cc_yr5_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr5_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr5_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr5_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr5_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr5_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr5_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr5_lo$X_yr[i] - cc_yr5_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr5_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr5_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr5_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr5_lo[i,13:ncol(cc_yr5_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr5_lo$X_yr[i] - cc_yr5_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr5_lo[i,13:ncol(cc_yr5_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr5_hi = cc_yr5_hi[rowSums(is.na(cc_yr5_hi)) <= 10, ]
cc_yr5_lo = cc_yr5_lo[rowSums(is.na(cc_yr5_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr5_hi$cc_cat = "High"
cc_yr5_lo$cc_cat = "Low"
cc_yr5 = rbind(cc_yr5_hi, cc_yr5_lo)
cc_yr5$cc_cat = factor(cc_yr5$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 10 #####
### ID High and Low CC DAs ###
yr10_mn = weighted.mean(x = cc_dev0_pop5$yr10_cc, w = cc_dev0_pop5$yr10_pop, na.rm = TRUE) 
yr10_mn # Mean: weighted by population
yr10_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr10_cc, w = cc_dev0_pop5$yr10_pop, na.rm = TRUE))
yr10_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr10_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr10_cc > yr10_mn + yr10_sd))))
colnames(cc_yr10_hi) = cols # Create data frame to fill

cc_yr10_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr10_cc > yr10_mn + yr10_sd) # DAUIDs with high CC 
cc_yr10_hi$CC = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$yr10_cc > yr10_mn + yr10_sd) 
cc_yr10_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr10_hi$DAUID) 
cc_yr10_hi$X_yr = cc_yr10_hi$Trans_yr + 10 # Year X post-transition
cc_yr10_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr10_hi$DAUID) 

# 1 SD below mean
cc_yr10_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr10_cc < yr10_mn - yr10_sd))))
colnames(cc_yr10_lo) = cols # Create data frame to fill

cc_yr10_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr10_cc < yr10_mn - yr10_sd) # DAUIDs with high CC 
cc_yr10_lo$CC = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$yr10_cc < yr10_mn - yr10_sd) 
cc_yr10_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr10_lo$DAUID) 
cc_yr10_lo$X_yr = cc_yr10_lo$Trans_yr + 10 # Year X post-transition
cc_yr10_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr10_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr10_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr10_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr10_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr10_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr10_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr10_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr10_hi$X_yr[i] - cc_yr10_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr10_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr10_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr10_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr10_hi[i,13:ncol(cc_yr10_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr10_hi$X_yr[i] - cc_yr10_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr10_hi[i,13:ncol(cc_yr10_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr10_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr10_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr10_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr10_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr10_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr10_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr10_lo$X_yr[i] - cc_yr10_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr10_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr10_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr10_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr10_lo[i,13:ncol(cc_yr10_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr10_lo$X_yr[i] - cc_yr10_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr10_lo[i,13:ncol(cc_yr10_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr10_hi = cc_yr10_hi[rowSums(is.na(cc_yr10_hi)) <= 10, ]
cc_yr10_lo = cc_yr10_lo[rowSums(is.na(cc_yr10_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr10_hi$cc_cat = "High"
cc_yr10_lo$cc_cat = "Low"
cc_yr10 = rbind(cc_yr10_hi, cc_yr10_lo)
cc_yr10$cc_cat = factor(cc_yr10$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 15 #####
### ID High and Low CC DAs ###
yr15_mn = weighted.mean(x = cc_dev0_pop5$yr15_cc, w = cc_dev0_pop5$yr15_pop, na.rm = TRUE) 
yr15_mn # Mean: weighted by population
yr15_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr15_cc, w = cc_dev0_pop5$yr15_pop, na.rm = TRUE))
yr15_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr15_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr15_cc > yr15_mn + yr15_sd))))
colnames(cc_yr15_hi) = cols # Create data frame to fill

cc_yr15_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr15_cc > yr15_mn + yr15_sd) # DAUIDs with high CC 
cc_yr15_hi$CC = subset(cc_dev0_pop5$yr15_cc, cc_dev0_pop5$yr15_cc > yr15_mn + yr15_sd) 
cc_yr15_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr15_hi$DAUID) 
cc_yr15_hi$X_yr = cc_yr15_hi$Trans_yr + 15 # Year X post-transition
cc_yr15_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr15_hi$DAUID) 

# 1 SD below mean
cc_yr15_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr15_cc < yr15_mn - yr15_sd))))
colnames(cc_yr15_lo) = cols # Create data frame to fill

cc_yr15_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr15_cc < yr15_mn - yr15_sd) # DAUIDs with high CC 
cc_yr15_lo$CC = subset(cc_dev0_pop5$yr15_cc, cc_dev0_pop5$yr15_cc < yr15_mn - yr15_sd) 
cc_yr15_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr15_lo$DAUID) 
cc_yr15_lo$X_yr = cc_yr15_lo$Trans_yr + 15 # Year X post-transition
cc_yr15_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr15_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr15_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr15_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr15_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr15_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr15_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr15_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr15_hi$X_yr[i] - cc_yr15_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr15_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr15_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr15_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr15_hi[i,13:ncol(cc_yr15_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr15_hi$X_yr[i] - cc_yr15_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr15_hi[i,13:ncol(cc_yr15_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr15_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr15_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr15_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr15_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr15_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr15_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr15_lo$X_yr[i] - cc_yr15_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr15_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr15_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr15_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr15_lo[i,13:ncol(cc_yr15_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr15_lo$X_yr[i] - cc_yr15_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr15_lo[i,13:ncol(cc_yr15_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr15_hi = cc_yr15_hi[rowSums(is.na(cc_yr15_hi)) <= 10, ]
cc_yr15_lo = cc_yr15_lo[rowSums(is.na(cc_yr15_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr15_hi$cc_cat = "High"
cc_yr15_lo$cc_cat = "Low"
cc_yr15 = rbind(cc_yr15_hi, cc_yr15_lo)
cc_yr15$cc_cat = factor(cc_yr15$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 20 #####
### ID High and Low CC DAs ###
yr20_mn = weighted.mean(x = cc_dev0_pop5$yr20_cc, w = cc_dev0_pop5$yr20_pop, na.rm = TRUE) 
yr20_mn # Mean: weighted by population
yr20_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr20_cc, w = cc_dev0_pop5$yr20_pop, na.rm = TRUE))
yr20_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr20_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr20_cc > yr20_mn + yr20_sd))))
colnames(cc_yr20_hi) = cols # Create data frame to fill

cc_yr20_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr20_cc > yr20_mn + yr20_sd) # DAUIDs with high CC 
cc_yr20_hi$CC = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$yr20_cc > yr20_mn + yr20_sd) 
cc_yr20_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr20_hi$DAUID) 
cc_yr20_hi$X_yr = cc_yr20_hi$Trans_yr + 20 # Year X post-transition
cc_yr20_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr20_hi$DAUID) 

# 1 SD below mean
cc_yr20_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr20_cc < yr20_mn - yr20_sd))))
colnames(cc_yr20_lo) = cols # Create data frame to fill

cc_yr20_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr20_cc < yr20_mn - yr20_sd) # DAUIDs with high CC 
cc_yr20_lo$CC = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$yr20_cc < yr20_mn - yr20_sd) 
cc_yr20_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr20_lo$DAUID) 
cc_yr20_lo$X_yr = cc_yr20_lo$Trans_yr + 20 # Year X post-transition
cc_yr20_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr20_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr20_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr20_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr20_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr20_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr20_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr20_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr20_hi$X_yr[i] - cc_yr20_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr20_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr20_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr20_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr20_hi[i,13:ncol(cc_yr20_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr20_hi$X_yr[i] - cc_yr20_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr20_hi[i,13:ncol(cc_yr20_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr20_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr20_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr20_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr20_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr20_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr20_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr20_lo$X_yr[i] - cc_yr20_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr20_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr20_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr20_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr20_lo[i,13:ncol(cc_yr20_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr20_lo$X_yr[i] - cc_yr20_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr20_lo[i,13:ncol(cc_yr20_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr20_hi = cc_yr20_hi[rowSums(is.na(cc_yr20_hi)) <= 10, ]
cc_yr20_lo = cc_yr20_lo[rowSums(is.na(cc_yr20_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr20_hi$cc_cat = "High"
cc_yr20_lo$cc_cat = "Low"
cc_yr20 = rbind(cc_yr20_hi, cc_yr20_lo)
cc_yr20$cc_cat = factor(cc_yr20$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 25 #####
### ID High and Low CC DAs ###
yr25_mn = weighted.mean(x = cc_dev0_pop5$yr25_cc, w = cc_dev0_pop5$yr25_pop, na.rm = TRUE) 
yr25_mn # Mean: weighted by population
yr25_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr25_cc, w = cc_dev0_pop5$yr25_pop, na.rm = TRUE))
yr25_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr25_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr25_cc > yr25_mn + yr25_sd))))
colnames(cc_yr25_hi) = cols # Create data frame to fill

cc_yr25_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr25_cc > yr25_mn + yr25_sd) # DAUIDs with high CC 
cc_yr25_hi$CC = subset(cc_dev0_pop5$yr25_cc, cc_dev0_pop5$yr25_cc > yr25_mn + yr25_sd) 
cc_yr25_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr25_hi$DAUID) 
cc_yr25_hi$X_yr = cc_yr25_hi$Trans_yr + 25 # Year X post-transition
cc_yr25_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr25_hi$DAUID) 

# 1 SD below mean
cc_yr25_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr25_cc < yr25_mn - yr25_sd))))
colnames(cc_yr25_lo) = cols # Create data frame to fill

cc_yr25_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr25_cc < yr25_mn - yr25_sd) # DAUIDs with high CC 
cc_yr25_lo$CC = subset(cc_dev0_pop5$yr25_cc, cc_dev0_pop5$yr25_cc < yr25_mn - yr25_sd) 
cc_yr25_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr25_lo$DAUID) 
cc_yr25_lo$X_yr = cc_yr25_lo$Trans_yr + 25 # Year X post-transition
cc_yr25_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr25_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr25_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr25_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr25_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr25_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr25_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr25_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr25_hi$X_yr[i] - cc_yr25_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr25_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr25_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr25_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr25_hi[i,13:ncol(cc_yr25_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr25_hi$X_yr[i] - cc_yr25_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr25_hi[i,13:ncol(cc_yr25_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr25_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr25_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr25_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr25_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr25_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr25_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr25_lo$X_yr[i] - cc_yr25_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr25_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr25_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr25_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr25_lo[i,13:ncol(cc_yr25_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr25_lo$X_yr[i] - cc_yr25_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr25_lo[i,13:ncol(cc_yr25_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr25_hi = cc_yr25_hi[rowSums(is.na(cc_yr25_hi)) <= 10, ]
cc_yr25_lo = cc_yr25_lo[rowSums(is.na(cc_yr25_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr25_hi$cc_cat = "High"
cc_yr25_lo$cc_cat = "Low"
cc_yr25 = rbind(cc_yr25_hi, cc_yr25_lo)
cc_yr25$cc_cat = factor(cc_yr25$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 30 #####
### ID High and Low CC DAs ###
yr30_mn = weighted.mean(x = cc_dev0_pop5$yr30_cc, w = cc_dev0_pop5$yr30_pop, na.rm = TRUE) 
yr30_mn # Mean: weighted by population
yr30_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr30_cc, w = cc_dev0_pop5$yr30_pop, na.rm = TRUE))
yr30_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr30_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr30_cc > yr30_mn + yr30_sd))))
colnames(cc_yr30_hi) = cols # Create data frame to fill

cc_yr30_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr30_cc > yr30_mn + yr30_sd) # DAUIDs with high CC 
cc_yr30_hi$CC = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$yr30_cc > yr30_mn + yr30_sd) 
cc_yr30_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr30_hi$DAUID) 
cc_yr30_hi$X_yr = cc_yr30_hi$Trans_yr + 30 # Year X post-transition
cc_yr30_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr30_hi$DAUID) 

# 1 SD below mean
cc_yr30_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr30_cc < yr30_mn - yr30_sd))))
colnames(cc_yr30_lo) = cols # Create data frame to fill

cc_yr30_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr30_cc < yr30_mn - yr30_sd) # DAUIDs with high CC 
cc_yr30_lo$CC = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$yr30_cc < yr30_mn - yr30_sd) 
cc_yr30_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr30_lo$DAUID) 
cc_yr30_lo$X_yr = cc_yr30_lo$Trans_yr + 30 # Year X post-transition
cc_yr30_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr30_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr30_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr30_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr30_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr30_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr30_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr30_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr30_hi$X_yr[i] - cc_yr30_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr30_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr30_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr30_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr30_hi[i,13:ncol(cc_yr30_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr30_hi$X_yr[i] - cc_yr30_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr30_hi[i,13:ncol(cc_yr30_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr30_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr30_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr30_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr30_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr30_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr30_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr30_lo$X_yr[i] - cc_yr30_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr30_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr30_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr30_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr30_lo[i,13:ncol(cc_yr30_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr30_lo$X_yr[i] - cc_yr30_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr30_lo[i,13:ncol(cc_yr30_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr30_hi = cc_yr30_hi[rowSums(is.na(cc_yr30_hi)) <= 10, ]
cc_yr30_lo = cc_yr30_lo[rowSums(is.na(cc_yr30_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr30_hi$cc_cat = "High"
cc_yr30_lo$cc_cat = "Low"
cc_yr30 = rbind(cc_yr30_hi, cc_yr30_lo)
cc_yr30$cc_cat = factor(cc_yr30$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 35 #####
### ID High and Low CC DAs ###
yr35_mn = weighted.mean(x = cc_dev0_pop5$yr35_cc, w = cc_dev0_pop5$yr35_pop, na.rm = TRUE) 
yr35_mn # Mean: weighted by population
yr35_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr35_cc, w = cc_dev0_pop5$yr35_pop, na.rm = TRUE))
yr35_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr35_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr35_cc > yr35_mn + yr35_sd))))
colnames(cc_yr35_hi) = cols # Create data frame to fill

cc_yr35_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr35_cc > yr35_mn + yr35_sd) # DAUIDs with high CC 
cc_yr35_hi$CC = subset(cc_dev0_pop5$yr35_cc, cc_dev0_pop5$yr35_cc > yr35_mn + yr35_sd) 
cc_yr35_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr35_hi$DAUID) 
cc_yr35_hi$X_yr = cc_yr35_hi$Trans_yr + 35 # Year X post-transition
cc_yr35_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr35_hi$DAUID) 

# 1 SD below mean
cc_yr35_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr35_cc < yr35_mn - yr35_sd))))
colnames(cc_yr35_lo) = cols # Create data frame to fill

cc_yr35_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr35_cc < yr35_mn - yr35_sd) # DAUIDs with high CC 
cc_yr35_lo$CC = subset(cc_dev0_pop5$yr35_cc, cc_dev0_pop5$yr35_cc < yr35_mn - yr35_sd) 
cc_yr35_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr35_lo$DAUID) 
cc_yr35_lo$X_yr = cc_yr35_lo$Trans_yr + 35 # Year X post-transition
cc_yr35_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr35_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr35_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr35_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr35_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr35_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr35_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr35_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr35_hi$X_yr[i] - cc_yr35_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr35_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr35_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr35_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr35_hi[i,13:ncol(cc_yr35_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr35_hi$X_yr[i] - cc_yr35_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr35_hi[i,13:ncol(cc_yr35_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr35_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr35_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr35_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr35_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr35_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr35_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr35_lo$X_yr[i] - cc_yr35_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr35_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr35_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr35_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr35_lo[i,13:ncol(cc_yr35_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr35_lo$X_yr[i] - cc_yr35_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr35_lo[i,13:ncol(cc_yr35_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr35_hi = cc_yr35_hi[rowSums(is.na(cc_yr35_hi)) <= 10, ]
cc_yr35_lo = cc_yr35_lo[rowSums(is.na(cc_yr35_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr35_hi$cc_cat = "High"
cc_yr35_lo$cc_cat = "Low"
cc_yr35 = rbind(cc_yr35_hi, cc_yr35_lo)
cc_yr35$cc_cat = factor(cc_yr35$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 40 #####
### ID High and Low CC DAs ###
yr40_mn = weighted.mean(x = cc_dev0_pop5$yr40_cc, w = cc_dev0_pop5$yr40_pop, na.rm = TRUE) 
yr40_mn # Mean: weighted by population
yr40_sd = sqrt(wtd.var(x = cc_dev0_pop5$yr40_cc, w = cc_dev0_pop5$yr40_pop, na.rm = TRUE))
yr40_sd # Standard deviation: weighted by population

# 1 SD above mean
cc_yr40_hi = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr40_cc > yr40_mn + yr40_sd))))
colnames(cc_yr40_hi) = cols # Create data frame to fill

cc_yr40_hi$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr40_cc > yr40_mn + yr40_sd) # DAUIDs with high CC 
cc_yr40_hi$CC = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$yr40_cc > yr40_mn + yr40_sd) 
cc_yr40_hi$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr40_hi$DAUID) 
cc_yr40_hi$X_yr = cc_yr40_hi$Trans_yr + 40 # Year X post-transition
cc_yr40_hi$Mun = subset(da$Municipality, da$DAUID %in% cc_yr40_hi$DAUID) 

# 1 SD below mean
cc_yr40_lo = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr40_cc < yr40_mn - yr40_sd))))
colnames(cc_yr40_lo) = cols # Create data frame to fill

cc_yr40_lo$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr40_cc < yr40_mn - yr40_sd) # DAUIDs with high CC 
cc_yr40_lo$CC = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$yr40_cc < yr40_mn - yr40_sd) 
cc_yr40_lo$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr40_lo$DAUID) 
cc_yr40_lo$X_yr = cc_yr40_lo$Trans_yr + 40 # Year X post-transition
cc_yr40_lo$Mun = subset(da$Municipality, da$DAUID %in% cc_yr40_lo$DAUID) 

### Find closest Census year to each DA PostDev Year 0 ###
# 1 SD above mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr40_hi$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr40_hi$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr40_hi)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr40_hi$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr40_hi$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr40_hi[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr40_hi$X_yr[i] - cc_yr40_hi$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr40_hi[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr40_hi$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr40_hi$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr40_hi[i,13:ncol(cc_yr40_hi)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr40_hi$X_yr[i] - cc_yr40_hi$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr40_hi[i,13:ncol(cc_yr40_hi)] = NA
  }
}

# 1 SD below mean
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr40_lo$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr40_lo$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr40_lo)) { # For all DAs  that fit tlos criteria
  s1_bf = subset(s_bf, DAUID == cc_yr40_lo$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr40_lo$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr40_lo[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr40_lo$X_yr[i] - cc_yr40_lo$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr40_lo[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr40_lo$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr40_lo$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr40_lo[i,13:ncol(cc_yr40_lo)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr40_lo$X_yr[i] - cc_yr40_lo$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr40_lo[i,13:ncol(cc_yr40_lo)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr40_hi = cc_yr40_hi[rowSums(is.na(cc_yr40_hi)) <= 10, ]
cc_yr40_lo = cc_yr40_lo[rowSums(is.na(cc_yr40_lo)) <= 10, ]
###

### Merge into one table for plotting ###
cc_yr40_hi$cc_cat = "High"
cc_yr40_lo$cc_cat = "Low"
cc_yr40 = rbind(cc_yr40_hi, cc_yr40_lo)
cc_yr40$cc_cat = factor(cc_yr40$cc_cat, levels = c("Low", "High"))
###
#####

##### PostDev Year 45 #####
# Not enough DAs / Population to add to final figures



##### ALL DAs #####
##### PostDev Year 0 #####
cc_yr0_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr0_cc >= 0))))
colnames(cc_yr0_all) = cols # Create data frame to fill

cc_yr0_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr0_cc >= 0) # DAUIDs with high CC 
cc_yr0_all$CC = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$yr0_cc >= 0) 
cc_yr0_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr0_all$DAUID) 
cc_yr0_all$X_yr = cc_yr0_all$Trans_yr # Year X post-transition
cc_yr0_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr0_all$DAUID)

### Find closest Census year to each DA PostDev Year 0 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr0_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr0_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr0_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr0_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr0_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr0_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr0_all$X_yr[i] - cc_yr0_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr0_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr0_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr0_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr0_all[i,13:ncol(cc_yr0_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr0_all$X_yr[i] - cc_yr0_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr0_all[i,13:ncol(cc_yr0_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr0_all = cc_yr0_all[rowSums(is.na(cc_yr0_all)) <= 10, ]
###
#####

##### PostDev Year 5 #####
cc_yr5_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr5_cc >= 0))))
colnames(cc_yr5_all) = cols # Create data frame to fill

cc_yr5_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr5_cc >= 0) # DAUIDs with high CC 
cc_yr5_all$CC = subset(cc_dev0_pop5$yr5_cc, cc_dev0_pop5$yr5_cc >= 0) 
cc_yr5_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr5_all$DAUID) 
cc_yr5_all$X_yr = cc_yr5_all$Trans_yr + 5 # Year X post-transition
cc_yr5_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr5_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr5_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr5_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr5_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr5_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr5_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr5_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr5_all$X_yr[i] - cc_yr5_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr5_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr5_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr5_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr5_all[i,13:ncol(cc_yr5_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr5_all$X_yr[i] - cc_yr5_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr5_all[i,13:ncol(cc_yr5_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr5_all = cc_yr5_all[rowSums(is.na(cc_yr5_all)) <= 10, ]
###
#####

##### PostDev Year 10 #####
cc_yr10_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr10_cc >= 0))))
colnames(cc_yr10_all) = cols # Create data frame to fill

cc_yr10_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr10_cc >= 0) # DAUIDs with high CC 
cc_yr10_all$CC = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$yr10_cc >= 0) 
cc_yr10_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr10_all$DAUID) 
cc_yr10_all$X_yr = cc_yr10_all$Trans_yr + 10 # Year X post-transition
cc_yr10_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr10_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr10_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr10_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr10_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr10_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr10_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr10_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr10_all$X_yr[i] - cc_yr10_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr10_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr10_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr10_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr10_all[i,13:ncol(cc_yr10_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr10_all$X_yr[i] - cc_yr10_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr10_all[i,13:ncol(cc_yr10_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr10_all = cc_yr10_all[rowSums(is.na(cc_yr10_all)) <= 10, ]
###
#####

##### PostDev Year 15 #####
cc_yr15_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr15_cc >= 0))))
colnames(cc_yr15_all) = cols # Create data frame to fill

cc_yr15_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr15_cc >= 0) # DAUIDs with high CC 
cc_yr15_all$CC = subset(cc_dev0_pop5$yr15_cc, cc_dev0_pop5$yr15_cc >= 0) 
cc_yr15_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr15_all$DAUID) 
cc_yr15_all$X_yr = cc_yr15_all$Trans_yr + 15 # Year X post-transition
cc_yr15_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr15_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr15_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr15_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr15_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr15_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr15_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr15_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr15_all$X_yr[i] - cc_yr15_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr15_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr15_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr15_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr15_all[i,13:ncol(cc_yr15_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr15_all$X_yr[i] - cc_yr15_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr15_all[i,13:ncol(cc_yr15_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr15_all = cc_yr15_all[rowSums(is.na(cc_yr15_all)) <= 10, ]
###
#####

##### PostDev Year 20 #####
cc_yr20_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr20_cc >= 0))))
colnames(cc_yr20_all) = cols # Create data frame to fill

cc_yr20_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr20_cc >= 0) # DAUIDs with high CC 
cc_yr20_all$CC = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$yr20_cc >= 0) 
cc_yr20_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr20_all$DAUID) 
cc_yr20_all$X_yr = cc_yr20_all$Trans_yr + 20 # Year X post-transition
cc_yr20_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr20_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr20_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr20_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr20_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr20_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr20_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr20_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr20_all$X_yr[i] - cc_yr20_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr20_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr20_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr20_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr20_all[i,13:ncol(cc_yr20_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr20_all$X_yr[i] - cc_yr20_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr20_all[i,13:ncol(cc_yr20_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr20_all = cc_yr20_all[rowSums(is.na(cc_yr20_all)) <= 10, ]
###
#####

##### PostDev Year 25 #####
cc_yr25_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr25_cc >= 0))))
colnames(cc_yr25_all) = cols # Create data frame to fill

cc_yr25_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr25_cc >= 0) # DAUIDs with high CC 
cc_yr25_all$CC = subset(cc_dev0_pop5$yr25_cc, cc_dev0_pop5$yr25_cc >= 0) 
cc_yr25_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr25_all$DAUID) 
cc_yr25_all$X_yr = cc_yr25_all$Trans_yr + 25 # Year X post-transition
cc_yr25_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr25_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr25_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr25_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr25_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr25_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr25_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr25_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr25_all$X_yr[i] - cc_yr25_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr25_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr25_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr25_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr25_all[i,13:ncol(cc_yr25_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr25_all$X_yr[i] - cc_yr25_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr25_all[i,13:ncol(cc_yr25_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr25_all = cc_yr25_all[rowSums(is.na(cc_yr25_all)) <= 10, ]
###
#####

##### PostDev Year 30 #####
cc_yr30_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr30_cc >= 0))))
colnames(cc_yr30_all) = cols # Create data frame to fill

cc_yr30_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr30_cc >= 0) # DAUIDs with high CC 
cc_yr30_all$CC = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$yr30_cc >= 0) 
cc_yr30_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr30_all$DAUID) 
cc_yr30_all$X_yr = cc_yr30_all$Trans_yr + 30 # Year X post-transition
cc_yr30_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr30_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr30_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr30_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr30_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr30_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr30_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr30_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr30_all$X_yr[i] - cc_yr30_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr30_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr30_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr30_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr30_all[i,13:ncol(cc_yr30_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr30_all$X_yr[i] - cc_yr30_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr30_all[i,13:ncol(cc_yr30_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr30_all = cc_yr30_all[rowSums(is.na(cc_yr30_all)) <= 10, ]
###
#####

##### PostDev Year 35 #####
cc_yr35_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr35_cc >= 0))))
colnames(cc_yr35_all) = cols # Create data frame to fill

cc_yr35_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr35_cc >= 0) # DAUIDs with high CC 
cc_yr35_all$CC = subset(cc_dev0_pop5$yr35_cc, cc_dev0_pop5$yr35_cc >= 0) 
cc_yr35_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr35_all$DAUID) 
cc_yr35_all$X_yr = cc_yr35_all$Trans_yr + 35 # Year X post-transition
cc_yr35_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr35_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr35_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr35_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr35_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr35_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr35_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr35_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr35_all$X_yr[i] - cc_yr35_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr35_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr35_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr35_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr35_all[i,13:ncol(cc_yr35_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr35_all$X_yr[i] - cc_yr35_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr35_all[i,13:ncol(cc_yr35_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr35_all = cc_yr35_all[rowSums(is.na(cc_yr35_all)) <= 10, ]
###
#####

##### PostDev Year 40 #####
cc_yr40_all = as.data.frame(matrix(ncol = 22, nrow = nrow(subset(cc_dev0_pop5, yr40_cc >= 0))))
colnames(cc_yr40_all) = cols # Create data frame to fill

cc_yr40_all$DAUID = subset(cc_dev0_pop5$DAUID, cc_dev0_pop5$yr40_cc >= 0) # DAUIDs with high CC 
cc_yr40_all$CC = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$yr40_cc >= 0) 
cc_yr40_all$Trans_yr = subset(da$Transition, da$DAUID %in% cc_yr40_all$DAUID) 
cc_yr40_all$X_yr = cc_yr40_all$Trans_yr + 40 # Year X post-transition
cc_yr40_all$Mun = subset(da$Municipality, da$DAUID %in% cc_yr40_all$DAUID)

### Find closest Census year to each DA PostDev Year 5 ###
s_bf = subset(census[c(1:7,17)], DAUID %in% cc_yr40_all$DAUID) # Just columns with data in 2011
s_oth = subset(census[c(1,8:17)], DAUID %in% cc_yr40_all$DAUID & CensusYear != 2011) # All other columns (remove 2011)

for (i in 1:nrow(cc_yr40_all)) { # For all DAs  that fit this criteria
  s1_bf = subset(s_bf, DAUID == cc_yr40_all$DAUID[i]) # Built form: Subset census years that match the selected DA
  s2_bf = subset(s1_bf, CensusYear == CensusYear[which.min(abs(s1_bf$CensusYear - cc_yr40_all$X_yr[i]))]) # Built form: Find the census year closest to selected post-dev year
  cc_yr40_all[i,6:12] = s2_bf[2:ncol(s2_bf)] # Built form: Add to final table
  if (abs(cc_yr40_all$X_yr[i] - cc_yr40_all$Cen_yr_bf[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr40_all[i,6:12] = NA
  }
  
  s1_oth = subset(s_oth, DAUID == cc_yr40_all$DAUID[i]) # Other: Subset census years that match the selected DA
  s2_oth = subset(s1_oth, CensusYear == CensusYear[which.min(abs(s1_oth$CensusYear - cc_yr40_all$X_yr[i]))]) # Other: Find the census year closest to selected post-dev year
  cc_yr40_all[i,13:ncol(cc_yr40_all)] = s2_oth[2:ncol(s2_oth)] # Other: Add to final table
  if (abs(cc_yr40_all$X_yr[i] - cc_yr40_all$Cen_yr_oth[i]) >= 10) { # If difference is at least 10, set to NA
    cc_yr40_all[i,13:ncol(cc_yr40_all)] = NA
  }
}

### Remove DAs here both parts (bf, oth) set to NA ###
cc_yr40_all = cc_yr40_all[rowSums(is.na(cc_yr40_all)) <= 10, ]
###
#####

##### Export Tables for further analysis #####
#write.csv(cc_dev0_pop5, "cc_dev0_pop5.csv")

#write.csv(cc_yr0, "cc_yr0_hilo.csv")
#write.csv(cc_yr5, "cc_yr5_hilo.csv")
#write.csv(cc_yr10, "cc_yr10_hilo.csv")
#write.csv(cc_yr15, "cc_yr15_hilo.csv")
#write.csv(cc_yr20, "cc_yr20_hilo.csv")
#write.csv(cc_yr25, "cc_yr25_hilo.csv")
#write.csv(cc_yr30, "cc_yr30_hilo.csv")
#write.csv(cc_yr35, "cc_yr35_hilo.csv")
#write.csv(cc_yr40, "cc_yr40_hilo.csv")

#write.csv(cc_yr0_all, "cc_yr0_all.csv")
#write.csv(cc_yr5_all, "cc_yr5_all.csv")
#write.csv(cc_yr10_all, "cc_yr10_all.csv")
#write.csv(cc_yr15_all, "cc_yr15_all.csv")
#write.csv(cc_yr20_all, "cc_yr20_all.csv")
#write.csv(cc_yr25_all, "cc_yr25_all.csv")
#write.csv(cc_yr30_all, "cc_yr30_all.csv")
#write.csv(cc_yr35_all, "cc_yr35_all.csv")
#write.csv(cc_yr40_all, "cc_yr40_all.csv")

##### % of Different Census years in each table (use BF version) #####
# High and Low
cc_yr0 %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr0)) # Most in 1981 (27%), 2006 (20%)

cc_yr10 %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr10)) # Most  in 2011 (30%)

cc_yr20 %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr20)) # Most  in 2016 (36%), 1996 (25%)

cc_yr30 %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr30)) # Most  in 2016 (47%), 2006 (36%)

cc_yr40 %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr40)) # 2016 (100%)

# All
cc_yr0_all %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr0_all)) # Most in 1981 (23%)

cc_yr10_all %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr10_all)) # Most  in 2011 (20%)

cc_yr20_all %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr20_all)) # Most  in 2016 (25%), 1996 (22%)

cc_yr30_all %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr30_all)) # 2016 (47%), 2006(32%), 2011 (21%)

cc_yr40_all %>%
  group_by(Cen_yr_bf) %>%
  summarize( percent = 100 * n() / nrow(cc_yr40_all)) # 2016 (100%)
#####

##### Test - convert All to percentile? #####
#cc_yr40_per = cc_yr40 %>%
#  mutate(MedInc = percent_rank(MedInc)) # Works... for single column

