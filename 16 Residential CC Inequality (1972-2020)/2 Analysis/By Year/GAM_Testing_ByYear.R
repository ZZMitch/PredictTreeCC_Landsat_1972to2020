##### Adjust R2 / pvalues of GAM models if data is spatially autocorrelated - alters CC_Census_byYear plots #####

# Find effective sample size? 
# Subsampling? 

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

library(data.table)
library(mgcv) # GAM
#citation("mgcv")
library(ape) # Morans I
#citation("ape")

##### Bring in Data #####
### Base DA Data ###
da = read.csv("DA_data1.csv")

### Canopy Cover Data ###
cc_postdev = read.csv("cc_postdev.csv", check.names = FALSE) # CC by year (NA before transition)

### Census Data ###
census1971 = read.csv("Census Data/Census1971_Peel_final.csv")
census1981 = read.csv("Census Data/Census1981_Peel_final.csv")
census1986 = read.csv("Census Data/Census1986_Peel_final.csv")
census1991 = read.csv("Census Data/Census1991_Peel_final.csv")
census1996 = read.csv("Census Data/Census1996_Peel_final.csv")
census2001 = read.csv("Census Data/Census2001_Peel_final.csv")
census2006 = read.csv("Census Data/Census2006_Peel_final.csv")
census2011 = read.csv("Census Data/Census2011_Peel_final.csv") # Missing variables
census2016 = read.csv("Census Data/Census2016_Peel_final.csv")
#####

##### Add CC to Census tables #####
cc_census = setDT(cc_postdev)[DAUID %in% census1971$DAUID] # Reduce cc_postdev to just DAs within Census 1971
census1971$cc = cc_census$`1972` # Add census year CC (1972 in this case) to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1981$DAUID] # Reduce cc_postdev to just DAs within Census 1981
census1981$cc = cc_census$`1981` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1986$DAUID] # Reduce cc_postdev to just DAs within Census 1986
census1986$cc = cc_census$`1986` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1991$DAUID] # Reduce cc_postdev to just DAs within Census 1991
census1991$cc = cc_census$`1991` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1996$DAUID] # Reduce cc_postdev to just DAs within Census 1996
census1996$cc = cc_census$`1996` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2001$DAUID] # Reduce cc_postdev to just DAs within Census 2001
census2001$cc = cc_census$`2001` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2006$DAUID] # Reduce cc_postdev to just DAs within Census 2006
census2006$cc = cc_census$`2006` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2011$DAUID] # Reduce cc_postdev to just DAs within Census 2011
census2011$cc = cc_census$`2011` # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2016$DAUID] # Reduce cc_postdev to just DAs within Census 2016
census2016$cc = cc_census$`2016` # Add census year CC to census table for comparison
#####

##### Add Municipality to Census tables #####
mun_census = setDT(da)[DAUID %in% census1971$DAUID] # Reduce da to just DAs within Census 1971
census1971$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census1981$DAUID] # Reduce da to just DAs within Census 1981
census1981$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census1986$DAUID] # Reduce da to just DAs within Census 1986
census1986$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census1991$DAUID] # Reduce da to just DAs within Census 1991
census1991$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census1996$DAUID] # Reduce da to just DAs within Census 1996
census1996$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census2001$DAUID] # Reduce da to just DAs within Census 2001
census2001$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census2006$DAUID] # Reduce da to just DAs within Census 2006
census2006$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census2011$DAUID] # Reduce da to just DAs within Census 2011
census2011$mun = mun_census$Municipality 

mun_census = setDT(da)[DAUID %in% census2016$DAUID] # Reduce da to just DAs within Census 2016
census2016$mun = mun_census$Municipality 
#####

##### Add 2015 Inflation-corrected MedInc to Census tables #####
# See CorrectInflation Excel file for details
census1971$MedInci = census1971$MedInc * 6.209756
census1981$MedInci = census1981$MedInc * 2.847875
census1986$MedInci = census1986$MedInc * 2.01106
census1991$MedInci = census1991$MedInc * 1.619593
census1996$MedInci = census1996$MedInc * 1.451539
census2001$MedInci = census2001$MedInc * 1.33020
census2006$MedInci = census2006$MedInc * 1.184186
census2011$MedInci = census2011$MedInc
census2016$MedInci = census2016$MedInc
#####

##### Add Latitude  to Census tables #####
lat_census = setDT(da)[DAUID %in% census1971$DAUID] # Reduce da to just DAs within Census 1971
census1971$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census1981$DAUID] # Reduce da to just DAs within Census 1981
census1981$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census1986$DAUID] # Reduce da to just DAs within Census 1986
census1986$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census1991$DAUID] # Reduce da to just DAs within Census 1991
census1991$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census1996$DAUID] # Reduce da to just DAs within Census 1996
census1996$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census2001$DAUID] # Reduce da to just DAs within Census 2001
census2001$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census2006$DAUID] # Reduce da to just DAs within Census 2006
census2006$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census2011$DAUID] # Reduce da to just DAs within Census 2011
census2011$lat = lat_census$Latitude 

lat_census = setDT(da)[DAUID %in% census2016$DAUID] # Reduce da to just DAs within Census 2016
census2016$lat = lat_census$Latitude 
#####

##### Add Longitude  to Census tables #####
long_census = setDT(da)[DAUID %in% census1971$DAUID] # Reduce da to just DAs within Census 1971
census1971$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census1981$DAUID] # Reduce da to just DAs within Census 1981
census1981$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census1986$DAUID] # Reduce da to just DAs within Census 1986
census1986$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census1991$DAUID] # Reduce da to just DAs within Census 1991
census1991$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census1996$DAUID] # Reduce da to just DAs within Census 1996
census1996$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census2001$DAUID] # Reduce da to just DAs within Census 2001
census2001$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census2006$DAUID] # Reduce da to just DAs within Census 2006
census2006$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census2011$DAUID] # Reduce da to just DAs within Census 2011
census2011$long = long_census$Longitude 

long_census = setDT(da)[DAUID %in% census2016$DAUID] # Reduce da to just DAs within Census 2016
census2016$long = long_census$Longitude 
#####

##### Create Longform Data #####
census1971$CensusYear = 1971
census1981$CensusYear = 1981
census1986$CensusYear = 1986
census1991$CensusYear = 1991
census1996$CensusYear = 1996
census2001$CensusYear = 2001
census2006$CensusYear = 2006
census2011$CensusYear = 2011
census2016$CensusYear = 2016

census = rbind(census1971, census1981, census1986, census1991, census1996, census2001, census2006, census2011, census2016)
#####

##### Set PopDen above 20000 to NA #####
census1971$PopDen[census1971$PopDen >= 20000] = NA
census1981$PopDen[census1981$PopDen >= 20000] = NA
census1986$PopDen[census1986$PopDen >= 20000] = NA
census1991$PopDen[census1991$PopDen >= 20000] = NA
census1996$PopDen[census1996$PopDen >= 20000] = NA
census2001$PopDen[census2001$PopDen >= 20000] = NA
census2006$PopDen[census2006$PopDen >= 20000] = NA
census2011$PopDen[census2011$PopDen >= 20000] = NA
census2016$PopDen[census2016$PopDen >= 20000] = NA
#####

##### Create Distance Tables - for Moran's I #####
da71.dist = as.matrix(dist(cbind(census1971$long, census1971$lat)))
da71.dist = 1 / da71.dist
diag(da71.dist) <- 0

da81.dist = as.matrix(dist(cbind(census1981$long, census1981$lat)))
da81.dist = 1 / da81.dist
diag(da81.dist) <- 0

da86.dist = as.matrix(dist(cbind(census1986$long, census1986$lat)))
da86.dist = 1 / da86.dist
diag(da86.dist) <- 0

da91.dist = as.matrix(dist(cbind(census1991$long, census1991$lat)))
da91.dist = 1 / da91.dist
diag(da91.dist) <- 0

da96.dist = as.matrix(dist(cbind(census1996$long, census1996$lat)))
da96.dist = 1 / da96.dist
diag(da96.dist) <- 0

da01.dist = as.matrix(dist(cbind(census2001$long, census2001$lat)))
da01.dist = 1 / da01.dist
diag(da01.dist) <- 0

da06.dist = as.matrix(dist(cbind(census2006$long, census2006$lat)))
da06.dist = 1 / da06.dist
diag(da06.dist) <- 0

da11.dist = as.matrix(dist(cbind(census2011$long, census2011$lat)))
da11.dist = 1 / da11.dist
diag(da11.dist) <- 0

da16.dist = as.matrix(dist(cbind(census2016$long, census2016$lat)))
da16.dist = 1 / da16.dist
diag(da16.dist) <- 0

# Caledon
da91.dist.c = as.matrix(dist(cbind(subset(census1991$long, census1991$mun == "Caledon"), subset(census1991$lat, census1991$mun == "Caledon"))))
da91.dist.c = 1 / da91.dist.c
diag(da91.dist.c) <- 0

da96.dist.c = as.matrix(dist(cbind(subset(census1996$long, census1996$mun == "Caledon"), subset(census1996$lat, census1996$mun == "Caledon"))))
da96.dist.c = 1 / da96.dist.c
diag(da96.dist.c) <- 0

da01.dist.c = as.matrix(dist(cbind(subset(census2001$long, census2001$mun == "Caledon"), subset(census2001$lat, census2001$mun == "Caledon"))))
da01.dist.c = 1 / da01.dist.c
diag(da01.dist.c) <- 0

da06.dist.c = as.matrix(dist(cbind(subset(census2006$long, census2006$mun == "Caledon"), subset(census2006$lat, census2006$mun == "Caledon"))))
da06.dist.c = 1 / da06.dist.c
diag(da06.dist.c) <- 0

da11.dist.c = as.matrix(dist(cbind(subset(census2011$long, census2011$mun == "Caledon"), subset(census2011$lat, census2011$mun == "Caledon"))))
da11.dist.c = 1 / da11.dist.c
diag(da11.dist.c) <- 0

da16.dist.c = as.matrix(dist(cbind(subset(census2016$long, census2016$mun == "Caledon"), subset(census2016$lat, census2016$mun == "Caledon"))))
da16.dist.c = 1 / da16.dist.c
diag(da16.dist.c) <- 0
#####

# Initial Inputs
data = census2016
col = which(colnames(census2016) == "pMin")
dist = da16.dist
div = 3.5 # How big to make % sample

k = 50
family = "gaussian"

##### GAM TESTING #####
hist(data[,col]) # Mostly normal, gaussian fine

gam = gam(cc ~ s(data[,col], bs = "cs", k = k), family = family, data = data, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1) 
summary(gam) 
#gam.check(gam) # Should see convergence, p should not be significant
plot(gam)

Moran.I(data[,col], dist, na.rm = TRUE) # If significant - random sample

# Random Sample (X%) + Spatial Autocorrelation (Moran's I)
#sample = data[sample(nrow(data), nrow(data) / div), ]

#s.dist = as.matrix(dist(cbind(sample$long, sample$lat)))
#s.dist = 1 / s.dist
#diag(s.dist) <- 0

#Moran.I(sample[,col], s.dist, na.rm = TRUE) 

# If not significant, GAM model
#s.gam = gam(cc ~ s(sample[,col], bs = "cs", k = k), family = family, data = sample, weights = Pop, method = "REML")
#summary(s.gam)
#plot(s.gam)
#####

# Automate!
iter = 1000

s.tbl = as.data.frame(matrix(nrow = iter, ncol = 1))
colnames(s.tbl) = c("Iteration")
s.tbl$Iteration = 1:iter

for (i in 1:nrow(s.tbl)) {
  sample = data[sample(nrow(data), nrow(data) / div), ]
  s.dist = as.matrix(dist(cbind(sample$long, sample$lat)))
  s.dist = 1 / s.dist
  diag(s.dist) <- 0
  moran = Moran.I(sample[,col], s.dist, na.rm = TRUE) 
  s.tbl$moran_i[i] = moran$observed
  s.tbl$moran_p[i] = moran$p.value
  
  s.gam = gam(cc ~ s(sample[,col], bs = "cs", k = k), family = family, data = sample, weights = Pop, method = "REML") 
  s.tbl$gam_n[i] = summary(s.gam)$n
  s.tbl$gam_r2[i] = summary(s.gam)$r.sq
  s.tbl$gam_dev[i] = summary(s.gam)$dev
  s.tbl$gam_p[i] = summary(s.gam)$s.table[,4]
}

# How many iterations have not significant Moran's I?
nrow(subset(s.tbl, moran_p > 0.05)) # ~30s% the sweet spot (less than that - risk skewing to a certain subset of sample, more than that - removing too much data?) 

s.tbl = subset(s.tbl, moran_p > 0.05)
mean(s.tbl$gam_n)

mean(s.tbl$gam_r2)
sd(s.tbl$gam_r2)
hist(s.tbl$gam_r2)

mean(s.tbl$gam_dev)
sd(s.tbl$gam_dev)
hist(s.tbl$gam_dev)

mean(s.tbl$gam_p)
nrow(subset(s.tbl, gam_p > 0.05))
