##### This code compares CC and census data using multivarite GAM #####

setwd("C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/16 Residential CC Inequality (1972-2020)")

library(data.table)
library(mgcv)

##### Bring in Data #####
### Base DA Data ###
da = read.csv("C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/1 Study Area/Data/Dissemination Areas/Residential ROP/DA_data1.csv")

### Canopy Cover Data ###
cc_postdev = read.csv("C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/15 PostDevelopment Residential CC (1972-2020)/cc_postdev2.csv")

### Census Data ###
census1971 = read.csv("1 Census Data/4 Final/Census1971_Peel_final.csv")
census1981 = read.csv("1 Census Data/4 Final/Census1981_Peel_final.csv")
census1986 = read.csv("1 Census Data/4 Final/Census1986_Peel_final.csv")
census1991 = read.csv("1 Census Data/4 Final/Census1991_Peel_final.csv")
census1996 = read.csv("1 Census Data/4 Final/Census1996_Peel_final.csv")
census2001 = read.csv("1 Census Data/4 Final/Census2001_Peel_final.csv")
census2006 = read.csv("1 Census Data/4 Final/Census2006_Peel_final.csv")
census2011 = read.csv("1 Census Data/4 Final/Census2011_Peel_final.csv") # Missing variables
census2016 = read.csv("1 Census Data/4 Final/Census2016_Peel_final.csv")

##### Add CC to Census tables #####
cc_census = setDT(cc_postdev)[DAUID %in% census1971$DAUID] # Reduce cc_postdev to just DAs within Census 1971
census1971$cc = cc_census$Y1972 # Add census year CC (1972 in this case) to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1981$DAUID] # Reduce cc_postdev to just DAs within Census 1981
census1981$cc = cc_census$Y1981 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1986$DAUID] # Reduce cc_postdev to just DAs within Census 1986
census1986$cc = cc_census$Y1986 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1991$DAUID] # Reduce cc_postdev to just DAs within Census 1991
census1991$cc = cc_census$Y1991 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census1996$DAUID] # Reduce cc_postdev to just DAs within Census 1996
census1996$cc = cc_census$Y1996 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2001$DAUID] # Reduce cc_postdev to just DAs within Census 2001
census2001$cc = cc_census$Y2001 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2006$DAUID] # Reduce cc_postdev to just DAs within Census 2006
census2006$cc = cc_census$Y2006 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2011$DAUID] # Reduce cc_postdev to just DAs within Census 2011
census2011$cc = cc_census$Y2011 # Add census year CC to census table for comparison

cc_census = setDT(cc_postdev)[DAUID %in% census2016$DAUID] # Reduce cc_postdev to just DAs within Census 2016
census2016$cc = cc_census$Y2016 # Add census year CC to census table for comparison

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

##### Set PopDen above 20000 to high value #####
census1971$PopDen[census1971$PopDen >= 20000] = sample(15000:20000,100)
census1981$PopDen[census1981$PopDen >= 20000] = sample(15000:20000,100)
census1986$PopDen[census1986$PopDen >= 20000] = sample(15000:20000,100)
census1991$PopDen[census1991$PopDen >= 20000] = sample(15000:20000,100)
census1996$PopDen[census1996$PopDen >= 20000] = sample(15000:20000,100)
census2001$PopDen[census2001$PopDen >= 20000] = sample(15000:20000,100)
census2006$PopDen[census2006$PopDen >= 20000] = sample(15000:20000,100)
census2011$PopDen[census2011$PopDen >= 20000] = sample(15000:20000,100)
census2016$PopDen[census2016$PopDen >= 20000] = sample(15000:20000,100)
#####

##### Set MedInc above 250000 to high value #####
census1971$MedInci[census1971$MedInci >= 250000 & !is.na(census1971$MedInci)] = sample(225000:250000,100)
census1981$MedInci[census1981$MedInci >= 250000 & !is.na(census1981$MedInci)] = sample(225000:250000,100)
census1986$MedInci[census1986$MedInci >= 250000 & !is.na(census1986$MedInci)] = sample(225000:250000,100)
census1991$MedInci[census1991$MedInci >= 250000 & !is.na(census1991$MedInci)] = sample(225000:250000,100)
census1996$MedInci[census1996$MedInci >= 250000 & !is.na(census1996$MedInci)] = sample(225000:250000,100)
census2001$MedInci[census2001$MedInci >= 250000 & !is.na(census2001$MedInci)] = sample(225000:250000,100)
census2006$MedInci[census2006$MedInci >= 250000 & !is.na(census2006$MedInci)] = sample(225000:250000,100)
census2016$MedInci[census2016$MedInci >= 250000 & !is.na(census2016$MedInci)] = sample(225000:250000,100)
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

##### Multivariate GAMs #####

# Population Density (PopDen)
# Detached % (pDet)
# Attached % (pAtt)
# Apartment % (pApt)
# Most common dwelling type (MC_DwTy) - Model may not accept
# Median Household Income (MedInc)
# Visible Minorities % (pMin)
# Origins in Northwest Europe % (pNWEur) - Caledon only?

# For GAMs: default k
k = 50

# 1971 
# Create GAM model (50 k is too much with only 206 samples)
m71.gam = gam(cc ~ s(PopDen, bs = "cs") + 
                   s(pDet, bs = "cs") + 
                   #s(pAtt, bs = "cs") + 
                   #s(pApt, bs = "cs") + # low p, couldn't get k high enough to keep
                   s(MedInc, bs = "cs") + 
                   s(pMin, bs = "cs"), 
              data = census1971, weights = Pop, method = "REML")
summary(m71.gam) # R2 = 0.51***
gam.check(m71.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)
#plot(m71.gam)

# 1981
# Create GAM model
m81.gam = gam(cc ~ s(PopDen, bs = "cs", k = 50) + 
                   s(pDet, bs = "cs", k = 200) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 100) + 
                   s(pMin, bs = "cs", k = 50), 
              data = census1981, weights = Pop, method = "REML")
summary(m81.gam) # R2 = 0.58***
gam.check(m81.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)

# 1986
# Create GAM model
m86.gam = gam(cc ~ s(PopDen, bs = "cs", k = 200) + 
                   s(pDet, bs = "cs", k = 300) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 50) + 
                   s(pMin, bs = "cs", k = 50), 
              data = census1986, weights = Pop, method = "REML")
summary(m86.gam) # R2 = 0.61***
gam.check(m86.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)

# 1991
# Create GAM model
m91.gam = gam(cc ~ s(PopDen, bs = "cs", k = 50) + 
                   s(pDet, bs = "cs", k = 200) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 50) + 
                   s(pMin, bs = "cs", k = 50), 
              data = census1991, weights = Pop, method = "REML")
summary(m91.gam) # R2 = 0.61***
gam.check(m91.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)

# 1996
# Create GAM model
m96.gam = gam(cc ~ s(PopDen, bs = "cs", k = 50) + 
                   s(pDet, bs = "cs", k = 200) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 50) + 
                   s(pMin, bs = "cs", k = 50), 
              data = census1996, weights = Pop, method = "REML")
summary(m96.gam) # R2 = 0.56***
gam.check(m96.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)

# 2001
# Create GAM model
m01.gam = gam(cc ~ s(PopDen, bs = "cs", k = 50) + 
                   #s(pDet, bs = "cs", k = 50) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 50) + 
                   s(pMin, bs = "cs", k = 400), 
              data = census2001, weights = Pop, method = "REML")
summary(m01.gam) # R2 = 0.53***
gam.check(m01.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)

# 2006
# Create GAM model
m06.gam = gam(cc ~ s(PopDen, bs = "cs", k = 50) + 
                   s(pDet, bs = "cs", k = 200) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 50) + 
                   s(pMin, bs = "cs", k = 50), 
              data = census2006, weights = Pop, method = "REML")
summary(m06.gam) # R2 = 0.66***
gam.check(m06.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)

# 2011 - Missing data on pMin and MedInc

# 2016
# Create GAM model
m16.gam = gam(cc ~ s(PopDen, bs = "cs", k = 50) + 
                   s(pDet, bs = "cs", k = 200) + 
                   #s(pAtt, bs = "cs", k = 50) + 
                   #s(pApt, bs = "cs", k = 50) +
                   s(MedInc, bs = "cs", k = 50) + 
                   s(pMin, bs = "cs", k = 50), 
              data = census2016, weights = Pop, method = "REML")
summary(m16.gam) # R2 = 0.66***
gam.check(m16.gam) # Should see convergence, p should not be significant
# Manually set K higher if p is low (< 0.1)
