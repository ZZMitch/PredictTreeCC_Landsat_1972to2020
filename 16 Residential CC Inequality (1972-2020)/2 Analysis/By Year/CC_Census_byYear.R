##### This code compares CC data through time with Census data through time across Peel DAs #####

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

library(data.table)
library(mgcv)
library(ggplot2)
library(patchwork)
library(weights)
library(ggpubr)
library(scales) # Remove scientific notation on some axis labels
#library(spatstat) # weighted.median
#library(geoR) # variogram
#library(nlme) # spatial autocorrelation stuff
#library(ape) # morans I

pt_sz = 2
pt_sz_sm = 1
big_ln = 2 # 2 for Peel, 1.5 for by Municipality
sm_ln = 1
eq_sz = 3
eq_sz_sm = 2.5
axt_sz = 10
axn_sz = 8
anno_sz = 6
pt_mn = 84 # Min DA pop
pt_mx = 11174 # Max DA pop
pt_mn1 = 0.5 # size
pt_mx1 = 6 # size
alpha = 0.2 # 0.2 for Peel, 0.1 for by Municipality

# Municipality colors
# Mississauga: blue
# Brampton: red3
# Caledon: darkgreen

# For GAMs: default k
k = 50

# For combined GAM plots: line colors
colors = c("1971" = "black", "1981" = "sienna", "1986" = "purple4", "1991" = "blue", "1996" = "steelblue2", "2001" = "green4", 
           "2006" = "gold2", "2011" = "darkorange2", "2016" = "red3")
colors1 = c("1971" = "black", "1981" = "sienna", "1986" = "purple4", "1991" = "blue", "1996" = "steelblue2", "2001" = "green4", 
           "2006" = "gold2", "2016" = "red3") # No 2011
colors2 = c("1991" = "blue", "1996" = "steelblue2", "2001" = "green4", "2006" = "gold2", "2016" = "red3") # Caledon, no 2011

# Test sample size... examples - should be 10+ DAs & 7500+ population
# nrow(subset(census1971, MC_DwTy == "Apartment" & mun == "Brampton")) # Number of DAs
# sum(subset(census1971$Pop, census1971$MC_DwTy == "Apartment" & census1971$mun == "Brampton")) Population of DAs


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

### Linear model ###
# 1971
popden71.lm = lm(cc ~ PopDen, census1971)
summary(popden71.lm) # R2 = 0.17
plot(cc ~ PopDen, census1971)
abline(popden71.lm)

# 1981
popden81.lm = lm(cc ~ PopDen, census1981)
summary(popden81.lm) # R2 = 0.17
plot(cc ~ PopDen, census1981)
abline(popden81.lm)

# 1986
popden86.lm = lm(cc ~ PopDen, census1986)
summary(popden86.lm) # R2 = 0.17
plot(cc ~ PopDen, census1986)
abline(popden86.lm)

# 1991
popden91.lm = lm(cc ~ PopDen, census1991)
summary(popden91.lm) # R2 = 0.09
plot(cc ~ PopDen, census1991)
abline(popden91.lm)

# 1996
popden96.lm = lm(cc ~ PopDen, census1996)
summary(popden96.lm) # R2 = 0.17
plot(cc ~ PopDen, census1996)
abline(popden96.lm)

# 2001
popden01.lm = lm(cc ~ PopDen, census2001)
summary(popden01.lm) # R2 = 0.21
plot(cc ~ PopDen, census2001)
abline(popden01.lm)

# 2006
popden06.lm = lm(cc ~ PopDen, census2006)
summary(popden06.lm) # R2 = 0.33
plot(cc ~ PopDen, census2006)
abline(popden06.lm)

# 2011
popden11.lm = lm(cc ~ PopDen, census2011)
summary(popden11.lm) # R2 = 0.38
plot(cc ~ PopDen, census2011)
abline(popden11.lm)

# 2016
popden16.lm = lm(cc ~ PopDen, census2016)
summary(popden16.lm) # R2 = 0.42
plot(cc ~ PopDen, census2016)
abline(popden16.lm)

### Weighted GAM Figures ###
xmin = 0
xmax = 20000
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
popden71.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden71.gam) # R2 = 0.25***
gam.check(popden71.gam) # Should see convergence, p should not be significant
plot(popden71.gam)

popden71_pred = data.frame(PopDen = census1971$PopDen, cc = census1971$cc, Pop = census1971$Pop, pred = predict(popden71.gam, newdata = census1971))

# Plot
popden71.p = ggplot(popden71_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.24***", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden71.p

# 1981
# Create GAM model
popden81.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(popden81.gam) # R2 = 0.39***
gam.check(popden81.gam) # Should see convergence, p should not be significant
plot(popden81.gam)

popden81_pred = data.frame(PopDen = census1981$PopDen, cc = census1981$cc, Pop = census1981$Pop, pred = predict(popden81.gam, newdata = census1981))

# Plot
popden81.p = ggplot(popden81_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden81.p

# 1986
# Create GAM model
popden86.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(popden86.gam) # R2 = 0.41***
gam.check(popden86.gam) # Should see convergence, p should not be significant
plot(popden86.gam)

popden86_pred = data.frame(PopDen = census1986$PopDen, cc = census1986$cc, Pop = census1986$Pop, pred = predict(popden86.gam, newdata = census1986))

# Plot
popden86.p = ggplot(popden86_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden86.p

# 1991
# Create GAM model
popden91.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(popden91.gam) # R2 = 0.44***
gam.check(popden91.gam) # Should see convergence, p should not be significant
plot(popden91.gam)

popden91_pred = data.frame(PopDen = census1991$PopDen, cc = census1991$cc, Pop = census1991$Pop, pred = predict(popden91.gam, newdata = census1991))

# Plot
popden91.p = ggplot(popden91_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden91.p

# 1996
# Create GAM model
popden96.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(popden96.gam) # R2 = 0.39***
gam.check(popden96.gam) # Should see convergence, p should not be significant
plot(popden96.gam)

popden96_pred = data.frame(PopDen = census1996$PopDen, cc = census1996$cc, Pop = census1996$Pop, pred = predict(popden96.gam, newdata = census1996))

# Plot
popden96.p = ggplot(popden96_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.38***", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden96.p

# 2001
# Create GAM model
popden01.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(popden01.gam) # R2 = 0.41***
gam.check(popden01.gam) # Should see convergence, p should not be significant
plot(popden01.gam)

popden01_pred = data.frame(PopDen = census2001$PopDen, cc = census2001$cc, Pop = census2001$Pop, pred = predict(popden01.gam, newdata = census2001))

# Plot
popden01.p = ggplot(popden01_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.36***", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden01.p

# 2006
# Create GAM model
popden06.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(popden06.gam) # R2 = 0.48***
gam.check(popden06.gam) # Should see convergence, p should not be significant
plot(popden06.gam)

popden06_pred = data.frame(PopDen = census2006$PopDen, cc = census2006$cc, Pop = census2006$Pop, pred = predict(popden06.gam, newdata = census2006))

# Plot
popden06.p = ggplot(popden06_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.45***", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden06.p

# 2011
# Create GAM model
popden11.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census2011, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(popden11.gam) # R2 = 0.54***
gam.check(popden11.gam) # Should see convergence, p should not be significant
plot(popden11.gam)

popden11_pred = data.frame(PopDen = census2011$PopDen, cc = census2011$cc, Pop = census2011$Pop, pred = predict(popden11.gam, newdata = census2011))

# Plot
popden11.p = ggplot(popden11_pred, aes(x = PopDen, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = "Residential Population Density (People per km²)", limits = c(xmin, xmax), expand = c(0,0),
                     breaks = c(5000, 10000, 15000, 20000)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.49***", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden11.p

# 2016
# Create GAM model
popden16.gam = gam(cc ~ s(PopDen, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(popden16.gam) # R2 = 0.56***
gam.check(popden16.gam) # Should see convergence, p should not be significant
plot(popden16.gam)

popden16_pred = data.frame(PopDen = census2016$PopDen, cc = census2016$cc, Pop = census2016$Pop, pred = predict(popden16.gam, newdata = census2016))

# Plot
popden16.p = ggplot(popden16_pred, aes(x = PopDen, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(5000, 10000, 15000, 20000)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.53***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden16.p

tiff("PopDen_CC_Peel_71to16_1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(popden71.p | popden81.p | popden86.p) /
  (popden91.p | popden96.p | popden01.p) /
  (popden06.p | popden11.p | popden16.p)
dev.off()
###

# Peel - Combined Figure
# Find weighted mean location (closest real point) on predicted line
popden_mn_row71 = first(which(abs(popden71_pred$PopDen - weighted.mean(x = popden71_pred$PopDen, w = popden71_pred$Pop, na.rm = TRUE)) == 
                   min(abs(popden71_pred$PopDen - weighted.mean(x = popden71_pred$PopDen, w = popden71_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden71_pred[popden_mn_row71,]

popden_mn_row81 = first(which(abs(popden81_pred$PopDen - weighted.mean(x = popden81_pred$PopDen, w = popden81_pred$Pop, na.rm = TRUE)) == 
                   min(abs(popden81_pred$PopDen - weighted.mean(x = popden81_pred$PopDen, w = popden81_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden81_pred[popden_mn_row81,]

popden_mn_row86 = first(which(abs(popden86_pred$PopDen - weighted.mean(x = popden86_pred$PopDen, w = popden86_pred$Pop, na.rm = TRUE)) == 
                   min(abs(popden86_pred$PopDen - weighted.mean(x = popden86_pred$PopDen, w = popden86_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden86_pred[popden_mn_row86,]

popden_mn_row91 = first(which(abs(popden91_pred$PopDen - weighted.mean(x = popden91_pred$PopDen, w = popden91_pred$Pop, na.rm = TRUE)) == 
                   min(abs(popden91_pred$PopDen - weighted.mean(x = popden91_pred$PopDen, w = popden91_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden91_pred[popden_mn_row91,]

popden_mn_row96 = first(which(abs(popden96_pred$PopDen - weighted.mean(x = popden96_pred$PopDen, w = popden96_pred$Pop, na.rm = TRUE)) == 
                         min(abs(popden96_pred$PopDen - weighted.mean(x = popden96_pred$PopDen, w = popden96_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden96_pred[popden_mn_row96,]

popden_mn_row01 = first(which(abs(popden01_pred$PopDen - weighted.mean(x = popden01_pred$PopDen, w = popden01_pred$Pop, na.rm = TRUE)) == 
                         min(abs(popden01_pred$PopDen - weighted.mean(x = popden01_pred$PopDen, w = popden01_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden01_pred[popden_mn_row01,]

popden_mn_row06 = first(which(abs(popden06_pred$PopDen - weighted.mean(x = popden06_pred$PopDen, w = popden06_pred$Pop, na.rm = TRUE)) == 
                         min(abs(popden06_pred$PopDen - weighted.mean(x = popden06_pred$PopDen, w = popden06_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden06_pred[popden_mn_row06,]

popden_mn_row11 = first(which(abs(popden11_pred$PopDen - weighted.mean(x = popden11_pred$PopDen, w = popden11_pred$Pop, na.rm = TRUE)) == 
                         min(abs(popden11_pred$PopDen - weighted.mean(x = popden11_pred$PopDen, w = popden11_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden11_pred[popden_mn_row11,]

popden_mn_row16 = first(which(abs(popden16_pred$PopDen - weighted.mean(x = popden16_pred$PopDen, w = popden16_pred$Pop, na.rm = TRUE)) == 
                         min(abs(popden16_pred$PopDen - weighted.mean(x = popden16_pred$PopDen, w = popden16_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden16_pred[popden_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

# Plot
xmin = 0
xmax = 20000
ymin = 0
ymax = 45

popden_com.p = ggplot() +
  geom_line(aes(x = PopDen, y = pred, col = "1971"), data = popden71_pred) + 
  geom_point(aes(x = popden71_pred$PopDen[popden_mn_row71], y = popden71_pred$pred[popden_mn_row71]), color = "black") + 
  geom_line(aes(x = PopDen, y = pred, col = "1981"), data = popden81_pred) + 
  geom_point(aes(x = popden81_pred$PopDen[popden_mn_row81], y = popden81_pred$pred[popden_mn_row81]), color = "sienna") + 
  geom_line(aes(x = PopDen, y = pred, col = "1986"), data = popden86_pred) + 
  geom_point(aes(x = popden86_pred$PopDen[popden_mn_row86], y = popden86_pred$pred[popden_mn_row86]), color = "purple4") + 
  geom_line(aes(x = PopDen, y = pred, col = "1991"), data = popden91_pred) + 
  geom_point(aes(x = popden91_pred$PopDen[popden_mn_row91], y = popden91_pred$pred[popden_mn_row91]), color = "blue") + 
  geom_line(aes(x = PopDen, y = pred, col = "1996"), data = popden96_pred) + 
  geom_point(aes(x = popden96_pred$PopDen[popden_mn_row96], y = popden96_pred$pred[popden_mn_row96]), color = "steelblue2") + 
  geom_line(aes(x = PopDen, y = pred, col = "2001"), data = popden01_pred) + 
  geom_point(aes(x = popden01_pred$PopDen[popden_mn_row01], y = popden01_pred$pred[popden_mn_row01]), color = "green4") + 
  geom_line(aes(x = PopDen, y = pred, col = "2006"), data = popden06_pred) + 
  geom_point(aes(x = popden06_pred$PopDen[popden_mn_row06], y = popden06_pred$pred[popden_mn_row06]), color = "gold2") + 
  geom_line(aes(x = PopDen, y = pred, col = "2011"), data = popden11_pred) + 
  geom_point(aes(x = popden11_pred$PopDen[popden_mn_row11], y = popden11_pred$pred[popden_mn_row11]), color = "darkorange2") +
  geom_line(aes(x = PopDen, y = pred, col = "2016"), data = popden16_pred) +
  geom_point(aes(x = popden16_pred$PopDen[popden_mn_row16], y = popden16_pred$pred[popden_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 15000, y = 42, label = "R² = 0.24***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 15000, y = 40, label = "R² = 0.39***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 15000, y = 38, label = "R² = 0.41***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 15000, y = 36, label = "R² = 0.44***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 15000, y = 34, label = "R² = 0.38***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 15000, y = 32, label = "R² = 0.36***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 15000, y = 30, label = "R² = 0.45***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 15000, y = 28, label = "R² = 0.49***", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 15000, y = 26, label = "R² = 0.53***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "Residential Population Density (People per km²)", expand = c(0,0)) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.4, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
popden_com.p

#tiff("PopDen_CC_Peel_combined1.tif", units = "cm", width = 10, height = 10, res = 300)
popden_com.p
#dev.off()

# By Municipality - Separate Figures
# 1971
# Create GAM models
popden71.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden71.gam_m) # R2 = 0.22***
gam.check(popden71.gam_m) # Should see convergence, p should not be significant
plot(popden71.gam_m)

popden71_pred_m = data.frame(PopDen = subset(census1971$PopDen, census1971$mun == "Mississauga"), 
                             cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                             Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                             pred = predict(popden71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

popden71.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1971, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden71.gam_b) # R2 = 0.11*
gam.check(popden71.gam_b) # Should see convergence, p should not be significant
plot(popden71.gam_b)

popden71_pred_b = data.frame(PopDen = subset(census1971$PopDen, census1971$mun == "Brampton"), 
                             cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                             Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                             pred = predict(popden71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
popden71_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = PopDen, y = pred), data = popden71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.11*   ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden71_mun.p

# 1981
# Create GAM models
popden81.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden81.gam_m) # R2 = 0.45***
gam.check(popden81.gam_m) # Should see convergence, p should not be significant
plot(popden81.gam_m)

popden81_pred_m = data.frame(PopDen = subset(census1981$PopDen, census1981$mun == "Mississauga"), 
                             cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                             Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                             pred = predict(popden81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

popden81.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden81.gam_b) # R2 = 0.25***
gam.check(popden81.gam_b) # Should see convergence, p should not be significant
plot(popden81.gam_b)

popden81_pred_b = data.frame(PopDen = subset(census1981$PopDen, census1981$mun == "Brampton"), 
                             cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                             Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                             pred = predict(popden81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
popden81_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = PopDen, y = pred), data = popden81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.45***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.25***", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden81_mun.p

# 1986
# Create GAM models
popden86.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden86.gam_m) # R2 = 0.48***
gam.check(popden86.gam_m) # Should see convergence, p should not be significant
plot(popden86.gam_m)

popden86_pred_m = data.frame(PopDen = subset(census1986$PopDen, census1986$mun == "Mississauga"), 
                             cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                             Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                             pred = predict(popden86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

popden86.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden86.gam_b) # R2 = 0.27***
gam.check(popden86.gam_b) # Should see convergence, p should not be significant
plot(popden86.gam_b)

popden86_pred_b = data.frame(PopDen = subset(census1986$PopDen, census1986$mun == "Brampton"), 
                             cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                             Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                             pred = predict(popden86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
popden86_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = PopDen, y = pred), data = popden86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.27***", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden86_mun.p

# 1991
# Create GAM models
popden91.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden91.gam_m) # R2 = 0.44***
gam.check(popden91.gam_m) # Should see convergence, p should not be significant
plot(popden91.gam_m)

popden91_pred_m = data.frame(PopDen = subset(census1991$PopDen, census1991$mun == "Mississauga"), 
                             cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                             pred = predict(popden91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

popden91.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden91.gam_b) # R2 = 0.33***
gam.check(popden91.gam_b) # Should see convergence, p should not be significant
plot(popden91.gam_b)

popden91_pred_b = data.frame(PopDen = subset(census1991$PopDen, census1991$mun == "Brampton"), 
                             cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                             pred = predict(popden91.gam_b, newdata = subset(census1991, mun == "Brampton")))

popden91.gam_c = gam(cc ~ s(PopDen, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden91.gam_c) # R2 = 0.71***
gam.check(popden91.gam_c) # Should see convergence, p should not be significant
plot(popden91.gam_c)

popden91_pred_c = data.frame(PopDen = subset(census1991$PopDen, census1991$mun == "Caledon"), 
                             cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                             pred = predict(popden91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
popden91_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = PopDen, y = pred), data = popden91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.43***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.33***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 15000, y = 41, label = "R² = 0.71***", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden91_mun.p

# 1996
# Create GAM models
popden96.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden96.gam_m) # R2 = 0.43***
gam.check(popden96.gam_m) # Should see convergence, p should not be significant
plot(popden96.gam_m)

popden96_pred_m = data.frame(PopDen = subset(census1996$PopDen, census1996$mun == "Mississauga"), 
                             cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                             pred = predict(popden96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

popden96.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden96.gam_b) # R2 = 0.20***
gam.check(popden96.gam_b) # Should see convergence, p should not be significant
plot(popden96.gam_b)

popden96_pred_b = data.frame(PopDen = subset(census1996$PopDen, census1996$mun == "Brampton"), 
                             cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                             pred = predict(popden96.gam_b, newdata = subset(census1996, mun == "Brampton")))

popden96.gam_c = gam(cc ~ s(PopDen, bs = "cs", k =  33), data = subset(census1996, mun == "Caledon"), # k = 33 (# of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden96.gam_c) # R2 = 0.34***
gam.check(popden96.gam_c) # Should see convergence, p should not be significant
plot(popden96.gam_c)

popden96_pred_c = data.frame(PopDen = subset(census1996$PopDen, census1996$mun == "Caledon"), 
                             cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                             pred = predict(popden96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
popden96_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = PopDen, y = pred), data = popden96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.43***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.20***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 15000, y = 41, label = "R² = 0.34***", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden96_mun.p

# 2001
# Create GAM models
popden01.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k * 8), data = subset(census2001, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden01.gam_m) # R2 = 0.47***
gam.check(popden01.gam_m) # Should see convergence, p should not be significant
plot(popden01.gam_m)

popden01_pred_m = data.frame(PopDen = subset(census2001$PopDen, census2001$mun == "Mississauga"), 
                             cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                             pred = predict(popden01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

popden01.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden01.gam_b) # R2 = 0.24***
gam.check(popden01.gam_b) # Should see convergence, p should not be significant
plot(popden01.gam_b)

popden01_pred_b = data.frame(PopDen = subset(census2001$PopDen, census2001$mun == "Brampton"), 
                             cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                             pred = predict(popden01.gam_b, newdata = subset(census2001, mun == "Brampton")))

popden01.gam_c = gam(cc ~ s(PopDen, bs = "cs", k = 40), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden01.gam_c) # R2 = 0.64***
gam.check(popden01.gam_c) # Should see convergence, p should not be significant
plot(popden01.gam_c)

popden01_pred_c = data.frame(PopDen = subset(census2001$PopDen, census2001$mun == "Caledon"), 
                             cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                             pred = predict(popden01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
popden01_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = PopDen, y = pred), data = popden01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.47***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.24***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 15000, y = 41, label = "R² = 0.64***", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
popden01_mun.p

# 2006
# Create GAM models
popden06.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden06.gam_m) # R2 = 0.56***
gam.check(popden06.gam_m) # Should see convergence, p should not be significant
plot(popden06.gam_m)

popden06_pred_m = data.frame(PopDen = subset(census2006$PopDen, census2006$mun == "Mississauga"), 
                             cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                             pred = predict(popden06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

popden06.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden06.gam_b) # R2 = 0.26***
gam.check(popden06.gam_b) # Should see convergence, p should not be significant
plot(popden06.gam_b)

popden06_pred_b = data.frame(PopDen = subset(census2006$PopDen, census2006$mun == "Brampton"), 
                             cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                             pred = predict(popden06.gam_b, newdata = subset(census2006, mun == "Brampton")))

popden06.gam_c = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2006, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden06.gam_c) # R2 = 0.87***
gam.check(popden06.gam_c) # Should see convergence, p should not be significant
plot(popden06.gam_c)

popden06_pred_c = data.frame(PopDen = subset(census2006$PopDen, census2006$mun == "Caledon"), 
                             cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                             pred = predict(popden06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
popden06_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = PopDen, y = pred), data = popden06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.26***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 15000, y = 41, label = "R² = 0.87***", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden06_mun.p

# 2011
# Create GAM models
popden11.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2011, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden11.gam_m) # R2 = 0.62***
gam.check(popden11.gam_m) # Should see convergence, p should not be significant
plot(popden11.gam_m)

popden11_pred_m = data.frame(PopDen = subset(census2011$PopDen, census2011$mun == "Mississauga"), 
                             cc = subset(census2011$cc, census2011$mun == "Mississauga"), 
                             Pop = subset(census2011$Pop, census2011$mun == "Mississauga"), 
                             pred = predict(popden11.gam_m, newdata = subset(census2011, mun == "Mississauga")))

popden11.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2011, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden11.gam_b) # R2 = 0.35***
gam.check(popden11.gam_b) # Should see convergence, p should not be significant
plot(popden11.gam_b)

popden11_pred_b = data.frame(PopDen = subset(census2011$PopDen, census2011$mun == "Brampton"), 
                             cc = subset(census2011$cc, census2011$mun == "Brampton"), 
                             Pop = subset(census2011$Pop, census2011$mun == "Brampton"), 
                             pred = predict(popden11.gam_b, newdata = subset(census2011, mun == "Brampton")))

popden11.gam_c = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2011, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden11.gam_c) # R2 = 0.85***
gam.check(popden11.gam_c) # Should see convergence, p should not be significant
plot(popden11.gam_c)

popden11_pred_c = data.frame(PopDen = subset(census2011$PopDen, census2011$mun == "Caledon"), 
                             cc = subset(census2011$cc, census2011$mun == "Caledon"), 
                             Pop = subset(census2011$Pop, census2011$mun == "Caledon"), 
                             pred = predict(popden11.gam_c, newdata = subset(census2011, mun == "Caledon")))

# Plot
popden11_mun.p = ggplot() +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden11_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden11_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden11_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = PopDen, y = pred), data = popden11_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden11_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden11_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden11_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = "Residential Population Density (People per km²)", limits = c(xmin, xmax), expand = c(0,0),
                     breaks = c(5000, 10000, 15000, 20000)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.54***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.62***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.35***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 15000, y = 41, label = "R² = 0.85***", size = eq_sz, col = "darkgreen") +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden11_mun.p

# 2016
# Create GAM models
popden16.gam_m = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden16.gam_m) # R2 = 0.64***
gam.check(popden16.gam_m) # Should see convergence, p should not be significant
plot(popden16.gam_m)

popden16_pred_m = data.frame(PopDen = subset(census2016$PopDen, census2016$mun == "Mississauga"), 
                             cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                             pred = predict(popden16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

popden16.gam_b = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden16.gam_b) # R2 = 0.43***
gam.check(popden16.gam_b) # Should see convergence, p should not be significant
plot(popden16.gam_b)

popden16_pred_b = data.frame(PopDen = subset(census2016$PopDen, census2016$mun == "Brampton"), 
                             cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                             pred = predict(popden16.gam_b, newdata = subset(census2016, mun == "Brampton")))

popden16.gam_c = gam(cc ~ s(PopDen, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(popden16.gam_c) # R2 = 0.70***
gam.check(popden16.gam_c) # Should see convergence, p should not be significant
plot(popden16.gam_c)

popden16_pred_c = data.frame(PopDen = subset(census2016$PopDen, census2016$mun == "Caledon"), 
                             cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                             pred = predict(popden16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
popden16_mun.p = ggplot(popden16_pred, aes(x = PopDen, y = cc, size = Pop)) + 
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = PopDen, y = cc, size = Pop), data = popden16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = PopDen, y = pred), data = popden16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = PopDen, y = pred), data = popden16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = PopDen, y = pred), data = popden16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(5000, 10000, 15000, 20000)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.64***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 15000, y = 45, label = "R² = 0.43***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 15000, y = 41, label = "R² = 0.70***", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden16_mun.p

#tiff("PopDen_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(popden71_mun.p | popden81_mun.p | popden86_mun.p) /
  (popden91_mun.p | popden96_mun.p | popden01_mun.p) /
  (popden06_mun.p | popden11_mun.p | popden16_mun.p)
#dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 15000
ymin = 0
ymax = 45

popden_com.p # Already built above

# Mississauga
# Find weighted mean location (closest real point) on predicted line
popden_m_mn_row71 = first(which(abs(popden71_pred_m$PopDen - weighted.mean(x = popden71_pred_m$PopDen, w = popden71_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden71_pred_m$PopDen - weighted.mean(x = popden71_pred_m$PopDen, w = popden71_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden71_pred_m[popden_m_mn_row71,]

popden_m_mn_row81 = first(which(abs(popden81_pred_m$PopDen - weighted.mean(x = popden81_pred_m$PopDen, w = popden81_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden81_pred_m$PopDen - weighted.mean(x = popden81_pred_m$PopDen, w = popden81_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden81_pred_m[popden_m_mn_row81,]

popden_m_mn_row86 = first(which(abs(popden86_pred_m$PopDen - weighted.mean(x = popden86_pred_m$PopDen, w = popden86_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden86_pred_m$PopDen - weighted.mean(x = popden86_pred_m$PopDen, w = popden86_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden86_pred_m[popden_m_mn_row86,]

popden_m_mn_row91 = first(which(abs(popden91_pred_m$PopDen - weighted.mean(x = popden91_pred_m$PopDen, w = popden91_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden91_pred_m$PopDen - weighted.mean(x = popden91_pred_m$PopDen, w = popden91_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden91_pred_m[popden_m_mn_row91,]

popden_m_mn_row96 = first(which(abs(popden96_pred_m$PopDen - weighted.mean(x = popden96_pred_m$PopDen, w = popden96_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden96_pred_m$PopDen - weighted.mean(x = popden96_pred_m$PopDen, w = popden96_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden96_pred_m[popden_m_mn_row96,]

popden_m_mn_row01 = first(which(abs(popden01_pred_m$PopDen - weighted.mean(x = popden01_pred_m$PopDen, w = popden01_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden01_pred_m$PopDen - weighted.mean(x = popden01_pred_m$PopDen, w = popden01_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden01_pred_m[popden_m_mn_row01,]

popden_m_mn_row06 = first(which(abs(popden06_pred_m$PopDen - weighted.mean(x = popden06_pred_m$PopDen, w = popden06_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden06_pred_m$PopDen - weighted.mean(x = popden06_pred_m$PopDen, w = popden06_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden06_pred_m[popden_m_mn_row06,]

popden_m_mn_row11 = first(which(abs(popden11_pred_m$PopDen - weighted.mean(x = popden11_pred_m$PopDen, w = popden11_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden11_pred_m$PopDen - weighted.mean(x = popden11_pred_m$PopDen, w = popden11_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden11_pred_m[popden_m_mn_row11,]

popden_m_mn_row16 = first(which(abs(popden16_pred_m$PopDen - weighted.mean(x = popden16_pred_m$PopDen, w = popden16_pred_m$Pop, na.rm = TRUE)) == 
                         min(abs(popden16_pred_m$PopDen - weighted.mean(x = popden16_pred_m$PopDen, w = popden16_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden16_pred_m[popden_m_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

popden_com_m.p = ggplot() +
  geom_line(aes(x = PopDen, y = pred, col = "1971"), data = popden71_pred_m) + 
  geom_point(aes(x = popden71_pred_m$PopDen[popden_m_mn_row71], y = popden71_pred_m$pred[popden_m_mn_row71]), color = "black") +
  geom_line(aes(x = PopDen, y = pred, col = "1981"), data = popden81_pred_m) + 
  geom_point(aes(x = popden81_pred_m$PopDen[popden_m_mn_row81], y = popden81_pred_m$pred[popden_m_mn_row81]), color = "sienna") +
  geom_line(aes(x = PopDen, y = pred, col = "1986"), data = popden86_pred_m) + 
  geom_point(aes(x = popden86_pred_m$PopDen[popden_m_mn_row86], y = popden86_pred_m$pred[popden_m_mn_row86]), color = "purple4") +
  geom_line(aes(x = PopDen, y = pred, col = "1991"), data = popden91_pred_m) + 
  geom_point(aes(x = popden91_pred_m$PopDen[popden_m_mn_row91], y = popden91_pred_m$pred[popden_m_mn_row91]), color = "blue") +
  geom_line(aes(x = PopDen, y = pred, col = "1996"), data = popden96_pred_m) + 
  geom_point(aes(x = popden96_pred_m$PopDen[popden_m_mn_row96], y = popden96_pred_m$pred[popden_m_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = PopDen, y = pred, col = "2001"), data = subset(popden01_pred_m, PopDen > 1000)) + 
  geom_point(aes(x = popden01_pred_m$PopDen[popden_m_mn_row01], y = popden01_pred_m$pred[popden_m_mn_row01]), color = "green4") +
  geom_line(aes(x = PopDen, y = pred, col = "2006"), data = popden06_pred_m) + 
  geom_point(aes(x = popden06_pred_m$PopDen[popden_m_mn_row06], y = popden06_pred_m$pred[popden_m_mn_row06]), color = "gold2") +
  geom_line(aes(x = PopDen, y = pred, col = "2011"), data = popden11_pred_m) + 
  geom_point(aes(x = popden11_pred_m$PopDen[popden_m_mn_row11], y = popden11_pred_m$pred[popden_m_mn_row11]), color = "darkorange2") +
  geom_line(aes(x = PopDen, y = pred, col = "2016"), data = popden16_pred_m) +
  geom_point(aes(x = popden16_pred_m$PopDen[popden_m_mn_row16], y = popden16_pred_m$pred[popden_m_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 11500, y = 43, label = "R² = 0.22***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 11500, y = 41, label = "R² = 0.45***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 11500, y = 39, label = "R² = 0.48***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 11500, y = 37, label = "R² = 0.43***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 11500, y = 35, label = "R² = 0.43***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 11500, y = 33, label = "R² = 0.47***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 11500, y = 31, label = "R² = 0.56***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 11500, y = 29, label = "R² = 0.62***", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 11500, y = 27, label = "R² = 0.64***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 2000, y = 5, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, expand = c(0,0)) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
popden_com_m.p

# Brampton
# Find weighted mean location (closest real point) on predicted line
popden_b_mn_row71 = first(which(abs(popden71_pred_b$PopDen - weighted.mean(x = popden71_pred_b$PopDen, w = popden71_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden71_pred_b$PopDen - weighted.mean(x = popden71_pred_b$PopDen, w = popden71_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden71_pred_b[popden_b_mn_row71,]

popden_b_mn_row81 = first(which(abs(popden81_pred_b$PopDen - weighted.mean(x = popden81_pred_b$PopDen, w = popden81_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden81_pred_b$PopDen - weighted.mean(x = popden81_pred_b$PopDen, w = popden81_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden81_pred_b[popden_b_mn_row81,]

popden_b_mn_row86 = first(which(abs(popden86_pred_b$PopDen - weighted.mean(x = popden86_pred_b$PopDen, w = popden86_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden86_pred_b$PopDen - weighted.mean(x = popden86_pred_b$PopDen, w = popden86_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden86_pred_b[popden_b_mn_row86,]

popden_b_mn_row91 = first(which(abs(popden91_pred_b$PopDen - weighted.mean(x = popden91_pred_b$PopDen, w = popden91_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden91_pred_b$PopDen - weighted.mean(x = popden91_pred_b$PopDen, w = popden91_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden91_pred_b[popden_b_mn_row91,]

popden_b_mn_row96 = first(which(abs(popden96_pred_b$PopDen - weighted.mean(x = popden96_pred_b$PopDen, w = popden96_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden96_pred_b$PopDen - weighted.mean(x = popden96_pred_b$PopDen, w = popden96_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden96_pred_b[popden_b_mn_row96,]

popden_b_mn_row01 = first(which(abs(popden01_pred_b$PopDen - weighted.mean(x = popden01_pred_b$PopDen, w = popden01_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden01_pred_b$PopDen - weighted.mean(x = popden01_pred_b$PopDen, w = popden01_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden01_pred_b[popden_b_mn_row01,]

popden_b_mn_row06 = first(which(abs(popden06_pred_b$PopDen - weighted.mean(x = popden06_pred_b$PopDen, w = popden06_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden06_pred_b$PopDen - weighted.mean(x = popden06_pred_b$PopDen, w = popden06_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden06_pred_b[popden_b_mn_row06,]

popden_b_mn_row11 = first(which(abs(popden11_pred_b$PopDen - weighted.mean(x = popden11_pred_b$PopDen, w = popden11_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden11_pred_b$PopDen - weighted.mean(x = popden11_pred_b$PopDen, w = popden11_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden11_pred_b[popden_b_mn_row11,]

popden_b_mn_row16 = first(which(abs(popden16_pred_b$PopDen - weighted.mean(x = popden16_pred_b$PopDen, w = popden16_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(popden16_pred_b$PopDen - weighted.mean(x = popden16_pred_b$PopDen, w = popden16_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden16_pred_b[popden_b_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

popden_com_b.p = ggplot() +
  geom_line(aes(x = PopDen, y = pred, col = "1971"), data = popden71_pred_b) + 
  geom_point(aes(x = popden71_pred_b$PopDen[popden_b_mn_row71], y = popden71_pred_b$pred[popden_b_mn_row71]), color = "black") +
  geom_line(aes(x = PopDen, y = pred, col = "1981"), data = popden81_pred_b) + 
  geom_point(aes(x = popden81_pred_b$PopDen[popden_b_mn_row81], y = popden81_pred_b$pred[popden_b_mn_row81]), color = "sienna") +
  geom_line(aes(x = PopDen, y = pred, col = "1986"), data = popden86_pred_b) + 
  geom_point(aes(x = popden86_pred_b$PopDen[popden_b_mn_row86], y = popden86_pred_b$pred[popden_b_mn_row86]), color = "purple4") +
  geom_line(aes(x = PopDen, y = pred, col = "1991"), data = popden91_pred_b) + 
  geom_point(aes(x = popden91_pred_b$PopDen[popden_b_mn_row91], y = popden91_pred_b$pred[popden_b_mn_row91]), color = "blue") +
  geom_line(aes(x = PopDen, y = pred, col = "1996"), data = popden96_pred_b) +
  geom_point(aes(x = popden96_pred_b$PopDen[popden_b_mn_row96], y = popden96_pred_b$pred[popden_b_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = PopDen, y = pred, col = "2001"), data = popden01_pred_b) + 
  geom_point(aes(x = popden01_pred_b$PopDen[popden_b_mn_row01], y = popden01_pred_b$pred[popden_b_mn_row01]), color = "green4") +
  geom_line(aes(x = PopDen, y = pred, col = "2006"), data = subset(popden06_pred_b, PopDen > 1000)) + 
  geom_point(aes(x = popden06_pred_b$PopDen[popden_b_mn_row06], y = popden06_pred_b$pred[popden_b_mn_row06]), color = "gold2") +
  geom_line(aes(x = PopDen, y = pred, col = "2011"), data = subset(popden11_pred_b, PopDen > 1000)) + 
  geom_point(aes(x = popden11_pred_b$PopDen[popden_b_mn_row11], y = popden11_pred_b$pred[popden_b_mn_row11]), color = "darkorange2") +
  geom_line(aes(x = PopDen, y = pred, col = "2016"), data = subset(popden16_pred_b, PopDen > 2000)) +
  geom_point(aes(x = popden16_pred_b$PopDen[popden_b_mn_row16], y = popden16_pred_b$pred[popden_b_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 11500, y = 43, label = "R² = 0.11*   ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 11500, y = 41, label = "R² = 0.25***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 11500, y = 39, label = "R² = 0.27***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 11500, y = 37, label = "R² = 0.33***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 11500, y = 35, label = "R² = 0.20***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 11500, y = 33, label = "R² = 0.24***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 11500, y = 31, label = "R² = 0.26***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 11500, y = 29, label = "R² = 0.35***", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 11500, y = 27, label = "R² = 0.43***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "Residential Population Density (People per km²)", expand = c(0,0), breaks = c(5000, 10000, 15000, 20000)) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden_com_b.p

# Caledon
# Find weighted mean location (closest real point) on predicted line
popden_c_mn_row91 = first(which(abs(popden91_pred_c$PopDen - weighted.mean(x = popden91_pred_c$PopDen, w = popden91_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(popden91_pred_c$PopDen - weighted.mean(x = popden91_pred_c$PopDen, w = popden91_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden91_pred_c[popden_c_mn_row91,]

popden_c_mn_row96 = first(which(abs(popden96_pred_c$PopDen - weighted.mean(x = popden96_pred_c$PopDen, w = popden96_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(popden96_pred_c$PopDen - weighted.mean(x = popden96_pred_c$PopDen, w = popden96_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden96_pred_c[popden_c_mn_row96,]

popden_c_mn_row01 = first(which(abs(popden01_pred_c$PopDen - weighted.mean(x = popden01_pred_c$PopDen, w = popden01_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(popden01_pred_c$PopDen - weighted.mean(x = popden01_pred_c$PopDen, w = popden01_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden01_pred_c[popden_c_mn_row01,]

popden_c_mn_row06 = first(which(abs(popden06_pred_c$PopDen - weighted.mean(x = popden06_pred_c$PopDen, w = popden06_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(popden06_pred_c$PopDen - weighted.mean(x = popden06_pred_c$PopDen, w = popden06_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden06_pred_c[popden_c_mn_row06,]

popden_c_mn_row11 = first(which(abs(popden11_pred_c$PopDen - weighted.mean(x = popden11_pred_c$PopDen, w = popden11_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(popden11_pred_c$PopDen - weighted.mean(x = popden11_pred_c$PopDen, w = popden11_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden11_pred_c[popden_c_mn_row11,]

popden_c_mn_row16 = first(which(abs(popden16_pred_c$PopDen - weighted.mean(x = popden16_pred_c$PopDen, w = popden16_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(popden16_pred_c$PopDen - weighted.mean(x = popden16_pred_c$PopDen, w = popden16_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
popden16_pred_c[popden_c_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

popden_com_c.p = ggplot() +
  geom_line(aes(x = PopDen, y = pred, col = "1991"), data = subset(popden91_pred_c, PopDen < 12000)) + 
  geom_point(aes(x = popden91_pred_c$PopDen[popden_c_mn_row91], y = popden91_pred_c$pred[popden_c_mn_row91]), color = "blue") +
  geom_line(aes(x = PopDen, y = pred, col = "1996"), data = popden96_pred_c) + 
  geom_point(aes(x = popden96_pred_c$PopDen[popden_c_mn_row96], y = popden96_pred_c$pred[popden_c_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = PopDen, y = pred, col = "2001"), data = subset(popden01_pred_c, PopDen < 10000)) + 
  geom_point(aes(x = popden01_pred_c$PopDen[popden_c_mn_row01], y = popden01_pred_c$pred[popden_c_mn_row01]), color = "green4") +
  geom_line(aes(x = PopDen, y = pred, col = "2006"), data = popden06_pred_c) + 
  geom_point(aes(x = popden06_pred_c$PopDen[popden_c_mn_row06], y = popden06_pred_c$pred[popden_c_mn_row06]), color = "gold2") +
  geom_line(aes(x = PopDen, y = pred, col = "2011"), data = popden11_pred_c) + 
  geom_point(aes(x = popden11_pred_c$PopDen[popden_c_mn_row11], y = popden11_pred_c$pred[popden_c_mn_row11]), color = "darkorange2") +
  geom_line(aes(x = PopDen, y = pred, col = "2016"), data = subset(popden16_pred_c, PopDen > 1000)) +
  geom_point(aes(x = popden16_pred_c$PopDen[popden_c_mn_row16], y = popden16_pred_c$pred[popden_c_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 11500, y = 37, label = "R² = 0.71***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 11500, y = 35, label = "R² = 0.34***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 11500, y = 33, label = "R² = 0.64***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 11500, y = 31, label = "R² = 0.87***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 11500, y = 29, label = "R² = 0.85***", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 11500, y = 27, label = "R² = 0.70***", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(5000, 10000, 15000, 20000)) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
popden_com_c.p

#tiff("PopDen_CC_bymun_combined1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(popden_com.p + annotate(geom = "text", x = 800, y = 5, label = "A", fontface = "bold", size = 6)) /
  (popden_com_m.p | popden_com_b.p | popden_com_c.p)
#dev.off()

#tiff("PopDen_CC_bymun_only.tif", units = "cm", width = 16.5, height = 8, res = 300)
popden_com_m.p | popden_com_b.p | popden_com_c.p
#dev.off()

# Bar plot type plots?
# Split variable into chunks and compare (rather than GAM)

# Post-dev type plots? 
# Rather than Census vs. CC in year X, look more specifically at post-development recovery 
# Variables like: %CC recovered in X years, slope over X years, max value after dev etc. 
# DAs that developed in the years after census year X...
#####

##### CC by year- see individual relationships - % Detached Housing #####
### Linear model ###
# 1971
pDet71.lm = lm(cc ~ pDet, census1971)
summary(pDet71.lm) # R2 = 0.37
plot(cc ~ pDet, census1971)
abline(pDet71.lm)

# 1981
pDet81.lm = lm(cc ~ pDet, census1981)
summary(pDet81.lm) # R2 = 0.22
plot(cc ~ pDet, census1981)
abline(pDet81.lm)

# 1986
pDet86.lm = lm(cc ~ pDet, census1986)
summary(pDet86.lm) # R2 = 0.10
plot(cc ~ pDet, census1986)
abline(pDet86.lm)

# 1991
pDet91.lm = lm(cc ~ pDet, census1991)
summary(pDet91.lm) # R2 = 0.05
plot(cc ~ pDet, census1991)
abline(pDet91.lm)

# 1996
pDet96.lm = lm(cc ~ pDet, census1996)
summary(pDet96.lm) # R2 = 0.04
plot(cc ~ pDet, census1996)
abline(pDet96.lm)

# 2001
pDet01.lm = lm(cc ~ pDet, census2001)
summary(pDet01.lm) # R2 = 0.05
plot(cc ~ pDet, census2001)
abline(pDet01.lm)

# 2006
pDet06.lm = lm(cc ~ pDet, census2006)
summary(pDet06.lm) # R2 = 0.04
plot(cc ~ pDet, census2006)
abline(pDet06.lm)

# 2011
pDet11.lm = lm(cc ~ pDet, census2011)
summary(pDet11.lm) # R2 = 0.04
plot(cc ~ pDet, census2011)
abline(pDet11.lm)

# 2016
pDet16.lm = lm(cc ~ pDet, census2016)
summary(pDet16.lm) # R2 = 0.07
plot(cc ~ pDet, census2016)
abline(pDet16.lm)

# Not much here... 
#####

##### CC by year- see individual relationships - % Attached Housing #####
### Linear model ###
# 1971
pAtt71.lm = lm(cc ~ pAtt, census1971)
summary(pAtt71.lm) # R2 = 0.14
plot(cc ~ pAtt, census1971)
abline(pAtt71.lm)

# 1981
pAtt81.lm = lm(cc ~ pAtt, census1981)
summary(pAtt81.lm) # R2 = 0.15
plot(cc ~ pAtt, census1981)
abline(pAtt81.lm)

# 1986
pAtt86.lm = lm(cc ~ pAtt, census1986)
summary(pAtt86.lm) # R2 = 0.06
plot(cc ~ pAtt, census1986)
abline(pAtt86.lm)

# 1991
pAtt91.lm = lm(cc ~ pAtt, census1991)
summary(pAtt91.lm) # R2 = 0.03
plot(cc ~ pAtt, census1991)
abline(pAtt91.lm)

# 1996
pAtt96.lm = lm(cc ~ pAtt, census1996)
summary(pAtt96.lm) # R2 = 0.03
plot(cc ~ pAtt, census1996)
abline(pAtt96.lm)

# 2001
pAtt01.lm = lm(cc ~ pAtt, census2001)
summary(pAtt01.lm) # R2 = 0.05
plot(cc ~ pAtt, census2001)
abline(pAtt01.lm)

# 2006
pAtt06.lm = lm(cc ~ pAtt, census2006)
summary(pAtt06.lm) # R2 = 0.04
plot(cc ~ pAtt, census2006)
abline(pAtt06.lm)

# 2011
pAtt11.lm = lm(cc ~ pAtt, census2011)
summary(pAtt11.lm) # R2 = 0.05
plot(cc ~ pAtt, census2011)
abline(pAtt11.lm)

# 2016
pAtt16.lm = lm(cc ~ pAtt, census2016)
summary(pAtt16.lm) # R2 = 0.07
plot(cc ~ pAtt, census2016)
abline(pAtt16.lm)

# Not much here... 
#####

##### CC by year- see individual relationships - % High-rise Apartments #####
### Linear model ###
# 1971
pApt71.lm = lm(cc ~ pApt, census1971)
summary(pApt71.lm) # R2 = 0.10
plot(cc ~ pApt, census1971)
abline(pApt71.lm)

# 1981
pApt81.lm = lm(cc ~ pApt, census1981)
summary(pApt81.lm) # R2 = 0.02
plot(cc ~ pApt, census1981)
abline(pApt81.lm)

# 1986
pApt86.lm = lm(cc ~ pApt, census1986)
summary(pApt86.lm) # R2 = 0.02
plot(cc ~ pApt, census1986)
abline(pApt86.lm)

# 1991
pApt91.lm = lm(cc ~ pApt, census1991)
summary(pApt91.lm) # R2 = 0.02
plot(cc ~ pApt, census1991)
abline(pApt91.lm)

# 1996
pApt96.lm = lm(cc ~ pApt, census1996)
summary(pApt96.lm) # R2 = 0.00
plot(cc ~ pApt, census1996)
abline(pApt96.lm)

# 2001
pApt01.lm = lm(cc ~ pApt, census2001)
summary(pApt01.lm) # R2 = 0.00
plot(cc ~ pApt, census2001)
abline(pApt01.lm)

# 2006
pApt06.lm = lm(cc ~ pApt, census2006)
summary(pApt06.lm) # R2 = 0.00
plot(cc ~ pApt, census2006)
abline(pApt06.lm)

# 2011
pApt11.lm = lm(cc ~ pApt, census2011)
summary(pApt11.lm) # R2 = 0.00
plot(cc ~ pApt, census2011)
abline(pApt11.lm)

# 2016
pApt16.lm = lm(cc ~ pApt, census2016)
summary(pApt16.lm) # R2 = 0.00
plot(cc ~ pApt, census2016)
abline(pApt16.lm)

# Not much here... 
#####

##### CC by year - see individual relationships - Most Common Dwelling Type (boxplots) #####
ymin = 0
ymax = 53

# 1971
census1971$MC_DwTy = factor(census1971$MC_DwTy, levels = c("Detached", "Attached", "Apartment"))

# Weighted mean, sig difference
wtd.t.test(x = subset(census1971$cc, census1971$MC_DwTy == "Detached"), y = subset(census1971$cc, census1971$MC_DwTy == "Attached"),
           weight = subset(census1971$Pop, census1971$MC_DwTy == "Detached"), weighty = subset(census1971$Pop, census1971$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 8.1***
wtd.t.test(x = subset(census1971$cc, census1971$MC_DwTy == "Apartment"), y = subset(census1971$cc, census1971$MC_DwTy == "Attached"),
           weight = subset(census1971$Pop, census1971$MC_DwTy == "Apartment"), weighty = subset(census1971$Pop, census1971$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 0.4
wtd.t.test(x = subset(census1971$cc, census1971$MC_DwTy == "Detached"), y = subset(census1971$cc, census1971$MC_DwTy == "Apartment"),
           weight = subset(census1971$Pop, census1971$MC_DwTy == "Detached"), weighty = subset(census1971$Pop, census1971$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 7.7***

MC_DwTy71.p = ggboxplot(census1971, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (8.1)", " ns (0.4)", "*** (7.7)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy71.p

# 1981
# Weighted mean, sig difference
wtd.t.test(x = subset(census1981$cc, census1981$MC_DwTy == "Detached"), y = subset(census1981$cc, census1981$MC_DwTy == "Attached"),
           weight = subset(census1981$Pop, census1981$MC_DwTy == "Detached"), weighty = subset(census1981$Pop, census1981$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 7.9***
wtd.t.test(x = subset(census1981$cc, census1981$MC_DwTy == "Apartment"), y = subset(census1981$cc, census1981$MC_DwTy == "Attached"),
           weight = subset(census1981$Pop, census1981$MC_DwTy == "Apartment"), weighty = subset(census1981$Pop, census1981$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = -0.1
wtd.t.test(x = subset(census1981$cc, census1981$MC_DwTy == "Detached"), y = subset(census1981$cc, census1981$MC_DwTy == "Apartment"),
           weight = subset(census1981$Pop, census1981$MC_DwTy == "Detached"), weighty = subset(census1981$Pop, census1981$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 7.7***

# Plot
MC_DwTy81.p = ggboxplot(census1981, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (7.9)", " ns (-0.1)", "*** (7.7)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTy81.p

# 1986
# Weighted mean, sig difference
wtd.t.test(x = subset(census1986$cc, census1986$MC_DwTy == "Detached"), y = subset(census1986$cc, census1986$MC_DwTy == "Attached"),
           weight = subset(census1986$Pop, census1986$MC_DwTy == "Detached"), weighty = subset(census1986$Pop, census1986$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 5.0***
wtd.t.test(x = subset(census1986$cc, census1986$MC_DwTy == "Apartment"), y = subset(census1986$cc, census1986$MC_DwTy == "Attached"),
           weight = subset(census1986$Pop, census1986$MC_DwTy == "Apartment"), weighty = subset(census1986$Pop, census1986$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = -1.5*
wtd.t.test(x = subset(census1986$cc, census1986$MC_DwTy == "Detached"), y = subset(census1986$cc, census1986$MC_DwTy == "Apartment"),
           weight = subset(census1986$Pop, census1986$MC_DwTy == "Detached"), weighty = subset(census1986$Pop, census1986$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 6.5***

# Plot
MC_DwTy86.p = ggboxplot(census1986, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (5.0)", " * (-1.5)", "*** (6.5)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTy86.p

# 1991
# Weighted mean, sig difference
wtd.t.test(x = subset(census1991$cc, census1991$MC_DwTy == "Detached"), y = subset(census1991$cc, census1991$MC_DwTy == "Attached"),
           weight = subset(census1991$Pop, census1991$MC_DwTy == "Detached"), weighty = subset(census1991$Pop, census1991$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 3.0***
wtd.t.test(x = subset(census1991$cc, census1991$MC_DwTy == "Apartment"), y = subset(census1991$cc, census1991$MC_DwTy == "Attached"),
           weight = subset(census1991$Pop, census1991$MC_DwTy == "Apartment"), weighty = subset(census1991$Pop, census1991$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = -1.4*
wtd.t.test(x = subset(census1991$cc, census1991$MC_DwTy == "Detached"), y = subset(census1991$cc, census1991$MC_DwTy == "Apartment"),
           weight = subset(census1991$Pop, census1991$MC_DwTy == "Detached"), weighty = subset(census1991$Pop, census1991$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 4.4***

# Plot
MC_DwTy91.p = ggboxplot(census1991, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (3.0)", " * (-1.4)", "*** (4.4)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy91.p

# 1996
# Weighted mean, sig difference
wtd.t.test(x = subset(census1996$cc, census1996$MC_DwTy == "Detached"), y = subset(census1996$cc, census1996$MC_DwTy == "Attached"),
           weight = subset(census1996$Pop, census1996$MC_DwTy == "Detached"), weighty = subset(census1996$Pop, census1996$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 3.1***
wtd.t.test(x = subset(census1996$cc, census1996$MC_DwTy == "Apartment"), y = subset(census1996$cc, census1996$MC_DwTy == "Attached"),
           weight = subset(census1996$Pop, census1996$MC_DwTy == "Apartment"), weighty = subset(census1996$Pop, census1996$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = -0.3
wtd.t.test(x = subset(census1996$cc, census1996$MC_DwTy == "Detached"), y = subset(census1996$cc, census1996$MC_DwTy == "Apartment"),
           weight = subset(census1996$Pop, census1996$MC_DwTy == "Detached"), weighty = subset(census1996$Pop, census1996$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 3.4***

# Plot
MC_DwTy96.p = ggboxplot(census1996, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (3.1)", " * (-0.3)", "*** (3.4)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTy96.p

# 2001
# Weighted mean, sig difference
wtd.t.test(x = subset(census2001$cc, census2001$MC_DwTy == "Detached"), y = subset(census2001$cc, census2001$MC_DwTy == "Attached"),
           weight = subset(census2001$Pop, census2001$MC_DwTy == "Detached"), weighty = subset(census2001$Pop, census2001$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 3.3***
wtd.t.test(x = subset(census2001$cc, census2001$MC_DwTy == "Apartment"), y = subset(census2001$cc, census2001$MC_DwTy == "Attached"),
           weight = subset(census2001$Pop, census2001$MC_DwTy == "Apartment"), weighty = subset(census2001$Pop, census2001$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 1.4*
wtd.t.test(x = subset(census2001$cc, census2001$MC_DwTy == "Detached"), y = subset(census2001$cc, census2001$MC_DwTy == "Apartment"),
           weight = subset(census2001$Pop, census2001$MC_DwTy == "Detached"), weighty = subset(census2001$Pop, census2001$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 1.8**

# Plot
MC_DwTy01.p = ggboxplot(census2001, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (3.3)", " * (1.4)", "** (1.8)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTy01.p

# 2006
# Weighted mean, sig difference
wtd.t.test(x = subset(census2006$cc, census2006$MC_DwTy == "Detached"), y = subset(census2006$cc, census2006$MC_DwTy == "Attached"),
           weight = subset(census2006$Pop, census2006$MC_DwTy == "Detached"), weighty = subset(census2006$Pop, census2006$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 2.7***
wtd.t.test(x = subset(census2006$cc, census2006$MC_DwTy == "Apartment"), y = subset(census2006$cc, census2006$MC_DwTy == "Attached"),
           weight = subset(census2006$Pop, census2006$MC_DwTy == "Apartment"), weighty = subset(census2006$Pop, census2006$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 2.1***
wtd.t.test(x = subset(census2006$cc, census2006$MC_DwTy == "Detached"), y = subset(census2006$cc, census2006$MC_DwTy == "Apartment"),
           weight = subset(census2006$Pop, census2006$MC_DwTy == "Detached"), weighty = subset(census2006$Pop, census2006$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 0.6

# Plot
MC_DwTy06.p = ggboxplot(census2006, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (2.7)", " *** (2.1)", "(0.6)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy06.p

# 2011
# Weighted mean, sig difference
wtd.t.test(x = subset(census2011$cc, census2011$MC_DwTy == "Detached"), y = subset(census2011$cc, census2011$MC_DwTy == "Attached"),
           weight = subset(census2011$Pop, census2011$MC_DwTy == "Detached"), weighty = subset(census2011$Pop, census2011$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 2.1***
wtd.t.test(x = subset(census2011$cc, census2011$MC_DwTy == "Apartment"), y = subset(census2011$cc, census2011$MC_DwTy == "Attached"),
           weight = subset(census2011$Pop, census2011$MC_DwTy == "Apartment"), weighty = subset(census2011$Pop, census2011$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 1.7**
wtd.t.test(x = subset(census2011$cc, census2011$MC_DwTy == "Detached"), y = subset(census2011$cc, census2011$MC_DwTy == "Apartment"),
           weight = subset(census2011$Pop, census2011$MC_DwTy == "Detached"), weighty = subset(census2011$Pop, census2011$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 0.4

# Create blank plot
MC_DwTy11.p = ggboxplot(census2011, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (2.1)", " ** (1.7)", "(0.4)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = "Most Common Dwelling Type") + 
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy11.p

# 2016
wtd.t.test(x = subset(census2016$cc, census2016$MC_DwTy == "Detached"), y = subset(census2016$cc, census2016$MC_DwTy == "Attached"),
           weight = subset(census2016$Pop, census2016$MC_DwTy == "Detached"), weighty = subset(census2016$Pop, census2016$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 2.5***
wtd.t.test(x = subset(census2016$cc, census2016$MC_DwTy == "Apartment"), y = subset(census2016$cc, census2016$MC_DwTy == "Attached"),
           weight = subset(census2016$Pop, census2016$MC_DwTy == "Apartment"), weighty = subset(census2016$Pop, census2016$MC_DwTy == "Attached"), bootse = TRUE)
# Diff = 1.7**
wtd.t.test(x = subset(census2016$cc, census2016$MC_DwTy == "Detached"), y = subset(census2016$cc, census2016$MC_DwTy == "Apartment"),
           weight = subset(census2016$Pop, census2016$MC_DwTy == "Detached"), weighty = subset(census2016$Pop, census2016$MC_DwTy == "Apartment"), bootse = TRUE)
# Diff = 0.8

# Plot
MC_DwTy16.p = ggboxplot(census2016, x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (2.5)", " ** (1.7)", "(0.8)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = "Most Common Dwelling Type") + 
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy16.p

#tiff("MC_DwTy_CC_Peel_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(MC_DwTy71.p | MC_DwTy81.p | MC_DwTy86.p) /
  (MC_DwTy91.p | MC_DwTy96.p | MC_DwTy01.p) /
  (MC_DwTy06.p | MC_DwTy11.p | MC_DwTy16.p)
#dev.off()

# Combined boxplot
census$CensusYear = factor(census$CensusYear, levels = c("1971", "1976", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016"))

MC_DwTy_com_det.p = ggboxplot(subset(census, MC_DwTy == "Detached"), x = "CensusYear", y = "cc", weight = "Pop") +
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = NULL, breaks = c("1971", "", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016")) +
  geom_point(data = NULL, aes(x = "1976", y = 0), pch = NA) + # Plot empty point to add  1976
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$MC_DwTy == "Detached" & census1971$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$MC_DwTy == "Detached" & census1971$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$MC_DwTy == "Detached" & census1981$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$MC_DwTy == "Detached" & census1981$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$MC_DwTy == "Detached" & census1986$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$MC_DwTy == "Detached" & census1986$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Detached" & census1991$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Detached" & census1991$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Detached" & census1991$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Detached" & census1996$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Detached" & census1996$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Detached" & census1996$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Detached" & census2001$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Detached" & census2001$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Detached" & census2001$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Detached" & census2006$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Detached" & census2006$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Detached" & census2006$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Detached" & census2011$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Detached" & census2011$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Detached" & census2011$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Detached" & census2016$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Detached" & census2016$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Detached" & census2016$mun == "Caledon"))), color = "darkgreen") +
  ggtitle("Detached") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy_com_det.p

MC_DwTy_com_att.p = ggboxplot(subset(census, MC_DwTy == "Attached"), x = "CensusYear", y = "cc", weight = "Pop") +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = NULL, breaks = c("1971", "", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016")) +
  geom_point(data = NULL, aes(x = "1976", y = 0), pch = NA) + # Plot empty point to add  1976
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$MC_DwTy == "Attached" & census1971$mun == "Mississauga"))), color = "purple") +
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$MC_DwTy == "Attached" & census1971$mun == "Brampton"))), color = "purple") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$MC_DwTy == "Attached" & census1981$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$MC_DwTy == "Attached" & census1981$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$MC_DwTy == "Attached" & census1986$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$MC_DwTy == "Attached" & census1986$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Attached" & census1991$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Attached" & census1991$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Attached" & census1996$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Attached" & census1996$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Attached" & census2001$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Attached" & census2001$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Attached" & census2006$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Attached" & census2006$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Attached" & census2011$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Attached" & census2011$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Attached" & census2016$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Attached" & census2016$mun == "Brampton"))), color = "red3") +
  ggtitle("Attached") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy_com_att.p

MC_DwTy_com_apt.p = ggboxplot(subset(census, MC_DwTy == "Apartment"), x = "CensusYear", y = "cc", weight = "Pop")  +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  geom_point(data = NULL, aes(x = "1976", y = 0), pch = NA) + # Plot empty point to add  1976
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$MC_DwTy == "Apartment" & census1971$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$MC_DwTy == "Apartment" & census1981$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$MC_DwTy == "Apartment" & census1981$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$MC_DwTy == "Apartment" & census1986$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$MC_DwTy == "Apartment" & census1986$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Apartment" & census1991$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$MC_DwTy == "Apartment" & census1991$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Apartment" & census1996$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$MC_DwTy == "Apartment" & census1996$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Apartment" & census2001$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$MC_DwTy == "Apartment" & census2001$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Apartment" & census2006$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$MC_DwTy == "Apartment" & census2006$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Apartment" & census2011$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$MC_DwTy == "Apartment" & census2011$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Apartment" & census2016$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$MC_DwTy == "Apartment" & census2016$mun == "Brampton"))), color = "red3") +
  ggtitle("Apartment") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTy_com_apt.p

#tiff("MC_DwTy_CC_com.tif", units = "cm", width = 16.5, height = 8, res = 300)
MC_DwTy_com_det.p | MC_DwTy_com_att.p | MC_DwTy_com_apt.p
#dev.off()
#####

##### CC by year - see individual relationships - 90+% one Dwelling Type (boxplots) #####
ymin = 0
ymax = 53
DwTy_inc = 90 # Above X% one dwelling type

# 1971
census1971$MC_DwTy = factor(census1971$MC_DwTy, levels = c("Detached", "Attached", "Apartment"))

# Weighted mean, sig difference
# Not enough Attached DAs/Population for other tests
wtd.t.test(x = subset(census1971$cc, census1971$pDet >= 90), y = subset(census1971$cc, census1971$pApt >= 90),
           weight = subset(census1971$Pop, census1971$pDet >= 90), weighty = subset(census1971$Pop, census1971$pApt >= 90), bootse = TRUE)
# Diff = 12.1***

MC_DwTyX71.p = ggboxplot(subset(census1971, pDet >= 90 | pAtt > 100 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") + # Not enough data for pAtt
  geom_bracket(xmin = c("Detached"), xmax = c("Apartment"), y.position = c(45), 
               label = c("*** (12.1)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX71.p

# 1981
census1981$MC_DwTy = factor(census1981$MC_DwTy, levels = c("Detached", "Attached", "Apartment"))

# Weighted mean, sig difference
wtd.t.test(x = subset(census1981$cc, census1981$pDet >= 90), y = subset(census1981$cc, census1981$pAtt >= 90),
           weight = subset(census1981$Pop, census1981$pDet >= 90), weighty = subset(census1981$Pop, census1981$pAtt >= 90), bootse = TRUE)
# Diff = 12.3***
wtd.t.test(x = subset(census1981$cc, census1981$pApt >= 90), y = subset(census1981$cc, census1981$pAtt >= 90),
           weight = subset(census1981$Pop, census1981$pApt >= 90), weighty = subset(census1981$Pop, census1981$pAtt >= 90), bootse = TRUE)
# Diff = -2.0*
wtd.t.test(x = subset(census1981$cc, census1981$pDet >= 90), y = subset(census1981$cc, census1981$pApt >= 90),
           weight = subset(census1981$Pop, census1981$pDet >= 90), weighty = subset(census1981$Pop, census1981$pApt >= 90), bootse = TRUE)
# Diff = 14.3***

# Plot
MC_DwTyX81.p = ggboxplot(subset(census1981, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (12.3)", " * (-2.0)", "*** (14.3)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTyX81.p

# 1986
# Weighted mean, sig difference
wtd.t.test(x = subset(census1986$cc, census1986$pDet >= 90), y = subset(census1986$cc, census1986$pAtt >= 90),
           weight = subset(census1986$Pop, census1986$pDet >= 90), weighty = subset(census1986$Pop, census1986$pAtt >= 90), bootse = TRUE)
# Diff = 7.5***
wtd.t.test(x = subset(census1986$cc, census1986$pApt >= 90), y = subset(census1986$cc, census1986$pAtt >= 90),
           weight = subset(census1986$Pop, census1986$pApt >= 90), weighty = subset(census1986$Pop, census1986$pAtt >= 90), bootse = TRUE)
# Diff = -0.5
wtd.t.test(x = subset(census1986$cc, census1986$pDet >= 90), y = subset(census1986$cc, census1986$pApt >= 90),
           weight = subset(census1986$Pop, census1986$pDet >= 90), weighty = subset(census1986$Pop, census1986$pApt >= 90), bootse = TRUE)
# Diff = 8.1***

# Plot
MC_DwTyX86.p = ggboxplot(subset(census1986, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (7.5)", " ns (-0.5)", "*** (8.1)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTyX86.p

# 1991
# Weighted mean, sig difference
wtd.t.test(x = subset(census1991$cc, census1991$pDet >= 90), y = subset(census1991$cc, census1991$pAtt >= 90),
           weight = subset(census1991$Pop, census1991$pDet >= 90), weighty = subset(census1991$Pop, census1991$pAtt >= 90), bootse = TRUE)
# Diff = 5.8***
wtd.t.test(x = subset(census1991$cc, census1991$pApt >= 90), y = subset(census1991$cc, census1991$pAtt >= 90),
           weight = subset(census1991$Pop, census1991$pApt >= 90), weighty = subset(census1991$Pop, census1991$pAtt >= 90), bootse = TRUE)
# Diff = -1.3
wtd.t.test(x = subset(census1991$cc, census1991$pDet >= 90), y = subset(census1991$cc, census1991$pApt >= 90),
           weight = subset(census1991$Pop, census1991$pDet >= 90), weighty = subset(census1991$Pop, census1991$pApt >= 90), bootse = TRUE)
# Diff = 7.2***

# Plot
MC_DwTyX91.p = ggboxplot(subset(census1991, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (5.8)", " ns (-1.3)", "*** (7.2)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX91.p

# 1996
# Weighted mean, sig difference
wtd.t.test(x = subset(census1996$cc, census1996$pDet >= 90), y = subset(census1996$cc, census1996$pAtt >= 90),
           weight = subset(census1996$Pop, census1996$pDet >= 90), weighty = subset(census1996$Pop, census1996$pAtt >= 90), bootse = TRUE)
# Diff = 6.1***
wtd.t.test(x = subset(census1996$cc, census1996$pApt >= 90), y = subset(census1996$cc, census1996$pAtt >= 90),
           weight = subset(census1996$Pop, census1996$pApt >= 90), weighty = subset(census1996$Pop, census1996$pAtt >= 90), bootse = TRUE)
# Diff = -0.3
wtd.t.test(x = subset(census1996$cc, census1996$pDet >= 90), y = subset(census1996$cc, census1996$pApt >= 90),
           weight = subset(census1996$Pop, census1996$pDet >= 90), weighty = subset(census1996$Pop, census1996$pApt >= 90), bootse = TRUE)
# Diff = 6.4***

# Plot
MC_DwTyX96.p = ggboxplot(subset(census1996, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (6.1)", " ns (-0.3)", "*** (6.4)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTyX96.p

# 2001
# Weighted mean, sig difference
wtd.t.test(x = subset(census2001$cc, census2001$pDet >= 90), y = subset(census2001$cc, census2001$pAtt >= 90),
           weight = subset(census2001$Pop, census2001$pDet >= 90), weighty = subset(census2001$Pop, census2001$pAtt >= 90), bootse = TRUE)
# Diff = 5.8***
wtd.t.test(x = subset(census2001$cc, census2001$pApt >= 90), y = subset(census2001$cc, census2001$pAtt >= 90),
           weight = subset(census2001$Pop, census2001$pApt >= 90), weighty = subset(census2001$Pop, census2001$pAtt >= 90), bootse = TRUE)
# Diff = 1.5
wtd.t.test(x = subset(census2001$cc, census2001$pDet >= 90), y = subset(census2001$cc, census2001$pApt >= 90),
           weight = subset(census2001$Pop, census2001$pDet >= 90), weighty = subset(census2001$Pop, census2001$pApt >= 90), bootse = TRUE)
# Diff = 4.3***

# Plot
MC_DwTyX01.p = ggboxplot(subset(census2001, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (5.8)", " ns (1.5)", "*** (4.3)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MC_DwTyX01.p

# 2006
# Weighted mean, sig difference
wtd.t.test(x = subset(census2006$cc, census2006$pDet >= 90), y = subset(census2006$cc, census2006$pAtt >= 90),
           weight = subset(census2006$Pop, census2006$pDet >= 90), weighty = subset(census2006$Pop, census2006$pAtt >= 90), bootse = TRUE)
# Diff = 6.2***
wtd.t.test(x = subset(census2006$cc, census2006$pApt >= 90), y = subset(census2006$cc, census2006$pAtt >= 90),
           weight = subset(census2006$Pop, census2006$pApt >= 90), weighty = subset(census2006$Pop, census2006$pAtt >= 90), bootse = TRUE)
# Diff = 1.3
wtd.t.test(x = subset(census2006$cc, census2006$pDet >= 90), y = subset(census2006$cc, census2006$pApt >= 90),
           weight = subset(census2006$Pop, census2006$pDet >= 90), weighty = subset(census2006$Pop, census2006$pApt >= 90), bootse = TRUE)
# Diff = 4.9***

# Plot
MC_DwTyX06.p = ggboxplot(subset(census2006, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (6.2)", " ns (1.3)", "*** (4.9)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX06.p

# 2011
# Weighted mean, sig difference
wtd.t.test(x = subset(census2011$cc, census2011$pDet >= 90), y = subset(census2011$cc, census2011$pAtt >= 90),
           weight = subset(census2011$Pop, census2011$pDet >= 90), weighty = subset(census2011$Pop, census2011$pAtt >= 90), bootse = TRUE)
# Diff = 6.4***
wtd.t.test(x = subset(census2011$cc, census2011$pApt >= 90), y = subset(census2011$cc, census2011$pAtt >= 90),
           weight = subset(census2011$Pop, census2011$pApt >= 90), weighty = subset(census2011$Pop, census2011$pAtt >= 90), bootse = TRUE)
# Diff = 0.0
wtd.t.test(x = subset(census2011$cc, census2011$pDet >= 90), y = subset(census2011$cc, census2011$pApt >= 90),
           weight = subset(census2011$Pop, census2011$pDet >= 90), weighty = subset(census2011$Pop, census2011$pApt >= 90), bootse = TRUE)
# Diff = 6.4***

# Create blank plot
MC_DwTyX11.p = ggboxplot(subset(census2011, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (6.4)", " ns (0.0)", "*** (6.4)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = "Most Common Dwelling Type (90+%)") + 
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX11.p

# 2016
# Weighted mean, sig difference
wtd.t.test(x = subset(census2016$cc, census2016$pDet >= 90), y = subset(census2016$cc, census2016$pAtt >= 90),
           weight = subset(census2016$Pop, census2016$pDet >= 90), weighty = subset(census2016$Pop, census2016$pAtt >= 90), bootse = TRUE)
# Diff = 7.1***
wtd.t.test(x = subset(census2016$cc, census2016$pApt >= 90), y = subset(census2016$cc, census2016$pAtt >= 90),
           weight = subset(census2016$Pop, census2016$pApt >= 90), weighty = subset(census2016$Pop, census2016$pAtt >= 90), bootse = TRUE)
# Diff = -0.3
wtd.t.test(x = subset(census2016$cc, census2016$pDet >= 90), y = subset(census2016$cc, census2016$pApt >= 90),
           weight = subset(census2016$Pop, census2016$pDet >= 90), weighty = subset(census2016$Pop, census2016$pApt >= 90), bootse = TRUE)
# Diff = 7.4***

# Plot
MC_DwTyX16.p = ggboxplot(subset(census2016, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "cc", fill = "MC_DwTy", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(45, 45, 47.5), 
               label = c("*** (7.1)", " ns (-0.3)", "*** (7.4)"), tip.length = 0.01) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = "Most Common Dwelling Type") + 
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX16.p

#tiff("MC_DwTyX_CC_Peel_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(MC_DwTyX71.p | MC_DwTyX81.p | MC_DwTyX86.p) /
  (MC_DwTyX91.p | MC_DwTyX96.p | MC_DwTyX01.p) /
  (MC_DwTyX06.p | MC_DwTyX11.p | MC_DwTyX16.p)
#dev.off()

# Combined boxplot
ymin = 0
ymax = 45

census$CensusYear = factor(census$CensusYear, levels = c("1971", "1976", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016"))

MC_DwTyX_com_det.p = ggboxplot(subset(census, pDet >= 90), x = "CensusYear", y = "cc", weight = "Pop") +
  scale_y_continuous(name = "Residential Canopy Cover (%)", expand = c(0,0)) +
  scale_x_discrete(name = NULL, breaks = c("1971", "", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016")) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  geom_point(data = NULL, aes(x = "1976", y = 0), pch = NA) + # Plot empty point to add  1976
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$pDet >= 90 & census1971$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$pDet >= 90 & census1971$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$pDet >= 90 & census1981$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$pDet >= 90 & census1981$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$pDet >= 90 & census1986$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$pDet >= 90 & census1986$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$pDet >= 90 & census1991$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$pDet >= 90 & census1991$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pDet >= 90 & census1996$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pDet >= 90 & census1996$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pDet >= 90 & census1996$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pDet >= 90 & census2001$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pDet >= 90 & census2001$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pDet >= 90 & census2001$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pDet >= 90 & census2006$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pDet >= 90 & census2006$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pDet >= 90 & census2006$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pDet >= 90 & census2011$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pDet >= 90 & census2011$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pDet >= 90 & census2011$mun == "Caledon"))), color = "darkgreen") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pDet >= 90 & census2016$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pDet >= 90 & census2016$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pDet >= 90 & census2016$mun == "Caledon"))), color = "darkgreen") +
  ggtitle("Detached (90+%)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX_com_det.p

MC_DwTyX_com_att.p = ggboxplot(subset(census, pAtt >= 90 & CensusYear != "1971"), x = "CensusYear", y = "cc", weight = "Pop") + # Not enough Data in 1971
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = NULL, breaks = c("1971", "", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016")) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  geom_point(data = NULL, aes(x = "1971", y = 0), pch = NA) + # Plot empty point to add  1971
  geom_point(data = NULL, aes(x = "1976", y = 0), pch = NA) + # Plot empty point to add  1976
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$pAtt >= 90 & census1981$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$pAtt >= 90 & census1981$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$pAtt >= 90 & census1986$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$pAtt >= 90 & census1986$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$pAtt >= 90 & census1991$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$pAtt >= 90 & census1991$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pAtt >= 90 & census1996$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pAtt >= 90 & census1996$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pAtt >= 90 & census2001$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pAtt >= 90 & census2001$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pAtt >= 90 & census2006$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pAtt >= 90 & census2006$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pAtt >= 90 & census2011$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pAtt >= 90 & census2011$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pAtt >= 90 & census2016$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pAtt >= 90 & census2016$mun == "Brampton"))), color = "red3") +
  ggtitle("Attached (90+%)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX_com_att.p

MC_DwTyX_com_apt.p = ggboxplot(subset(census, pApt >= 90), x = "CensusYear", y = "cc", weight = "Pop")  +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_x_discrete(name = NULL, breaks = c("1971", "", "1981", "1986", "1991", "1996", "2001", "2006", "2011", "2016")) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  geom_point(data = NULL, aes(x = "1976", y = 0), pch = NA) + # Plot empty point to add  1976
  geom_point(aes(x = "1971", y = median(subset(census1971$cc, census1971$pApt >= 90 & census1971$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1981", y = median(subset(census1981$cc, census1981$pApt >= 90 & census1981$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1986", y = median(subset(census1986$cc, census1986$pApt >= 90 & census1986$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$pApt >= 90 & census1991$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1991", y = median(subset(census1991$cc, census1991$pApt >= 90 & census1991$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pApt >= 90 & census1996$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "1996", y = median(subset(census1996$cc, census1996$pApt >= 90 & census1996$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pApt >= 90 & census2001$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2001", y = median(subset(census2001$cc, census2001$pApt >= 90 & census2001$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pApt >= 90 & census2006$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2006", y = median(subset(census2006$cc, census2006$pApt >= 90 & census2006$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pApt >= 90 & census2011$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2011", y = median(subset(census2011$cc, census2011$pApt >= 90 & census2011$mun == "Brampton"))), color = "red3") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pApt >= 90 & census2016$mun == "Mississauga"))), color = "blue") +
  geom_point(aes(x = "2016", y = median(subset(census2016$cc, census2016$pApt >= 90 & census2016$mun == "Brampton"))), color = "red3") +
  ggtitle("Apartment (90+%)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MC_DwTyX_com_apt.p

#tiff("MC_DwTyX_CC_com.tif", units = "cm", width = 16.5, height = 8, res = 300)
MC_DwTyX_com_det.p | MC_DwTyX_com_att.p | MC_DwTyX_com_apt.p
#dev.off()
#####

##### Combined built form plot - Population Density & Dwelling Type (90+%) #####
tiff("BuiltForm_71to16_1.tif", units = "cm", width = 12, height = 16.5, res = 300)
(popden_com.p + annotate(geom = "text", x = 700, y = 5, label = "A", fontface = "bold", size = 6)) / 
((MC_DwTyX_com_det.p + annotate(geom = "text", x = 1.5, y = 5, label = "B", fontface = "bold", size = 6)) | MC_DwTyX_com_att.p | MC_DwTyX_com_apt.p)
dev.off()
#####

##### CC by year - see individual relationships - Median Household Income #####
### Linear model ###
# 1971
MedInci71.lm = lm(cc ~ MedInci, census1971)
summary(MedInci71.lm) # R2 = 0.08***
plot(cc ~ MedInci, census1971)
abline(MedInci71.lm)

# 1981
MedInci81.lm = lm(cc ~ MedInci, census1981)
summary(MedInci81.lm) # R2 = 0.10***
plot(cc ~ MedInci, census1981)
abline(MedInci81.lm)

# 1986
MedInci86.lm = lm(cc ~ MedInci, census1986)
summary(MedInci86.lm) # R2 = 0.12***
plot(cc ~ MedInci, census1986)
abline(MedInci86.lm)

# 1991
MedInci91.lm = lm(cc ~ MedInci, census1991)
summary(MedInci91.lm) # R2 = 0.08***
plot(cc ~ MedInci, census1991)
abline(MedInci91.lm)

# 1996
MedInci96.lm = lm(cc ~ MedInci, census1996)
summary(MedInci96.lm) # R2 = 0.05***
plot(cc ~ MedInci, census1996)
abline(MedInci96.lm)

# 2001
MedInci01.lm = lm(cc ~ MedInci, census2001)
summary(MedInci01.lm) # R2 = 0.04***
plot(cc ~ MedInci, census2001)
abline(MedInci01.lm)

# 2006
MedInci06.lm = lm(cc ~ MedInci, census2006)
summary(MedInci06.lm) # R2 = 0.05***
plot(cc ~ MedInci, census2006)
abline(MedInci06.lm)

# 2011
# No Data

# 2016
MedInci16.lm = lm(cc ~ MedInci, census2016)
summary(MedInci16.lm) # R2 = 0.12***
plot(cc ~ MedInci, census2016)
abline(MedInci16.lm)

### Weighted GAM Figures ###
xmin = 20
xmax = 250
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
MedInci71.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci71.gam) # R2 = 0.16***
gam.check(MedInci71.gam) # Should see convergence, p should not be significant
plot(MedInci71.gam)

MedInci71_pred = data.frame(MedInci = census1971$MedInci, cc = census1971$cc, Pop = census1971$Pop, pred = predict(MedInci71.gam, newdata = census1971))

# Plot
MedInci71.p = ggplot(MedInci71_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.11*", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci71.p

# 1981
# Create GAM model
MedInci81.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(MedInci81.gam) # R2 = 0.21***
gam.check(MedInci81.gam) # Should see convergence, p should not be significant
plot(MedInci81.gam)

MedInci81_pred = data.frame(MedInci = census1981$MedInci, cc = census1981$cc, Pop = census1981$Pop, pred = predict(MedInci81.gam, newdata = census1981))

# Plot
MedInci81.p = ggplot(MedInci81_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.18***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci81.p

# 1986
# Create GAM model
MedInci86.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(MedInci86.gam) # R2 = 0.19***
gam.check(MedInci86.gam) # Should see convergence, p should not be significant
plot(MedInci86.gam)

MedInci86_pred = data.frame(MedInci = census1986$MedInci, cc = census1986$cc, Pop = census1986$Pop, pred = predict(MedInci86.gam, newdata = census1986))

# Plot
MedInci86.p = ggplot(MedInci86_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.17***", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci86.p

# 1991
# Create GAM model
MedInci91.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(MedInci91.gam) # R2 = 0.14***
gam.check(MedInci91.gam) # Should see convergence, p should not be significant
plot(MedInci91.gam)

MedInci91_pred = data.frame(MedInci = census1991$MedInci, cc = census1991$cc, Pop = census1991$Pop, pred = predict(MedInci91.gam, newdata = census1991))

# Plot
MedInci91.p = ggplot(MedInci91_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.12**", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci91.p

# 1996
# Create GAM model
MedInci96.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(MedInci96.gam) # R2 = 0.10***
gam.check(MedInci96.gam) # Should see convergence, p should not be significant
plot(MedInci96.gam)

MedInci96_pred = data.frame(MedInci = census1996$MedInci, cc = census1996$cc, Pop = census1996$Pop, pred = predict(MedInci96.gam, newdata = census1996))

# Plot
MedInci96.p = ggplot(MedInci96_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.09*", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci96.p

# 2001
# Create GAM model
MedInci01.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(MedInci01.gam) # R2 = 0.10***
gam.check(MedInci01.gam) # Should see convergence, p should not be significant
plot(MedInci01.gam)

MedInci01_pred = data.frame(MedInci = census2001$MedInci, cc = census2001$cc, Pop = census2001$Pop, pred = predict(MedInci01.gam, newdata = census2001))

# Plot
MedInci01.p = ggplot(MedInci01_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.10**", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci01.p

# 2006
# Create GAM model
MedInci06.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(MedInci06.gam) # R2 = 0.11***
gam.check(MedInci06.gam) # Should see convergence, p should not be significant
plot(MedInci06.gam)

MedInci06_pred = data.frame(MedInci = census2006$MedInci, cc = census2006$cc, Pop = census2006$Pop, pred = predict(MedInci06.gam, newdata = census2006))

# Plot
MedInci06.p = ggplot(MedInci06_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), labels = comma) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.11*", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci06.p

# 2011
# Create blank plot
MedInci11.p = ggplot() +
  scale_x_continuous(name = "Inflation-adjusted Median Household Income (1000s $ CAD)", limits = c(xmin, xmax), expand = c(0,0), labels = comma) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  annotate(geom = "text", x = 135, y = 26.5, label = "Data not collected in Census", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci11.p

# 2016
# Create GAM model
MedInci16.gam = gam(cc ~ s(MedInci, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(MedInci16.gam) # R2 = 0.22***
gam.check(MedInci16.gam) # Should see convergence, p should not be significant
plot(MedInci16.gam)

MedInci16_pred = data.frame(MedInci = census2016$MedInci, cc = census2016$cc, Pop = census2016$Pop, pred = predict(MedInci16.gam, newdata = census2016))

# Plot
MedInci16.p = ggplot(MedInci16_pred, aes(x = MedInci / 1000, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), labels = comma) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 200, y = 5, label = "R² = 0.21***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci16.p

tiff("MedInci_CC_Peel_71to16_1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(MedInci71.p | MedInci81.p | MedInci86.p) /
  (MedInci91.p | MedInci96.p | MedInci01.p) /
  (MedInci06.p | MedInci11.p | MedInci16.p)
dev.off()
###

# Peel - Combined Figure
# Find weighted mean location (closest real point) on predicted line
MedInci_mn_row71 = first(which(abs(MedInci71_pred$MedInci - weighted.mean(x = MedInci71_pred$MedInci, w = MedInci71_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci71_pred$MedInci - weighted.mean(x = MedInci71_pred$MedInci, w = MedInci71_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci71_pred[MedInci_mn_row71,]

MedInci_mn_row81 = first(which(abs(MedInci81_pred$MedInci - weighted.mean(x = MedInci81_pred$MedInci, w = MedInci81_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci81_pred$MedInci - weighted.mean(x = MedInci81_pred$MedInci, w = MedInci81_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci81_pred[MedInci_mn_row81,]

MedInci_mn_row86 = first(which(abs(MedInci86_pred$MedInci - weighted.mean(x = MedInci86_pred$MedInci, w = MedInci86_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci86_pred$MedInci - weighted.mean(x = MedInci86_pred$MedInci, w = MedInci86_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci86_pred[MedInci_mn_row86,]

MedInci_mn_row91 = first(which(abs(MedInci91_pred$MedInci - weighted.mean(x = MedInci91_pred$MedInci, w = MedInci91_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci91_pred$MedInci - weighted.mean(x = MedInci91_pred$MedInci, w = MedInci91_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci91_pred[MedInci_mn_row91,]

MedInci_mn_row96 = first(which(abs(MedInci96_pred$MedInci - weighted.mean(x = MedInci96_pred$MedInci, w = MedInci96_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci96_pred$MedInci - weighted.mean(x = MedInci96_pred$MedInci, w = MedInci96_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci96_pred[MedInci_mn_row96,]

MedInci_mn_row01 = first(which(abs(MedInci01_pred$MedInci - weighted.mean(x = MedInci01_pred$MedInci, w = MedInci01_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci01_pred$MedInci - weighted.mean(x = MedInci01_pred$MedInci, w = MedInci01_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci01_pred[MedInci_mn_row01,]

MedInci_mn_row06 = first(which(abs(MedInci06_pred$MedInci - weighted.mean(x = MedInci06_pred$MedInci, w = MedInci06_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci06_pred$MedInci - weighted.mean(x = MedInci06_pred$MedInci, w = MedInci06_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci06_pred[MedInci_mn_row06,]

MedInci_mn_row16 = first(which(abs(MedInci16_pred$MedInci - weighted.mean(x = MedInci16_pred$MedInci, w = MedInci16_pred$Pop, na.rm = TRUE)) == 
                                min(abs(MedInci16_pred$MedInci - weighted.mean(x = MedInci16_pred$MedInci, w = MedInci16_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci16_pred[MedInci_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

# Plot
xmin = 20
xmax = 250
ymin = 0
ymax = 45

MedInci_com.p = ggplot() +
  geom_line(aes(x = MedInci / 1000, y = pred, col = "1971"), data = MedInci71_pred) + 
  geom_point(aes(x = MedInci71_pred$MedInci[MedInci_mn_row71] / 1000, y = MedInci71_pred$pred[MedInci_mn_row71]), color = "black") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "1981"), data = MedInci81_pred) + 
  geom_point(aes(x = MedInci81_pred$MedInci[MedInci_mn_row81] / 1000, y = MedInci81_pred$pred[MedInci_mn_row81]), color = "sienna") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "1986"), data = MedInci86_pred) + 
  geom_point(aes(x = MedInci86_pred$MedInci[MedInci_mn_row86] / 1000, y = MedInci86_pred$pred[MedInci_mn_row86]), color = "purple4") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "1991"), data = MedInci91_pred) + 
  geom_point(aes(x = MedInci91_pred$MedInci[MedInci_mn_row91] / 1000, y = MedInci91_pred$pred[MedInci_mn_row91]), color = "blue") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "1996"), data = MedInci96_pred) + 
  geom_point(aes(x = MedInci96_pred$MedInci[MedInci_mn_row96] / 1000, y = MedInci96_pred$pred[MedInci_mn_row96]), color = "steelblue2") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "2001"), data = MedInci01_pred) + 
  geom_point(aes(x = MedInci01_pred$MedInci[MedInci_mn_row01] / 1000, y = MedInci01_pred$pred[MedInci_mn_row01]), color = "green4") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "2006"), data = MedInci06_pred) + 
  geom_point(aes(x = MedInci06_pred$MedInci[MedInci_mn_row06] / 1000, y = MedInci06_pred$pred[MedInci_mn_row06]), color = "gold2") + 
  geom_line(aes(x = MedInci / 1000, y = pred, col = "2016"), data = MedInci16_pred) +
  geom_point(aes(x = MedInci16_pred$MedInci[MedInci_mn_row16] / 1000, y = MedInci16_pred$pred[MedInci_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 60, y = 43, label = "R² = 0.11*", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 60, y = 41, label = "R² = 0.18***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 60, y = 39, label = "R² = 0.17***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 60, y = 37, label = "R² = 0.12**", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 60, y = 35, label = "R² = 0.09*", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 60, y = 33, label = "R² = 0.10**", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 60, y = 31, label = "R² = 0.11*", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 60, y = 29, label = "R² = 0.21***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "Inflation-adjusted Median Household Income (1000s $ CAD)", expand = c(0,0), labels = comma) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
MedInci_com.p

#tiff("MedInci_CC_Peel_combined1.tif", units = "cm", width = 12, height = 10, res = 300)
MedInci_com.p
#dev.off()

# By Municipality - Separate Figures
xmin = 20000
xmax = 336000
ymin = 0
ymax = 53

# 1971
# Create GAM models
MedInci71.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci71.gam_m) # R2 = 0.15***
gam.check(MedInci71.gam_m) # Should see convergence, p should not be significant
plot(MedInci71.gam_m)

MedInci71_pred_m = data.frame(MedInci = subset(census1971$MedInci, census1971$mun == "Mississauga"), 
                             cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                             Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                             pred = predict(MedInci71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

MedInci71.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1971, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci71.gam_b) # R2 = 0.00
gam.check(MedInci71.gam_b) # Should see convergence, p should not be significant
plot(MedInci71.gam_b)

MedInci71_pred_b = data.frame(MedInci = subset(census1971$MedInci, census1971$mun == "Brampton"), 
                             cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                             Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                             pred = predict(MedInci71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
MedInci71_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = MedInci, y = pred), data = MedInci71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.15***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.00    ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci71_mun.p

# 1981
# Create GAM models
MedInci81.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci81.gam_m) # R2 = 0.26***
gam.check(MedInci81.gam_m) # Should see convergence, p should not be significant
plot(MedInci81.gam_m)

MedInci81_pred_m = data.frame(MedInci = subset(census1981$MedInci, census1981$mun == "Mississauga"), 
                             cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                             Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                             pred = predict(MedInci81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

MedInci81.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci81.gam_b) # R2 = 0.03*
gam.check(MedInci81.gam_b) # Should see convergence, p should not be significant
plot(MedInci81.gam_b)

MedInci81_pred_b = data.frame(MedInci = subset(census1981$MedInci, census1981$mun == "Brampton"), 
                             cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                             Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                             pred = predict(MedInci81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
MedInci81_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = MedInci, y = pred), data = MedInci81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.26***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.03*   ", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci81_mun.p

# 1986
# Create GAM models
MedInci86.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci86.gam_m) # R2 = 0.26***
gam.check(MedInci86.gam_m) # Should see convergence, p should not be significant
plot(MedInci86.gam_m)

MedInci86_pred_m = data.frame(MedInci = subset(census1986$MedInci, census1986$mun == "Mississauga"), 
                             cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                             Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                             pred = predict(MedInci86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

MedInci86.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci86.gam_b) # R2 = 0.04***
gam.check(MedInci86.gam_b) # Should see convergence, p should not be significant
plot(MedInci86.gam_b)

MedInci86_pred_b = data.frame(MedInci = subset(census1986$MedInci, census1986$mun == "Brampton"), 
                             cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                             Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                             pred = predict(MedInci86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
MedInci86_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = MedInci, y = pred), data = MedInci86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.26***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.04***", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci86_mun.p

# 1991
# Create GAM models
MedInci91.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci91.gam_m) # R2 = 0.17***
gam.check(MedInci91.gam_m) # Should see convergence, p should not be significant
plot(MedInci91.gam_m)

MedInci91_pred_m = data.frame(MedInci = subset(census1991$MedInci, census1991$mun == "Mississauga"), 
                             cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                             pred = predict(MedInci91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

MedInci91.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci91.gam_b) # R2 = 0.05***
gam.check(MedInci91.gam_b) # Should see convergence, p should not be significant
plot(MedInci91.gam_b)

MedInci91_pred_b = data.frame(MedInci = subset(census1991$MedInci, census1991$mun == "Brampton"), 
                             cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                             pred = predict(MedInci91.gam_b, newdata = subset(census1991, mun == "Brampton")))

MedInci91.gam_c = gam(cc ~ s(MedInci, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci91.gam_c) # R2 = 0.30*
gam.check(MedInci91.gam_c) # Should see convergence, p should not be significant
plot(MedInci91.gam_c)

MedInci91_pred_c = data.frame(MedInci = subset(census1991$MedInci, census1991$mun == "Caledon"), 
                             cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                             pred = predict(MedInci91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
MedInci91_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = MedInci, y = pred), data = MedInci91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.17***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.05***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 275000, y = 8, label = "R² = 0.30*   ", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci91_mun.p

# 1996
# Create GAM models
MedInci96.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci96.gam_m) # R2 = 0.12***
gam.check(MedInci96.gam_m) # Should see convergence, p should not be significant
plot(MedInci96.gam_m)

MedInci96_pred_m = data.frame(MedInci = subset(census1996$MedInci, census1996$mun == "Mississauga"), 
                             cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                             pred = predict(MedInci96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

MedInci96.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci96.gam_b) # R2 = 0.03***
gam.check(MedInci96.gam_b) # Should see convergence, p should not be significant
plot(MedInci96.gam_b)

MedInci96_pred_b = data.frame(MedInci = subset(census1996$MedInci, census1996$mun == "Brampton"), 
                             cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                             pred = predict(MedInci96.gam_b, newdata = subset(census1996, mun == "Brampton")))

MedInci96.gam_c = gam(cc ~ s(MedInci, bs = "cs", k =  31), data = subset(census1996, mun == "Caledon"), # 33 = # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci96.gam_c) # R2 = 0.00
gam.check(MedInci96.gam_c) # Should see convergence, p should not be significant
plot(MedInci96.gam_c)

MedInci96_pred_c = data.frame(MedInci = subset(census1996$MedInci, census1996$mun == "Caledon"), 
                             cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                             pred = predict(MedInci96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
MedInci96_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = MedInci, y = pred), data = MedInci96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.12***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.03***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 275000, y = 8, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci96_mun.p

# 2001
# Create GAM models
MedInci01.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2001, mun == "Mississauga"),
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci01.gam_m) # R2 = 0.12***
gam.check(MedInci01.gam_m) # Should see convergence, p should not be significant
plot(MedInci01.gam_m)

MedInci01_pred_m = data.frame(MedInci = subset(census2001$MedInci, census2001$mun == "Mississauga"), 
                             cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                             pred = predict(MedInci01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

MedInci01.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci01.gam_b) # R2 = 0.03**
gam.check(MedInci01.gam_b) # Should see convergence, p should not be significant
plot(MedInci01.gam_b)

MedInci01_pred_b = data.frame(MedInci = subset(census2001$MedInci, census2001$mun == "Brampton"), 
                             cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                             pred = predict(MedInci01.gam_b, newdata = subset(census2001, mun == "Brampton")))

MedInci01.gam_c = gam(cc ~ s(MedInci, bs = "cs", k = 45), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci01.gam_c) # R2 = 0.24**
gam.check(MedInci01.gam_c) # Should see convergence, p should not be significant
plot(MedInci01.gam_c)

MedInci01_pred_c = data.frame(MedInci = subset(census2001$MedInci, census2001$mun == "Caledon"), 
                             cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                             pred = predict(MedInci01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
MedInci01_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = MedInci, y = pred), data = MedInci01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.12***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.03**  ", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 275000, y = 8, label = "R² = 0.24**  ", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
MedInci01_mun.p

# 2006
# Create GAM models
MedInci06.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"),
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci06.gam_m) # R2 = 0.14***
gam.check(MedInci06.gam_m) # Should see convergence, p should not be significant
plot(MedInci06.gam_m)

MedInci06_pred_m = data.frame(MedInci = subset(census2006$MedInci, census2006$mun == "Mississauga"), 
                             cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                             pred = predict(MedInci06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

MedInci06.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci06.gam_b) # R2 = 0.03***
gam.check(MedInci06.gam_b) # Should see convergence, p should not be significant
plot(MedInci06.gam_b)

MedInci06_pred_b = data.frame(MedInci = subset(census2006$MedInci, census2006$mun == "Brampton"), 
                             cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                             pred = predict(MedInci06.gam_b, newdata = subset(census2006, mun == "Brampton")))

MedInci06.gam_c = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2006, mun == "Caledon"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci06.gam_c) # R2 = 0.21**
gam.check(MedInci06.gam_c) # Should see convergence, p should not be significant
plot(MedInci06.gam_c)

MedInci06_pred_c = data.frame(MedInci = subset(census2006$MedInci, census2006$mun == "Caledon"), 
                             cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                             pred = predict(MedInci06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
MedInci06_mun.p = ggplot() +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = MedInci, y = pred), data = MedInci06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), labels = comma) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.14***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.03***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 275000, y = 8, label = "R² = 0.21**  ", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci06_mun.p

# 2011
MedInci11_mun.p = MedInci11.p
MedInci11_mun.p

# 2016
# Create GAM models
MedInci16.gam_m = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci16.gam_m) # R2 = 0.24***
gam.check(MedInci16.gam_m) # Should see convergence, p should not be significant
plot(MedInci16.gam_m)

MedInci16_pred_m = data.frame(MedInci = subset(census2016$MedInci, census2016$mun == "Mississauga"), 
                             cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                             pred = predict(MedInci16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

MedInci16.gam_b = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci16.gam_b) # R2 = 0.16***
gam.check(MedInci16.gam_b) # Should see convergence, p should not be significant
plot(MedInci16.gam_b)

MedInci16_pred_b = data.frame(MedInci = subset(census2016$MedInci, census2016$mun == "Brampton"), 
                             cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                             pred = predict(MedInci16.gam_b, newdata = subset(census2016, mun == "Brampton")))

MedInci16.gam_c = gam(cc ~ s(MedInci, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(MedInci16.gam_c) # R2 = 0.08
gam.check(MedInci16.gam_c) # Should see convergence, p should not be significant
plot(MedInci16.gam_c)

MedInci16_pred_c = data.frame(MedInci = subset(census2016$MedInci, census2016$mun == "Caledon"), 
                             cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                             pred = predict(MedInci16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
MedInci16_mun.p = ggplot(MedInci16_pred, aes(x = MedInci, y = cc, size = Pop)) + 
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = MedInci, y = cc, size = Pop), data = MedInci16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = MedInci, y = pred), data = MedInci16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = MedInci, y = pred), data = MedInci16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = MedInci, y = pred), data = MedInci16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), labels = comma) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 275000, y = 16, label = "R² = 0.24***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 275000, y = 12, label = "R² = 0.16***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 275000, y = 8, label = "R² = 0.08    ", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci16_mun.p

#tiff("MedInci_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(MedInci71_mun.p | MedInci81_mun.p | MedInci86_mun.p) /
  (MedInci91_mun.p | MedInci96_mun.p | MedInci01_mun.p) /
  (MedInci06_mun.p | MedInci11_mun.p | MedInci16_mun.p)
#dev.off()
###

# Peel - Combined Figure
MedInci_com.p # Already built above

xmin = 30000
xmax = 250000
ymin = 0
ymax = 45

# Mississauga
# Find weighted mean location (closest real point) on predicted line
MedInci_m_mn_row71 = first(which(abs(MedInci71_pred_m$MedInci - weighted.mean(x = MedInci71_pred_m$MedInci, w = MedInci71_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci71_pred_m$MedInci - weighted.mean(x = MedInci71_pred_m$MedInci, w = MedInci71_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci71_pred_m[MedInci_m_mn_row71,]

MedInci_m_mn_row81 = first(which(abs(MedInci81_pred_m$MedInci - weighted.mean(x = MedInci81_pred_m$MedInci, w = MedInci81_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci81_pred_m$MedInci - weighted.mean(x = MedInci81_pred_m$MedInci, w = MedInci81_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci81_pred_m[MedInci_m_mn_row81,]

MedInci_m_mn_row86 = first(which(abs(MedInci86_pred_m$MedInci - weighted.mean(x = MedInci86_pred_m$MedInci, w = MedInci86_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci86_pred_m$MedInci - weighted.mean(x = MedInci86_pred_m$MedInci, w = MedInci86_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci86_pred_m[MedInci_m_mn_row86,]

MedInci_m_mn_row91 = first(which(abs(MedInci91_pred_m$MedInci - weighted.mean(x = MedInci91_pred_m$MedInci, w = MedInci91_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci91_pred_m$MedInci - weighted.mean(x = MedInci91_pred_m$MedInci, w = MedInci91_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci91_pred_m[MedInci_m_mn_row91,]

MedInci_m_mn_row96 = first(which(abs(MedInci96_pred_m$MedInci - weighted.mean(x = MedInci96_pred_m$MedInci, w = MedInci96_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci96_pred_m$MedInci - weighted.mean(x = MedInci96_pred_m$MedInci, w = MedInci96_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci96_pred_m[MedInci_m_mn_row96,]

MedInci_m_mn_row01 = first(which(abs(MedInci01_pred_m$MedInci - weighted.mean(x = MedInci01_pred_m$MedInci, w = MedInci01_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci01_pred_m$MedInci - weighted.mean(x = MedInci01_pred_m$MedInci, w = MedInci01_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci01_pred_m[MedInci_m_mn_row01,]

MedInci_m_mn_row06 = first(which(abs(MedInci06_pred_m$MedInci - weighted.mean(x = MedInci06_pred_m$MedInci, w = MedInci06_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci06_pred_m$MedInci - weighted.mean(x = MedInci06_pred_m$MedInci, w = MedInci06_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci06_pred_m[MedInci_m_mn_row06,]

MedInci_m_mn_row16 = first(which(abs(MedInci16_pred_m$MedInci - weighted.mean(x = MedInci16_pred_m$MedInci, w = MedInci16_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci16_pred_m$MedInci - weighted.mean(x = MedInci16_pred_m$MedInci, w = MedInci16_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci16_pred_m[MedInci_m_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

# Mississauga
MedInci_com_m.p = ggplot() +
  geom_line(aes(x = MedInci, y = pred, col = "1971"), data = MedInci71_pred_m) + 
  geom_point(aes(x = MedInci71_pred_m$MedInci[MedInci_m_mn_row71], y = MedInci71_pred_m$pred[MedInci_m_mn_row71]), color = "black") +
  geom_line(aes(x = MedInci, y = pred, col = "1981"), data = subset(MedInci81_pred_m, MedInci < 200000)) + 
  geom_point(aes(x = MedInci81_pred_m$MedInci[MedInci_m_mn_row81], y = MedInci81_pred_m$pred[MedInci_m_mn_row81]), color = "sienna") +
  geom_line(aes(x = MedInci, y = pred, col = "1986"), data = MedInci86_pred_m) + 
  geom_point(aes(x = MedInci86_pred_m$MedInci[MedInci_m_mn_row86], y = MedInci86_pred_m$pred[MedInci_m_mn_row86]), color = "purple4") +
  geom_line(aes(x = MedInci, y = pred, col = "1991"), data = MedInci91_pred_m) + 
  geom_point(aes(x = MedInci91_pred_m$MedInci[MedInci_m_mn_row91], y = MedInci91_pred_m$pred[MedInci_m_mn_row91]), color = "blue") +
  geom_line(aes(x = MedInci, y = pred, col = "1996"), data = MedInci96_pred_m) + 
  geom_point(aes(x = MedInci96_pred_m$MedInci[MedInci_m_mn_row96], y = MedInci96_pred_m$pred[MedInci_m_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = MedInci, y = pred, col = "2001"), data = MedInci01_pred_m) + 
  geom_point(aes(x = MedInci01_pred_m$MedInci[MedInci_m_mn_row01], y = MedInci01_pred_m$pred[MedInci_m_mn_row01]), color = "green4") +
  geom_line(aes(x = MedInci, y = pred, col = "2006"), data = MedInci06_pred_m) + 
  geom_point(aes(x = MedInci06_pred_m$MedInci[MedInci_m_mn_row06], y = MedInci06_pred_m$pred[MedInci_m_mn_row06]), color = "gold2") +
  geom_line(aes(x = MedInci, y = pred, col = "2016"), data = MedInci16_pred_m) +
  geom_point(aes(x = MedInci16_pred_m$MedInci[MedInci_m_mn_row16], y = MedInci16_pred_m$pred[MedInci_m_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 200000, y = 16, label = "R² = 0.15***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 200000, y = 14, label = "R² = 0.26***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 200000, y = 12, label = "R² = 0.26***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 200000, y = 10, label = "R² = 0.17***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 200000, y = 8, label = "R² = 0.12***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 200000, y = 6, label = "R² = 0.12***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 200000, y = 4, label = "R² = 0.14***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 200000, y = 2, label = "R² = 0.24***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 55000, y = 42, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = c(50000, 100000, 150000, 200000)) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
MedInci_com_m.p

# Brampton
# Find weighted mean location (closest real point) on predicted line
MedInci_b_mn_row71 = first(which(abs(MedInci71_pred_b$MedInci - weighted.mean(x = MedInci71_pred_b$MedInci, w = MedInci71_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci71_pred_b$MedInci - weighted.mean(x = MedInci71_pred_b$MedInci, w = MedInci71_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci71_pred_b[MedInci_b_mn_row71,]

MedInci_b_mn_row81 = first(which(abs(MedInci81_pred_b$MedInci - weighted.mean(x = MedInci81_pred_b$MedInci, w = MedInci81_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci81_pred_b$MedInci - weighted.mean(x = MedInci81_pred_b$MedInci, w = MedInci81_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci81_pred_b[MedInci_b_mn_row81,]

MedInci_b_mn_row86 = first(which(abs(MedInci86_pred_b$MedInci - weighted.mean(x = MedInci86_pred_b$MedInci, w = MedInci86_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci86_pred_b$MedInci - weighted.mean(x = MedInci86_pred_b$MedInci, w = MedInci86_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci86_pred_b[MedInci_b_mn_row86,]

MedInci_b_mn_row91 = first(which(abs(MedInci91_pred_b$MedInci - weighted.mean(x = MedInci91_pred_b$MedInci, w = MedInci91_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci91_pred_b$MedInci - weighted.mean(x = MedInci91_pred_b$MedInci, w = MedInci91_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci91_pred_b[MedInci_b_mn_row91,]

MedInci_b_mn_row96 = first(which(abs(MedInci96_pred_b$MedInci - weighted.mean(x = MedInci96_pred_b$MedInci, w = MedInci96_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci96_pred_b$MedInci - weighted.mean(x = MedInci96_pred_b$MedInci, w = MedInci96_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci96_pred_b[MedInci_b_mn_row96,]

MedInci_b_mn_row01 = first(which(abs(MedInci01_pred_b$MedInci - weighted.mean(x = MedInci01_pred_b$MedInci, w = MedInci01_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci01_pred_b$MedInci - weighted.mean(x = MedInci01_pred_b$MedInci, w = MedInci01_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci01_pred_b[MedInci_b_mn_row01,]

MedInci_b_mn_row06 = first(which(abs(MedInci06_pred_b$MedInci - weighted.mean(x = MedInci06_pred_b$MedInci, w = MedInci06_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci06_pred_b$MedInci - weighted.mean(x = MedInci06_pred_b$MedInci, w = MedInci06_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci06_pred_b[MedInci_b_mn_row06,]

MedInci_b_mn_row16 = first(which(abs(MedInci16_pred_b$MedInci - weighted.mean(x = MedInci16_pred_b$MedInci, w = MedInci16_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci16_pred_b$MedInci - weighted.mean(x = MedInci16_pred_b$MedInci, w = MedInci16_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci16_pred_b[MedInci_b_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

MedInci_com_b.p = ggplot() +
  geom_line(aes(x = MedInci, y = pred, col = "1971"), data = MedInci71_pred_b) + 
  geom_point(aes(x = MedInci71_pred_b$MedInci[MedInci_b_mn_row71], y = MedInci71_pred_b$pred[MedInci_b_mn_row71]), color = "black") +
  geom_line(aes(x = MedInci, y = pred, col = "1981"), data = MedInci81_pred_b) + 
  geom_point(aes(x = MedInci81_pred_b$MedInci[MedInci_b_mn_row81], y = MedInci81_pred_b$pred[MedInci_b_mn_row81]), color = "sienna") +
  geom_line(aes(x = MedInci, y = pred, col = "1986"), data = MedInci86_pred_b) + 
  geom_point(aes(x = MedInci86_pred_b$MedInci[MedInci_b_mn_row86], y = MedInci86_pred_b$pred[MedInci_b_mn_row86]), color = "purple4") +
  geom_line(aes(x = MedInci, y = pred, col = "1991"), data = MedInci91_pred_b) + 
  geom_point(aes(x = MedInci91_pred_b$MedInci[MedInci_b_mn_row91], y = MedInci91_pred_b$pred[MedInci_b_mn_row91]), color = "blue") +
  geom_line(aes(x = MedInci, y = pred, col = "1996"), data = MedInci96_pred_b) +
  geom_point(aes(x = MedInci96_pred_b$MedInci[MedInci_b_mn_row96], y = MedInci96_pred_b$pred[MedInci_b_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = MedInci, y = pred, col = "2001"), data = subset(MedInci01_pred_b, MedInci < 200000)) + 
  geom_point(aes(x = MedInci01_pred_b$MedInci[MedInci_b_mn_row01], y = MedInci01_pred_b$pred[MedInci_b_mn_row01]), color = "green4") +
  geom_line(aes(x = MedInci, y = pred, col = "2006"), data = MedInci06_pred_b) + 
  geom_point(aes(x = MedInci06_pred_b$MedInci[MedInci_b_mn_row06], y = MedInci06_pred_b$pred[MedInci_b_mn_row06]), color = "gold2") +
  geom_line(aes(x = MedInci, y = pred, col = "2016"), data = MedInci16_pred_b) +
  geom_point(aes(x = MedInci16_pred_b$MedInci[MedInci_b_mn_row16], y = MedInci16_pred_b$pred[MedInci_b_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 200000, y = 16, label = "R² = 0.00     ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 200000, y = 14, label = "R² = 0.03*   ", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 200000, y = 12, label = "R² = 0.04***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 200000, y = 10, label = "R² = 0.05***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 200000, y = 8, label = "R² = 0.03***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 200000, y = 6, label = "R² = 0.03**  ", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 200000, y = 4, label = "R² = 0.03***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 200000, y = 2, label = "R² = 0.16***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "Inflation-adjusted Median Houshold Income ($ CAD)", limits = c(xmin, xmax), expand = c(0,0), breaks = c(50000, 100000, 150000, 200000)) + 
  coord_cartesian(xlim = c(xmin, xmax)) +
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci_com_b.p

# Caledon
# Caledon
# Find weighted mean location (closest real point) on predicted line
MedInci_c_mn_row91 = first(which(abs(MedInci91_pred_c$MedInci - weighted.mean(x = MedInci91_pred_c$MedInci, w = MedInci91_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci91_pred_c$MedInci - weighted.mean(x = MedInci91_pred_c$MedInci, w = MedInci91_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci91_pred_c[MedInci_c_mn_row91,]

MedInci_c_mn_row96 = first(which(abs(MedInci96_pred_c$MedInci - weighted.mean(x = MedInci96_pred_c$MedInci, w = MedInci96_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci96_pred_c$MedInci - weighted.mean(x = MedInci96_pred_c$MedInci, w = MedInci96_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci96_pred_c[MedInci_c_mn_row96,]

MedInci_c_mn_row01 = first(which(abs(MedInci01_pred_c$MedInci - weighted.mean(x = MedInci01_pred_c$MedInci, w = MedInci01_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci01_pred_c$MedInci - weighted.mean(x = MedInci01_pred_c$MedInci, w = MedInci01_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci01_pred_c[MedInci_c_mn_row01,]

MedInci_c_mn_row06 = first(which(abs(MedInci06_pred_c$MedInci - weighted.mean(x = MedInci06_pred_c$MedInci, w = MedInci06_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci06_pred_c$MedInci - weighted.mean(x = MedInci06_pred_c$MedInci, w = MedInci06_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci06_pred_c[MedInci_c_mn_row06,]

MedInci_c_mn_row16 = first(which(abs(MedInci16_pred_c$MedInci - weighted.mean(x = MedInci16_pred_c$MedInci, w = MedInci16_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(MedInci16_pred_c$MedInci - weighted.mean(x = MedInci16_pred_c$MedInci, w = MedInci16_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
MedInci16_pred_c[MedInci_c_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

MedInci_com_c.p = ggplot() +
  geom_line(aes(x = MedInci, y = pred, col = "1991"), data = MedInci91_pred_c) + 
  geom_point(aes(x = MedInci91_pred_c$MedInci[MedInci_c_mn_row91], y = MedInci91_pred_c$pred[MedInci_c_mn_row91]), color = "blue") +
  geom_line(aes(x = MedInci, y = pred, col = "1996"), data = MedInci96_pred_c) + 
  geom_point(aes(x = MedInci96_pred_c$MedInci[MedInci_c_mn_row96], y = MedInci96_pred_c$pred[MedInci_c_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = MedInci, y = pred, col = "2001"), data = MedInci01_pred_c) + 
  geom_point(aes(x = MedInci01_pred_c$MedInci[MedInci_c_mn_row01], y = MedInci01_pred_c$pred[MedInci_c_mn_row01]), color = "green4") +
  geom_line(aes(x = MedInci, y = pred, col = "2006"), data = MedInci06_pred_c) + 
  geom_point(aes(x = MedInci06_pred_c$MedInci[MedInci_c_mn_row06], y = MedInci06_pred_c$pred[MedInci_c_mn_row06]), color = "gold2") +
  geom_line(aes(x = MedInci, y = pred, col = "2016"), data = MedInci16_pred_c) +
  geom_point(aes(x = MedInci16_pred_c$MedInci[MedInci_c_mn_row16], y = MedInci16_pred_c$pred[MedInci_c_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 200000, y = 10, label = "R² = 0.30*   ", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 200000, y = 8, label = "R² = 0.00    ", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 200000, y = 6, label = "R² = 0.24**  ", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 200000, y = 4, label = "R² = 0.21**  ", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 200000, y = 2, label = "R² = 0.08    ", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(50000, 100000, 150000, 200000)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
MedInci_com_c.p

#tiff("MedInci_CC_bymun_combined1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(MedInci_com.p + annotate(geom = "text", x = 40000, y = 41, label = "A", fontface = "bold", size = 6)) /
  (MedInci_com_m.p | MedInci_com_b.p | MedInci_com_c.p)
#dev.off()

#tiff("MedInci_CC_bymun_only.tif", units = "cm", width = 16.5, height = 8, res = 300)
MedInci_com_m.p | MedInci_com_b.p | MedInci_com_c.p
#dev.off()
#####

##### CC by year - see individual relationships - % Immigrants #####
### Linear model ###
# 1971
pImm71.lm = lm(cc ~ pImm, census1971)
summary(pImm71.lm) # R2 = 0.20
plot(cc ~ pImm, census1971)
abline(pImm71.lm)

# 1981
pImm81.lm = lm(cc ~ pImm, census1981)
summary(pImm81.lm) # R2 = 0.02
plot(cc ~ pImm, census1981)
abline(pImm81.lm)

# 1986
pImm86.lm = lm(cc ~ pImm, census1986)
summary(pImm86.lm) # R2 = 0.18
plot(cc ~ pImm, census1986)
abline(pImm86.lm)

# 1991
pImm91.lm = lm(cc ~ pImm, census1991)
summary(pImm91.lm) # R2 = 0.17
plot(cc ~ pImm, census1991)
abline(pImm91.lm)

# 1996
pImm96.lm = lm(cc ~ pImm, census1996)
summary(pImm96.lm) # R2 = 0.19
plot(cc ~ pImm, census1996)
abline(pImm96.lm)

# 2001
pImm01.lm = lm(cc ~ pImm, census2001)
summary(pImm01.lm) # R2 = 0.22
plot(cc ~ pImm, census2001)
abline(pImm01.lm)

# 2006
pImm06.lm = lm(cc ~ pImm, census2006)
summary(pImm06.lm) # R2 = 0.30
plot(cc ~ pImm, census2006)
abline(pImm06.lm)

# 2011
# No Data

# 2016
pImm16.lm = lm(cc ~ pImm, census2016)
summary(pImm16.lm) # R2 = 0.37
plot(cc ~ pImm, census2016)
abline(pImm16.lm)
###

### Weighted GAM Figures ###
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
pImm71.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm71.gam) # R2 = 0.21***
gam.check(pImm71.gam) # Should see convergence, p should not be significant
plot(pImm71.gam)

pImm71_pred = data.frame(pImm = census1971$pImm, cc = census1971$cc, Pop = census1971$Pop, pred = predict(pImm71.gam, newdata = census1971))

# Plot
pImm71.p = ggplot(pImm71_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.21***", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm71.p

# 1981
# Create GAM model
pImm81.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(pImm81.gam) # R2 = 0.20***
gam.check(pImm81.gam) # Should see convergence, p should not be significant
plot(pImm81.gam)

pImm81_pred = data.frame(pImm = census1981$pImm, cc = census1981$cc, Pop = census1981$Pop, pred = predict(pImm81.gam, newdata = census1981))

# Plot
pImm81.p = ggplot(pImm81_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.20***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm81.p

# 1986
# Create GAM model
pImm86.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pImm86.gam) # R2 = 0.12***
gam.check(pImm86.gam) # Should see convergence, p should not be significant
plot(pImm86.gam)

pImm86_pred = data.frame(pImm = census1986$pImm, cc = census1986$cc, Pop = census1986$Pop, pred = predict(pImm86.gam, newdata = census1986))

# Plot
pImm86.p = ggplot(pImm86_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.12***", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm86.p

# 1991
# Create GAM model
pImm91.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pImm91.gam) # R2 = 0.19***
gam.check(pImm91.gam) # Should see convergence, p should not be significant
plot(pImm91.gam)

pImm91_pred = data.frame(pImm = census1991$pImm, cc = census1991$cc, Pop = census1991$Pop, pred = predict(pImm91.gam, newdata = census1991))

# Plot
pImm91.p = ggplot(pImm91_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.19***", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm91.p

# 1996
# Create GAM model
pImm96.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm96.gam) # R2 = 0.22***
gam.check(pImm96.gam) # Should see convergence, p should not be significant
plot(pImm96.gam)

pImm96_pred = data.frame(pImm = census1996$pImm, cc = census1996$cc, Pop = census1996$Pop, pred = predict(pImm96.gam, newdata = census1996))

# Plot
pImm96.p = ggplot(pImm96_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.22***", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm96.p

# 2001
# Create GAM model
pImm01.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm01.gam) # R2 = 0.23***
gam.check(pImm01.gam) # Should see convergence, p should not be significant
plot(pImm01.gam)

pImm01_pred = data.frame(pImm = census2001$pImm, cc = census2001$cc, Pop = census2001$Pop, pred = predict(pImm01.gam, newdata = census2001))

# Plot
pImm01.p = ggplot(pImm01_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.23***", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm01.p

# 2006
# Create GAM model
pImm06.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm06.gam) # R2 = 0.30***
gam.check(pImm06.gam) # Should see convergence, p should not be significant
plot(pImm06.gam)

pImm06_pred = data.frame(pImm = census2006$pImm, cc = census2006$cc, Pop = census2006$Pop, pred = predict(pImm06.gam, newdata = census2006))

# Plot
pImm06.p = ggplot(pImm06_pred, aes(x = pImm, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.30***", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm06.p

# 2011
# Create blank plot
pImm11.p = ggplot() +
  scale_x_continuous(name = "% Immigrants", limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  annotate(geom = "text", x = 50, y = 26.5, label = "Data not collected in Census", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm11.p

# 2016
# Create GAM model
pImm16.gam = gam(cc ~ s(pImm, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm16.gam) # R2 = 0.40***
gam.check(pImm16.gam) # Should see convergence, p should not be significant
plot(pImm16.gam)

pImm16_pred = data.frame(pImm = census2016$pImm, cc = census2016$cc, Pop = census2016$Pop, pred = predict(pImm16.gam, newdata = census2016))

# Plot
pImm16.p = ggplot(pImm16_pred, aes(x = pImm, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.40***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm16.p

#tiff("pImm_CC_Peel_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm71.p | pImm81.p | pImm86.p) /
  (pImm91.p | pImm96.p | pImm01.p) /
  (pImm06.p | pImm11.p | pImm16.p)
#dev.off()
###

# Peel - Combined Figure
pImm_com.p = ggplot() +
  geom_line(aes(x = pImm, y = pred, col = "1971"), data = pImm71_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "1981"), data = pImm81_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "1986"), data = pImm86_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "1991"), data = pImm91_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "1996"), data = pImm96_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "2001"), data = pImm01_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "2006"), data = pImm06_pred) + 
  geom_line(aes(x = pImm, y = pred, col = "2016"), data = pImm16_pred) +
  annotate(geom = "text", x = 75, y = 50, label = "R² = 0.21***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 75, y = 48, label = "R² = 0.20***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 75, y = 46, label = "R² = 0.12***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 75, y = 44, label = "R² = 0.19***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 75, y = 42, label = "R² = 0.22***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 75, y = 40, label = "R² = 0.23***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 75, y = 38, label = "R² = 0.30***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 75, y = 36, label = "Data not collected in Census", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 75, y = 34, label = "R² = 0.40***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "% Immigrants", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.4, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pImm_com.p

#tiff("pImm_CC_Peel_combined.tif", units = "cm", width = 10, height = 10, res = 300)
pImm_com.p
#dev.off()

# By Municipality - Separate Figures
# 1971
# Create GAM models
pImm71.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm71.gam_m) # R2 = 0.21***
gam.check(pImm71.gam_m) # Should see convergence, p should not be significant
plot(pImm71.gam_m)

pImm71_pred_m = data.frame(pImm = subset(census1971$pImm, census1971$mun == "Mississauga"), 
                             cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                             Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                             pred = predict(pImm71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

pImm71.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1971, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm71.gam_b) # R2 = 0.15**
gam.check(pImm71.gam_b) # Should see convergence, p should not be significant
plot(pImm71.gam_b)

pImm71_pred_b = data.frame(pImm = subset(census1971$pImm, census1971$mun == "Brampton"), 
                             cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                             Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                             pred = predict(pImm71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
pImm71_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = pImm, y = pred), data = pImm71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.22***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.15**  ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm71_mun.p

# 1981
# Create GAM models
pImm81.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm81.gam_m) # R2 = 0.25***
gam.check(pImm81.gam_m) # Should see convergence, p should not be significant
plot(pImm81.gam_m)

pImm81_pred_m = data.frame(pImm = subset(census1981$pImm, census1981$mun == "Mississauga"), 
                             cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                             Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                             pred = predict(pImm81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

pImm81.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm81.gam_b) # R2 = 0.17***
gam.check(pImm81.gam_b) # Should see convergence, p should not be significant
plot(pImm81.gam_b)

pImm81_pred_b = data.frame(pImm = subset(census1981$pImm, census1981$mun == "Brampton"), 
                             cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                             Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                             pred = predict(pImm81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
pImm81_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = pImm, y = pred), data = pImm81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.25***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.17***", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm81_mun.p

# 1986
# Create GAM models
pImm86.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm86.gam_m) # R2 = 0.18***
gam.check(pImm86.gam_m) # Should see convergence, p should not be significant
plot(pImm86.gam_m)

pImm86_pred_m = data.frame(pImm = subset(census1986$pImm, census1986$mun == "Mississauga"), 
                             cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                             Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                             pred = predict(pImm86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

pImm86.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm86.gam_b) # R2 = 0.06***
gam.check(pImm86.gam_b) # Should see convergence, p should not be significant
plot(pImm86.gam_b)

pImm86_pred_b = data.frame(pImm = subset(census1986$pImm, census1986$mun == "Brampton"), 
                             cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                             Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                             pred = predict(pImm86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
pImm86_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = pImm, y = pred), data = pImm86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.18***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.06***", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm86_mun.p

# 1991
# Create GAM models
pImm91.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm91.gam_m) # R2 = 0.26***
gam.check(pImm91.gam_m) # Should see convergence, p should not be significant
plot(pImm91.gam_m)

pImm91_pred_m = data.frame(pImm = subset(census1991$pImm, census1991$mun == "Mississauga"), 
                             cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                             pred = predict(pImm91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

pImm91.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm91.gam_b) # R2 = 0.14***
gam.check(pImm91.gam_b) # Should see convergence, p should not be significant
plot(pImm91.gam_b)

pImm91_pred_b = data.frame(pImm = subset(census1991$pImm, census1991$mun == "Brampton"), 
                             cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                             pred = predict(pImm91.gam_b, newdata = subset(census1991, mun == "Brampton")))

pImm91.gam_c = gam(cc ~ s(pImm, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm91.gam_c) # R2 = 0.05
gam.check(pImm91.gam_c) # Should see convergence, p should not be significant
plot(pImm91.gam_c)

pImm91_pred_c = data.frame(pImm = subset(census1991$pImm, census1991$mun == "Caledon"), 
                             cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                             Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                             pred = predict(pImm91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
pImm91_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm, y = pred), data = pImm91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.26***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.14***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.05    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm91_mun.p

# 1996
# Create GAM models
pImm96.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm96.gam_m) # R2 = 0.29***
gam.check(pImm96.gam_m) # Should see convergence, p should not be significant
plot(pImm96.gam_m)

pImm96_pred_m = data.frame(pImm = subset(census1996$pImm, census1996$mun == "Mississauga"), 
                             cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                             pred = predict(pImm96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

pImm96.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm96.gam_b) # R2 = 0.25***
gam.check(pImm96.gam_b) # Should see convergence, p should not be significant
plot(pImm96.gam_b)

pImm96_pred_b = data.frame(pImm = subset(census1996$pImm, census1996$mun == "Brampton"), 
                             cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                             pred = predict(pImm96.gam_b, newdata = subset(census1996, mun == "Brampton")))

pImm96.gam_c = gam(cc ~ s(pImm, bs = "cs", k =  33), data = subset(census1996, mun == "Caledon"), # k = 33 (# of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm96.gam_c) # R2 = 0.00
gam.check(pImm96.gam_c) # Should see convergence, p should not be significant
plot(pImm96.gam_c)

pImm96_pred_c = data.frame(pImm = subset(census1996$pImm, census1996$mun == "Caledon"), 
                             cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                             Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                             pred = predict(pImm96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
pImm96_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm, y = pred), data = pImm96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.29***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.25***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm96_mun.p

# 2001
# Create GAM models
pImm01.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2001, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm01.gam_m) # R2 = 0.33***
gam.check(pImm01.gam_m) # Should see convergence, p should not be significant
plot(pImm01.gam_m)

pImm01_pred_m = data.frame(pImm = subset(census2001$pImm, census2001$mun == "Mississauga"), 
                             cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                             pred = predict(pImm01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

pImm01.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm01.gam_b) # R2 = 0.27***
gam.check(pImm01.gam_b) # Should see convergence, p should not be significant
plot(pImm01.gam_b)

pImm01_pred_b = data.frame(pImm = subset(census2001$pImm, census2001$mun == "Brampton"), 
                             cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                             pred = predict(pImm01.gam_b, newdata = subset(census2001, mun == "Brampton")))

pImm01.gam_c = gam(cc ~ s(pImm, bs = "cs", k = 40), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm01.gam_c) # R2 = 0.28***
gam.check(pImm01.gam_c) # Should see convergence, p should not be significant
plot(pImm01.gam_c)

pImm01_pred_c = data.frame(pImm = subset(census2001$pImm, census2001$mun == "Caledon"), 
                             cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                             Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                             pred = predict(pImm01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
pImm01_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm, y = pred), data = pImm01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.33***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.27***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.28***", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm01_mun.p

# 2006
# Create GAM models
pImm06.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm06.gam_m) # R2 = 0.41***
gam.check(pImm06.gam_m) # Should see convergence, p should not be significant
plot(pImm06.gam_m)

pImm06_pred_m = data.frame(pImm = subset(census2006$pImm, census2006$mun == "Mississauga"), 
                             cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                             pred = predict(pImm06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

pImm06.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm06.gam_b) # R2 = 0.36***
gam.check(pImm06.gam_b) # Should see convergence, p should not be significant
plot(pImm06.gam_b)

pImm06_pred_b = data.frame(pImm = subset(census2006$pImm, census2006$mun == "Brampton"), 
                             cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                             pred = predict(pImm06.gam_b, newdata = subset(census2006, mun == "Brampton")))

pImm06.gam_c = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2006, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm06.gam_c) # R2 = 0.19**
gam.check(pImm06.gam_c) # Should see convergence, p should not be significant
plot(pImm06.gam_c)

pImm06_pred_c = data.frame(pImm = subset(census2006$pImm, census2006$mun == "Caledon"), 
                             cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                             Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                             pred = predict(pImm06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
pImm06_mun.p = ggplot() +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm, y = pred), data = pImm06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.41***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.36***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.19**  ", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm06_mun.p

# 2011
pImm11_mun.p = pImm11.p
pImm11_mun.p

# 2016
# Create GAM models
pImm16.gam_m = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm16.gam_m) # R2 = 0.50***
gam.check(pImm16.gam_m) # Should see convergence, p should not be significant
plot(pImm16.gam_m)

pImm16_pred_m = data.frame(pImm = subset(census2016$pImm, census2016$mun == "Mississauga"), 
                             cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                             pred = predict(pImm16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

pImm16.gam_b = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm16.gam_b) # R2 = 0.48***
gam.check(pImm16.gam_b) # Should see convergence, p should not be significant
plot(pImm16.gam_b)

pImm16_pred_b = data.frame(pImm = subset(census2016$pImm, census2016$mun == "Brampton"), 
                             cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                             pred = predict(pImm16.gam_b, newdata = subset(census2016, mun == "Brampton")))

pImm16.gam_c = gam(cc ~ s(pImm, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                     weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm16.gam_c) # R2 = 0.32***
gam.check(pImm16.gam_c) # Should see convergence, p should not be significant
plot(pImm16.gam_c)

pImm16_pred_c = data.frame(pImm = subset(census2016$pImm, census2016$mun == "Caledon"), 
                             cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                             Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                             pred = predict(pImm16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
pImm16_mun.p = ggplot(pImm16_pred, aes(x = pImm, y = cc, size = Pop)) + 
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm, y = cc, size = Pop), data = pImm16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm, y = pred), data = pImm16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm, y = pred), data = pImm16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm, y = pred), data = pImm16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(5000, 10000, 15000, 20000)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.50***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.48***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.32***", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm16_mun.p

tiff("pImm_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm71_mun.p | pImm81_mun.p | pImm86_mun.p) /
  (pImm91_mun.p | pImm96_mun.p | pImm01_mun.p) /
  (pImm06_mun.p | pImm11_mun.p | pImm16_mun.p)
dev.off()
###

# Peel - Combined Figure
pImm_com.p # Already built above

# Mississauga
pImm_com_m.p = ggplot() +
  geom_line(aes(x = pImm, y = pred, col = "1971"), data = pImm71_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "1981"), data = pImm81_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "1986"), data = pImm86_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "1991"), data = pImm91_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "1996"), data = pImm96_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "2001"), data = pImm01_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "2006"), data = pImm06_pred_m) + 
  #geom_line(aes(x = pImm, y = pred, col = "2011"), data = pImm11_pred_m) + 
  geom_line(aes(x = pImm, y = pred, col = "2016"), data = pImm16_pred_m) +
  annotate(geom = "text", x = 70, y = 50, label = "R² = 0.22***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 70, y = 48, label = "R² = 0.25***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 70, y = 46, label = "R² = 0.18***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 70, y = 44, label = "R² = 0.26***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 70, y = 42, label = "R² = 0.29***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 70, y = 40, label = "R² = 0.33***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 70, y = 38, label = "R² = 0.41***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 70, y = 36, label = "No Data", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 70, y = 34, label = "R² = 0.50***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 10, y = 49.5, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
pImm_com_m.p

# Brampton
pImm_com_b.p = ggplot() +
  geom_line(aes(x = pImm, y = pred, col = "1971"), data = pImm71_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "1981"), data = pImm81_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "1986"), data = pImm86_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "1991"), data = pImm91_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "1996"), data = pImm96_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "2001"), data = pImm01_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "2006"), data = pImm06_pred_b) + 
  #geom_line(aes(x = pImm, y = pred, col = "2011"), data = pImm11_pred_b) + 
  geom_line(aes(x = pImm, y = pred, col = "2016"), data = pImm16_pred_b) +
  annotate(geom = "text", x = 70, y = 50, label = "R² = 0.15**  ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 70, y = 48, label = "R² = 0.17***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 70, y = 46, label = "R² = 0.06***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 70, y = 44, label = "R² = 0.14***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 70, y = 42, label = "R² = 0.25***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 70, y = 40, label = "R² = 0.27***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 70, y = 38, label = "R² = 0.36***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 70, y = 36, label = "No Data", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 70, y = 34, label = "R² = 0.48***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "% Immigrants", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_com_b.p

# Caledon
pImm_com_c.p = ggplot() +
  #geom_line(aes(x = pImm, y = pred, col = "1971"), data = pImm71_pred_c) + 
  #geom_line(aes(x = pImm, y = pred, col = "1981"), data = pImm81_pred_c) + 
  #geom_line(aes(x = pImm, y = pred, col = "1986"), data = pImm86_pred_c) + 
  geom_line(aes(x = pImm, y = pred, col = "1991"), data = pImm91_pred_c) + 
  geom_line(aes(x = pImm, y = pred, col = "1996"), data = pImm96_pred_c) + 
  geom_line(aes(x = pImm, y = pred, col = "2001"), data = pImm01_pred_c) + 
  geom_line(aes(x = pImm, y = pred, col = "2006"), data = pImm06_pred_c) + 
  #geom_line(aes(x = pImm, y = pred, col = "2011"), data = pImm11_pred_c) + 
  geom_line(aes(x = pImm, y = pred, col = "2016"), data = pImm16_pred_c) +
  #annotate(geom = "text", x = 15000, y = 50, label = "R² = 0.11*   ", size = eq_sz, color = "black") +
  #annotate(geom = "text", x = 15000, y = 48, label = "R² = 0.25***", size = eq_sz, color = "sienna") +
  #annotate(geom = "text", x = 15000, y = 46, label = "R² = 0.27***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 70, y = 44, label = "R² = 0.05    ", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 70, y = 42, label = "R² = 0.00    ", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 70, y = 40, label = "R² = 0.28***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 70, y = 38, label = "R² = 0.19**  ", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 70, y = 36, label = "No Data", size = eq_sz, color = "darkorange2") +
  annotate(geom = "text", x = 70, y = 34, label = "R² = 0.32***", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_com_c.p

#tiff("pImm_CC_bymun_combined.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_com.p + annotate(geom = "text", x = 5, y = 48, label = "A", fontface = "bold", size = 6)) /
  (pImm_com_m.p | pImm_com_b.p | pImm_com_c.p)
#dev.off()
#####

##### CC by year - see individual relationships - % Immigrants from Americas #####
### Linear model ###
# 1971
pImm_Am71.lm = lm(cc ~ pImm_Am, census1971)
summary(pImm_Am71.lm) # R2 = 0.03*
plot(cc ~ pImm_Am, census1971)
abline(pImm_Am71.lm)

# 1981
pImm_Am81.lm = lm(cc ~ pImm_Am, census1981)
summary(pImm_Am81.lm) # R2 = 0.02
plot(cc ~ pImm_Am, census1981)
abline(pImm_Am81.lm)

# 1986
pImm_Am86.lm = lm(cc ~ pImm_Am, census1986)
summary(pImm_Am86.lm) # R2 = 0.01
plot(cc ~ pImm_Am, census1986)
abline(pImm_Am86.lm)

# 1991
pImm_Am91.lm = lm(cc ~ pImm_Am, census1991)
summary(pImm_Am91.lm) # R2 = 0.10***
plot(cc ~ pImm_Am, census1991)
abline(pImm_Am91.lm)

# 1996
pImm_Am96.lm = lm(cc ~ pImm_Am, census1996)
summary(pImm_Am96.lm) # R2 = 0.13***
plot(cc ~ pImm_Am, census1996)
abline(pImm_Am96.lm)

# 2001
pImm_Am01.lm = lm(cc ~ pImm_Am, census2001)
summary(pImm_Am01.lm) # R2 = 0.09***
plot(cc ~ pImm_Am, census2001)
abline(pImm_Am01.lm)

# 2006
pImm_Am06.lm = lm(cc ~ pImm_Am, census2006)
summary(pImm_Am06.lm) # R2 = 0.13***
plot(cc ~ pImm_Am, census2006)
abline(pImm_Am06.lm)

# 2011
# No Data

# 2016
pImm_Am16.lm = lm(cc ~ pImm_Am, census2016)
summary(pImm_Am16.lm) # R2 = 0.13***
plot(cc ~ pImm_Am, census2016)
abline(pImm_Am16.lm)
###
# Not much there
#####

##### CC by year - see individual relationships - % Immigrants from Europe #####
### Linear model ###
# 1971
pImm_Eu71.lm = lm(cc ~ pImm_Eu, census1971)
summary(pImm_Eu71.lm) # R2 = 0.14***
plot(cc ~ pImm_Eu, census1971)
abline(pImm_Eu71.lm)

# 1981
pImm_Eu81.lm = lm(cc ~ pImm_Eu, census1981)
summary(pImm_Eu81.lm) # R2 = 0.02***
plot(cc ~ pImm_Eu, census1981)
abline(pImm_Eu81.lm)

# 1986
pImm_Eu86.lm = lm(cc ~ pImm_Eu, census1986)
summary(pImm_Eu86.lm) # R2 = 0.00
plot(cc ~ pImm_Eu, census1986)
abline(pImm_Eu86.lm)

# 1991
pImm_Eu91.lm = lm(cc ~ pImm_Eu, census1991)
summary(pImm_Eu91.lm) # R2 = 0.01***
plot(cc ~ pImm_Eu, census1991)
abline(pImm_Eu91.lm)

# 1996
pImm_Eu96.lm = lm(cc ~ pImm_Eu, census1996)
summary(pImm_Eu96.lm) # R2 = 0.02***
plot(cc ~ pImm_Eu, census1996)
abline(pImm_Eu96.lm)

# 2001
pImm_Eu01.lm = lm(cc ~ pImm_Eu, census2001)
summary(pImm_Eu01.lm) # R2 = 0.05***
plot(cc ~ pImm_Eu, census2001)
abline(pImm_Eu01.lm)

# 2006
pImm_Eu06.lm = lm(cc ~ pImm_Eu, census2006)
summary(pImm_Eu06.lm) # R2 = 0.14***
plot(cc ~ pImm_Eu, census2006)
abline(pImm_Eu06.lm)

# 2011
# No Data

# 2016
pImm_Eu16.lm = lm(cc ~ pImm_Eu, census2016)
summary(pImm_Eu16.lm) # R2 = 0.19***
plot(cc ~ pImm_Eu, census2016)
abline(pImm_Eu16.lm)
###

### Weighted GAM Figures ###
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
pImm_Eu71.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu71.gam) # R2 = 0.16***
gam.check(pImm_Eu71.gam) # Should see convergence, p should not be significant
plot(pImm_Eu71.gam)

pImm_Eu71_pred = data.frame(pImm_Eu = census1971$pImm_Eu, cc = census1971$cc, Pop = census1971$Pop, pred = predict(pImm_Eu71.gam, newdata = census1971))

# Plot
pImm_Eu71.p = ggplot(pImm_Eu71_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.16***", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu71.p

# 1981
# Create GAM model
pImm_Eu81.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(pImm_Eu81.gam) # R2 = 0.02***
gam.check(pImm_Eu81.gam) # Should see convergence, p should not be significant
plot(pImm_Eu81.gam)

pImm_Eu81_pred = data.frame(pImm_Eu = census1981$pImm_Eu, cc = census1981$cc, Pop = census1981$Pop, pred = predict(pImm_Eu81.gam, newdata = census1981))

# Plot
pImm_Eu81.p = ggplot(pImm_Eu81_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.02***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu81.p

# 1986
# Create GAM model
pImm_Eu86.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pImm_Eu86.gam) # R2 = 0.00
gam.check(pImm_Eu86.gam) # Should see convergence, p should not be significant
plot(pImm_Eu86.gam)

pImm_Eu86_pred = data.frame(pImm_Eu = census1986$pImm_Eu, cc = census1986$cc, Pop = census1986$Pop, pred = predict(pImm_Eu86.gam, newdata = census1986))

# Plot
pImm_Eu86.p = ggplot(pImm_Eu86_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.00    ", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu86.p

# 1991
# Create GAM model
pImm_Eu91.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pImm_Eu91.gam) # R2 = 0.03***
gam.check(pImm_Eu91.gam) # Should see convergence, p should not be significant
plot(pImm_Eu91.gam)

pImm_Eu91_pred = data.frame(pImm_Eu = census1991$pImm_Eu, cc = census1991$cc, Pop = census1991$Pop, pred = predict(pImm_Eu91.gam, newdata = census1991))

# Plot
pImm_Eu91.p = ggplot(pImm_Eu91_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.03***", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu91.p

# 1996
# Create GAM model
pImm_Eu96.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_Eu96.gam) # R2 = 0.04***
gam.check(pImm_Eu96.gam) # Should see convergence, p should not be significant
plot(pImm_Eu96.gam)

pImm_Eu96_pred = data.frame(pImm_Eu = census1996$pImm_Eu, cc = census1996$cc, Pop = census1996$Pop, pred = predict(pImm_Eu96.gam, newdata = census1996))

# Plot
pImm_Eu96.p = ggplot(pImm_Eu96_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.04***", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu96.p

# 2001
# Create GAM model
pImm_Eu01.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_Eu01.gam) # R2 = 0.06***
gam.check(pImm_Eu01.gam) # Should see convergence, p should not be significant
plot(pImm_Eu01.gam)

pImm_Eu01_pred = data.frame(pImm_Eu = census2001$pImm_Eu, cc = census2001$cc, Pop = census2001$Pop, pred = predict(pImm_Eu01.gam, newdata = census2001))

# Plot
pImm_Eu01.p = ggplot(pImm_Eu01_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.06***", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu01.p

# 2006
# Create GAM model
pImm_Eu06.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_Eu06.gam) # R2 = 0.21***
gam.check(pImm_Eu06.gam) # Should see convergence, p should not be significant
plot(pImm_Eu06.gam)

pImm_Eu06_pred = data.frame(pImm_Eu = census2006$pImm_Eu, cc = census2006$cc, Pop = census2006$Pop, pred = predict(pImm_Eu06.gam, newdata = census2006))

# Plot
pImm_Eu06.p = ggplot(pImm_Eu06_pred, aes(x = pImm_Eu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.21***", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu06.p

# 2011
# Create blank plot
pImm_Eu11.p = ggplot() +
  scale_x_continuous(name = "% Immigrants from Europe", limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  annotate(geom = "text", x = 50, y = 26.5, label = "Data not collected in Census", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu11.p

# 2016
# Create GAM model
pImm_Eu16.gam = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_Eu16.gam) # R2 = 0.37***
gam.check(pImm_Eu16.gam) # Should see convergence, p should not be significant
plot(pImm_Eu16.gam)

pImm_Eu16_pred = data.frame(pImm_Eu = census2016$pImm_Eu, cc = census2016$cc, Pop = census2016$Pop, pred = predict(pImm_Eu16.gam, newdata = census2016))

# Plot
pImm_Eu16.p = ggplot(pImm_Eu16_pred, aes(x = pImm_Eu, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.37***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu16.p

#tiff("pImm_Eu_CC_Peel_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_Eu71.p | pImm_Eu81.p | pImm_Eu86.p) /
  (pImm_Eu91.p | pImm_Eu96.p | pImm_Eu01.p) /
  (pImm_Eu06.p | pImm_Eu11.p | pImm_Eu16.p)
#dev.off()
###

# Peel - Combined Figure
# Find weighted mean location (closest real point) on predicted line
pImm_Eu_mn_row71 = first(which(abs(pImm_Eu71_pred$pImm_Eu - weighted.mean(x = pImm_Eu71_pred$pImm_Eu, w = pImm_Eu71_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu71_pred$pImm_Eu - weighted.mean(x = pImm_Eu71_pred$pImm_Eu, w = pImm_Eu71_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu71_pred[pImm_Eu_mn_row71,]

pImm_Eu_mn_row81 = first(which(abs(pImm_Eu81_pred$pImm_Eu - weighted.mean(x = pImm_Eu81_pred$pImm_Eu, w = pImm_Eu81_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu81_pred$pImm_Eu - weighted.mean(x = pImm_Eu81_pred$pImm_Eu, w = pImm_Eu81_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu81_pred[pImm_Eu_mn_row81,]

pImm_Eu_mn_row86 = first(which(abs(pImm_Eu86_pred$pImm_Eu - weighted.mean(x = pImm_Eu86_pred$pImm_Eu, w = pImm_Eu86_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu86_pred$pImm_Eu - weighted.mean(x = pImm_Eu86_pred$pImm_Eu, w = pImm_Eu86_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu86_pred[pImm_Eu_mn_row86,]

pImm_Eu_mn_row91 = first(which(abs(pImm_Eu91_pred$pImm_Eu - weighted.mean(x = pImm_Eu91_pred$pImm_Eu, w = pImm_Eu91_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu91_pred$pImm_Eu - weighted.mean(x = pImm_Eu91_pred$pImm_Eu, w = pImm_Eu91_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu91_pred[pImm_Eu_mn_row91,]

pImm_Eu_mn_row96 = first(which(abs(pImm_Eu96_pred$pImm_Eu - weighted.mean(x = pImm_Eu96_pred$pImm_Eu, w = pImm_Eu96_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu96_pred$pImm_Eu - weighted.mean(x = pImm_Eu96_pred$pImm_Eu, w = pImm_Eu96_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu96_pred[pImm_Eu_mn_row96,]

pImm_Eu_mn_row01 = first(which(abs(pImm_Eu01_pred$pImm_Eu - weighted.mean(x = pImm_Eu01_pred$pImm_Eu, w = pImm_Eu01_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu01_pred$pImm_Eu - weighted.mean(x = pImm_Eu01_pred$pImm_Eu, w = pImm_Eu01_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu01_pred[pImm_Eu_mn_row01,]

pImm_Eu_mn_row06 = first(which(abs(pImm_Eu06_pred$pImm_Eu - weighted.mean(x = pImm_Eu06_pred$pImm_Eu, w = pImm_Eu06_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu06_pred$pImm_Eu - weighted.mean(x = pImm_Eu06_pred$pImm_Eu, w = pImm_Eu06_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu06_pred[pImm_Eu_mn_row06,]

pImm_Eu_mn_row16 = first(which(abs(pImm_Eu16_pred$pImm_Eu - weighted.mean(x = pImm_Eu16_pred$pImm_Eu, w = pImm_Eu16_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_Eu16_pred$pImm_Eu - weighted.mean(x = pImm_Eu16_pred$pImm_Eu, w = pImm_Eu16_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu16_pred[pImm_Eu_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

# Plot
xmin = 0
xmax = 75
ymin = 0
ymax = 45

pImm_Eu_com.p = ggplot() +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1971"), data = pImm_Eu71_pred) + 
  geom_point(aes(x = pImm_Eu71_pred$pImm_Eu[pImm_Eu_mn_row71], y = pImm_Eu71_pred$pred[pImm_Eu_mn_row71]), color = "black") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "1981"), data = pImm_Eu81_pred) + 
  geom_point(aes(x = pImm_Eu81_pred$pImm_Eu[pImm_Eu_mn_row81], y = pImm_Eu81_pred$pred[pImm_Eu_mn_row81]), color = "sienna") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "1986"), data = pImm_Eu86_pred) + 
  geom_point(aes(x = pImm_Eu86_pred$pImm_Eu[pImm_Eu_mn_row86], y = pImm_Eu86_pred$pred[pImm_Eu_mn_row86]), color = "purple4") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "1991"), data = subset(pImm_Eu91_pred, pImm_Eu < 51)) + 
  geom_point(aes(x = pImm_Eu91_pred$pImm_Eu[pImm_Eu_mn_row91], y = pImm_Eu91_pred$pred[pImm_Eu_mn_row91]), color = "blue") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "1996"), data = pImm_Eu96_pred) + 
  geom_point(aes(x = pImm_Eu96_pred$pImm_Eu[pImm_Eu_mn_row96], y = pImm_Eu96_pred$pred[pImm_Eu_mn_row96]), color = "steelblue2") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "2001"), data = subset(pImm_Eu01_pred, pImm_Eu < 51)) + 
  geom_point(aes(x = pImm_Eu01_pred$pImm_Eu[pImm_Eu_mn_row01], y = pImm_Eu01_pred$pred[pImm_Eu_mn_row01]), color = "green4") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "2006"), data = pImm_Eu06_pred) + 
  geom_point(aes(x = pImm_Eu06_pred$pImm_Eu[pImm_Eu_mn_row06], y = pImm_Eu06_pred$pred[pImm_Eu_mn_row06]), color = "gold2") + 
  geom_line(aes(x = pImm_Eu, y = pred, col = "2016"), data = pImm_Eu16_pred) +
  geom_point(aes(x = pImm_Eu16_pred$pImm_Eu[pImm_Eu_mn_row16], y = pImm_Eu16_pred$pred[pImm_Eu_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 63, y = 42, label = "R² = 0.16***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 63, y = 40, label = "R² = 0.02***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 63, y = 38, label = "R² = 0.00    ", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 63, y = 36, label = "R² = 0.03***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 63, y = 34, label = "R² = 0.04***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 63, y = 32, label = "R² = 0.06***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 63, y = 30, label = "R² = 0.21***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 63, y = 28, label = "R² = 0.37***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "% Immigrants from Europe", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.4, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pImm_Eu_com.p

#tiff("pImm_Eu_CC_Peel_combined1.tif", units = "cm", width = 10, height = 10, res = 300)
pImm_Eu_com.p
#dev.off()

# By Municipality - Separate Figures
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# 1971
# Create GAM models
pImm_Eu71.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu71.gam_m) # R2 = 0.25***
gam.check(pImm_Eu71.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu71.gam_m)

pImm_Eu71_pred_m = data.frame(pImm_Eu = subset(census1971$pImm_Eu, census1971$mun == "Mississauga"), 
                           cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                           Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                           pred = predict(pImm_Eu71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

pImm_Eu71.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1971, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu71.gam_b) # R2 = 0.12**
gam.check(pImm_Eu71.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu71.gam_b)

pImm_Eu71_pred_b = data.frame(pImm_Eu = subset(census1971$pImm_Eu, census1971$mun == "Brampton"), 
                           cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                           Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                           pred = predict(pImm_Eu71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
pImm_Eu71_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.25***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.12**  ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu71_mun.p

# 1981
# Create GAM models
pImm_Eu81.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu81.gam_m) # R2 = 0.03***
gam.check(pImm_Eu81.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu81.gam_m)

pImm_Eu81_pred_m = data.frame(pImm_Eu = subset(census1981$pImm_Eu, census1981$mun == "Mississauga"), 
                           cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                           Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                           pred = predict(pImm_Eu81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

pImm_Eu81.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu81.gam_b) # R2 = 0.00
gam.check(pImm_Eu81.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu81.gam_b)

pImm_Eu81_pred_b = data.frame(pImm_Eu = subset(census1981$pImm_Eu, census1981$mun == "Brampton"), 
                           cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                           Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                           pred = predict(pImm_Eu81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
pImm_Eu81_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.03***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.00    ", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu81_mun.p

# 1986
# Create GAM models
pImm_Eu86.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu86.gam_m) # R2 = 0.00
gam.check(pImm_Eu86.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu86.gam_m)

pImm_Eu86_pred_m = data.frame(pImm_Eu = subset(census1986$pImm_Eu, census1986$mun == "Mississauga"), 
                           cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                           Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                           pred = predict(pImm_Eu86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

pImm_Eu86.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu86.gam_b) # R2 = 0.00
gam.check(pImm_Eu86.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu86.gam_b)

pImm_Eu86_pred_b = data.frame(pImm_Eu = subset(census1986$pImm_Eu, census1986$mun == "Brampton"), 
                           cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                           Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                           pred = predict(pImm_Eu86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
pImm_Eu86_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.00    ", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.00    ", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu86_mun.p

# 1991
# Create GAM models
pImm_Eu91.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu91.gam_m) # R2 = 0.02***
gam.check(pImm_Eu91.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu91.gam_m)

pImm_Eu91_pred_m = data.frame(pImm_Eu = subset(census1991$pImm_Eu, census1991$mun == "Mississauga"), 
                           cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                           pred = predict(pImm_Eu91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

pImm_Eu91.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu91.gam_b) # R2 = 0.14***
gam.check(pImm_Eu91.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu91.gam_b)

pImm_Eu91_pred_b = data.frame(pImm_Eu = subset(census1991$pImm_Eu, census1991$mun == "Brampton"), 
                           cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                           pred = predict(pImm_Eu91.gam_b, newdata = subset(census1991, mun == "Brampton")))

pImm_Eu91.gam_c = gam(cc ~ s(pImm_Eu, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu91.gam_c) # R2 = 0.09
gam.check(pImm_Eu91.gam_c) # Should see convergence, p should not be significant
plot(pImm_Eu91.gam_c)

pImm_Eu91_pred_c = data.frame(pImm_Eu = subset(census1991$pImm_Eu, census1991$mun == "Caledon"), 
                           cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                           pred = predict(pImm_Eu91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
pImm_Eu91_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.02***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.14***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.09    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu91_mun.p

# 1996
# Create GAM models
pImm_Eu96.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu96.gam_m) # R2 = 0.02***
gam.check(pImm_Eu96.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu96.gam_m)

pImm_Eu96_pred_m = data.frame(pImm_Eu = subset(census1996$pImm_Eu, census1996$mun == "Mississauga"), 
                           cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                           pred = predict(pImm_Eu96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

pImm_Eu96.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu96.gam_b) # R2 = 0.04***
gam.check(pImm_Eu96.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu96.gam_b)

pImm_Eu96_pred_b = data.frame(pImm_Eu = subset(census1996$pImm_Eu, census1996$mun == "Brampton"), 
                           cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                           pred = predict(pImm_Eu96.gam_b, newdata = subset(census1996, mun == "Brampton")))

pImm_Eu96.gam_c = gam(cc ~ s(pImm_Eu, bs = "cs", k =  32), data = subset(census1996, mun == "Caledon"), # k = (45 =  of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu96.gam_c) # R2 = 0.00
gam.check(pImm_Eu96.gam_c) # Should see convergence, p should not be significant
plot(pImm_Eu96.gam_c)

pImm_Eu96_pred_c = data.frame(pImm_Eu = subset(census1996$pImm_Eu, census1996$mun == "Caledon"), 
                           cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                           pred = predict(pImm_Eu96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
pImm_Eu96_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.02***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.04***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu96_mun.p

# 2001
# Create GAM models
pImm_Eu01.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2001, mun == "Mississauga"),
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu01.gam_m) # R2 = 0.04***
gam.check(pImm_Eu01.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu01.gam_m)

pImm_Eu01_pred_m = data.frame(pImm_Eu = subset(census2001$pImm_Eu, census2001$mun == "Mississauga"), 
                           cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                           pred = predict(pImm_Eu01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

pImm_Eu01.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu01.gam_b) # R2 = 0.10***
gam.check(pImm_Eu01.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu01.gam_b)

pImm_Eu01_pred_b = data.frame(pImm_Eu = subset(census2001$pImm_Eu, census2001$mun == "Brampton"), 
                           cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                           pred = predict(pImm_Eu01.gam_b, newdata = subset(census2001, mun == "Brampton")))

pImm_Eu01.gam_c = gam(cc ~ s(pImm_Eu, bs = "cs", k = 40), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu01.gam_c) # R2 = 0.05
gam.check(pImm_Eu01.gam_c) # Should see convergence, p should not be significant
plot(pImm_Eu01.gam_c)

pImm_Eu01_pred_c = data.frame(pImm_Eu = subset(census2001$pImm_Eu, census2001$mun == "Caledon"), 
                           cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                           pred = predict(pImm_Eu01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
pImm_Eu01_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.04***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.10***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.05    ", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_Eu01_mun.p

# 2006
# Create GAM models
pImm_Eu06.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"),
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu06.gam_m) # R2 = 0.16***
gam.check(pImm_Eu06.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu06.gam_m)

pImm_Eu06_pred_m = data.frame(pImm_Eu = subset(census2006$pImm_Eu, census2006$mun == "Mississauga"), 
                           cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                           pred = predict(pImm_Eu06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

pImm_Eu06.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu06.gam_b) # R2 = 0.22***
gam.check(pImm_Eu06.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu06.gam_b)

pImm_Eu06_pred_b = data.frame(pImm_Eu = subset(census2006$pImm_Eu, census2006$mun == "Brampton"), 
                           cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                           pred = predict(pImm_Eu06.gam_b, newdata = subset(census2006, mun == "Brampton")))

pImm_Eu06.gam_c = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2006, mun == "Caledon"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu06.gam_c) # R2 = 0.00
gam.check(pImm_Eu06.gam_c) # Should see convergence, p should not be significant
plot(pImm_Eu06.gam_c)

pImm_Eu06_pred_c = data.frame(pImm_Eu = subset(census2006$pImm_Eu, census2006$mun == "Caledon"), 
                           cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                           pred = predict(pImm_Eu06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
pImm_Eu06_mun.p = ggplot() +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.16***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.22***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu06_mun.p

# 2011
pImm_Eu11_mun.p = pImm_Eu11.p
pImm_Eu11_mun.p

# 2016
# Create GAM models
pImm_Eu16.gam_m = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu16.gam_m) # R2 = 0.29***
gam.check(pImm_Eu16.gam_m) # Should see convergence, p should not be significant
plot(pImm_Eu16.gam_m)

pImm_Eu16_pred_m = data.frame(pImm_Eu = subset(census2016$pImm_Eu, census2016$mun == "Mississauga"), 
                           cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                           pred = predict(pImm_Eu16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

pImm_Eu16.gam_b = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu16.gam_b) # R2 = 0.37***
gam.check(pImm_Eu16.gam_b) # Should see convergence, p should not be significant
plot(pImm_Eu16.gam_b)

pImm_Eu16_pred_b = data.frame(pImm_Eu = subset(census2016$pImm_Eu, census2016$mun == "Brampton"), 
                           cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                           pred = predict(pImm_Eu16.gam_b, newdata = subset(census2016, mun == "Brampton")))

pImm_Eu16.gam_c = gam(cc ~ s(pImm_Eu, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_Eu16.gam_c) # R2 = 0.06
gam.check(pImm_Eu16.gam_c) # Should see convergence, p should not be significant
plot(pImm_Eu16.gam_c)

pImm_Eu16_pred_c = data.frame(pImm_Eu = subset(census2016$pImm_Eu, census2016$mun == "Caledon"), 
                           cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                           pred = predict(pImm_Eu16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
pImm_Eu16_mun.p = ggplot(pImm_Eu16_pred, aes(x = pImm_Eu, y = cc, size = Pop)) + 
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_Eu, y = cc, size = Pop), data = pImm_Eu16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_Eu, y = pred), data = pImm_Eu16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.29***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.37***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.06    ", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu16_mun.p

#tiff("pImm_Eu_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_Eu71_mun.p | pImm_Eu81_mun.p | pImm_Eu86_mun.p) /
  (pImm_Eu91_mun.p | pImm_Eu96_mun.p | pImm_Eu01_mun.p) /
  (pImm_Eu06_mun.p | pImm_Eu11_mun.p | pImm_Eu16_mun.p)
#dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 75
ymin = 0
ymax = 45

pImm_Eu_com.p # Already built above

# Mississauga
# Find weighted mean location (closest real point) on predicted line
pImm_Eu_m_mn_row71 = first(which(abs(pImm_Eu71_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu71_pred_m$pImm_Eu, w = pImm_Eu71_pred_m$Pop, na.rm = TRUE)) == 
                                     min(abs(pImm_Eu71_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu71_pred_m$pImm_Eu, w = pImm_Eu71_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu71_pred_m[pImm_Eu_m_mn_row71,]

pImm_Eu_m_mn_row81 = first(which(abs(pImm_Eu81_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu81_pred_m$pImm_Eu, w = pImm_Eu81_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu81_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu81_pred_m$pImm_Eu, w = pImm_Eu81_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu81_pred_m[pImm_Eu_m_mn_row81,]

pImm_Eu_m_mn_row86 = first(which(abs(pImm_Eu86_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu86_pred_m$pImm_Eu, w = pImm_Eu86_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu86_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu86_pred_m$pImm_Eu, w = pImm_Eu86_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu86_pred_m[pImm_Eu_m_mn_row86,]

pImm_Eu_m_mn_row91 = first(which(abs(pImm_Eu91_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu91_pred_m$pImm_Eu, w = pImm_Eu91_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu91_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu91_pred_m$pImm_Eu, w = pImm_Eu91_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu91_pred_m[pImm_Eu_m_mn_row91,]

pImm_Eu_m_mn_row96 = first(which(abs(pImm_Eu96_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu96_pred_m$pImm_Eu, w = pImm_Eu96_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu96_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu96_pred_m$pImm_Eu, w = pImm_Eu96_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu96_pred_m[pImm_Eu_m_mn_row96,]

pImm_Eu_m_mn_row01 = first(which(abs(pImm_Eu01_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu01_pred_m$pImm_Eu, w = pImm_Eu01_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu01_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu01_pred_m$pImm_Eu, w = pImm_Eu01_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu01_pred_m[pImm_Eu_m_mn_row01,]

pImm_Eu_m_mn_row06 = first(which(abs(pImm_Eu06_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu06_pred_m$pImm_Eu, w = pImm_Eu06_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu06_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu06_pred_m$pImm_Eu, w = pImm_Eu06_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu06_pred_m[pImm_Eu_m_mn_row06,]

pImm_Eu_m_mn_row16 = first(which(abs(pImm_Eu16_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu16_pred_m$pImm_Eu, w = pImm_Eu16_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu16_pred_m$pImm_Eu - weighted.mean(x = pImm_Eu16_pred_m$pImm_Eu, w = pImm_Eu16_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu16_pred_m[pImm_Eu_m_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pImm_Eu_com_m.p = ggplot() +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1971"), data = pImm_Eu71_pred_m) + 
  geom_point(aes(x = pImm_Eu71_pred_m$pImm_Eu[pImm_Eu_m_mn_row71], y = pImm_Eu71_pred_m$pred[pImm_Eu_m_mn_row71]), color = "black") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1981"), data = pImm_Eu81_pred_m) + 
  geom_point(aes(x = pImm_Eu81_pred_m$pImm_Eu[pImm_Eu_m_mn_row81], y = pImm_Eu81_pred_m$pred[pImm_Eu_m_mn_row81]), color = "sienna") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1986"), data = pImm_Eu86_pred_m) + 
  geom_point(aes(x = pImm_Eu86_pred_m$pImm_Eu[pImm_Eu_m_mn_row86], y = pImm_Eu86_pred_m$pred[pImm_Eu_m_mn_row86]), color = "purple4") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1991"), data = pImm_Eu91_pred_m) + 
  geom_point(aes(x = pImm_Eu91_pred_m$pImm_Eu[pImm_Eu_m_mn_row91], y = pImm_Eu91_pred_m$pred[pImm_Eu_m_mn_row91]), color = "blue") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1996"), data = pImm_Eu96_pred_m) + 
  geom_point(aes(x = pImm_Eu96_pred_m$pImm_Eu[pImm_Eu_m_mn_row96], y = pImm_Eu96_pred_m$pred[pImm_Eu_m_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2001"), data = pImm_Eu01_pred_m) + 
  geom_point(aes(x = pImm_Eu01_pred_m$pImm_Eu[pImm_Eu_m_mn_row01], y = pImm_Eu01_pred_m$pred[pImm_Eu_m_mn_row01]), color = "green4") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2006"), data = pImm_Eu06_pred_m) + 
  geom_point(aes(x = pImm_Eu06_pred_m$pImm_Eu[pImm_Eu_m_mn_row06], y = pImm_Eu06_pred_m$pred[pImm_Eu_m_mn_row06]), color = "gold2") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2016"), data = pImm_Eu16_pred_m) +
  geom_point(aes(x = pImm_Eu16_pred_m$pImm_Eu[pImm_Eu_m_mn_row16], y = pImm_Eu16_pred_m$pred[pImm_Eu_m_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 55, y = 42, label = "R² = 0.25***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 55, y = 40, label = "R² = 0.03***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 55, y = 38, label = "R² = 0.00    ", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 55, y = 36, label = "R² = 0.02***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 55, y = 34, label = "R² = 0.02***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 55, y = 32, label = "R² = 0.04***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 55, y = 30, label = "R² = 0.16***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 55, y = 28, label = "R² = 0.29***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 10, y = 41, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
pImm_Eu_com_m.p

# Brampton

# Find weighted mean location (closest real point) on predicted line
pImm_Eu_b_mn_row71 = first(which(abs(pImm_Eu71_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu71_pred_b$pImm_Eu, w = pImm_Eu71_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu71_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu71_pred_b$pImm_Eu, w = pImm_Eu71_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu71_pred_b[pImm_Eu_b_mn_row71,]

pImm_Eu_b_mn_row81 = first(which(abs(pImm_Eu81_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu81_pred_b$pImm_Eu, w = pImm_Eu81_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu81_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu81_pred_b$pImm_Eu, w = pImm_Eu81_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu81_pred_b[pImm_Eu_b_mn_row81,]

pImm_Eu_b_mn_row86 = first(which(abs(pImm_Eu86_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu86_pred_b$pImm_Eu, w = pImm_Eu86_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu86_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu86_pred_b$pImm_Eu, w = pImm_Eu86_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu86_pred_b[pImm_Eu_b_mn_row86,]

pImm_Eu_b_mn_row91 = first(which(abs(pImm_Eu91_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu91_pred_b$pImm_Eu, w = pImm_Eu91_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu91_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu91_pred_b$pImm_Eu, w = pImm_Eu91_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu91_pred_b[pImm_Eu_b_mn_row91,]

pImm_Eu_b_mn_row96 = first(which(abs(pImm_Eu96_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu96_pred_b$pImm_Eu, w = pImm_Eu96_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu96_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu96_pred_b$pImm_Eu, w = pImm_Eu96_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu96_pred_b[pImm_Eu_b_mn_row96,]

pImm_Eu_b_mn_row01 = first(which(abs(pImm_Eu01_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu01_pred_b$pImm_Eu, w = pImm_Eu01_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu01_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu01_pred_b$pImm_Eu, w = pImm_Eu01_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu01_pred_b[pImm_Eu_b_mn_row01,]

pImm_Eu_b_mn_row06 = first(which(abs(pImm_Eu06_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu06_pred_b$pImm_Eu, w = pImm_Eu06_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu06_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu06_pred_b$pImm_Eu, w = pImm_Eu06_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu06_pred_b[pImm_Eu_b_mn_row06,]

pImm_Eu_b_mn_row16 = first(which(abs(pImm_Eu16_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu16_pred_b$pImm_Eu, w = pImm_Eu16_pred_b$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu16_pred_b$pImm_Eu - weighted.mean(x = pImm_Eu16_pred_b$pImm_Eu, w = pImm_Eu16_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu16_pred_b[pImm_Eu_b_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pImm_Eu_com_b.p = ggplot() +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1971"), data = pImm_Eu71_pred_b) + 
  geom_point(aes(x = pImm_Eu71_pred_b$pImm_Eu[pImm_Eu_b_mn_row71], y = pImm_Eu71_pred_b$pred[pImm_Eu_b_mn_row71]), color = "black") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1981"), data = pImm_Eu81_pred_b) + 
  geom_point(aes(x = pImm_Eu81_pred_b$pImm_Eu[pImm_Eu_b_mn_row81], y = pImm_Eu81_pred_b$pred[pImm_Eu_b_mn_row81]), color = "sienna") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1986"), data = pImm_Eu86_pred_b) + 
  geom_point(aes(x = pImm_Eu86_pred_b$pImm_Eu[pImm_Eu_b_mn_row86], y = pImm_Eu86_pred_b$pred[pImm_Eu_b_mn_row86]), color = "purple4") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1991"), data = subset(pImm_Eu91_pred_b, pImm_Eu < 50)) + 
  geom_point(aes(x = pImm_Eu91_pred_b$pImm_Eu[pImm_Eu_b_mn_row91], y = pImm_Eu91_pred_b$pred[pImm_Eu_b_mn_row91]), color = "blue") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1996"), data = pImm_Eu96_pred_b) +
  geom_point(aes(x = pImm_Eu96_pred_b$pImm_Eu[pImm_Eu_b_mn_row96], y = pImm_Eu96_pred_b$pred[pImm_Eu_b_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2001"), data = subset(pImm_Eu01_pred_b, pImm_Eu < 50)) + 
  geom_point(aes(x = pImm_Eu01_pred_b$pImm_Eu[pImm_Eu_b_mn_row01], y = pImm_Eu01_pred_b$pred[pImm_Eu_b_mn_row01]), color = "green4") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2006"), data = pImm_Eu06_pred_b) + 
  geom_point(aes(x = pImm_Eu06_pred_b$pImm_Eu[pImm_Eu_b_mn_row06], y = pImm_Eu06_pred_b$pred[pImm_Eu_b_mn_row06]), color = "gold2") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2016"), data = pImm_Eu16_pred_b) +
  geom_point(aes(x = pImm_Eu16_pred_b$pImm_Eu[pImm_Eu_b_mn_row16], y = pImm_Eu16_pred_b$pred[pImm_Eu_b_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 55, y = 42, label = "R² = 0.12**  ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 55, y = 40, label = "R² = 0.00    ", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 55, y = 38, label = "R² = 0.00    ", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 55, y = 36, label = "R² = 0.14***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 55, y = 34, label = "R² = 0.04***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 55, y = 32, label = "R² = 0.10***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 55, y = 30, label = "R² = 0.22***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 55, y = 28, label = "R² = 0.37***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "% Immigrants from Europe", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu_com_b.p

# Caledon
# Find weighted mean location (closest real point) on predicted line
pImm_Eu_c_mn_row91 = first(which(abs(pImm_Eu91_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu91_pred_c$pImm_Eu, w = pImm_Eu91_pred_c$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu91_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu91_pred_c$pImm_Eu, w = pImm_Eu91_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu91_pred_c[pImm_Eu_c_mn_row91,]

pImm_Eu_c_mn_row96 = first(which(abs(pImm_Eu96_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu96_pred_c$pImm_Eu, w = pImm_Eu96_pred_c$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu96_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu96_pred_c$pImm_Eu, w = pImm_Eu96_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu96_pred_c[pImm_Eu_c_mn_row96,]

pImm_Eu_c_mn_row01 = first(which(abs(pImm_Eu01_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu01_pred_c$pImm_Eu, w = pImm_Eu01_pred_c$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu01_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu01_pred_c$pImm_Eu, w = pImm_Eu01_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu01_pred_c[pImm_Eu_c_mn_row01,]

pImm_Eu_c_mn_row06 = first(which(abs(pImm_Eu06_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu06_pred_c$pImm_Eu, w = pImm_Eu06_pred_c$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu06_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu06_pred_c$pImm_Eu, w = pImm_Eu06_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu06_pred_c[pImm_Eu_c_mn_row06,]

pImm_Eu_c_mn_row16 = first(which(abs(pImm_Eu16_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu16_pred_c$pImm_Eu, w = pImm_Eu16_pred_c$Pop, na.rm = TRUE)) == 
                                   min(abs(pImm_Eu16_pred_c$pImm_Eu - weighted.mean(x = pImm_Eu16_pred_c$pImm_Eu, w = pImm_Eu16_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_Eu16_pred_c[pImm_Eu_c_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pImm_Eu_com_c.p = ggplot() +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1991"), data = pImm_Eu91_pred_c) + 
  geom_point(aes(x = pImm_Eu91_pred_c$pImm_Eu[pImm_Eu_c_mn_row91], y = pImm_Eu91_pred_c$pred[pImm_Eu_c_mn_row91]), color = "blue") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "1996"), data = pImm_Eu96_pred_c) + 
  geom_point(aes(x = pImm_Eu96_pred_c$pImm_Eu[pImm_Eu_c_mn_row96], y = pImm_Eu96_pred_c$pred[pImm_Eu_c_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2001"), data = pImm_Eu01_pred_c) + 
  geom_point(aes(x = pImm_Eu01_pred_c$pImm_Eu[pImm_Eu_c_mn_row01], y = pImm_Eu01_pred_c$pred[pImm_Eu_c_mn_row01]), color = "green4") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2006"), data = pImm_Eu06_pred_c) + 
  geom_point(aes(x = pImm_Eu06_pred_c$pImm_Eu[pImm_Eu_c_mn_row06], y = pImm_Eu06_pred_c$pred[pImm_Eu_c_mn_row06]), color = "gold2") +
  geom_line(aes(x = pImm_Eu, y = pred, col = "2016"), data = pImm_Eu16_pred_c) +
  geom_point(aes(x = pImm_Eu16_pred_c$pImm_Eu[pImm_Eu_c_mn_row16], y = pImm_Eu16_pred_c$pred[pImm_Eu_c_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 55, y = 36, label = "R² = 0.09    ", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 55, y = 34, label = "R² = 0.00    ", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 55, y = 32, label = "R² = 0.05    ", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 55, y = 30, label = "R² = 0.00    ", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 55, y = 28, label = "R² = 0.06    ", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_Eu_com_c.p

#tiff("pImm_Eu_CC_bymun_combined1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_Eu_com.p + annotate(geom = "text", x = 3, y = 40, label = "A", fontface = "bold", size = 6)) /
  (pImm_Eu_com_m.p | pImm_Eu_com_b.p | pImm_Eu_com_c.p)
#dev.off()
##### 

##### CC by year - see individual relationships - % Immigrants from Africa #####
### Linear model ###
# 1971
pImm_Af71.lm = lm(cc ~ pImm_Af, census1971)
summary(pImm_Af71.lm) # R2 = 0.03*
plot(cc ~ pImm_Af, census1971)
abline(pImm_Af71.lm)

# 1981
pImm_Af81.lm = lm(cc ~ pImm_Af, census1981)
summary(pImm_Af81.lm) # R2 = 0.02**
plot(cc ~ pImm_Af, census1981)
abline(pImm_Af81.lm)

# 1986
pImm_Af86.lm = lm(cc ~ pImm_Af, census1986)
summary(pImm_Af86.lm) # R2 = 0.02***
plot(cc ~ pImm_Af, census1986)
abline(pImm_Af86.lm)

# 1991
pImm_Af91.lm = lm(cc ~ pImm_Af, census1991)
summary(pImm_Af91.lm) # R2 = 0.02***
plot(cc ~ pImm_Af, census1991)
abline(pImm_Af91.lm)

# 1996
pImm_Af96.lm = lm(cc ~ pImm_Af, census1996)
summary(pImm_Af96.lm) # R2 = 0.03***
plot(cc ~ pImm_Af, census1996)
abline(pImm_Af96.lm)

# 2001
pImm_Af01.lm = lm(cc ~ pImm_Af, census2001)
summary(pImm_Af01.lm) # R2 = 0.00*
plot(cc ~ pImm_Af, census2001)
abline(pImm_Af01.lm)

# 2006
pImm_Af06.lm = lm(cc ~ pImm_Af, census2006)
summary(pImm_Af06.lm) # R2 = 0.06***
plot(cc ~ pImm_Af, census2006)
abline(pImm_Af06.lm)

# 2011
# No Data

# 2016
pImm_Af16.lm = lm(cc ~ pImm_Af, census2016)
summary(pImm_Af16.lm) # R2 = 0.06***
plot(cc ~ pImm_Af, census2016)
abline(pImm_Af16.lm)
# Not much there
#####

##### CC by year - see individual relationships - % Immigrants from Asia #####
### Linear model ###
# 1971
pImm_As71.lm = lm(cc ~ pImm_As, census1971)
summary(pImm_As71.lm) # R2 = 0.07***
plot(cc ~ pImm_As, census1971)
abline(pImm_As71.lm)

# 1981
pImm_As81.lm = lm(cc ~ pImm_As, census1981)
summary(pImm_As81.lm) # R2 = 0.12***
plot(cc ~ pImm_As, census1981)
abline(pImm_As81.lm)

# 1986
pImm_As86.lm = lm(cc ~ pImm_As, census1986)
summary(pImm_As86.lm) # R2 = 0.11***
plot(cc ~ pImm_As, census1986)
abline(pImm_As86.lm)

# 1991
pImm_As91.lm = lm(cc ~ pImm_As, census1991)
summary(pImm_As91.lm) # R2 = 0.17***
plot(cc ~ pImm_As, census1991)
abline(pImm_As91.lm)

# 1996
pImm_As96.lm = lm(cc ~ pImm_As, census1996)
summary(pImm_As96.lm) # R2 = 0.19***
plot(cc ~ pImm_As, census1996)
abline(pImm_As96.lm)

# 2001
pImm_As01.lm = lm(cc ~ pImm_As, census2001)
summary(pImm_As01.lm) # R2 = 0.20***
plot(cc ~ pImm_As, census2001)
abline(pImm_As01.lm)

# 2006
pImm_As06.lm = lm(cc ~ pImm_As, census2006)
summary(pImm_As06.lm) # R2 = 0.31***
plot(cc ~ pImm_As, census2006)
abline(pImm_As06.lm)

# 2011
# No Data

# 2016
pImm_As16.lm = lm(cc ~ pImm_As, census2016)
summary(pImm_As16.lm) # R2 = 0.37***
plot(cc ~ pImm_As, census2016)
abline(pImm_As16.lm)
# Not much there
##### 

##### CC by year - see individual relationships - % Immigrants from not Europe #####
### Linear model ###
# 1971
pImm_notEu71.lm = lm(cc ~ pImm_notEu, census1971)
summary(pImm_notEu71.lm) # R2 = 0.07***
plot(cc ~ pImm_notEu, census1971)
abline(pImm_notEu71.lm)

# 1981
pImm_notEu81.lm = lm(cc ~ pImm_notEu, census1981)
summary(pImm_notEu81.lm) # R2 = 0.18***
plot(cc ~ pImm_notEu, census1981)
abline(pImm_notEu81.lm)

# 1986
pImm_notEu86.lm = lm(cc ~ pImm_notEu, census1986)
summary(pImm_notEu86.lm) # R2 = 0.15***
plot(cc ~ pImm_notEu, census1986)
abline(pImm_notEu86.lm)

# 1991
pImm_notEu91.lm = lm(cc ~ pImm_notEu, census1991)
summary(pImm_notEu91.lm) # R2 = 0.24***
plot(cc ~ pImm_notEu, census1991)
abline(pImm_notEu91.lm)

# 1996
pImm_notEu96.lm = lm(cc ~ pImm_notEu, census1996)
summary(pImm_notEu96.lm) # R2 = 0.27***
plot(cc ~ pImm_notEu, census1996)
abline(pImm_notEu96.lm)

# 2001
pImm_notEu01.lm = lm(cc ~ pImm_notEu, census2001)
summary(pImm_notEu01.lm) # R2 = 0.25***
plot(cc ~ pImm_notEu, census2001)
abline(pImm_notEu01.lm)

# 2006
pImm_notEu06.lm = lm(cc ~ pImm_notEu, census2006)
summary(pImm_notEu06.lm) # R2 = 0.41***
plot(cc ~ pImm_notEu, census2006)
abline(pImm_notEu06.lm)

# 2011
# No Data

# 2016
pImm_notEu16.lm = lm(cc ~ pImm_notEu, census2016)
summary(pImm_notEu16.lm) # R2 = 0.47***
plot(cc ~ pImm_notEu, census2016)
abline(pImm_notEu16.lm)

### Weighted GAM Figures ###
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
pImm_notEu71.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu71.gam) # R2 = 0.04**
gam.check(pImm_notEu71.gam) # Should see convergence, p should not be significant
plot(pImm_notEu71.gam)

pImm_notEu71_pred = data.frame(pImm_notEu = census1971$pImm_notEu, cc = census1971$cc, Pop = census1971$Pop, pred = predict(pImm_notEu71.gam, newdata = census1971))

# Plot
pImm_notEu71.p = ggplot(pImm_notEu71_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.04**  ", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu71.p

# 1981
# Create GAM model
pImm_notEu81.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(pImm_notEu81.gam) # R2 = 0.21***
gam.check(pImm_notEu81.gam) # Should see convergence, p should not be significant
plot(pImm_notEu81.gam)

pImm_notEu81_pred = data.frame(pImm_notEu = census1981$pImm_notEu, cc = census1981$cc, Pop = census1981$Pop, pred = predict(pImm_notEu81.gam, newdata = census1981))

# Plot
pImm_notEu81.p = ggplot(pImm_notEu81_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.21***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu81.p

# 1986
# Create GAM model
pImm_notEu86.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pImm_notEu86.gam) # R2 = 0.17***
gam.check(pImm_notEu86.gam) # Should see convergence, p should not be significant
plot(pImm_notEu86.gam)

pImm_notEu86_pred = data.frame(pImm_notEu = census1986$pImm_notEu, cc = census1986$cc, Pop = census1986$Pop, pred = predict(pImm_notEu86.gam, newdata = census1986))

# Plot
pImm_notEu86.p = ggplot(pImm_notEu86_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.17***", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu86.p

# 1991
# Create GAM model
pImm_notEu91.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pImm_notEu91.gam) # R2 = 0.27***
gam.check(pImm_notEu91.gam) # Should see convergence, p should not be significant
plot(pImm_notEu91.gam)

pImm_notEu91_pred = data.frame(pImm_notEu = census1991$pImm_notEu, cc = census1991$cc, Pop = census1991$Pop, pred = predict(pImm_notEu91.gam, newdata = census1991))

# Plot
pImm_notEu91.p = ggplot(pImm_notEu91_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.27***", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu91.p

# 1996
# Create GAM model
pImm_notEu96.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_notEu96.gam) # R2 = 0.32***
gam.check(pImm_notEu96.gam) # Should see convergence, p should not be significant
plot(pImm_notEu96.gam)

pImm_notEu96_pred = data.frame(pImm_notEu = census1996$pImm_notEu, cc = census1996$cc, Pop = census1996$Pop, pred = predict(pImm_notEu96.gam, newdata = census1996))

# Plot
pImm_notEu96.p = ggplot(pImm_notEu96_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.32***", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu96.p

# 2001
# Create GAM model
pImm_notEu01.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_notEu01.gam) # R2 = 0.30***
gam.check(pImm_notEu01.gam) # Should see convergence, p should not be significant
plot(pImm_notEu01.gam)

pImm_notEu01_pred = data.frame(pImm_notEu = census2001$pImm_notEu, cc = census2001$cc, Pop = census2001$Pop, pred = predict(pImm_notEu01.gam, newdata = census2001))

# Plot
pImm_notEu01.p = ggplot(pImm_notEu01_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.30***", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu01.p

# 2006
# Create GAM model
pImm_notEu06.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_notEu06.gam) # R2 = 0.41***
gam.check(pImm_notEu06.gam) # Should see convergence, p should not be significant
plot(pImm_notEu06.gam)

pImm_notEu06_pred = data.frame(pImm_notEu = census2006$pImm_notEu, cc = census2006$cc, Pop = census2006$Pop, pred = predict(pImm_notEu06.gam, newdata = census2006))

# Plot
pImm_notEu06.p = ggplot(pImm_notEu06_pred, aes(x = pImm_notEu, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.41***", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu06.p

# 2011
# Create blank plot
pImm_notEu11.p = ggplot() +
  scale_x_continuous(name = "% Immigrants from outside Europe", limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  annotate(geom = "text", x = 50, y = 26.5, label = "Data not collected in Census", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu11.p

# 2016
# Create GAM model
pImm_notEu16.gam = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pImm_notEu16.gam) # R2 = 0.50***
gam.check(pImm_notEu16.gam) # Should see convergence, p should not be significant
plot(pImm_notEu16.gam)

pImm_notEu16_pred = data.frame(pImm_notEu = census2016$pImm_notEu, cc = census2016$cc, Pop = census2016$Pop, pred = predict(pImm_notEu16.gam, newdata = census2016))

# Plot
pImm_notEu16.p = ggplot(pImm_notEu16_pred, aes(x = pImm_notEu, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.50***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu16.p

#tiff("pImm_notEu_CC_Peel_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_notEu71.p | pImm_notEu81.p | pImm_notEu86.p) /
  (pImm_notEu91.p | pImm_notEu96.p | pImm_notEu01.p) /
  (pImm_notEu06.p | pImm_notEu11.p | pImm_notEu16.p)
#dev.off()
###

# Peel - Combined Figure
# Peel - Combined Figure
# Find weighted mean location (closest real point) on predicted line
pImm_notEu_mn_row71 = first(which(abs(pImm_notEu71_pred$pImm_notEu - weighted.mean(x = pImm_notEu71_pred$pImm_notEu, w = pImm_notEu71_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu71_pred$pImm_notEu - weighted.mean(x = pImm_notEu71_pred$pImm_notEu, w = pImm_notEu71_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu71_pred[pImm_notEu_mn_row71,]

pImm_notEu_mn_row81 = first(which(abs(pImm_notEu81_pred$pImm_notEu - weighted.mean(x = pImm_notEu81_pred$pImm_notEu, w = pImm_notEu81_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu81_pred$pImm_notEu - weighted.mean(x = pImm_notEu81_pred$pImm_notEu, w = pImm_notEu81_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu81_pred[pImm_notEu_mn_row81,]

pImm_notEu_mn_row86 = first(which(abs(pImm_notEu86_pred$pImm_notEu - weighted.mean(x = pImm_notEu86_pred$pImm_notEu, w = pImm_notEu86_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu86_pred$pImm_notEu - weighted.mean(x = pImm_notEu86_pred$pImm_notEu, w = pImm_notEu86_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu86_pred[pImm_notEu_mn_row86,]

pImm_notEu_mn_row91 = first(which(abs(pImm_notEu91_pred$pImm_notEu - weighted.mean(x = pImm_notEu91_pred$pImm_notEu, w = pImm_notEu91_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu91_pred$pImm_notEu - weighted.mean(x = pImm_notEu91_pred$pImm_notEu, w = pImm_notEu91_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu91_pred[pImm_notEu_mn_row91,]

pImm_notEu_mn_row96 = first(which(abs(pImm_notEu96_pred$pImm_notEu - weighted.mean(x = pImm_notEu96_pred$pImm_notEu, w = pImm_notEu96_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu96_pred$pImm_notEu - weighted.mean(x = pImm_notEu96_pred$pImm_notEu, w = pImm_notEu96_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu96_pred[pImm_notEu_mn_row96,]

pImm_notEu_mn_row01 = first(which(abs(pImm_notEu01_pred$pImm_notEu - weighted.mean(x = pImm_notEu01_pred$pImm_notEu, w = pImm_notEu01_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu01_pred$pImm_notEu - weighted.mean(x = pImm_notEu01_pred$pImm_notEu, w = pImm_notEu01_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu01_pred[pImm_notEu_mn_row01,]

pImm_notEu_mn_row06 = first(which(abs(pImm_notEu06_pred$pImm_notEu - weighted.mean(x = pImm_notEu06_pred$pImm_notEu, w = pImm_notEu06_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu06_pred$pImm_notEu - weighted.mean(x = pImm_notEu06_pred$pImm_notEu, w = pImm_notEu06_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu06_pred[pImm_notEu_mn_row06,]

pImm_notEu_mn_row16 = first(which(abs(pImm_notEu16_pred$pImm_notEu - weighted.mean(x = pImm_notEu16_pred$pImm_notEu, w = pImm_notEu16_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pImm_notEu16_pred$pImm_notEu - weighted.mean(x = pImm_notEu16_pred$pImm_notEu, w = pImm_notEu16_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu16_pred[pImm_notEu_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

# Plot
xmin = 0
xmax = 75
ymin = 0
ymax = 45

pImm_notEu_com.p = ggplot() +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1971"), data = pImm_notEu71_pred) + 
  geom_point(aes(x = pImm_notEu71_pred$pImm_notEu[pImm_notEu_mn_row71], y = pImm_notEu71_pred$pred[pImm_notEu_mn_row71]), color = "black") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "1981"), data = pImm_notEu81_pred) + 
  geom_point(aes(x = pImm_notEu81_pred$pImm_notEu[pImm_notEu_mn_row81], y = pImm_notEu81_pred$pred[pImm_notEu_mn_row81]), color = "sienna") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "1986"), data = pImm_notEu86_pred) + 
  geom_point(aes(x = pImm_notEu86_pred$pImm_notEu[pImm_notEu_mn_row86], y = pImm_notEu86_pred$pred[pImm_notEu_mn_row86]), color = "purple4") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "1991"), data = pImm_notEu91_pred) + 
  geom_point(aes(x = pImm_notEu91_pred$pImm_notEu[pImm_notEu_mn_row91], y = pImm_notEu91_pred$pred[pImm_notEu_mn_row91]), color = "blue") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "1996"), data = pImm_notEu96_pred) + 
  geom_point(aes(x = pImm_notEu96_pred$pImm_notEu[pImm_notEu_mn_row96], y = pImm_notEu96_pred$pred[pImm_notEu_mn_row96]), color = "steelblue2") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "2001"), data = pImm_notEu01_pred) + 
  geom_point(aes(x = pImm_notEu01_pred$pImm_notEu[pImm_notEu_mn_row01], y = pImm_notEu01_pred$pred[pImm_notEu_mn_row01]), color = "green4") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "2006"), data = pImm_notEu06_pred) + 
  geom_point(aes(x = pImm_notEu06_pred$pImm_notEu[pImm_notEu_mn_row06], y = pImm_notEu06_pred$pred[pImm_notEu_mn_row06]), color = "gold2") + 
  geom_line(aes(x = pImm_notEu, y = pred, col = "2016"), data = pImm_notEu16_pred) +
  geom_point(aes(x = pImm_notEu16_pred$pImm_notEu[pImm_notEu_mn_row16], y = pImm_notEu16_pred$pred[pImm_notEu_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 60, y = 42, label = "R² = 0.04**  ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 60, y = 40, label = "R² = 0.21***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 60, y = 38, label = "R² = 0.17***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 60, y = 36, label = "R² = 0.27***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 60, y = 34, label = "R² = 0.32***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 60, y = 32, label = "R² = 0.30***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 60, y = 30, label = "R² = 0.41***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 60, y = 28, label = "R² = 0.50***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "% Immigrants from outside Europe", expand = c(0,0)) + 
  coord_cartesian(xlim = c(xmin, xmax)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.4, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pImm_notEu_com.p

#tiff("pImm_notEu_CC_Peel_combined1.tif", units = "cm", width = 10, height = 10, res = 300)
pImm_notEu_com.p
#dev.off()

# By Municipality - Separate Figures
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# 1971
# Create GAM models
pImm_notEu71.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu71.gam_m) # R2 = 0.07**
gam.check(pImm_notEu71.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu71.gam_m)

pImm_notEu71_pred_m = data.frame(pImm_notEu = subset(census1971$pImm_notEu, census1971$mun == "Mississauga"), 
                              cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                              Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                              pred = predict(pImm_notEu71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

pImm_notEu71.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = 45), data = subset(census1971, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu71.gam_b) # R2 = 0.11
gam.check(pImm_notEu71.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu71.gam_b)

pImm_notEu71_pred_b = data.frame(pImm_notEu = subset(census1971$pImm_notEu, census1971$mun == "Brampton"), 
                              cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                              Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                              pred = predict(pImm_notEu71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
pImm_notEu71_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.07**  ", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.11    ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu71_mun.p

# 1981
# Create GAM models
pImm_notEu81.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu81.gam_m) # R2 = 0.25***
gam.check(pImm_notEu81.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu81.gam_m)

pImm_notEu81_pred_m = data.frame(pImm_notEu = subset(census1981$pImm_notEu, census1981$mun == "Mississauga"), 
                              cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                              Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                              pred = predict(pImm_notEu81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

pImm_notEu81.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu81.gam_b) # R2 = 0.27***
gam.check(pImm_notEu81.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu81.gam_b)

pImm_notEu81_pred_b = data.frame(pImm_notEu = subset(census1981$pImm_notEu, census1981$mun == "Brampton"), 
                              cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                              Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                              pred = predict(pImm_notEu81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
pImm_notEu81_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.25***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.27***", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu81_mun.p

# 1986
# Create GAM models
pImm_notEu86.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu86.gam_m) # R2 = 0.23***
gam.check(pImm_notEu86.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu86.gam_m)

pImm_notEu86_pred_m = data.frame(pImm_notEu = subset(census1986$pImm_notEu, census1986$mun == "Mississauga"), 
                              cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                              Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                              pred = predict(pImm_notEu86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

pImm_notEu86.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu86.gam_b) # R2 = 0.17***
gam.check(pImm_notEu86.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu86.gam_b)

pImm_notEu86_pred_b = data.frame(pImm_notEu = subset(census1986$pImm_notEu, census1986$mun == "Brampton"), 
                              cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                              Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                              pred = predict(pImm_notEu86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
pImm_notEu86_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.23***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.17***", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu86_mun.p

# 1991
# Create GAM models
pImm_notEu91.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu91.gam_m) # R2 = 0.34***
gam.check(pImm_notEu91.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu91.gam_m)

pImm_notEu91_pred_m = data.frame(pImm_notEu = subset(census1991$pImm_notEu, census1991$mun == "Mississauga"), 
                              cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                              Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                              pred = predict(pImm_notEu91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

pImm_notEu91.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu91.gam_b) # R2 = 0.25***
gam.check(pImm_notEu91.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu91.gam_b)

pImm_notEu91_pred_b = data.frame(pImm_notEu = subset(census1991$pImm_notEu, census1991$mun == "Brampton"), 
                              cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                              Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                              pred = predict(pImm_notEu91.gam_b, newdata = subset(census1991, mun == "Brampton")))

pImm_notEu91.gam_c = gam(cc ~ s(pImm_notEu, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu91.gam_c) # R2 = 0.00
gam.check(pImm_notEu91.gam_c) # Should see convergence, p should not be significant
plot(pImm_notEu91.gam_c)

pImm_notEu91_pred_c = data.frame(pImm_notEu = subset(census1991$pImm_notEu, census1991$mun == "Caledon"), 
                              cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                              Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                              pred = predict(pImm_notEu91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
pImm_notEu91_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.34***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.25***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu91_mun.p

# 1996
# Create GAM models
pImm_notEu96.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu96.gam_m) # R2 = 0.38***
gam.check(pImm_notEu96.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu96.gam_m)

pImm_notEu96_pred_m = data.frame(pImm_notEu = subset(census1996$pImm_notEu, census1996$mun == "Mississauga"), 
                              cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                              Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                              pred = predict(pImm_notEu96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

pImm_notEu96.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu96.gam_b) # R2 = 0.32***
gam.check(pImm_notEu96.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu96.gam_b)

pImm_notEu96_pred_b = data.frame(pImm_notEu = subset(census1996$pImm_notEu, census1996$mun == "Brampton"), 
                              cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                              Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                              pred = predict(pImm_notEu96.gam_b, newdata = subset(census1996, mun == "Brampton")))

pImm_notEu96.gam_c = gam(cc ~ s(pImm_notEu, bs = "cs", k =  24), data = subset(census1996, mun == "Caledon"), # 33 = # of Caledon DAs)
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu96.gam_c) # R2 = 0.01
gam.check(pImm_notEu96.gam_c) # Should see convergence, p should not be significant
plot(pImm_notEu96.gam_c)

pImm_notEu96_pred_c = data.frame(pImm_notEu = subset(census1996$pImm_notEu, census1996$mun == "Caledon"), 
                              cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                              Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                              pred = predict(pImm_notEu96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
pImm_notEu96_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.38***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.32***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.01    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu96_mun.p

# 2001
# Create GAM models
pImm_notEu01.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2001, mun == "Mississauga"),
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu01.gam_m) # R2 = 0.37***
gam.check(pImm_notEu01.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu01.gam_m)

pImm_notEu01_pred_m = data.frame(pImm_notEu = subset(census2001$pImm_notEu, census2001$mun == "Mississauga"), 
                              cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                              Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                              pred = predict(pImm_notEu01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

pImm_notEu01.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu01.gam_b) # R2 = 0.29***
gam.check(pImm_notEu01.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu01.gam_b)

pImm_notEu01_pred_b = data.frame(pImm_notEu = subset(census2001$pImm_notEu, census2001$mun == "Brampton"), 
                              cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                              Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                              pred = predict(pImm_notEu01.gam_b, newdata = subset(census2001, mun == "Brampton")))

pImm_notEu01.gam_c = gam(cc ~ s(pImm_notEu, bs = "cs", k = 30), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu01.gam_c) # R2 = 0.00
gam.check(pImm_notEu01.gam_c) # Should see convergence, p should not be significant
plot(pImm_notEu01.gam_c)

pImm_notEu01_pred_c = data.frame(pImm_notEu = subset(census2001$pImm_notEu, census2001$mun == "Caledon"), 
                              cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                              Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                              pred = predict(pImm_notEu01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
pImm_notEu01_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.37***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.29***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pImm_notEu01_mun.p

# 2006
# Create GAM models
pImm_notEu06.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"),
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu06.gam_m) # R2 = 0.48***
gam.check(pImm_notEu06.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu06.gam_m)

pImm_notEu06_pred_m = data.frame(pImm_notEu = subset(census2006$pImm_notEu, census2006$mun == "Mississauga"), 
                              cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                              Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                              pred = predict(pImm_notEu06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

pImm_notEu06.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu06.gam_b) # R2 = 0.41***
gam.check(pImm_notEu06.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu06.gam_b)

pImm_notEu06_pred_b = data.frame(pImm_notEu = subset(census2006$pImm_notEu, census2006$mun == "Brampton"), 
                              cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                              Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                              pred = predict(pImm_notEu06.gam_b, newdata = subset(census2006, mun == "Brampton")))

pImm_notEu06.gam_c = gam(cc ~ s(pImm_notEu, bs = "cs", k = 45), data = subset(census2006, mun == "Caledon"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu06.gam_c) # R2 = 0.14**
gam.check(pImm_notEu06.gam_c) # Should see convergence, p should not be significant
plot(pImm_notEu06.gam_c)

pImm_notEu06_pred_c = data.frame(pImm_notEu = subset(census2006$pImm_notEu, census2006$mun == "Caledon"), 
                              cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                              Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                              pred = predict(pImm_notEu06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
pImm_notEu06_mun.p = ggplot() +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.48***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.41***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.14**  ", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu06_mun.p

# 2011
pImm_notEu11_mun.p = pImm_notEu11.p
pImm_notEu11_mun.p

# 2016
# Create GAM models
pImm_notEu16.gam_m = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu16.gam_m) # R2 = 0.53***
gam.check(pImm_notEu16.gam_m) # Should see convergence, p should not be significant
plot(pImm_notEu16.gam_m)

pImm_notEu16_pred_m = data.frame(pImm_notEu = subset(census2016$pImm_notEu, census2016$mun == "Mississauga"), 
                              cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                              Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                              pred = predict(pImm_notEu16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

pImm_notEu16.gam_b = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu16.gam_b) # R2 = 0.51***
gam.check(pImm_notEu16.gam_b) # Should see convergence, p should not be significant
plot(pImm_notEu16.gam_b)

pImm_notEu16_pred_b = data.frame(pImm_notEu = subset(census2016$pImm_notEu, census2016$mun == "Brampton"), 
                              cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                              Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                              pred = predict(pImm_notEu16.gam_b, newdata = subset(census2016, mun == "Brampton")))

pImm_notEu16.gam_c = gam(cc ~ s(pImm_notEu, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), 
                      weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pImm_notEu16.gam_c) # R2 = 0.32***
gam.check(pImm_notEu16.gam_c) # Should see convergence, p should not be significant
plot(pImm_notEu16.gam_c)

pImm_notEu16_pred_c = data.frame(pImm_notEu = subset(census2016$pImm_notEu, census2016$mun == "Caledon"), 
                              cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                              Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                              pred = predict(pImm_notEu16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
pImm_notEu16_mun.p = ggplot(pImm_notEu16_pred, aes(x = pImm_notEu, y = cc, size = Pop)) + 
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pImm_notEu, y = cc, size = Pop), data = pImm_notEu16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pImm_notEu, y = pred), data = pImm_notEu16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.53***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.51***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.32***", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu16_mun.p

#tiff("pImm_notEu_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_notEu71_mun.p | pImm_notEu81_mun.p | pImm_notEu86_mun.p) /
  (pImm_notEu91_mun.p | pImm_notEu96_mun.p | pImm_notEu01_mun.p) /
  (pImm_notEu06_mun.p | pImm_notEu11_mun.p | pImm_notEu16_mun.p)
#dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 75
ymin = 0
ymax = 45

pImm_notEu_com.p # Already built above

# Mississauga
# Find weighted mean location (closest real point) on predicted line
pImm_notEu_m_mn_row71 = first(which(abs(pImm_notEu71_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu71_pred_m$pImm_notEu, w = pImm_notEu71_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu71_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu71_pred_m$pImm_notEu, w = pImm_notEu71_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu71_pred_m[pImm_notEu_m_mn_row71,]

pImm_notEu_m_mn_row81 = first(which(abs(pImm_notEu81_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu81_pred_m$pImm_notEu, w = pImm_notEu81_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu81_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu81_pred_m$pImm_notEu, w = pImm_notEu81_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu81_pred_m[pImm_notEu_m_mn_row81,]

pImm_notEu_m_mn_row86 = first(which(abs(pImm_notEu86_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu86_pred_m$pImm_notEu, w = pImm_notEu86_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu86_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu86_pred_m$pImm_notEu, w = pImm_notEu86_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu86_pred_m[pImm_notEu_m_mn_row86,]

pImm_notEu_m_mn_row91 = first(which(abs(pImm_notEu91_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu91_pred_m$pImm_notEu, w = pImm_notEu91_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu91_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu91_pred_m$pImm_notEu, w = pImm_notEu91_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu91_pred_m[pImm_notEu_m_mn_row91,]

pImm_notEu_m_mn_row96 = first(which(abs(pImm_notEu96_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu96_pred_m$pImm_notEu, w = pImm_notEu96_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu96_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu96_pred_m$pImm_notEu, w = pImm_notEu96_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu96_pred_m[pImm_notEu_m_mn_row96,]

pImm_notEu_m_mn_row01 = first(which(abs(pImm_notEu01_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu01_pred_m$pImm_notEu, w = pImm_notEu01_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu01_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu01_pred_m$pImm_notEu, w = pImm_notEu01_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu01_pred_m[pImm_notEu_m_mn_row01,]

pImm_notEu_m_mn_row06 = first(which(abs(pImm_notEu06_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu06_pred_m$pImm_notEu, w = pImm_notEu06_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu06_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu06_pred_m$pImm_notEu, w = pImm_notEu06_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu06_pred_m[pImm_notEu_m_mn_row06,]

pImm_notEu_m_mn_row16 = first(which(abs(pImm_notEu16_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu16_pred_m$pImm_notEu, w = pImm_notEu16_pred_m$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu16_pred_m$pImm_notEu - weighted.mean(x = pImm_notEu16_pred_m$pImm_notEu, w = pImm_notEu16_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu16_pred_m[pImm_notEu_m_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends
  
pImm_notEu_com_m.p = ggplot() +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1971"), data = pImm_notEu71_pred_m) + 
  geom_point(aes(x = pImm_notEu71_pred_m$pImm_notEu[pImm_notEu_m_mn_row71], y = pImm_notEu71_pred_m$pred[pImm_notEu_m_mn_row71]), color = "black") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1981"), data = pImm_notEu81_pred_m) + 
  geom_point(aes(x = pImm_notEu81_pred_m$pImm_notEu[pImm_notEu_m_mn_row81], y = pImm_notEu81_pred_m$pred[pImm_notEu_m_mn_row81]), color = "sienna") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1986"), data = pImm_notEu86_pred_m) + 
  geom_point(aes(x = pImm_notEu86_pred_m$pImm_notEu[pImm_notEu_m_mn_row86], y = pImm_notEu86_pred_m$pred[pImm_notEu_m_mn_row86]), color = "purple4") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1991"), data = pImm_notEu91_pred_m) + 
  geom_point(aes(x = pImm_notEu91_pred_m$pImm_notEu[pImm_notEu_m_mn_row91], y = pImm_notEu91_pred_m$pred[pImm_notEu_m_mn_row91]), color = "blue") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1996"), data = pImm_notEu96_pred_m) + 
  geom_point(aes(x = pImm_notEu96_pred_m$pImm_notEu[pImm_notEu_m_mn_row96], y = pImm_notEu96_pred_m$pred[pImm_notEu_m_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2001"), data = pImm_notEu01_pred_m) + 
  geom_point(aes(x = pImm_notEu01_pred_m$pImm_notEu[pImm_notEu_m_mn_row01], y = pImm_notEu01_pred_m$pred[pImm_notEu_m_mn_row01]), color = "green4") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2006"), data = pImm_notEu06_pred_m) + 
  geom_point(aes(x = pImm_notEu06_pred_m$pImm_notEu[pImm_notEu_m_mn_row06], y = pImm_notEu06_pred_m$pred[pImm_notEu_m_mn_row06]), color = "gold2") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2016"), data = pImm_notEu16_pred_m) +
  geom_point(aes(x = pImm_notEu16_pred_m$pImm_notEu[pImm_notEu_m_mn_row16], y = pImm_notEu16_pred_m$pred[pImm_notEu_m_mn_row16]), color = "red3") + 
  annotate(geom = "text", x = 55, y = 42, label = "R² = 0.07***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 55, y = 40, label = "R² = 0.25***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 55, y = 38, label = "R² = 0.23***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 55, y = 36, label = "R² = 0.34***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 55, y = 34, label = "R² = 0.38***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 55, y = 32, label = "R² = 0.37***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 55, y = 30, label = "R² = 0.48***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 55, y = 28, label = "R² = 0.53***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 10, y = 41, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, expand = c(0,0)) + 
  coord_cartesian(xlim = c(xmin, xmax)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
pImm_notEu_com_m.p

# Brampton
# Find weighted mean location (closest real point) on predicted line
pImm_notEu_b_mn_row71 = first(which(abs(pImm_notEu71_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu71_pred_b$pImm_notEu, w = pImm_notEu71_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu71_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu71_pred_b$pImm_notEu, w = pImm_notEu71_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu71_pred_b[pImm_notEu_b_mn_row71,]

pImm_notEu_b_mn_row81 = first(which(abs(pImm_notEu81_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu81_pred_b$pImm_notEu, w = pImm_notEu81_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu81_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu81_pred_b$pImm_notEu, w = pImm_notEu81_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu81_pred_b[pImm_notEu_b_mn_row81,]

pImm_notEu_b_mn_row86 = first(which(abs(pImm_notEu86_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu86_pred_b$pImm_notEu, w = pImm_notEu86_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu86_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu86_pred_b$pImm_notEu, w = pImm_notEu86_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu86_pred_b[pImm_notEu_b_mn_row86,]

pImm_notEu_b_mn_row91 = first(which(abs(pImm_notEu91_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu91_pred_b$pImm_notEu, w = pImm_notEu91_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu91_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu91_pred_b$pImm_notEu, w = pImm_notEu91_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu91_pred_b[pImm_notEu_b_mn_row91,]

pImm_notEu_b_mn_row96 = first(which(abs(pImm_notEu96_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu96_pred_b$pImm_notEu, w = pImm_notEu96_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu96_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu96_pred_b$pImm_notEu, w = pImm_notEu96_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu96_pred_b[pImm_notEu_b_mn_row96,]

pImm_notEu_b_mn_row01 = first(which(abs(pImm_notEu01_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu01_pred_b$pImm_notEu, w = pImm_notEu01_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu01_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu01_pred_b$pImm_notEu, w = pImm_notEu01_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu01_pred_b[pImm_notEu_b_mn_row01,]

pImm_notEu_b_mn_row06 = first(which(abs(pImm_notEu06_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu06_pred_b$pImm_notEu, w = pImm_notEu06_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu06_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu06_pred_b$pImm_notEu, w = pImm_notEu06_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu06_pred_b[pImm_notEu_b_mn_row06,]

pImm_notEu_b_mn_row16 = first(which(abs(pImm_notEu16_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu16_pred_b$pImm_notEu, w = pImm_notEu16_pred_b$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu16_pred_b$pImm_notEu - weighted.mean(x = pImm_notEu16_pred_b$pImm_notEu, w = pImm_notEu16_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu16_pred_b[pImm_notEu_b_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pImm_notEu_com_b.p = ggplot() +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1971"), data = pImm_notEu71_pred_b) + 
  geom_point(aes(x = pImm_notEu71_pred_b$pImm_notEu[pImm_notEu_b_mn_row71], y = pImm_notEu71_pred_b$pred[pImm_notEu_b_mn_row71]), color = "black") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1981"), data = subset(pImm_notEu81_pred_b, pImm_notEu < 40)) + 
  geom_point(aes(x = pImm_notEu81_pred_b$pImm_notEu[pImm_notEu_b_mn_row81], y = pImm_notEu81_pred_b$pred[pImm_notEu_b_mn_row81]), color = "sienna") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1986"), data = subset(pImm_notEu86_pred_b, pImm_notEu < 40)) + 
  geom_point(aes(x = pImm_notEu86_pred_b$pImm_notEu[pImm_notEu_b_mn_row86], y = pImm_notEu86_pred_b$pred[pImm_notEu_b_mn_row86]), color = "purple4") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1991"), data = pImm_notEu91_pred_b) + 
  geom_point(aes(x = pImm_notEu91_pred_b$pImm_notEu[pImm_notEu_b_mn_row91], y = pImm_notEu91_pred_b$pred[pImm_notEu_b_mn_row91]), color = "blue") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1996"), data = pImm_notEu96_pred_b) +
  geom_point(aes(x = pImm_notEu96_pred_b$pImm_notEu[pImm_notEu_b_mn_row96], y = pImm_notEu96_pred_b$pred[pImm_notEu_b_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2001"), data = pImm_notEu01_pred_b) + 
  geom_point(aes(x = pImm_notEu01_pred_b$pImm_notEu[pImm_notEu_b_mn_row01], y = pImm_notEu01_pred_b$pred[pImm_notEu_b_mn_row01]), color = "green4") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2006"), data = pImm_notEu06_pred_b) + 
  geom_point(aes(x = pImm_notEu06_pred_b$pImm_notEu[pImm_notEu_b_mn_row06], y = pImm_notEu06_pred_b$pred[pImm_notEu_b_mn_row06]), color = "gold2") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2016"), data = pImm_notEu16_pred_b) +
  geom_point(aes(x = pImm_notEu16_pred_b$pImm_notEu[pImm_notEu_b_mn_row16], y = pImm_notEu16_pred_b$pred[pImm_notEu_b_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 55, y = 42, label = "R² = 0.11    ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 55, y = 40, label = "R² = 0.27***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 55, y = 38, label = "R² = 0.23***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 55, y = 36, label = "R² = 0.25***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 55, y = 34, label = "R² = 0.32***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 55, y = 32, label = "R² = 0.29***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 55, y = 30, label = "R² = 0.41***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 55, y = 28, label = "R² = 0.51***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "% Immigrants from outside Europe", expand = c(0,0)) + 
  coord_cartesian(xlim = c(xmin, xmax)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu_com_b.p

# Caledon
# Find weighted mean location (closest real point) on predicted line
pImm_notEu_c_mn_row91 = first(which(abs(pImm_notEu91_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu91_pred_c$pImm_notEu, w = pImm_notEu91_pred_c$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu91_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu91_pred_c$pImm_notEu, w = pImm_notEu91_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu91_pred_c[pImm_notEu_c_mn_row91,]

pImm_notEu_c_mn_row96 = first(which(abs(pImm_notEu96_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu96_pred_c$pImm_notEu, w = pImm_notEu96_pred_c$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu96_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu96_pred_c$pImm_notEu, w = pImm_notEu96_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu96_pred_c[pImm_notEu_c_mn_row96,]

pImm_notEu_c_mn_row01 = first(which(abs(pImm_notEu01_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu01_pred_c$pImm_notEu, w = pImm_notEu01_pred_c$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu01_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu01_pred_c$pImm_notEu, w = pImm_notEu01_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu01_pred_c[pImm_notEu_c_mn_row01,]

pImm_notEu_c_mn_row06 = first(which(abs(pImm_notEu06_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu06_pred_c$pImm_notEu, w = pImm_notEu06_pred_c$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu06_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu06_pred_c$pImm_notEu, w = pImm_notEu06_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu06_pred_c[pImm_notEu_c_mn_row06,]

pImm_notEu_c_mn_row16 = first(which(abs(pImm_notEu16_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu16_pred_c$pImm_notEu, w = pImm_notEu16_pred_c$Pop, na.rm = TRUE)) == 
                                      min(abs(pImm_notEu16_pred_c$pImm_notEu - weighted.mean(x = pImm_notEu16_pred_c$pImm_notEu, w = pImm_notEu16_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pImm_notEu16_pred_c[pImm_notEu_c_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pImm_notEu_com_c.p = ggplot() +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1991"), data = pImm_notEu91_pred_c) + 
  geom_point(aes(x = pImm_notEu91_pred_c$pImm_notEu[pImm_notEu_c_mn_row91], y = pImm_notEu91_pred_c$pred[pImm_notEu_c_mn_row91]), color = "blue") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "1996"), data = subset(pImm_notEu96_pred_c, pImm_notEu < 10)) + 
  geom_point(aes(x = pImm_notEu96_pred_c$pImm_notEu[pImm_notEu_c_mn_row96], y = pImm_notEu96_pred_c$pred[pImm_notEu_c_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2001"), data = pImm_notEu01_pred_c) + 
  geom_point(aes(x = pImm_notEu01_pred_c$pImm_notEu[pImm_notEu_c_mn_row01], y = pImm_notEu01_pred_c$pred[pImm_notEu_c_mn_row01]), color = "green4") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2006"), data = subset(pImm_notEu06_pred_c, pImm_notEu < 20)) + 
  geom_point(aes(x = pImm_notEu06_pred_c$pImm_notEu[pImm_notEu_c_mn_row06], y = pImm_notEu06_pred_c$pred[pImm_notEu_c_mn_row06]), color = "gold2") +
  geom_line(aes(x = pImm_notEu, y = pred, col = "2016"), data = pImm_notEu16_pred_c) +
  geom_point(aes(x = pImm_notEu16_pred_c$pImm_notEu[pImm_notEu_c_mn_row16], y = pImm_notEu16_pred_c$pred[pImm_notEu_c_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 55, y = 36, label = "R² = 0.00    ", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 55, y = 34, label = "R² = 0.01    ", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 55, y = 32, label = "R² = 0.00    ", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 55, y = 30, label = "R² = 0.14**  ", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 55, y = 28, label = "R² = 0.32***", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pImm_notEu_com_c.p

tiff("pImm_notEu_CC_bymun_combined1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_notEu_com.p + annotate(geom = "text", x = 5, y = 40, label = "A", fontface = "bold", size = 6)) /
  (pImm_notEu_com_m.p | pImm_notEu_com_b.p | pImm_notEu_com_c.p)
dev.off()
#####

##### Combine immigration plot - Immigrants from Europe and Immigrants from not Europe #####
#tiff("Immigration_cc_71to16_combined.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_Eu_com.p + annotate(geom = "text", x = 5, y = 40, label = "A", fontface = "bold", size = 6)) / 
  (pImm_notEu_com.p + annotate(geom = "text", x = 5, y = 40, label = "B", fontface = "bold", size = 6))
#dev.off()

tiff("Immigration_cc_71to16_bymun.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pImm_Eu_com_m.p | pImm_Eu_com_b.p | pImm_Eu_com_c.p) /
(pImm_notEu_com_m.p | pImm_notEu_com_b.p | pImm_notEu_com_c.p)
dev.off()
#####

##### CC by year - see individual relationships % Minority #####
### Linear model ###
# 1971
pMin71.lm = lm(cc ~ pMin, census1971)
summary(pMin71.lm) # R2 = 0.08***
plot(cc ~ pMin, census1971)
abline(pMin71.lm)

# 1981
pMin81.lm = lm(cc ~ pMin, census1981)
summary(pMin81.lm) # R2 = 0.16***
plot(cc ~ pMin, census1981)
abline(pMin81.lm)

# 1986
pMin86.lm = lm(cc ~ pMin, census1986)
summary(pMin86.lm) # R2 = 0.17***
plot(cc ~ pMin, census1986)
abline(pMin86.lm)

# 1991
pMin91.lm = lm(cc ~ pMin, census1991)
summary(pMin91.lm) # R2 = 0.23***
plot(cc ~ pMin, census1991)
abline(pMin91.lm)

# 1996
pMin96.lm = lm(cc ~ pMin, census1996)
summary(pMin96.lm) # R2 = 0.29***
plot(cc ~ pMin, census1996)
abline(pMin96.lm)

# 2001
pMin01.lm = lm(cc ~ pMin, census2001)
summary(pMin01.lm) # R2 = 0.35***
plot(cc ~ pMin, census2001)
abline(pMin01.lm)

# 2006
pMin06.lm = lm(cc ~ pMin, census2006)
summary(pMin06.lm) # R2 = 0.48***
plot(cc ~ pMin, census2006)
abline(pMin06.lm)

# 2011
# No Data

# 2016
pMin16.lm = lm(cc ~ pMin, census2016)
summary(pMin16.lm) # R2 = 0.51***
plot(cc ~ pMin, census2016)
abline(pMin16.lm)

### Weighted GAM Figures ###
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
pMin71.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin71.gam) # R2 = 0.09***
gam.check(pMin71.gam) # Should see convergence, p should not be significant
plot(pMin71.gam)

pMin71_pred = data.frame(pMin = census1971$pMin, cc = census1971$cc, Pop = census1971$Pop, pred = predict(pMin71.gam, newdata = census1971))

# Plot
pMin71.p = ggplot(pMin71_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.08**", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin71.p

# 1981
# Create GAM model
pMin81.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(pMin81.gam) # R2 = 0.16***
gam.check(pMin81.gam) # Should see convergence, p should not be significant
plot(pMin81.gam)

pMin81_pred = data.frame(pMin = census1981$pMin, cc = census1981$cc, Pop = census1981$Pop, pred = predict(pMin81.gam, newdata = census1981))

# Plot
pMin81.p = ggplot(pMin81_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.16***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin81.p

# 1986
# Create GAM model
pMin86.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pMin86.gam) # R2 = 0.18***
gam.check(pMin86.gam) # Should see convergence, p should not be significant
plot(pMin86.gam)

pMin86_pred = data.frame(pMin = census1986$pMin, cc = census1986$cc, Pop = census1986$Pop, pred = predict(pMin86.gam, newdata = census1986))

# Plot
pMin86.p = ggplot(pMin86_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.16***", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin86.p

# 1991
# Create GAM model
pMin91.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pMin91.gam) # R2 = 0.26***
gam.check(pMin91.gam) # Should see convergence, p should not be significant
plot(pMin91.gam)

pMin91_pred = data.frame(pMin = census1991$pMin, cc = census1991$cc, Pop = census1991$Pop, pred = predict(pMin91.gam, newdata = census1991))

# Plot
pMin91.p = ggplot(pMin91_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.24***", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin91.p

# 1996
# Create GAM model
pMin96.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pMin96.gam) # R2 = 0.36***
gam.check(pMin96.gam) # Should see convergence, p should not be significant
plot(pMin96.gam)

pMin96_pred = data.frame(pMin = census1996$pMin, cc = census1996$cc, Pop = census1996$Pop, pred = predict(pMin96.gam, newdata = census1996))

# Plot
pMin96.p = ggplot(pMin96_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.33***", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin96.p

# 2001
# Create GAM model
pMin01.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pMin01.gam) # R2 = 0.38***
gam.check(pMin01.gam) # Should see convergence, p should not be significant
plot(pMin01.gam)

pMin01_pred = data.frame(pMin = census2001$pMin, cc = census2001$cc, Pop = census2001$Pop, pred = predict(pMin01.gam, newdata = census2001))

# Plot
pMin01.p = ggplot(pMin01_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.36***", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin01.p

# 2006
# Create GAM model
pMin06.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pMin06.gam) # R2 = 0.47***
gam.check(pMin06.gam) # Should see convergence, p should not be significant
plot(pMin06.gam)

pMin06_pred = data.frame(pMin = census2006$pMin, cc = census2006$cc, Pop = census2006$Pop, pred = predict(pMin06.gam, newdata = census2006))

# Plot
pMin06.p = ggplot(pMin06_pred, aes(x = pMin, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.43***", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin06.p

# 2011
# Create blank plot
pMin11.p = ggplot() +
  scale_x_continuous(name = "Visible Minorities (%)", limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  annotate(geom = "text", x = 50, y = 26.5, label = "Data not collected in Census", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin11.p

# 2016
# Create GAM model
pMin16.gam = gam(cc ~ s(pMin, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pMin16.gam) # R2 = 0.55***
gam.check(pMin16.gam) # Should see convergence, p should not be significant
plot(pMin16.gam)

pMin16_pred = data.frame(pMin = census2016$pMin, cc = census2016$cc, Pop = census2016$Pop, pred = predict(pMin16.gam, newdata = census2016))

# Plot
pMin16.p = ggplot(pMin16_pred, aes(x = pMin, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.53***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin16.p

tiff("pMin_CC_Peel_71to16_1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pMin71.p | pMin81.p | pMin86.p) /
  (pMin91.p | pMin96.p | pMin01.p) /
  (pMin06.p | pMin11.p | pMin16.p)
dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 100
ymin = 0
ymax = 45

# Find weighted mean location (closest real point) on predicted line
pMin_mn_row71 = first(which(abs(pMin71_pred$pMin - weighted.mean(x = pMin71_pred$pMin, w = pMin71_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin71_pred$pMin - weighted.mean(x = pMin71_pred$pMin, w = pMin71_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin71_pred[pMin_mn_row71,]

pMin_mn_row81 = first(which(abs(pMin81_pred$pMin - weighted.mean(x = pMin81_pred$pMin, w = pMin81_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin81_pred$pMin - weighted.mean(x = pMin81_pred$pMin, w = pMin81_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin81_pred[pMin_mn_row81,]

pMin_mn_row86 = first(which(abs(pMin86_pred$pMin - weighted.mean(x = pMin86_pred$pMin, w = pMin86_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin86_pred$pMin - weighted.mean(x = pMin86_pred$pMin, w = pMin86_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin86_pred[pMin_mn_row86,]

pMin_mn_row91 = first(which(abs(pMin91_pred$pMin - weighted.mean(x = pMin91_pred$pMin, w = pMin91_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin91_pred$pMin - weighted.mean(x = pMin91_pred$pMin, w = pMin91_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin91_pred[pMin_mn_row91,]

pMin_mn_row96 = first(which(abs(pMin96_pred$pMin - weighted.mean(x = pMin96_pred$pMin, w = pMin96_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin96_pred$pMin - weighted.mean(x = pMin96_pred$pMin, w = pMin96_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin96_pred[pMin_mn_row96,]

pMin_mn_row01 = first(which(abs(pMin01_pred$pMin - weighted.mean(x = pMin01_pred$pMin, w = pMin01_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin01_pred$pMin - weighted.mean(x = pMin01_pred$pMin, w = pMin01_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin01_pred[pMin_mn_row01,]

pMin_mn_row06 = first(which(abs(pMin06_pred$pMin - weighted.mean(x = pMin06_pred$pMin, w = pMin06_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin06_pred$pMin - weighted.mean(x = pMin06_pred$pMin, w = pMin06_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin06_pred[pMin_mn_row06,]

pMin_mn_row16 = first(which(abs(pMin16_pred$pMin - weighted.mean(x = pMin16_pred$pMin, w = pMin16_pred$Pop, na.rm = TRUE)) == 
                                 min(abs(pMin16_pred$pMin - weighted.mean(x = pMin16_pred$pMin, w = pMin16_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin16_pred[pMin_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pMin_com.p = ggplot() +
  geom_line(aes(x = pMin, y = pred, col = "1971"), data = pMin71_pred) + 
  geom_point(aes(x = pMin71_pred$pMin[pMin_mn_row71], y = pMin71_pred$pred[pMin_mn_row71]), color = "black") + 
  geom_line(aes(x = pMin, y = pred, col = "1981"), data = pMin81_pred) + 
  geom_point(aes(x = pMin81_pred$pMin[pMin_mn_row81], y = pMin81_pred$pred[pMin_mn_row81]), color = "sienna") + 
  geom_line(aes(x = pMin, y = pred, col = "1986"), data = pMin86_pred) + 
  geom_point(aes(x = pMin86_pred$pMin[pMin_mn_row86], y = pMin86_pred$pred[pMin_mn_row86]), color = "purple4") + 
  geom_line(aes(x = pMin, y = pred, col = "1991"), data = pMin91_pred) + 
  geom_point(aes(x = pMin91_pred$pMin[pMin_mn_row91], y = pMin91_pred$pred[pMin_mn_row91]), color = "blue") + 
  geom_line(aes(x = pMin, y = pred, col = "1996"), data = pMin96_pred) + 
  geom_point(aes(x = pMin96_pred$pMin[pMin_mn_row96], y = pMin96_pred$pred[pMin_mn_row96]), color = "steelblue2") + 
  geom_line(aes(x = pMin, y = pred, col = "2001"), data = pMin01_pred) + 
  geom_point(aes(x = pMin01_pred$pMin[pMin_mn_row01], y = pMin01_pred$pred[pMin_mn_row01]), color = "green4") + 
  geom_line(aes(x = pMin, y = pred, col = "2006"), data = pMin06_pred) + 
  geom_point(aes(x = pMin06_pred$pMin[pMin_mn_row06], y = pMin06_pred$pred[pMin_mn_row06]), color = "gold2") + 
  geom_line(aes(x = pMin, y = pred, col = "2016"), data = pMin16_pred) +
  geom_point(aes(x = pMin16_pred$pMin[pMin_mn_row16], y = pMin16_pred$pred[pMin_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 75, y = 42, label = "R² = 0.08*", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 75, y = 40, label = "R² = 0.16***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 75, y = 38, label = "R² = 0.16***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 75, y = 36, label = "R² = 0.24***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 75, y = 34, label = "R² = 0.33***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 75, y = 32, label = "R² = 0.36***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 75, y = 30, label = "R² = 0.43***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 75, y = 28, label = "R² = 0.53***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "Visible Minorities (%)", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.4, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pMin_com.p

#tiff("pMin_CC_Peel_combined1.tif", units = "cm", width = 10, height = 10, res = 300)
pMin_com.p
#dev.off()

##### Combined SocioEco Figure #####
tiff("SocioEco_71to16_1.tif", units = "cm", width = 12, height = 16.5, res = 300)
(MedInci_com.p + annotate(geom = "text", x = 30, y = 5, label = "A", fontface = "bold", size = 6)) / 
  ((pMin_com.p + annotate(geom = "text", x = 5, y = 5, label = "B", fontface = "bold", size = 6))) 
dev.off()
#####

# By Municipality - Separate Figures
# 1971
# Create GAM models
pMin71.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin71.gam_m) # R2 = 0.12***
gam.check(pMin71.gam_m) # Should see convergence, p should not be significant
plot(pMin71.gam_m)

pMin71_pred_m = data.frame(pMin = subset(census1971$pMin, census1971$mun == "Mississauga"), 
                           cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                           Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                           pred = predict(pMin71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

pMin71.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1971, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin71.gam_b) # R2 = 0.07*
gam.check(pMin71.gam_b) # Should see convergence, p should not be significant
plot(pMin71.gam_b)

pMin71_pred_b = data.frame(pMin = subset(census1971$pMin, census1971$mun == "Brampton"), 
                           cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                           Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                           pred = predict(pMin71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
pMin71_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = pMin, y = pred), data = pMin71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.12***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.07*   ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin71_mun.p

# 1981
# Create GAM models
pMin81.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin81.gam_m) # R2 = 0.23***
gam.check(pMin81.gam_m) # Should see convergence, p should not be significant
plot(pMin81.gam_m)

pMin81_pred_m = data.frame(pMin = subset(census1981$pMin, census1981$mun == "Mississauga"), 
                           cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                           Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                           pred = predict(pMin81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

pMin81.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin81.gam_b) # R2 = 0.24***
gam.check(pMin81.gam_b) # Should see convergence, p should not be significant
plot(pMin81.gam_b)

pMin81_pred_b = data.frame(pMin = subset(census1981$pMin, census1981$mun == "Brampton"), 
                           cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                           Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                           pred = predict(pMin81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
pMin81_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = pMin, y = pred), data = pMin81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.23***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.24***", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin81_mun.p

# 1986
# Create GAM models
pMin86.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin86.gam_m) # R2 = 0.24***
gam.check(pMin86.gam_m) # Should see convergence, p should not be significant
plot(pMin86.gam_m)

pMin86_pred_m = data.frame(pMin = subset(census1986$pMin, census1986$mun == "Mississauga"), 
                           cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                           Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                           pred = predict(pMin86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

pMin86.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin86.gam_b) # R2 = 0.11***
gam.check(pMin86.gam_b) # Should see convergence, p should not be significant
plot(pMin86.gam_b)

pMin86_pred_b = data.frame(pMin = subset(census1986$pMin, census1986$mun == "Brampton"), 
                           cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                           Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                           pred = predict(pMin86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
pMin86_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = pMin, y = pred), data = pMin86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.24***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.11***", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin86_mun.p

# 1991
# Create GAM models
pMin91.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin91.gam_m) # R2 = 0.32***
gam.check(pMin91.gam_m) # Should see convergence, p should not be significant
plot(pMin91.gam_m)

pMin91_pred_m = data.frame(pMin = subset(census1991$pMin, census1991$mun == "Mississauga"), 
                           cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                           pred = predict(pMin91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

pMin91.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin91.gam_b) # R2 = 0.24***
gam.check(pMin91.gam_b) # Should see convergence, p should not be significant
plot(pMin91.gam_b)

pMin91_pred_b = data.frame(pMin = subset(census1991$pMin, census1991$mun == "Brampton"), 
                           cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                           pred = predict(pMin91.gam_b, newdata = subset(census1991, mun == "Brampton")))

pMin91.gam_c = gam(cc ~ s(pMin, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin91.gam_c) # R2 = 0.00
gam.check(pMin91.gam_c) # Should see convergence, p should not be significant
plot(pMin91.gam_c)

pMin91_pred_c = data.frame(pMin = subset(census1991$pMin, census1991$mun == "Caledon"), 
                           cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                           pred = predict(pMin91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
pMin91_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pMin, y = pred), data = pMin91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.32***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.24***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.00    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin91_mun.p

# 1996
# Create GAM models
pMin96.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin96.gam_m) # R2 = 0.40***
gam.check(pMin96.gam_m) # Should see convergence, p should not be significant
plot(pMin96.gam_m)

pMin96_pred_m = data.frame(pMin = subset(census1996$pMin, census1996$mun == "Mississauga"), 
                           cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                           pred = predict(pMin96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

pMin96.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin96.gam_b) # R2 = 0.35***
gam.check(pMin96.gam_b) # Should see convergence, p should not be significant
plot(pMin96.gam_b)

pMin96_pred_b = data.frame(pMin = subset(census1996$pMin, census1996$mun == "Brampton"), 
                           cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                           pred = predict(pMin96.gam_b, newdata = subset(census1996, mun == "Brampton")))

pMin96.gam_c = gam(cc ~ s(pMin, bs = "cs", k =  21), data = subset(census1996, mun == "Caledon"), # 33 = # of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin96.gam_c) # R2 = 0.03
gam.check(pMin96.gam_c) # Should see convergence, p should not be significant
plot(pMin96.gam_c)

pMin96_pred_c = data.frame(pMin = subset(census1996$pMin, census1996$mun == "Caledon"), 
                           cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                           pred = predict(pMin96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
pMin96_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pMin, y = pred), data = pMin96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.40***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.35***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.03    ", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin96_mun.p

# 2001
# Create GAM models
pMin01.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2001, mun == "Mississauga"),
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin01.gam_m) # R2 = 0.44***
gam.check(pMin01.gam_m) # Should see convergence, p should not be significant
plot(pMin01.gam_m)

pMin01_pred_m = data.frame(pMin = subset(census2001$pMin, census2001$mun == "Mississauga"), 
                           cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                           pred = predict(pMin01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

pMin01.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin01.gam_b) # R2 = 0.38***
gam.check(pMin01.gam_b) # Should see convergence, p should not be significant
plot(pMin01.gam_b)

pMin01_pred_b = data.frame(pMin = subset(census2001$pMin, census2001$mun == "Brampton"), 
                           cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                           pred = predict(pMin01.gam_b, newdata = subset(census2001, mun == "Brampton")))

pMin01.gam_c = gam(cc ~ s(pMin, bs = "cs", k = 39), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin01.gam_c) # R2 = 0.38***
gam.check(pMin01.gam_c) # Should see convergence, p should not be significant
plot(pMin01.gam_c)

pMin01_pred_c = data.frame(pMin = subset(census2001$pMin, census2001$mun == "Caledon"), 
                           cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                           pred = predict(pMin01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
pMin01_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pMin, y = pred), data = pMin01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.44***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.38***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.38***", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pMin01_mun.p

# 2006
# Create GAM models
pMin06.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"),
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin06.gam_m) # R2 = 0.51***
gam.check(pMin06.gam_m) # Should see convergence, p should not be significant
plot(pMin06.gam_m)

pMin06_pred_m = data.frame(pMin = subset(census2006$pMin, census2006$mun == "Mississauga"), 
                           cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                           pred = predict(pMin06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

pMin06.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin06.gam_b) # R2 = 0.48***
gam.check(pMin06.gam_b) # Should see convergence, p should not be significant
plot(pMin06.gam_b)

pMin06_pred_b = data.frame(pMin = subset(census2006$pMin, census2006$mun == "Brampton"), 
                           cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                           pred = predict(pMin06.gam_b, newdata = subset(census2006, mun == "Brampton")))

pMin06.gam_c = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2006, mun == "Caledon"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin06.gam_c) # R2 = 0.07*
gam.check(pMin06.gam_c) # Should see convergence, p should not be significant
plot(pMin06.gam_c)

pMin06_pred_c = data.frame(pMin = subset(census2006$pMin, census2006$mun == "Caledon"), 
                           cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                           pred = predict(pMin06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
pMin06_mun.p = ggplot() +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pMin, y = pred), data = pMin06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.51***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.48***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.07*   ", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin06_mun.p

# 2011
pMin11_mun.p = pMin11.p
pMin11_mun.p

# 2016
# Create GAM models
pMin16.gam_m = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin16.gam_m) # R2 = 0.52***
gam.check(pMin16.gam_m) # Should see convergence, p should not be significant
plot(pMin16.gam_m)

pMin16_pred_m = data.frame(pMin = subset(census2016$pMin, census2016$mun == "Mississauga"), 
                           cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                           pred = predict(pMin16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

pMin16.gam_b = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin16.gam_b) # R2 = 0.55***
gam.check(pMin16.gam_b) # Should see convergence, p should not be significant
plot(pMin16.gam_b)

pMin16_pred_b = data.frame(pMin = subset(census2016$pMin, census2016$mun == "Brampton"), 
                           cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                           pred = predict(pMin16.gam_b, newdata = subset(census2016, mun == "Brampton")))

pMin16.gam_c = gam(cc ~ s(pMin, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pMin16.gam_c) # R2 = 0.33***
gam.check(pMin16.gam_c) # Should see convergence, p should not be significant
plot(pMin16.gam_c)

pMin16_pred_c = data.frame(pMin = subset(census2016$pMin, census2016$mun == "Caledon"), 
                           cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                           pred = predict(pMin16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
pMin16_mun.p = ggplot(pMin16_pred, aes(x = pMin, y = cc, size = Pop)) + 
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pMin, y = cc, size = Pop), data = pMin16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pMin, y = pred), data = pMin16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pMin, y = pred), data = pMin16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pMin, y = pred), data = pMin16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.52***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.55***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.33***", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin16_mun.p

#tiff("pMin_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pMin71_mun.p | pMin81_mun.p | pMin86_mun.p) /
  (pMin91_mun.p | pMin96_mun.p | pMin01_mun.p) /
  (pMin06_mun.p | pMin11_mun.p | pMin16_mun.p)
#dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 100
ymin = 0
ymax = 45

pMin_com.p # Already built above

# Find weighted mean location (closest real point) on predicted line
pMin_m_mn_row71 = first(which(abs(pMin71_pred_m$pMin - weighted.mean(x = pMin71_pred_m$pMin, w = pMin71_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin71_pred_m$pMin - weighted.mean(x = pMin71_pred_m$pMin, w = pMin71_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin71_pred_m[pMin_m_mn_row71,]

pMin_m_mn_row81 = first(which(abs(pMin81_pred_m$pMin - weighted.mean(x = pMin81_pred_m$pMin, w = pMin81_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin81_pred_m$pMin - weighted.mean(x = pMin81_pred_m$pMin, w = pMin81_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin81_pred_m[pMin_m_mn_row81,]

pMin_m_mn_row86 = first(which(abs(pMin86_pred_m$pMin - weighted.mean(x = pMin86_pred_m$pMin, w = pMin86_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin86_pred_m$pMin - weighted.mean(x = pMin86_pred_m$pMin, w = pMin86_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin86_pred_m[pMin_m_mn_row86,]

pMin_m_mn_row91 = first(which(abs(pMin91_pred_m$pMin - weighted.mean(x = pMin91_pred_m$pMin, w = pMin91_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin91_pred_m$pMin - weighted.mean(x = pMin91_pred_m$pMin, w = pMin91_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin91_pred_m[pMin_m_mn_row91,]

pMin_m_mn_row96 = first(which(abs(pMin96_pred_m$pMin - weighted.mean(x = pMin96_pred_m$pMin, w = pMin96_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin96_pred_m$pMin - weighted.mean(x = pMin96_pred_m$pMin, w = pMin96_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin96_pred_m[pMin_m_mn_row96,]

pMin_m_mn_row01 = first(which(abs(pMin01_pred_m$pMin - weighted.mean(x = pMin01_pred_m$pMin, w = pMin01_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin01_pred_m$pMin - weighted.mean(x = pMin01_pred_m$pMin, w = pMin01_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin01_pred_m[pMin_m_mn_row01,]

pMin_m_mn_row06 = first(which(abs(pMin06_pred_m$pMin - weighted.mean(x = pMin06_pred_m$pMin, w = pMin06_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin06_pred_m$pMin - weighted.mean(x = pMin06_pred_m$pMin, w = pMin06_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin06_pred_m[pMin_m_mn_row06,]

pMin_m_mn_row16 = first(which(abs(pMin16_pred_m$pMin - weighted.mean(x = pMin16_pred_m$pMin, w = pMin16_pred_m$Pop, na.rm = TRUE)) == 
                                   min(abs(pMin16_pred_m$pMin - weighted.mean(x = pMin16_pred_m$pMin, w = pMin16_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin16_pred_m[pMin_m_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

# Mississauga
pMin_com_m.p = ggplot() +
  geom_line(aes(x = pMin, y = pred, col = "1971"), data = pMin71_pred_m) + 
  geom_point(aes(x = pMin71_pred_m$pMin[pMin_m_mn_row71], y = pMin71_pred_m$pred[pMin_m_mn_row71]), color = "black") +
  geom_line(aes(x = pMin, y = pred, col = "1981"), data = pMin81_pred_m) + 
  geom_point(aes(x = pMin81_pred_m$pMin[pMin_m_mn_row81], y = pMin81_pred_m$pred[pMin_m_mn_row81]), color = "sienna") +
  geom_line(aes(x = pMin, y = pred, col = "1986"), data = pMin86_pred_m) + 
  geom_point(aes(x = pMin86_pred_m$pMin[pMin_m_mn_row86], y = pMin86_pred_m$pred[pMin_m_mn_row86]), color = "purple4") +
  geom_line(aes(x = pMin, y = pred, col = "1991"), data = pMin91_pred_m) + 
  geom_point(aes(x = pMin91_pred_m$pMin[pMin_m_mn_row91], y = pMin91_pred_m$pred[pMin_m_mn_row91]), color = "blue") +
  geom_line(aes(x = pMin, y = pred, col = "1996"), data = pMin96_pred_m) + 
  geom_point(aes(x = pMin96_pred_m$pMin[pMin_m_mn_row96], y = pMin96_pred_m$pred[pMin_m_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pMin, y = pred, col = "2001"), data = pMin01_pred_m) + 
  geom_point(aes(x = pMin01_pred_m$pMin[pMin_m_mn_row01], y = pMin01_pred_m$pred[pMin_m_mn_row01]), color = "green4") +
  geom_line(aes(x = pMin, y = pred, col = "2006"), data = pMin06_pred_m) + 
  geom_point(aes(x = pMin06_pred_m$pMin[pMin_m_mn_row06], y = pMin06_pred_m$pred[pMin_m_mn_row06]), color = "gold2") +
  geom_line(aes(x = pMin, y = pred, col = "2016"), data = pMin16_pred_m) +
  geom_point(aes(x = pMin16_pred_m$pMin[pMin_m_mn_row16], y = pMin16_pred_m$pred[pMin_m_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 70, y = 42, label = "R² = 0.12***", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 70, y = 40, label = "R² = 0.23***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 70, y = 38, label = "R² = 0.24***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 70, y = 36, label = "R² = 0.32***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 70, y = 34, label = "R² = 0.40***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 70, y = 32, label = "R² = 0.44***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 70, y = 30, label = "R² = 0.51***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 70, y = 28, label = "R² = 0.52***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 10, y = 41, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
pMin_com_m.p

# Brampton
# Find weighted mean location (closest real point) on predicted line
pMin_b_mn_row71 = first(which(abs(pMin71_pred_b$pMin - weighted.mean(x = pMin71_pred_b$pMin, w = pMin71_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin71_pred_b$pMin - weighted.mean(x = pMin71_pred_b$pMin, w = pMin71_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin71_pred_b[pMin_b_mn_row71,]

pMin_b_mn_row81 = first(which(abs(pMin81_pred_b$pMin - weighted.mean(x = pMin81_pred_b$pMin, w = pMin81_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin81_pred_b$pMin - weighted.mean(x = pMin81_pred_b$pMin, w = pMin81_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin81_pred_b[pMin_b_mn_row81,]

pMin_b_mn_row86 = first(which(abs(pMin86_pred_b$pMin - weighted.mean(x = pMin86_pred_b$pMin, w = pMin86_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin86_pred_b$pMin - weighted.mean(x = pMin86_pred_b$pMin, w = pMin86_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin86_pred_b[pMin_b_mn_row86,]

pMin_b_mn_row91 = first(which(abs(pMin91_pred_b$pMin - weighted.mean(x = pMin91_pred_b$pMin, w = pMin91_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin91_pred_b$pMin - weighted.mean(x = pMin91_pred_b$pMin, w = pMin91_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin91_pred_b[pMin_b_mn_row91,]

pMin_b_mn_row96 = first(which(abs(pMin96_pred_b$pMin - weighted.mean(x = pMin96_pred_b$pMin, w = pMin96_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin96_pred_b$pMin - weighted.mean(x = pMin96_pred_b$pMin, w = pMin96_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin96_pred_b[pMin_b_mn_row96,]

pMin_b_mn_row01 = first(which(abs(pMin01_pred_b$pMin - weighted.mean(x = pMin01_pred_b$pMin, w = pMin01_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin01_pred_b$pMin - weighted.mean(x = pMin01_pred_b$pMin, w = pMin01_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin01_pred_b[pMin_b_mn_row01,]

pMin_b_mn_row06 = first(which(abs(pMin06_pred_b$pMin - weighted.mean(x = pMin06_pred_b$pMin, w = pMin06_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin06_pred_b$pMin - weighted.mean(x = pMin06_pred_b$pMin, w = pMin06_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin06_pred_b[pMin_b_mn_row06,]

pMin_b_mn_row16 = first(which(abs(pMin16_pred_b$pMin - weighted.mean(x = pMin16_pred_b$pMin, w = pMin16_pred_b$Pop, na.rm = TRUE)) == 
                                min(abs(pMin16_pred_b$pMin - weighted.mean(x = pMin16_pred_b$pMin, w = pMin16_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin16_pred_b[pMin_b_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pMin_com_b.p = ggplot() +
  geom_line(aes(x = pMin, y = pred, col = "1971"), data = pMin71_pred_b) + 
  geom_point(aes(x = pMin71_pred_b$pMin[pMin_b_mn_row71], y = pMin71_pred_b$pred[pMin_b_mn_row71]), color = "black") +
  geom_line(aes(x = pMin, y = pred, col = "1981"), data = subset(pMin81_pred_b, pMin < 45)) + 
  geom_point(aes(x = pMin81_pred_b$pMin[pMin_b_mn_row81], y = pMin81_pred_b$pred[pMin_b_mn_row81]), color = "sienna") +
  geom_line(aes(x = pMin, y = pred, col = "1986"), data = subset(pMin86_pred_b, pMin < 50)) + 
  geom_point(aes(x = pMin86_pred_b$pMin[pMin_b_mn_row86], y = pMin86_pred_b$pred[pMin_b_mn_row86]), color = "purple4") +
  geom_line(aes(x = pMin, y = pred, col = "1991"), data = subset(pMin91_pred_b, pMin < 65)) + 
  geom_point(aes(x = pMin91_pred_b$pMin[pMin_b_mn_row91], y = pMin91_pred_b$pred[pMin_b_mn_row91]), color = "blue") +
  geom_line(aes(x = pMin, y = pred, col = "1996"), data = pMin96_pred_b) +
  geom_point(aes(x = pMin96_pred_b$pMin[pMin_b_mn_row96], y = pMin96_pred_b$pred[pMin_b_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pMin, y = pred, col = "2001"), data = pMin01_pred_b) + 
  geom_point(aes(x = pMin01_pred_b$pMin[pMin_b_mn_row01], y = pMin01_pred_b$pred[pMin_b_mn_row01]), color = "green4") +
  geom_line(aes(x = pMin, y = pred, col = "2006"), data = pMin06_pred_b) + 
  geom_point(aes(x = pMin06_pred_b$pMin[pMin_b_mn_row06], y = pMin06_pred_b$pred[pMin_b_mn_row06]), color = "gold2") +
  geom_line(aes(x = pMin, y = pred, col = "2016"), data = pMin16_pred_b) +
  geom_point(aes(x = pMin16_pred_b$pMin[pMin_b_mn_row16], y = pMin16_pred_b$pred[pMin_b_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 70, y = 42, label = "R² = 0.07*   ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 70, y = 40, label = "R² = 0.24***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 70, y = 38, label = "R² = 0.11***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 70, y = 36, label = "R² = 0.24***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 70, y = 34, label = "R² = 0.35***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 70, y = 32, label = "R² = 0.38***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 70, y = 30, label = "R² = 0.48***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 70, y = 28, label = "R² = 0.55***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "% Racial Minorities", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pMin_com_b.p

# Caledon
# Find weighted mean location (closest real point) on predicted line
pMin_c_mn_row91 = first(which(abs(pMin91_pred_c$pMin - weighted.mean(x = pMin91_pred_c$pMin, w = pMin91_pred_c$Pop, na.rm = TRUE)) == 
                                min(abs(pMin91_pred_c$pMin - weighted.mean(x = pMin91_pred_c$pMin, w = pMin91_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin91_pred_c[pMin_c_mn_row91,]

pMin_c_mn_row96 = first(which(abs(pMin96_pred_c$pMin - weighted.mean(x = pMin96_pred_c$pMin, w = pMin96_pred_c$Pop, na.rm = TRUE)) == 
                                min(abs(pMin96_pred_c$pMin - weighted.mean(x = pMin96_pred_c$pMin, w = pMin96_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin96_pred_c[pMin_c_mn_row96,]

pMin_c_mn_row01 = first(which(abs(pMin01_pred_c$pMin - weighted.mean(x = pMin01_pred_c$pMin, w = pMin01_pred_c$Pop, na.rm = TRUE)) == 
                                min(abs(pMin01_pred_c$pMin - weighted.mean(x = pMin01_pred_c$pMin, w = pMin01_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin01_pred_c[pMin_c_mn_row01,]

pMin_c_mn_row06 = first(which(abs(pMin06_pred_c$pMin - weighted.mean(x = pMin06_pred_c$pMin, w = pMin06_pred_c$Pop, na.rm = TRUE)) == 
                                min(abs(pMin06_pred_c$pMin - weighted.mean(x = pMin06_pred_c$pMin, w = pMin06_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin06_pred_c[pMin_c_mn_row06,]

pMin_c_mn_row16 = first(which(abs(pMin16_pred_c$pMin - weighted.mean(x = pMin16_pred_c$pMin, w = pMin16_pred_c$Pop, na.rm = TRUE)) == 
                                min(abs(pMin16_pred_c$pMin - weighted.mean(x = pMin16_pred_c$pMin, w = pMin16_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pMin16_pred_c[pMin_c_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pMin_com_c.p = ggplot() +
  geom_line(aes(x = pMin, y = pred, col = "1991"), data = pMin91_pred_c) + 
  geom_point(aes(x = pMin91_pred_c$pMin[pMin_c_mn_row91], y = pMin91_pred_c$pred[pMin_c_mn_row91]), color = "blue") +
  geom_line(aes(x = pMin, y = pred, col = "1996"), data = subset(pMin96_pred_c, pMin < 20)) + 
  geom_point(aes(x = pMin96_pred_c$pMin[pMin_c_mn_row96], y = pMin96_pred_c$pred[pMin_c_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pMin, y = pred, col = "2001"), data = subset(pMin01_pred_c, pMin < 25)) + 
  geom_point(aes(x = pMin01_pred_c$pMin[pMin_c_mn_row01], y = pMin01_pred_c$pred[pMin_c_mn_row01]), color = "green4") +
  geom_line(aes(x = pMin, y = pred, col = "2006"), data = subset(pMin06_pred_c, pMin < 30)) + 
  geom_point(aes(x = pMin06_pred_c$pMin[pMin_c_mn_row06], y = pMin06_pred_c$pred[pMin_c_mn_row06]), color = "gold2") +
  geom_line(aes(x = pMin, y = pred, col = "2016"), data = pMin16_pred_c) +
  geom_point(aes(x = pMin16_pred_c$pMin[pMin_c_mn_row16], y = pMin16_pred_c$pred[pMin_c_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 70, y = 36, label = "R² = 0.00", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 70, y = 34, label = "R² = 0.03", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 70, y = 32, label = "R² = 0.38***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 70, y = 30, label = "R² = 0.07", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 70, y = 28, label = "R² = 0.22*", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = "Visible Minorities (%)", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors2) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),       
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pMin_com_c.p

#tiff("pMin_CC_bymun_combined1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pMin_com.p + annotate(geom = "text", x = 5, y = 40, label = "A", fontface = "bold", size = 6)) /
  (pMin_com_m.p | pMin_com_b.p | pMin_com_c.p)
#dev.off()
#####

##### CC by year - see individual relationships - % Origins in NW Europe #####
### Linear model ###
# 1971
pNWEur71.lm = lm(cc ~ pNWEur, census1971)
summary(pNWEur71.lm) # R2 = 0.02*
plot(cc ~ pNWEur, census1971)
abline(pNWEur71.lm)

# 1981
pNWEur81.lm = lm(cc ~ pNWEur, census1981)
summary(pNWEur81.lm) # R2 = 0.12***
plot(cc ~ pNWEur, census1981)
abline(pNWEur81.lm)

# 1986
pNWEur86.lm = lm(cc ~ pNWEur, census1986)
summary(pNWEur86.lm) # R2 = 0.11***
plot(cc ~ pNWEur, census1986)
abline(pNWEur86.lm)

# 1991
pNWEur91.lm = lm(cc ~ pNWEur, census1991)
summary(pNWEur91.lm) # R2 = 0.19***
plot(cc ~ pNWEur, census1991)
abline(pNWEur91.lm)

# 1996
pNWEur96.lm = lm(cc ~ pNWEur, census1996)
summary(pNWEur96.lm) # R2 = 0.30***
plot(cc ~ pNWEur, census1996)
abline(pNWEur96.lm)

# 2001
pNWEur01.lm = lm(cc ~ pNWEur, census2001)
summary(pNWEur01.lm) # R2 = 0.35***
plot(cc ~ pNWEur, census2001)
abline(pNWEur01.lm)

# 2006
pNWEur06.lm = lm(cc ~ pNWEur, census2006)
summary(pNWEur06.lm) # R2 = 0.44***
plot(cc ~ pNWEur, census2006)
abline(pNWEur06.lm)

# 2011
# No Data

# 2016
pNWEur16.lm = lm(cc ~ pNWEur, census2016)
summary(pNWEur16.lm) # R2 = 0.46***
plot(cc ~ pNWEur, census2016)
abline(pNWEur16.lm)

### Weighted GAM Figures ###
xmin = 0
xmax = 100
ymin = 0
ymax = 53

# Peel - Separate Figures
# 1971
# Create GAM model
pNWEur71.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census1971, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur71.gam) # R2 = 0.03*
gam.check(pNWEur71.gam) # Should see convergence, p should not be significant
plot(pNWEur71.gam)

pNWEur71_pred = data.frame(pNWEur = census1971$pNWEur, cc = census1971$cc, Pop = census1971$Pop, pred = predict(pNWEur71.gam, newdata = census1971))

# Plot
pNWEur71.p = ggplot(pNWEur71_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.03*", size = eq_sz) +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur71.p

# 1981
# Create GAM model
pNWEur81.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census1981, weights = Pop, method = "REML") # Manually set K higher if is significant
summary(pNWEur81.gam) # R2 = 0.10***
gam.check(pNWEur81.gam) # Should see convergence, p should not be significant
plot(pNWEur81.gam)

pNWEur81_pred = data.frame(pNWEur = census1981$pNWEur, cc = census1981$cc, Pop = census1981$Pop, pred = predict(pNWEur81.gam, newdata = census1981))

# Plot
pNWEur81.p = ggplot(pNWEur81_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.09***", size = eq_sz) +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur81.p

# 1986
# Create GAM model
pNWEur86.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census1986, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pNWEur86.gam) # R2 = 0.09***
gam.check(pNWEur86.gam) # Should see convergence, p should not be significant
plot(pNWEur86.gam)

pNWEur86_pred = data.frame(pNWEur = census1986$pNWEur, cc = census1986$cc, Pop = census1986$Pop, pred = predict(pNWEur86.gam, newdata = census1986))

# Plot
pNWEur86.p = ggplot(pNWEur86_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.09***", size = eq_sz) +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur86.p

# 1991
# Create GAM model
pNWEur91.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census1991, weights = Pop, method = "REML") # Manually set K higher if p is significant 
summary(pNWEur91.gam) # R2 = 0.19***
gam.check(pNWEur91.gam) # Should see convergence, p should not be significant
plot(pNWEur91.gam)

pNWEur91_pred = data.frame(pNWEur = census1991$pNWEur, cc = census1991$cc, Pop = census1991$Pop, pred = predict(pNWEur91.gam, newdata = census1991))

# Plot
pNWEur91.p = ggplot(pNWEur91_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.19***", size = eq_sz) +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur91.p

# 1996
# Create GAM model
pNWEur96.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census1996, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pNWEur96.gam) # R2 = 0.33***
gam.check(pNWEur96.gam) # Should see convergence, p should not be significant
plot(pNWEur96.gam)

pNWEur96_pred = data.frame(pNWEur = census1996$pNWEur, cc = census1996$cc, Pop = census1996$Pop, pred = predict(pNWEur96.gam, newdata = census1996))

# Plot
pNWEur96.p = ggplot(pNWEur96_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.33***", size = eq_sz) +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur96.p

# 2001
# Create GAM model
pNWEur01.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census2001, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pNWEur01.gam) # R2 = 0.36***
gam.check(pNWEur01.gam) # Should see convergence, p should not be significant
plot(pNWEur01.gam)

pNWEur01_pred = data.frame(pNWEur = census2001$pNWEur, cc = census2001$cc, Pop = census2001$Pop, pred = predict(pNWEur01.gam, newdata = census2001))

# Plot
pNWEur01.p = ggplot(pNWEur01_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.36***", size = eq_sz) +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur01.p

# 2006
# Create GAM model
pNWEur06.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census2006, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pNWEur06.gam) # R2 = 0.44***
gam.check(pNWEur06.gam) # Should see convergence, p should not be significant
plot(pNWEur06.gam)

pNWEur06_pred = data.frame(pNWEur = census2006$pNWEur, cc = census2006$cc, Pop = census2006$Pop, pred = predict(pNWEur06.gam, newdata = census2006))

# Plot
pNWEur06.p = ggplot(pNWEur06_pred, aes(x = pNWEur, y = cc, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.44***", size = eq_sz) +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur06.p

# 2011
# Create blank plot
pNWEur11.p = ggplot() +
  scale_x_continuous(name = "% with Origins in NW Europe", limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  annotate(geom = "text", x = 50, y = 26.5, label = "Data not collected in Census", size = eq_sz) +
  ggtitle("2011") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur11.p

# 2016
# Create GAM model
pNWEur16.gam = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = census2016, weights = Pop, method = "REML") # Manually set K higher if p is significant
summary(pNWEur16.gam) # R2 = 0.49***
gam.check(pNWEur16.gam) # Should see convergence, p should not be significant
plot(pNWEur16.gam)

pNWEur16_pred = data.frame(pNWEur = census2016$pNWEur, cc = census2016$cc, Pop = census2016$Pop, pred = predict(pNWEur16.gam, newdata = census2016))

# Plot
pNWEur16.p = ggplot(pNWEur16_pred, aes(x = pNWEur, y = cc, size = Pop)) + 
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.49***", size = eq_sz) +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur16.p

#tiff("pNWEur_CC_Peel_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pNWEur71.p | pNWEur81.p | pNWEur86.p) /
  (pNWEur91.p | pNWEur96.p | pNWEur01.p) /
  (pNWEur06.p | pNWEur11.p | pNWEur16.p)
#dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 100
ymin = 0
ymax = 45

# Find weighted mean location (closest real point) on predicted line
pNWEur_mn_row71 = first(which(abs(pNWEur71_pred$pNWEur - weighted.mean(x = pNWEur71_pred$pNWEur, w = pNWEur71_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur71_pred$pNWEur - weighted.mean(x = pNWEur71_pred$pNWEur, w = pNWEur71_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur71_pred[pNWEur_mn_row71,]

pNWEur_mn_row81 = first(which(abs(pNWEur81_pred$pNWEur - weighted.mean(x = pNWEur81_pred$pNWEur, w = pNWEur81_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur81_pred$pNWEur - weighted.mean(x = pNWEur81_pred$pNWEur, w = pNWEur81_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur81_pred[pNWEur_mn_row81,]

pNWEur_mn_row86 = first(which(abs(pNWEur86_pred$pNWEur - weighted.mean(x = pNWEur86_pred$pNWEur, w = pNWEur86_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur86_pred$pNWEur - weighted.mean(x = pNWEur86_pred$pNWEur, w = pNWEur86_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur86_pred[pNWEur_mn_row86,]

pNWEur_mn_row91 = first(which(abs(pNWEur91_pred$pNWEur - weighted.mean(x = pNWEur91_pred$pNWEur, w = pNWEur91_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur91_pred$pNWEur - weighted.mean(x = pNWEur91_pred$pNWEur, w = pNWEur91_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur91_pred[pNWEur_mn_row91,]

pNWEur_mn_row96 = first(which(abs(pNWEur96_pred$pNWEur - weighted.mean(x = pNWEur96_pred$pNWEur, w = pNWEur96_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur96_pred$pNWEur - weighted.mean(x = pNWEur96_pred$pNWEur, w = pNWEur96_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur96_pred[pNWEur_mn_row96,]

pNWEur_mn_row01 = first(which(abs(pNWEur01_pred$pNWEur - weighted.mean(x = pNWEur01_pred$pNWEur, w = pNWEur01_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur01_pred$pNWEur - weighted.mean(x = pNWEur01_pred$pNWEur, w = pNWEur01_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur01_pred[pNWEur_mn_row01,]

pNWEur_mn_row06 = first(which(abs(pNWEur06_pred$pNWEur - weighted.mean(x = pNWEur06_pred$pNWEur, w = pNWEur06_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur06_pred$pNWEur - weighted.mean(x = pNWEur06_pred$pNWEur, w = pNWEur06_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur06_pred[pNWEur_mn_row06,]

pNWEur_mn_row16 = first(which(abs(pNWEur16_pred$pNWEur - weighted.mean(x = pNWEur16_pred$pNWEur, w = pNWEur16_pred$Pop, na.rm = TRUE)) == 
                              min(abs(pNWEur16_pred$pNWEur - weighted.mean(x = pNWEur16_pred$pNWEur, w = pNWEur16_pred$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur16_pred[pNWEur_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pNWEur_com.p = ggplot() +
  geom_line(aes(x = pNWEur, y = pred, col = "1971"), data = pNWEur71_pred) + 
  geom_point(aes(x = pNWEur71_pred$pNWEur[pNWEur_mn_row71], y = pNWEur71_pred$pred[pNWEur_mn_row71]), color = "black") + 
  geom_line(aes(x = pNWEur, y = pred, col = "1981"), data = pNWEur81_pred) + 
  geom_point(aes(x = pNWEur81_pred$pNWEur[pNWEur_mn_row81], y = pNWEur81_pred$pred[pNWEur_mn_row81]), color = "sienna") + 
  geom_line(aes(x = pNWEur, y = pred, col = "1986"), data = pNWEur86_pred) + 
  geom_point(aes(x = pNWEur86_pred$pNWEur[pNWEur_mn_row86], y = pNWEur86_pred$pred[pNWEur_mn_row86]), color = "purple4") + 
  geom_line(aes(x = pNWEur, y = pred, col = "1991"), data = pNWEur91_pred) + 
  geom_point(aes(x = pNWEur91_pred$pNWEur[pNWEur_mn_row91], y = pNWEur91_pred$pred[pNWEur_mn_row91]), color = "blue") + 
  geom_line(aes(x = pNWEur, y = pred, col = "1996"), data = pNWEur96_pred) + 
  geom_point(aes(x = pNWEur96_pred$pNWEur[pNWEur_mn_row96], y = pNWEur96_pred$pred[pNWEur_mn_row96]), color = "steelblue2") + 
  geom_line(aes(x = pNWEur, y = pred, col = "2001"), data = pNWEur01_pred) + 
  geom_point(aes(x = pNWEur01_pred$pNWEur[pNWEur_mn_row01], y = pNWEur01_pred$pred[pNWEur_mn_row01]), color = "green4") + 
  geom_line(aes(x = pNWEur, y = pred, col = "2006"), data = pNWEur06_pred) + 
  geom_point(aes(x = pNWEur06_pred$pNWEur[pNWEur_mn_row06], y = pNWEur06_pred$pred[pNWEur_mn_row06]), color = "gold2") + 
  geom_line(aes(x = pNWEur, y = pred, col = "2016"), data = subset(pNWEur16_pred, pNWEur < 75)) +
  geom_point(aes(x = pNWEur16_pred$pNWEur[pNWEur_mn_row16], y = pNWEur16_pred$pred[pNWEur_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 25, y = 42, label = "R² = 0.03*   ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 25, y = 40, label = "R² = 0.09***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 25, y = 38, label = "R² = 0.09***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 25, y = 36, label = "R² = 0.19***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 25, y = 34, label = "R² = 0.33***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 25, y = 32, label = "R² = 0.36***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 25, y = 30, label = "R² = 0.44***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 25, y = 28, label = "R² = 0.49***", size = eq_sz, color = "red3") +
  scale_x_continuous(name = "% with Origins in NW Europe", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.4, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pNWEur_com.p

#tiff("pNWEur_CC_Peel_combined1.tif", units = "cm", width = 10, height = 10, res = 300)
pNWEur_com.p
#dev.off()

# By Municipality - Separate Figures
# 1971
# Create GAM models
pNWEur71.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1971, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur71.gam_m) # R2 = 0.05**
gam.check(pNWEur71.gam_m) # Should see convergence, p should not be significant
plot(pNWEur71.gam_m)

pNWEur71_pred_m = data.frame(pNWEur = subset(census1971$pNWEur, census1971$mun == "Mississauga"), 
                           cc = subset(census1971$cc, census1971$mun == "Mississauga"), 
                           Pop = subset(census1971$Pop, census1971$mun == "Mississauga"), 
                           pred = predict(pNWEur71.gam_m, newdata = subset(census1971, mun == "Mississauga")))

pNWEur71.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1971, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur71.gam_b) # R2 = 0.5
gam.check(pNWEur71.gam_b) # Should see convergence, p should not be significant
plot(pNWEur71.gam_b)

pNWEur71_pred_b = data.frame(pNWEur = subset(census1971$pNWEur, census1971$mun == "Brampton"), 
                           cc = subset(census1971$cc, census1971$mun == "Brampton"), 
                           Pop = subset(census1971$Pop, census1971$mun == "Brampton"), 
                           pred = predict(pNWEur71.gam_b, newdata = subset(census1971, mun == "Brampton")))

# No GAM for Caledon, only 4 DAs

# Plot
pNWEur71_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur71_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur71_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = subset(census1971, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (4) for GAM
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur71_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur71_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur71_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.22***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.05**  ", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.05    ", size = eq_sz, col = "red3") +
  ggtitle("1971") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur71_mun.p

# 1981
# Create GAM models
pNWEur81.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1981, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur81.gam_m) # R2 = 0.16***
gam.check(pNWEur81.gam_m) # Should see convergence, p should not be significant
plot(pNWEur81.gam_m)

pNWEur81_pred_m = data.frame(pNWEur = subset(census1981$pNWEur, census1981$mun == "Mississauga"), 
                           cc = subset(census1981$cc, census1981$mun == "Mississauga"), 
                           Pop = subset(census1981$Pop, census1981$mun == "Mississauga"), 
                           pred = predict(pNWEur81.gam_m, newdata = subset(census1981, mun == "Mississauga")))

pNWEur81.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1981, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur81.gam_b) # R2 = 0.15***
gam.check(pNWEur81.gam_b) # Should see convergence, p should not be significant
plot(pNWEur81.gam_b)

pNWEur81_pred_b = data.frame(pNWEur = subset(census1981$pNWEur, census1981$mun == "Brampton"), 
                           cc = subset(census1981$cc, census1981$mun == "Brampton"), 
                           Pop = subset(census1981$Pop, census1981$mun == "Brampton"), 
                           pred = predict(pNWEur81.gam_b, newdata = subset(census1981, mun == "Brampton")))

# No GAM for Caledon, only 6 DAs

# Plot
pNWEur81_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur81_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur81_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = subset(census1981, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (6) for GAM
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur81_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur81_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur81_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.16***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.15***", size = eq_sz, col = "red3") +
  ggtitle("1981") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur81_mun.p

# 1986
# Create GAM models
pNWEur86.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1986, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur86.gam_m) # R2 = 0.07***
gam.check(pNWEur86.gam_m) # Should see convergence, p should not be significant
plot(pNWEur86.gam_m)

pNWEur86_pred_m = data.frame(pNWEur = subset(census1986$pNWEur, census1986$mun == "Mississauga"), 
                           cc = subset(census1986$cc, census1986$mun == "Mississauga"), 
                           Pop = subset(census1986$Pop, census1986$mun == "Mississauga"), 
                           pred = predict(pNWEur86.gam_m, newdata = subset(census1986, mun == "Mississauga")))

pNWEur86.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1986, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur86.gam_b) # R2 = 0.07***
gam.check(pNWEur86.gam_b) # Should see convergence, p should not be significant
plot(pNWEur86.gam_b)

pNWEur86_pred_b = data.frame(pNWEur = subset(census1986$pNWEur, census1986$mun == "Brampton"), 
                           cc = subset(census1986$cc, census1986$mun == "Brampton"), 
                           Pop = subset(census1986$Pop, census1986$mun == "Brampton"), 
                           pred = predict(pNWEur86.gam_b, newdata = subset(census1986, mun == "Brampton")))

# No GAM for Caledon, only 8 DAs

# Plot
pNWEur86_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur86_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur86_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = subset(census1986, mun == "Caledon"), alpha = alpha, col = "darkgreen") + # Not enough DAs (8) for GAM
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur86_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur86_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur86_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.07***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.07***", size = eq_sz, col = "red3") +
  ggtitle("1986") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur86_mun.p

# 1991
# Create GAM models
pNWEur91.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1991, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur91.gam_m) # R2 = 0.27***
gam.check(pNWEur91.gam_m) # Should see convergence, p should not be significant
plot(pNWEur91.gam_m)

pNWEur91_pred_m = data.frame(pNWEur = subset(census1991$pNWEur, census1991$mun == "Mississauga"), 
                           cc = subset(census1991$cc, census1991$mun == "Mississauga"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Mississauga"), 
                           pred = predict(pNWEur91.gam_m, newdata = subset(census1991, mun == "Mississauga")))

pNWEur91.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1991, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur91.gam_b) # R2 = 0.19***
gam.check(pNWEur91.gam_b) # Should see convergence, p should not be significant
plot(pNWEur91.gam_b)

pNWEur91_pred_b = data.frame(pNWEur = subset(census1991$pNWEur, census1991$mun == "Brampton"), 
                           cc = subset(census1991$cc, census1991$mun == "Brampton"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Brampton"), 
                           pred = predict(pNWEur91.gam_b, newdata = subset(census1991, mun == "Brampton")))

pNWEur91.gam_c = gam(cc ~ s(pNWEur, bs = "cs", k = 12), data = subset(census1991, mun == "Caledon"), # k = 12 (# of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur91.gam_c) # R2 = 0.58**
gam.check(pNWEur91.gam_c) # Should see convergence, p should not be significant
plot(pNWEur91.gam_c)

pNWEur91_pred_c = data.frame(pNWEur = subset(census1991$pNWEur, census1991$mun == "Caledon"), 
                           cc = subset(census1991$cc, census1991$mun == "Caledon"), 
                           Pop = subset(census1991$Pop, census1991$mun == "Caledon"), 
                           pred = predict(pNWEur91.gam_c, newdata = subset(census1991, mun == "Caledon")))

# Plot
pNWEur91_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur91_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur91_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur91_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur91_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur91_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur91_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur91_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.44***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.27***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.19***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.58**  ", size = eq_sz, col = "darkgreen") +
  ggtitle("1991") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur91_mun.p

# 1996
# Create GAM models
pNWEur96.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1996, mun == "Mississauga"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur96.gam_m) # R2 = 0.37***
gam.check(pNWEur96.gam_m) # Should see convergence, p should not be significant
plot(pNWEur96.gam_m)

pNWEur96_pred_m = data.frame(pNWEur = subset(census1996$pNWEur, census1996$mun == "Mississauga"), 
                           cc = subset(census1996$cc, census1996$mun == "Mississauga"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Mississauga"), 
                           pred = predict(pNWEur96.gam_m, newdata = subset(census1996, mun == "Mississauga")))

pNWEur96.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census1996, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur96.gam_b) # R2 = 0.39***
gam.check(pNWEur96.gam_b) # Should see convergence, p should not be significant
plot(pNWEur96.gam_b)

pNWEur96_pred_b = data.frame(pNWEur = subset(census1996$pNWEur, census1996$mun == "Brampton"), 
                           cc = subset(census1996$cc, census1996$mun == "Brampton"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Brampton"), 
                           pred = predict(pNWEur96.gam_b, newdata = subset(census1996, mun == "Brampton")))

pNWEur96.gam_c = gam(cc ~ s(pNWEur, bs = "cs", k =  21), data = subset(census1996, mun == "Caledon"), # 33 = # of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur96.gam_c) # R2 = 0.52***
gam.check(pNWEur96.gam_c) # Should see convergence, p should not be significant
plot(pNWEur96.gam_c)

pNWEur96_pred_c = data.frame(pNWEur = subset(census1996$pNWEur, census1996$mun == "Caledon"), 
                           cc = subset(census1996$cc, census1996$mun == "Caledon"), 
                           Pop = subset(census1996$Pop, census1996$mun == "Caledon"), 
                           pred = predict(pNWEur96.gam_c, newdata = subset(census1996, mun == "Caledon")))

# Plot
pNWEur96_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur96_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur96_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur96_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur96_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur96_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur96_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur96_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.39***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.37***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.39***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.52***", size = eq_sz, col = "darkgreen") +
  ggtitle("1996") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur96_mun.p

# 2001
# Create GAM models
pNWEur01.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2001, mun == "Mississauga"),
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur01.gam_m) # R2 = 0.41***
gam.check(pNWEur01.gam_m) # Should see convergence, p should not be significant
plot(pNWEur01.gam_m)

pNWEur01_pred_m = data.frame(pNWEur = subset(census2001$pNWEur, census2001$mun == "Mississauga"), 
                           cc = subset(census2001$cc, census2001$mun == "Mississauga"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Mississauga"), 
                           pred = predict(pNWEur01.gam_m, newdata = subset(census2001, mun == "Mississauga")))

pNWEur01.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2001, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur01.gam_b) # R2 = 0.40***
gam.check(pNWEur01.gam_b) # Should see convergence, p should not be significant
plot(pNWEur01.gam_b)

pNWEur01_pred_b = data.frame(pNWEur = subset(census2001$pNWEur, census2001$mun == "Brampton"), 
                           cc = subset(census2001$cc, census2001$mun == "Brampton"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Brampton"), 
                           pred = predict(pNWEur01.gam_b, newdata = subset(census2001, mun == "Brampton")))

pNWEur01.gam_c = gam(cc ~ s(pNWEur, bs = "cs", k = 39), data = subset(census2001, mun == "Caledon"), # k = 40 (close to # of Caledon DAs)
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur01.gam_c) # R2 = 0.74***
gam.check(pNWEur01.gam_c) # Should see convergence, p should not be significant
plot(pNWEur01.gam_c)

pNWEur01_pred_c = data.frame(pNWEur = subset(census2001$pNWEur, census2001$mun == "Caledon"), 
                           cc = subset(census2001$cc, census2001$mun == "Caledon"), 
                           Pop = subset(census2001$Pop, census2001$mun == "Caledon"), 
                           pred = predict(pNWEur01.gam_c, newdata = subset(census2001, mun == "Caledon")))

# Plot
pNWEur01_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur01_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur01_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur01_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur01_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur01_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur01_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur01_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) +  
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.41***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.41***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.40***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.74***", size = eq_sz, col = "darkgreen") +
  ggtitle("2001") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
pNWEur01_mun.p

# 2006
# Create GAM models
pNWEur06.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2006, mun == "Mississauga"),
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur06.gam_m) # R2 = 0.46***
gam.check(pNWEur06.gam_m) # Should see convergence, p should not be significant
plot(pNWEur06.gam_m)

pNWEur06_pred_m = data.frame(pNWEur = subset(census2006$pNWEur, census2006$mun == "Mississauga"), 
                           cc = subset(census2006$cc, census2006$mun == "Mississauga"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Mississauga"), 
                           pred = predict(pNWEur06.gam_m, newdata = subset(census2006, mun == "Mississauga")))

pNWEur06.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2006, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur06.gam_b) # R2 = 0.54***
gam.check(pNWEur06.gam_b) # Should see convergence, p should not be significant
plot(pNWEur06.gam_b)

pNWEur06_pred_b = data.frame(pNWEur = subset(census2006$pNWEur, census2006$mun == "Brampton"), 
                           cc = subset(census2006$cc, census2006$mun == "Brampton"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Brampton"), 
                           pred = predict(pNWEur06.gam_b, newdata = subset(census2006, mun == "Brampton")))

pNWEur06.gam_c = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2006, mun == "Caledon"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur06.gam_c) # R2 = 0.72***
gam.check(pNWEur06.gam_c) # Should see convergence, p should not be significant
plot(pNWEur06.gam_c)

pNWEur06_pred_c = data.frame(pNWEur = subset(census2006$pNWEur, census2006$mun == "Caledon"), 
                           cc = subset(census2006$cc, census2006$mun == "Caledon"), 
                           Pop = subset(census2006$Pop, census2006$mun == "Caledon"), 
                           pred = predict(pNWEur06.gam_c, newdata = subset(census2006, mun == "Caledon")))

# Plot
pNWEur06_mun.p = ggplot() +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur06_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur06_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur06_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur06_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur06_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur06_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur06_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.48***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.46***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.54***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.72***", size = eq_sz, col = "darkgreen") +
  ggtitle("2006") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur06_mun.p

# 2011
pNWEur11_mun.p = pNWEur11.p
pNWEur11_mun.p

# 2016
# Create GAM models
pNWEur16.gam_m = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2016, mun == "Mississauga"), # K increased until p-value > 0.05
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur16.gam_m) # R2 = 0.50***
gam.check(pNWEur16.gam_m) # Should see convergence, p should not be significant
plot(pNWEur16.gam_m)

pNWEur16_pred_m = data.frame(pNWEur = subset(census2016$pNWEur, census2016$mun == "Mississauga"), 
                           cc = subset(census2016$cc, census2016$mun == "Mississauga"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Mississauga"), 
                           pred = predict(pNWEur16.gam_m, newdata = subset(census2016, mun == "Mississauga")))

pNWEur16.gam_b = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2016, mun == "Brampton"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur16.gam_b) # R2 = 0.53***
gam.check(pNWEur16.gam_b) # Should see convergence, p should not be significant
plot(pNWEur16.gam_b)

pNWEur16_pred_b = data.frame(pNWEur = subset(census2016$pNWEur, census2016$mun == "Brampton"), 
                           cc = subset(census2016$cc, census2016$mun == "Brampton"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Brampton"), 
                           pred = predict(pNWEur16.gam_b, newdata = subset(census2016, mun == "Brampton")))

pNWEur16.gam_c = gam(cc ~ s(pNWEur, bs = "cs", k = k), data = subset(census2016, mun == "Caledon"), 
                   weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1)
summary(pNWEur16.gam_c) # R2 = 0.73***
gam.check(pNWEur16.gam_c) # Should see convergence, p should not be significant
plot(pNWEur16.gam_c)

pNWEur16_pred_c = data.frame(pNWEur = subset(census2016$pNWEur, census2016$mun == "Caledon"), 
                           cc = subset(census2016$cc, census2016$mun == "Caledon"), 
                           Pop = subset(census2016$Pop, census2016$mun == "Caledon"), 
                           pred = predict(pNWEur16.gam_c, newdata = subset(census2016, mun == "Caledon")))

# Plot
pNWEur16_mun.p = ggplot(pNWEur16_pred, aes(x = pNWEur, y = cc, size = Pop)) + 
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur16_pred_m, alpha = alpha, col = "blue") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur16_pred_b, alpha = alpha, col = "red3") +
  geom_point(aes(x = pNWEur, y = cc, size = Pop), data = pNWEur16_pred_c, alpha = alpha, col = "darkgreen") + 
  #geom_line(aes(x = pNWEur, y = pred), data = pNWEur16_pred, col = "black", lwd = big_ln) + # Peel GAM
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur16_pred_m, col = "blue", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur16_pred_b, col = "red3", lwd = big_ln) + # Add weighted GAM above into plot
  geom_line(aes(x = pNWEur, y = pred), data = pNWEur16_pred_c, col = "darkgreen", lwd = big_ln) + # Add weighted GAM above into plot
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0), breaks = c(25, 50, 75, 100)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_size_continuous(limits = c(pt_mn, pt_mx), range = c(pt_mn1, pt_mx1)) + 
  #annotate(geom = "text", x = 15000, y = 49, label = "R² = 0.56***", size = eq_sz) + # Peel R2
  annotate(geom = "text", x = 80, y = 49, label = "R² = 0.50***", size = eq_sz, col = "blue") +
  annotate(geom = "text", x = 80, y = 45, label = "R² = 0.53***", size = eq_sz, col = "red3") +
  annotate(geom = "text", x = 80, y = 41, label = "R² = 0.73***", size = eq_sz, col = "darkgreen") +
  ggtitle("2016") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.2, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur16_mun.p

#tiff("pNWEur_CC_bymun_71to16.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pNWEur71_mun.p | pNWEur81_mun.p | pNWEur86_mun.p) /
  (pNWEur91_mun.p | pNWEur96_mun.p | pNWEur01_mun.p) /
  (pNWEur06_mun.p | pNWEur11_mun.p | pNWEur16_mun.p)
#dev.off()
###

# Peel - Combined Figure
xmin = 0
xmax = 100
ymin = 0
ymax = 45

pNWEur_com.p # Already built above

# Mississauga
# Find weighted mean location (closest real point) on predicted line
pNWEur_m_mn_row71 = first(which(abs(pNWEur71_pred_m$pNWEur - weighted.mean(x = pNWEur71_pred_m$pNWEur, w = pNWEur71_pred_m$Pop, na.rm = TRUE)) == 
                                    min(abs(pNWEur71_pred_m$pNWEur - weighted.mean(x = pNWEur71_pred_m$pNWEur, w = pNWEur71_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur71_pred_m[pNWEur_m_mn_row71,]

pNWEur_m_mn_row81 = first(which(abs(pNWEur81_pred_m$pNWEur - weighted.mean(x = pNWEur81_pred_m$pNWEur, w = pNWEur81_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur81_pred_m$pNWEur - weighted.mean(x = pNWEur81_pred_m$pNWEur, w = pNWEur81_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur81_pred_m[pNWEur_m_mn_row81,]

pNWEur_m_mn_row86 = first(which(abs(pNWEur86_pred_m$pNWEur - weighted.mean(x = pNWEur86_pred_m$pNWEur, w = pNWEur86_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur86_pred_m$pNWEur - weighted.mean(x = pNWEur86_pred_m$pNWEur, w = pNWEur86_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur86_pred_m[pNWEur_m_mn_row86,]

pNWEur_m_mn_row91 = first(which(abs(pNWEur91_pred_m$pNWEur - weighted.mean(x = pNWEur91_pred_m$pNWEur, w = pNWEur91_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur91_pred_m$pNWEur - weighted.mean(x = pNWEur91_pred_m$pNWEur, w = pNWEur91_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur91_pred_m[pNWEur_m_mn_row91,]

pNWEur_m_mn_row96 = first(which(abs(pNWEur96_pred_m$pNWEur - weighted.mean(x = pNWEur96_pred_m$pNWEur, w = pNWEur96_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur96_pred_m$pNWEur - weighted.mean(x = pNWEur96_pred_m$pNWEur, w = pNWEur96_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur96_pred_m[pNWEur_m_mn_row96,]

pNWEur_m_mn_row01 = first(which(abs(pNWEur01_pred_m$pNWEur - weighted.mean(x = pNWEur01_pred_m$pNWEur, w = pNWEur01_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur01_pred_m$pNWEur - weighted.mean(x = pNWEur01_pred_m$pNWEur, w = pNWEur01_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur01_pred_m[pNWEur_m_mn_row01,]

pNWEur_m_mn_row06 = first(which(abs(pNWEur06_pred_m$pNWEur - weighted.mean(x = pNWEur06_pred_m$pNWEur, w = pNWEur06_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur06_pred_m$pNWEur - weighted.mean(x = pNWEur06_pred_m$pNWEur, w = pNWEur06_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur06_pred_m[pNWEur_m_mn_row06,]

pNWEur_m_mn_row16 = first(which(abs(pNWEur16_pred_m$pNWEur - weighted.mean(x = pNWEur16_pred_m$pNWEur, w = pNWEur16_pred_m$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur16_pred_m$pNWEur - weighted.mean(x = pNWEur16_pred_m$pNWEur, w = pNWEur16_pred_m$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur16_pred_m[pNWEur_m_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends
  
pNWEur_com_m.p = ggplot() +
  geom_line(aes(x = pNWEur, y = pred, col = "1971"), data = pNWEur71_pred_m) + 
  geom_point(aes(x = pNWEur71_pred_m$pNWEur[pNWEur_m_mn_row71], y = pNWEur71_pred_m$pred[pNWEur_m_mn_row71]), color = "black") +
  geom_line(aes(x = pNWEur, y = pred, col = "1981"), data = pNWEur81_pred_m) + 
  geom_point(aes(x = pNWEur81_pred_m$pNWEur[pNWEur_m_mn_row81], y = pNWEur81_pred_m$pred[pNWEur_m_mn_row81]), color = "sienna") +
  geom_line(aes(x = pNWEur, y = pred, col = "1986"), data = pNWEur86_pred_m) + 
  geom_point(aes(x = pNWEur86_pred_m$pNWEur[pNWEur_m_mn_row86], y = pNWEur86_pred_m$pred[pNWEur_m_mn_row86]), color = "purple4") +
  geom_line(aes(x = pNWEur, y = pred, col = "1991"), data = pNWEur91_pred_m) + 
  geom_point(aes(x = pNWEur91_pred_m$pNWEur[pNWEur_m_mn_row91], y = pNWEur91_pred_m$pred[pNWEur_m_mn_row91]), color = "blue") +
  geom_line(aes(x = pNWEur, y = pred, col = "1996"), data = pNWEur96_pred_m) + 
  geom_point(aes(x = pNWEur96_pred_m$pNWEur[pNWEur_m_mn_row96], y = pNWEur96_pred_m$pred[pNWEur_m_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pNWEur, y = pred, col = "2001"), data = pNWEur01_pred_m) + 
  geom_point(aes(x = pNWEur01_pred_m$pNWEur[pNWEur_m_mn_row01], y = pNWEur01_pred_m$pred[pNWEur_m_mn_row01]), color = "green4") +
  geom_line(aes(x = pNWEur, y = pred, col = "2006"), data = pNWEur06_pred_m) + 
  geom_point(aes(x = pNWEur06_pred_m$pNWEur[pNWEur_m_mn_row06], y = pNWEur06_pred_m$pred[pNWEur_m_mn_row06]), color = "gold2") +
  geom_line(aes(x = pNWEur, y = pred, col = "2016"), data = subset(pNWEur16_pred_m, pNWEur < 70)) +
  geom_point(aes(x = pNWEur16_pred_m$pNWEur[pNWEur_m_mn_row16], y = pNWEur16_pred_m$pred[pNWEur_m_mn_row16]), color = "red3") +  
  annotate(geom = "text", x = 25, y = 42, label = "R² = 0.05**  ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 25, y = 40, label = "R² = 0.16***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 25, y = 38, label = "R² = 0.07***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 25, y = 36, label = "R² = 0.27***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 25, y = 34, label = "R² = 0.37***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 25, y = 32, label = "R² = 0.41***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 25, y = 30, label = "R² = 0.46***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 25, y = 28, label = "R² = 0.50***", size = eq_sz, color = "red3") +
  annotate(geom = "text", x = 10, y = 5, label = "B", fontface = "bold", size = 6) +
  ggtitle("Mississauga") +
  scale_x_continuous(name = NULL, limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = expression("CC"[r]*" (%)"), limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
pNWEur_com_m.p

# Brampton
# Find weighted mean location (closest real point) on predicted line
pNWEur_b_mn_row71 = first(which(abs(pNWEur71_pred_b$pNWEur - weighted.mean(x = pNWEur71_pred_b$pNWEur, w = pNWEur71_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur71_pred_b$pNWEur - weighted.mean(x = pNWEur71_pred_b$pNWEur, w = pNWEur71_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur71_pred_b[pNWEur_b_mn_row71,]

pNWEur_b_mn_row81 = first(which(abs(pNWEur81_pred_b$pNWEur - weighted.mean(x = pNWEur81_pred_b$pNWEur, w = pNWEur81_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur81_pred_b$pNWEur - weighted.mean(x = pNWEur81_pred_b$pNWEur, w = pNWEur81_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur81_pred_b[pNWEur_b_mn_row81,]

pNWEur_b_mn_row86 = first(which(abs(pNWEur86_pred_b$pNWEur - weighted.mean(x = pNWEur86_pred_b$pNWEur, w = pNWEur86_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur86_pred_b$pNWEur - weighted.mean(x = pNWEur86_pred_b$pNWEur, w = pNWEur86_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur86_pred_b[pNWEur_b_mn_row86,]

pNWEur_b_mn_row91 = first(which(abs(pNWEur91_pred_b$pNWEur - weighted.mean(x = pNWEur91_pred_b$pNWEur, w = pNWEur91_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur91_pred_b$pNWEur - weighted.mean(x = pNWEur91_pred_b$pNWEur, w = pNWEur91_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur91_pred_b[pNWEur_b_mn_row91,]

pNWEur_b_mn_row96 = first(which(abs(pNWEur96_pred_b$pNWEur - weighted.mean(x = pNWEur96_pred_b$pNWEur, w = pNWEur96_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur96_pred_b$pNWEur - weighted.mean(x = pNWEur96_pred_b$pNWEur, w = pNWEur96_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur96_pred_b[pNWEur_b_mn_row96,]

pNWEur_b_mn_row01 = first(which(abs(pNWEur01_pred_b$pNWEur - weighted.mean(x = pNWEur01_pred_b$pNWEur, w = pNWEur01_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur01_pred_b$pNWEur - weighted.mean(x = pNWEur01_pred_b$pNWEur, w = pNWEur01_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur01_pred_b[pNWEur_b_mn_row01,]

pNWEur_b_mn_row06 = first(which(abs(pNWEur06_pred_b$pNWEur - weighted.mean(x = pNWEur06_pred_b$pNWEur, w = pNWEur06_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur06_pred_b$pNWEur - weighted.mean(x = pNWEur06_pred_b$pNWEur, w = pNWEur06_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur06_pred_b[pNWEur_b_mn_row06,]

pNWEur_b_mn_row16 = first(which(abs(pNWEur16_pred_b$pNWEur - weighted.mean(x = pNWEur16_pred_b$pNWEur, w = pNWEur16_pred_b$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur16_pred_b$pNWEur - weighted.mean(x = pNWEur16_pred_b$pNWEur, w = pNWEur16_pred_b$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur16_pred_b[pNWEur_b_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pNWEur_com_b.p = ggplot() +
  geom_line(aes(x = pNWEur, y = pred, col = "1971"), data = subset(pNWEur71_pred_b, pNWEur > 65)) + 
  geom_point(aes(x = pNWEur71_pred_b$pNWEur[pNWEur_b_mn_row71], y = pNWEur71_pred_b$pred[pNWEur_b_mn_row71]), color = "black") +
  geom_line(aes(x = pNWEur, y = pred, col = "1981"), data = subset(pNWEur81_pred_b, pNWEur > 30)) + 
  geom_point(aes(x = pNWEur81_pred_b$pNWEur[pNWEur_b_mn_row81], y = pNWEur81_pred_b$pred[pNWEur_b_mn_row81]), color = "sienna") +
  geom_line(aes(x = pNWEur, y = pred, col = "1986"), data = pNWEur86_pred_b) + 
  geom_point(aes(x = pNWEur86_pred_b$pNWEur[pNWEur_b_mn_row86], y = pNWEur86_pred_b$pred[pNWEur_b_mn_row86]), color = "purple4") +
  geom_line(aes(x = pNWEur, y = pred, col = "1991"), data = pNWEur91_pred_b) + 
  geom_point(aes(x = pNWEur91_pred_b$pNWEur[pNWEur_b_mn_row91], y = pNWEur91_pred_b$pred[pNWEur_b_mn_row91]), color = "blue") +
  geom_line(aes(x = pNWEur, y = pred, col = "1996"), data = pNWEur96_pred_b) +
  geom_point(aes(x = pNWEur96_pred_b$pNWEur[pNWEur_b_mn_row96], y = pNWEur96_pred_b$pred[pNWEur_b_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pNWEur, y = pred, col = "2001"), data = pNWEur01_pred_b) + 
  geom_point(aes(x = pNWEur01_pred_b$pNWEur[pNWEur_b_mn_row01], y = pNWEur01_pred_b$pred[pNWEur_b_mn_row01]), color = "green4") +
  geom_line(aes(x = pNWEur, y = pred, col = "2006"), data = pNWEur06_pred_b) + 
  geom_point(aes(x = pNWEur06_pred_b$pNWEur[pNWEur_b_mn_row06], y = pNWEur06_pred_b$pred[pNWEur_b_mn_row06]), color = "gold2") +
  geom_line(aes(x = pNWEur, y = pred, col = "2016"), data = subset(pNWEur16_pred_b, pNWEur < 70)) +
  geom_point(aes(x = pNWEur16_pred_b$pNWEur[pNWEur_b_mn_row16], y = pNWEur16_pred_b$pred[pNWEur_b_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 25, y = 42, label = "R² = 0.05     ", size = eq_sz, color = "black") +
  annotate(geom = "text", x = 25, y = 40, label = "R² = 0.15***", size = eq_sz, color = "sienna") +
  annotate(geom = "text", x = 25, y = 38, label = "R² = 0.07***", size = eq_sz, color = "purple4") +
  annotate(geom = "text", x = 25, y = 36, label = "R² = 0.19***", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 25, y = 34, label = "R² = 0.39***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 25, y = 32, label = "R² = 0.40***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 25, y = 30, label = "R² = 0.54***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 25, y = 28, label = "R² = 0.53***", size = eq_sz, color = "red3") +
  ggtitle("Brampton") +
  scale_x_continuous(name = "% with Origins in NW Europe", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.position = "none")
pNWEur_com_b.p

# Caledon

# Find weighted mean location (closest real point) on predicted line
pNWEur_c_mn_row91 = first(which(abs(pNWEur91_pred_c$pNWEur - weighted.mean(x = pNWEur91_pred_c$pNWEur, w = pNWEur91_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur91_pred_c$pNWEur - weighted.mean(x = pNWEur91_pred_c$pNWEur, w = pNWEur91_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur91_pred_c[pNWEur_c_mn_row91,]

pNWEur_c_mn_row96 = first(which(abs(pNWEur96_pred_c$pNWEur - weighted.mean(x = pNWEur96_pred_c$pNWEur, w = pNWEur96_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur96_pred_c$pNWEur - weighted.mean(x = pNWEur96_pred_c$pNWEur, w = pNWEur96_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur96_pred_c[pNWEur_c_mn_row96,]

pNWEur_c_mn_row01 = first(which(abs(pNWEur01_pred_c$pNWEur - weighted.mean(x = pNWEur01_pred_c$pNWEur, w = pNWEur01_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur01_pred_c$pNWEur - weighted.mean(x = pNWEur01_pred_c$pNWEur, w = pNWEur01_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur01_pred_c[pNWEur_c_mn_row01,]

pNWEur_c_mn_row06 = first(which(abs(pNWEur06_pred_c$pNWEur - weighted.mean(x = pNWEur06_pred_c$pNWEur, w = pNWEur06_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur06_pred_c$pNWEur - weighted.mean(x = pNWEur06_pred_c$pNWEur, w = pNWEur06_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur06_pred_c[pNWEur_c_mn_row06,]

pNWEur_c_mn_row16 = first(which(abs(pNWEur16_pred_c$pNWEur - weighted.mean(x = pNWEur16_pred_c$pNWEur, w = pNWEur16_pred_c$Pop, na.rm = TRUE)) == 
                                  min(abs(pNWEur16_pred_c$pNWEur - weighted.mean(x = pNWEur16_pred_c$pNWEur, w = pNWEur16_pred_c$Pop, na.rm = TRUE)), na.rm = TRUE)))
pNWEur16_pred_c[pNWEur_c_mn_row16,]

# Cut off GAM lines if there are outliers/weird changes near ends

pNWEur_com_c.p = ggplot() +
  geom_line(aes(x = pNWEur, y = pred, col = "1991"), data = pNWEur91_pred_c) + 
  geom_point(aes(x = pNWEur91_pred_c$pNWEur[pNWEur_c_mn_row91], y = pNWEur91_pred_c$pred[pNWEur_c_mn_row91]), color = "blue") +
  geom_line(aes(x = pNWEur, y = pred, col = "1996"), data = subset(pNWEur96_pred_c, pNWEur > 30)) + 
  geom_point(aes(x = pNWEur96_pred_c$pNWEur[pNWEur_c_mn_row96], y = pNWEur96_pred_c$pred[pNWEur_c_mn_row96]), color = "steelblue2") +
  geom_line(aes(x = pNWEur, y = pred, col = "2001"), data = pNWEur01_pred_c) + 
  geom_point(aes(x = pNWEur01_pred_c$pNWEur[pNWEur_c_mn_row01], y = pNWEur01_pred_c$pred[pNWEur_c_mn_row01]), color = "green4") +
  geom_line(aes(x = pNWEur, y = pred, col = "2006"), data = subset(pNWEur06_pred_c, pNWEur > 20)) + 
  geom_point(aes(x = pNWEur06_pred_c$pNWEur[pNWEur_c_mn_row06], y = pNWEur06_pred_c$pred[pNWEur_c_mn_row06]), color = "gold2") +
  geom_line(aes(x = pNWEur, y = pred, col = "2016"), data = pNWEur16_pred_c) +
  geom_point(aes(x = pNWEur16_pred_c$pNWEur[pNWEur_c_mn_row16], y = pNWEur16_pred_c$pred[pNWEur_c_mn_row16]), color = "red3") +
  annotate(geom = "text", x = 25, y = 36, label = "R² = 0.54**", size = eq_sz, color = "blue") +
  annotate(geom = "text", x = 25, y = 34, label = "R² = 0.46***", size = eq_sz, color = "steelblue2") +
  annotate(geom = "text", x = 25, y = 32, label = "R² = 0.71***", size = eq_sz, color = "green4") +
  annotate(geom = "text", x = 25, y = 30, label = "R² = 0.65***", size = eq_sz, color = "gold2") +
  annotate(geom = "text", x = 25, y = 28, label = "R² = 0.70***", size = eq_sz, color = "red3") +
  ggtitle("Caledon") +
  scale_x_continuous(name = "Origins in nortwest Europe (%)", limits = c(xmin, xmax), expand = c(0,0)) + 
  scale_y_continuous(name = "Residential Canopy Cover (%)", limits = c(ymin, ymax), expand = c(0,0)) +
  scale_color_manual(values = colors2) +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1),
        plot.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray95"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.1, "cm"),
        legend.key.size = unit(0.225, "cm"),
        legend.text = element_text(size = anno_sz))
pNWEur_com_c.p

#tiff("pNWEur_CC_bymun_combined1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pNWEur_com.p + annotate(geom = "text", x = 5, y = 48, label = "A", fontface = "bold", size = 6)) /
  (pNWEur_com_m.p | pNWEur_com_b.p | pNWEur_com_c.p)
#dev.off()
#####

##### Combined Racial plots - % Minorities and % Origins in NW Europe #####
#tiff("Racial_CC_71to16_combined.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pMin_com.p + annotate(geom = "text", x = 5, y = 40, label = "A", fontface = "bold", size = 6)) /
  (pNWEur_com.p + annotate(geom = "text", x = 5, y = 40, label = "B", fontface = "bold", size = 6))
#dev.off()

#tiff("Racial_CC_71to16_byMun.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pMin_com_m.p | pMin_com_b.p | pMin_com_c.p) / 
(pNWEur_com_m.p | pNWEur_com_b.p | pNWEur_com_c.p)
#dev.off()

tiff("Racial_CC_71to16_Caledon.tif", units = "cm", width = 12, height = 16.5, res = 300)
(pMin_com_c.p + annotate(geom = "text", x = 5, y = 40, label = "A", fontface = "bold", size = 6)) /
  (pNWEur_com_c.p + annotate(geom = "text", x = 5, y = 40, label = "B", fontface = "bold", size = 6))
dev.off()
#####