library(mgcv)
library(weights)
library(ape) # Morans I

##### Bring in Data #####
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change/CensusCC Compare/By PostDevelopment Year")

### Base DA Data ###
da = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change/DA_data1.csv")

#tbl = read.csv("cc_yr20_all.csv")

#tbl =  read.csv("cc_dev0_pop5.csv")

#tbl = read.csv("cc_yr0_hilo.csv")
#tbl = read.csv("cc_yr10_hilo.csv")
#tbl = read.csv("cc_yr20_hilo.csv")
#tbl = read.csv("cc_yr30_hilo.csv")
tbl = read.csv("cc_yr40_hilo.csv")
#####

##### Set PopDen above 20000 to High random value #####
tbl$PopDen[tbl$PopDen >= 20000] = sample(max(subset(tbl$PopDen, tbl$PopDen < 20000)):20000,1000)
#####

##### Add Latitude and Longitude  to table #####
lat_tbl = setDT(da)[DAUID %in% tbl$DAUID] 
tbl$lat = lat_tbl$Latitude 

long_tbl = setDT(da)[DAUID %in% tbl$DAUID] 
tbl$long = lat_tbl$Longitude
#####

##### Create Distance Tables - for Moran's I #####
#da.dist = as.matrix(dist(cbind(tbl$long, tbl$lat)))
#da.dist = 1 / da.dist
#diag(da.dist) <- 0
#####

# Initial Inputs
#col = which(colnames(tbl) == "pMin")
colx = which(colnames(tbl) == "cc_cat")
coly = which(colnames(tbl) == "cc_cat")

#data = tbl
datax = subset(tbl, tbl[,colx] == "High") # & tbl$yr40_cc >= 0
datay = subset(tbl, tbl[,coly] == "Low") #  & tbl$yr40_cc >= 0

#dist = da.dist
dist.x = as.matrix(dist(cbind(datax$long, datax$lat)))
dist.x = 1 / dist.x
diag(dist.x) <- 0

dist.y = as.matrix(dist(cbind(datay$long, datay$lat)))
dist.y = 1 / dist.y
diag(dist.y) <- 0

divx = 1 # How big to make % sample
divy = 1.1

#k = 15
#family = "gaussian"

##### GAM TESTING #####
hist(data[,col]) # Mostly normal, gaussian fine

gam = gam(CC ~ s(data[,col], bs = "cs", k = k), family = family, data = data, weights = Pop, method = "REML") # Manually set K higher if p is low (< 0.1) 
summary(gam) 
#gam.check(gam) # Should see convergence, p should not be significant
plot(gam)

Moran.I(data[,col], dist, na.rm = TRUE) # If significant - random sample
Moran.I(data$CC, dist, na.rm = TRUE) # If significant - random sample

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
  moranx = Moran.I(sample[,col], s.dist, na.rm = TRUE) 
  s.tbl$moran_ix[i] = moranx$observed
  s.tbl$moran_px[i] = moranx$p.value
  morany = Moran.I(sample$CC, s.dist, na.rm = TRUE) 
  s.tbl$moran_iy[i] = morany$observed
  s.tbl$moran_py[i] = morany$p.value
  
  s.gam = gam(CC ~ s(sample[,col], bs = "cs", k = k), family = family, data = sample, weights = Pop, method = "REML") 
  s.tbl$gam_n[i] = summary(s.gam)$n
  s.tbl$gam_r2[i] = summary(s.gam)$r.sq
  s.tbl$gam_dev[i] = summary(s.gam)$dev
  s.tbl$gam_p[i] = summary(s.gam)$s.table[,4]
}

# How many iterations have not significant Moran's I?
nrow(subset(s.tbl, moran_px > 0.05 & moran_py > 0.05)) 

s.tbl = subset(s.tbl, moran_px > 0.05 & moran_py > 0.05)
mean(s.tbl$gam_n)

mean(s.tbl$gam_r2)
sd(s.tbl$gam_r2)
hist(s.tbl$gam_r2)

mean(s.tbl$gam_dev)
sd(s.tbl$gam_dev)
hist(s.tbl$gam_dev)

mean(s.tbl$gam_p)
nrow(subset(s.tbl, gam_p > 0.05))
#####

### Try, sample for weighted different of means... 
# McDWTy for for year 20 figure
# Bunch of histogram figures
# Just sig - so once hits *** can stop for that set

nrow(datax)
nrow(datay)

wtd.t.test(x = datax$pMin,
           y = datay$pMin,
           weight = datax$Pop,
           weighty = datay$Pop,
           bootse = TRUE) 

Moran.I(datax$pMin, dist.x, na.rm = TRUE) # If significant - random sample
Moran.I(datay$pMin, dist.y, na.rm = TRUE) # If significant - random sample

# Automate!
iter = 1000 # 100 # 1000 # 5000

s.tbl = as.data.frame(matrix(nrow = iter, ncol = 1))
colnames(s.tbl) = c("Iteration")
s.tbl$Iteration = 1:iter

for (i in 1:nrow(s.tbl)) {
  samplex = datax[sample(nrow(datax), nrow(datax) / divx), ]
  s.distx = as.matrix(dist(cbind(samplex$long, samplex$lat)))
  s.distx = 1 / s.distx
  diag(s.distx) <- 0
  moranx = Moran.I(samplex$pMin, s.distx, na.rm = TRUE) 
  s.tbl$moran_ix[i] = moranx$observed
  s.tbl$moran_px[i] = moranx$p.value
  
  sampley = datay[sample(nrow(datay), nrow(datay) / divy), ]
  s.disty = as.matrix(dist(cbind(sampley$long, sampley$lat)))
  s.disty = 1 / s.disty
  diag(s.disty) <- 0
  morany = Moran.I(sampley$pMin, s.disty, na.rm = TRUE) 
  s.tbl$moran_iy[i] = morany$observed
  s.tbl$moran_py[i] = morany$p.value
  
  t = wtd.t.test(x = samplex$pMin,
                 y = sampley$pMin,
                 weight = samplex$Pop,
                 weighty = sampley$Pop,
                 bootse = TRUE)
  s.tbl$t_nx[i] = nrow(samplex)
  s.tbl$t_ny[i] = nrow(sampley)
  s.tbl$t_mnx[i] = t$additional[2]
  s.tbl$t_mny[i] = t$additional[3]
  s.tbl$t_diff[i] = t$additional[1]
  s.tbl$t_p[i] = t$coefficients[3]
}

# How many iterations have not significant Moran's I?
nrow(subset(s.tbl, moran_px > 0.05))
nrow(subset(s.tbl, moran_py > 0.05))
nrow(subset(s.tbl, moran_py > 0.05 & moran_px > 0.05)) # Try to have at least 100

s.tbl = subset(s.tbl, moran_py > 0.05 & moran_px > 0.05)
mean(s.tbl$t_nx) # Try to have at least 50
mean(s.tbl$t_ny) # Try to have at least 50

mean(s.tbl$t_mnx)
mean(s.tbl$t_mny)
mean(s.tbl$t_diff)

median(s.tbl$t_p)
hist(s.tbl$t_p)
nrow(subset(s.tbl, t_p > 0.05))

