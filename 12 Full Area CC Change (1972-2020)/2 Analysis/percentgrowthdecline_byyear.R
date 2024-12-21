#### Mask always-nonforest + Calculate % growth/decline (yearly 1972-2020) ####

library(raster)

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/Spatiotemporal canopy change")

##### Initialize Data #####
# Consistent non-forest (1 = consistent non-forest, convert to NA)
nonforest = raster("MaskAlwaysNonForest/always_nonforest10_nowater.tif")
plot(nonforest)
nonforest = reclassify(nonforest, cbind(1,NA))
plot(nonforest) # Consistent non-forest = NA

# %CC (1972-2020 - missing 1973)
cc_72_20 = stack("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/cc_72to20.tif")
#plot(cc_72_20)
# cc_72_20 = cc_72_20 * 100 # %CC # Don't need to run (takes some time) - just set classes differently
#cc_73 = (cc_72_20$cc_72to20.1 + cc_72_20$cc_72to20.2) / 2 # 1973: Average of 1972 and 1974
#plot(cc_73)

# CC classes1
# 1 = 0 - 10
# 2 = 10 - 25
# 3 = 25 - 50
# 4 = 50 - 75
# 5 = 75 - 100
rcl_cc = c(0,0.1,1, 0.1,0.25,2, 0.25,0.5,3, 0.5,0.75,4, 0.75,1,5)
rclm_cc = matrix(rcl_cc, ncol = 3, byrow = TRUE)

# CC tables to fill
ROPCRW_cc = data.frame(matrix(ncol = 49, nrow = 5))
colnames(ROPCRW_cc)  = c("CC", "1972", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                         "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
ROPCRW_cc[1] = c(1, 2, 3, 4, 5)

ROPCRW_City_cc = ROPCRW_cc
ROPCRW_Town_cc = ROPCRW_cc
ROPCRW_Agr_cc = ROPCRW_cc
ROPCRW_For_cc = ROPCRW_cc

ROPCRW_cc_stat = data.frame(matrix(ncol = 49, nrow = 2))
colnames(ROPCRW_cc_stat)  = c("Stats", "1972", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                         "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
ROPCRW_cc_stat[1] = c("mean", "sd")

#ROPCRW_City_cc_stat = ROPCRW_cc_stat
#ROPCRW_Town_cc_stat = ROPCRW_cc_stat
#ROPCRW_Agr_cc_stat = ROPCRW_cc_stat
#ROPCRW_For_cc_stat = ROPCRW_cc_stat

# Other option: 10% intervals...
# CC classes1
# 1 = 0 - 10
# 2 = 10 - 20
# 3 = 20 - 30
# 4 = 30 - 40
# 5 = 40 - 50
# 6 = 50 - 60
# 7 = 60 - 70
# 8 = 70 - 80
# 9 = 80 - 90
# 10 = 90 - 100
#rcl_cc = c(0,0.1,1, 0.1,0.2,2, 0.2,0.3,3, 0.3,0.4,4, 0.4,0.5,5, 0.5,0.6,6, 0.6,0.7,7, 0.7,0.8,8, 0.8,0.9,9, 0.9,1,10)
#rclm_cc = matrix(rcl_cc, ncol = 3, byrow = TRUE)

# Change classes
# 1 Growth = 1-100
# 0 Stable = -1 to 1
# -1 Decline = -1 to -5
# -2 Decline = -5 to -10
# -3 Decline = -10 to -20
# -4 Decline = -20 to -50
# -5 Decline = -50+
#rcl = c(1,100,1, -1,1,0, -5,-1,-1, -10,-5,-2, -20,-10,-3, -50,-20,-4, -100,-50,-5)
rcl = c(0.01,1,1, -0.01,0.01,0, -0.05,-0.01,-1, -0.1,-0.05,-2, -0.2,-0.1,-3, -0.5,-0.2,-4, -1,-0.5,-5)
rclm = matrix(rcl, ncol = 3, byrow = TRUE)

ch_col = c("black", "saddlebrown", "red3", "orange3", "gold", "gray70", "darkgreen")

# ROP_CRW shapefile
ROPCRW = shapefile("C:/Users/mitch/Google Drive/PHD/Spatial Data/CVC_ROP/CVC_ROP2.shp")
plot(ROPCRW)
# Regions shapefile
ROPCRW_reg = shapefile("CreateRegions/CVCROP_Regions1.shp")
plot(ROPCRW_reg)

# Change tables to fill #
ROPCRW_ch = data.frame(matrix(ncol = 48, nrow = 7))
colnames(ROPCRW_ch)  = c("Change", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                                   "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                                   "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
ROPCRW_ch[1] = c(-5, -4, -3, -2, -1, 0, 1)

ROPCRW_City_ch = ROPCRW_ch
ROPCRW_Town_ch = ROPCRW_ch
ROPCRW_Agr_ch = ROPCRW_ch
ROPCRW_For_ch = ROPCRW_ch

ROPCRW_ch_stat = data.frame(matrix(ncol = 48, nrow = 2))
colnames(ROPCRW_ch_stat)  = c("Stats", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                               "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                               "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
ROPCRW_ch_stat[1] = c("mean", "sd")

ROPCRW_City_ch_stat = ROPCRW_ch_stat
ROPCRW_Town_ch_stat = ROPCRW_ch_stat
ROPCRW_Agr_ch_stat = ROPCRW_ch_stat
ROPCRW_For_ch_stat = ROPCRW_ch_stat

# Example change sites (Fig. 7)
ExCh = shapefile("ExampleChanges/ExampleChanges.shp")
plot(ExCh)
#####

##### Calculate average %CC time-series and CC categories #####
cc_72_20_m = mask(cc_72_20, nonforest) # Mask out consistent non-forest
#plot(cc_72_20_m$cc_72to20.48)

# ROP-CRW
ROPCRW_cc_stat[1,2:49] = cellStats(cc_72_20_m, stat = "mean", na.rm = TRUE) * 100 # mean
ROPCRW_cc_stat[2,2:49] = cellStats(cc_72_20_m, stat = "sd", na.rm = TRUE) * 100   # sd

cc_72_20_mr = reclassify(cc_72_20_m, rclm_cc)
#plot(cc_72_20_mr$cc_72to20.48)
#writeRaster(cc_72_20_mr$cc_72to20.48, "cc_rcl_2020.tif")
#barplot(cc_72_20_mr$cc_72to20.48, maxpixels = 10000000)

e = extract(cc_72_20_mr, ROPCRW, df = TRUE)
e = na.omit(e)
class.prop = lapply(e, FUN = function(x) {prop.table(table(x)) * 100})
ROPCRW_cc[2:49] = class.prop[2:49]                                                        # change %

# ROP-CRW Regions
e2 = extract(cc_72_20_m, ROPCRW_reg, fun = mean, na.rm = TRUE, df = TRUE) # Adding weights = TRUE makes it take much longer
e2$ID = c("Agriculture", "City Development", "Forest", "Town Development")
colnames(e2)  = c("Type", "1972", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                              "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                              "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

e3 = extract(cc_72_20_m, ROPCRW_reg, fun = sd, na.rm = TRUE, df = TRUE)
e3$ID = c("Agriculture", "City Development", "Forest", "Town Development")
colnames(e3)  = c("Type", "1972", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                  "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                  "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")


e4 = extract(cc_72_20_mr, ROPCRW_reg, df = TRUE)
e4 = na.omit(e4)
e4_city = subset(e4, ID == 2)
e4_town = subset(e4, ID == 4)
e4_agr = subset(e4, ID == 1)
e4_for = subset(e4, ID == 3)
class.prop2 = lapply(e4_city, FUN = function(x) {prop.table(table(x)) * 100})
ROPCRW_City_cc[2:49] = class.prop2[2:49]
class.prop2 = lapply(e4_town, FUN = function(x) {prop.table(table(x)) * 100})
ROPCRW_Town_cc[2:49] = class.prop2[2:49]
class.prop2 = lapply(e4_agr, FUN = function(x) {prop.table(table(x)) * 100})
ROPCRW_Agr_cc[2:49] = class.prop2[2:49]
class.prop2 = lapply(e4_for, FUN = function(x) {prop.table(table(x)) * 100})
ROPCRW_For_cc[2:49] = class.prop2[2:49]
#####
#write.csv(ROPCRW_cc, "ROPCRW_cccat_per74to20.csv")
#write.csv(ROPCRW_City_cc, "ROPCRW_cccat_per74to20_city.csv")
#write.csv(ROPCRW_Town_cc, "ROPCRW_cccat_per74to20_town.csv")
#write.csv(ROPCRW_Agr_cc, "ROPCRW_cccat_per74to20_agr.csv")
#write.csv(ROPCRW_For_cc, "ROPCRW_cccat_per74to20_for.csv")

#write.csv(ROPCRW_cc_stat, "ROPCRW_cc_stat.csv")
#write.csv(e2, "ROPCRW_cc_reg_mn.csv")
#write.csv(e3, "ROPCRW_cc_reg_sd.csv")


##### Calculate Change between all years #####
# Helpful: https://gis.stackexchange.com/questions/297852/calculating-statistics-per-area-for-categorical-raster-using-r

# 1974 - 1972
#cc1 = cc_72_20$cc_72to20.1 # 1972
#cc2 = cc_72_20$cc_72to20.2 # 1974
#col = 2
# 1975 - 1974
#cc1 = cc_72_20$cc_72to20.2 # 1974
#cc2 = cc_72_20$cc_72to20.3 # 1975
#col = 3
# 1976 - 1975
#cc1 = cc_72_20$cc_72to20.3 # 1975
#cc2 = cc_72_20$cc_72to20.4 # 1976
#col = 4
# 1977 - 1976
#cc1 = cc_72_20$cc_72to20.4 # 1976
#cc2 = cc_72_20$cc_72to20.5 # 1977
#col = 5
# 1978 - 1977
#cc1 = cc_72_20$cc_72to20.5 # 1977
#cc2 = cc_72_20$cc_72to20.6 # 1978
#col = 6
# 1979 - 1978
#cc1 = cc_72_20$cc_72to20.6 # 1978
#cc2 = cc_72_20$cc_72to20.7 # 1979
#col = 7
# 1980 - 1979
#cc1 = cc_72_20$cc_72to20.7 # 1979
#cc2 = cc_72_20$cc_72to20.8 # 1980
#col = 8
# 1981 - 1980
#cc1 = cc_72_20$cc_72to20.8 # 1980
#cc2 = cc_72_20$cc_72to20.9 # 1981
#col = 9
# 1982 - 1981
#cc1 = cc_72_20$cc_72to20.9  # 1981
#cc2 = cc_72_20$cc_72to20.10 # 1982
#col = 10
# 1983 - 1982
#cc1 = cc_72_20$cc_72to20.10  # 1982
#cc2 = cc_72_20$cc_72to20.11  # 1983
#col = 11
# 1984 - 1983
#cc1 = cc_72_20$cc_72to20.11  # 1983
#cc2 = cc_72_20$cc_72to20.12  # 1984
#col = 12
# 1985 - 1984
#cc1 = cc_72_20$cc_72to20.12  # 1984
#cc2 = cc_72_20$cc_72to20.13  # 1985
#col = 13
# 1986 - 1985
#cc1 = cc_72_20$cc_72to20.13  # 1985
#cc2 = cc_72_20$cc_72to20.14  # 1986
#col = 14
# 1987 - 1986
#cc1 = cc_72_20$cc_72to20.14  # 1986
#cc2 = cc_72_20$cc_72to20.15  # 1987
#col = 15
# 1988 - 1987
#cc1 = cc_72_20$cc_72to20.15  # 1987
#cc2 = cc_72_20$cc_72to20.16  # 1988
#col = 16
# 1989 - 1988
#cc1 = cc_72_20$cc_72to20.16  # 1988
#cc2 = cc_72_20$cc_72to20.17  # 1989
#col = 17
# 1990 - 1989
#cc1 = cc_72_20$cc_72to20.17  # 1989
#cc2 = cc_72_20$cc_72to20.18  # 1990
#col = 18
# 1991 - 1990
#cc1 = cc_72_20$cc_72to20.18  # 1990
#cc2 = cc_72_20$cc_72to20.19  # 1991
#col = 19
# 1992 - 1991
#cc1 = cc_72_20$cc_72to20.19  # 1991
#cc2 = cc_72_20$cc_72to20.20  # 1992
#col = 20
# 1993 - 1992
#cc1 = cc_72_20$cc_72to20.20  # 1992
#cc2 = cc_72_20$cc_72to20.21  # 1993
#col = 21
# 1994 - 1993
#cc1 = cc_72_20$cc_72to20.21  # 1993
#cc2 = cc_72_20$cc_72to20.22  # 1994
#col = 22
# 1995 - 1994
#cc1 = cc_72_20$cc_72to20.22  # 1994
#cc2 = cc_72_20$cc_72to20.23  # 1995
#col = 23
# 1996 - 1995
#cc1 = cc_72_20$cc_72to20.23  # 1995
#cc2 = cc_72_20$cc_72to20.24  # 1996
#col = 24
# 1997 - 1996
#cc1 = cc_72_20$cc_72to20.24  # 1996
#cc2 = cc_72_20$cc_72to20.25  # 1997
#col = 25
# 1998 - 1997
#cc1 = cc_72_20$cc_72to20.25  # 1997
#cc2 = cc_72_20$cc_72to20.26  # 1998
#col = 26
# 1999 - 1998
#cc1 = cc_72_20$cc_72to20.26  # 1998
#cc2 = cc_72_20$cc_72to20.27  # 1999
#col = 27
# 2000 - 1999
#cc1 = cc_72_20$cc_72to20.27  # 1999
#cc2 = cc_72_20$cc_72to20.28  # 2000
#col = 28
# 2001 - 2000
#cc1 = cc_72_20$cc_72to20.28  # 2000
#cc2 = cc_72_20$cc_72to20.29  # 2001
#col = 29
# 2002 - 2001
#cc1 = cc_72_20$cc_72to20.29  # 2001
#cc2 = cc_72_20$cc_72to20.30  # 2002
#col = 30
# 2003 - 2002
#cc1 = cc_72_20$cc_72to20.30  # 2002
#cc2 = cc_72_20$cc_72to20.31  # 2003
#col = 31
# 2004 - 2003
#cc1 = cc_72_20$cc_72to20.31  # 2003
#cc2 = cc_72_20$cc_72to20.32  # 2004
#col = 32
# 2005 - 2004
#cc1 = cc_72_20$cc_72to20.32  # 2004
#cc2 = cc_72_20$cc_72to20.33  # 2005
#col = 33
# 2006 - 2005
#cc1 = cc_72_20$cc_72to20.33  # 2005
#cc2 = cc_72_20$cc_72to20.34  # 2006
#col = 34
# 2007 - 2006
#cc1 = cc_72_20$cc_72to20.34  # 2006
#cc2 = cc_72_20$cc_72to20.35  # 2007
#col = 35
# 2008 - 2007
#cc1 = cc_72_20$cc_72to20.35  # 2007
#cc2 = cc_72_20$cc_72to20.36  # 2008
#col = 36
# 2009 - 2008
#cc1 = cc_72_20$cc_72to20.36  # 2008
#cc2 = cc_72_20$cc_72to20.37  # 2009
#col = 37
# 2010 - 2009
#cc1 = cc_72_20$cc_72to20.37  # 2009
#cc2 = cc_72_20$cc_72to20.38  # 2010
#col = 38
# 2011 - 2010
#cc1 = cc_72_20$cc_72to20.38  # 2010
#cc2 = cc_72_20$cc_72to20.39  # 2011
#col = 39
# 2012 - 2011
#cc1 = cc_72_20$cc_72to20.39  # 2011
#cc2 = cc_72_20$cc_72to20.40  # 2012
#col = 40
# 2013 - 2012
#cc1 = cc_72_20$cc_72to20.40  # 2012
#cc2 = cc_72_20$cc_72to20.41  # 2013
#col = 41
# 2014 - 2013
#cc1 = cc_72_20$cc_72to20.41  # 2013
#cc2 = cc_72_20$cc_72to20.42  # 2014
#col = 42
# 2015 - 2014
#cc1 = cc_72_20$cc_72to20.42  # 2014
#cc2 = cc_72_20$cc_72to20.43  # 2015
#col = 43
# 2016 - 2015
#cc1 = cc_72_20$cc_72to20.43  # 2015
#cc2 = cc_72_20$cc_72to20.44  # 2016
#col = 44
# 2017 - 2016
#cc1 = cc_72_20$cc_72to20.44  # 2016
#cc2 = cc_72_20$cc_72to20.45  # 2017
#col = 45
# 2018 - 2017
#cc1 = cc_72_20$cc_72to20.45  # 2017
#cc2 = cc_72_20$cc_72to20.46  # 2018
#col = 46
# 2019 - 2018
#cc1 = cc_72_20$cc_72to20.46  # 2018
#cc2 = cc_72_20$cc_72to20.47  # 2019
#col = 47
# 2020 - 2019
#cc1 = cc_72_20$cc_72to20.47  # 2019
#cc2 = cc_72_20$cc_72to20.48  # 2020
#col = 48

### Calculate change ###
plot(cc1)
plot(cc2)

cc_d = cc2 - cc1
cc_d = mask(cc_d, nonforest) # Mask out consistent non-forest
plot(cc_d)

# ROP-CRW
ROPCRW_ch_stat[1,col] = cellStats(cc_d, stat = "mean", na.rm = TRUE) * 100                 # mean
ROPCRW_ch_stat[2,col] = cellStats(cc_d, stat = "sd", na.rm = TRUE, asSample = FALSE) * 100 # sd

cc_d_r = reclassify(cc_d, rclm)
plot(cc_d_r, col = ch_col)

barplot(cc_d_r, col = ch_col, maxpixels = 10000000)

e = extract(cc_d_r, ROPCRW)
class.prop = lapply(e, FUN = function(x) {prop.table(table(x)) * 100})
ROPCRW_ch[col] = class.prop[[1]]                                                           # change %

# ROP-CRW Regions
e2 = extract(cc_d, ROPCRW_reg, fun = mean, na.rm = TRUE, df = TRUE) # Adding weights = TRUE makes it take much longer
e3 = extract(cc_d, ROPCRW_reg, fun = sd, na.rm = TRUE, df = TRUE)

e4 = extract(cc_d_r, ROPCRW_reg)
class.prop2 = lapply(e4, FUN = function(x) {prop.table(table(x)) * 100})

# City Development (2)
ROPCRW_City_ch_stat[1,col] = e2[2,2] * 100 # mean
ROPCRW_City_ch_stat[2,col] = e3[2,2] * 100 # sd
ROPCRW_City_ch[col] = class.prop2[[2]]     # change %

# Town Development (4)
ROPCRW_Town_ch_stat[1,col] = e2[4,2] * 100 # mean
ROPCRW_Town_ch_stat[2,col] = e3[4,2] * 100 # sd
ROPCRW_Town_ch[col] = class.prop2[[4]]     # change %
# If error because -5 class has no pixels... 
#ROPCRW_Town_ch[1,col] = 0       
#ROPCRW_Town_ch[2:7,col] =  class.prop2[[4]]

# Agriculture (1)
ROPCRW_Agr_ch_stat[1,col] = e2[1,2] * 100 # mean
ROPCRW_Agr_ch_stat[2,col] = e3[1,2] * 100 # sd
ROPCRW_Agr_ch[col] = class.prop2[[1]]     # change %
# If error because -5 class has no pixels... 
#ROPCRW_Agr_ch[1,col] = 0       
#ROPCRW_Agr_ch[2:7,col] =  class.prop2[[1]]

# Forest (3)
ROPCRW_For_ch_stat[1,col] = e2[3,2] * 100 # mean
ROPCRW_For_ch_stat[2,col] = e3[3,2] * 100 # sd
ROPCRW_For_ch[col] = class.prop2[[3]]     # change %
# If error because -5 class has no pixels... 
#ROPCRW_For_ch[1,col] = 0       
#ROPCRW_For_ch[2:7,col] =  class.prop2[[3]]
#####
#write.csv(ROPCRW_ch, "ROPCRW_changecat_per74to20.csv")
#write.csv(ROPCRW_City_ch, "ROPCRW_changecat_per74to20_city.csv")
#write.csv(ROPCRW_Town_ch, "ROPCRW_changecat_per74to20_town.csv")
#write.csv(ROPCRW_Agr_ch, "ROPCRW_changecat_per74to20_agr.csv")
#write.csv(ROPCRW_For_ch, "ROPCRW_changecat_per74to20_for.csv")

#write.csv(ROPCRW_ch_stat, "ROPCRW_change_stat.csv")
#write.csv(ROPCRW_City_ch_stat, "ROPCRW_change_stat_city.csv")
#write.csv(ROPCRW_Town_ch_stat, "ROPCRW_change_stat_town.csv")
#write.csv(ROPCRW_Agr_ch_stat, "ROPCRW_change_stat_agr.csv")
#write.csv(ROPCRW_For_ch_stat, "ROPCRW_change_stat_for.csv")

##### Export CC time-series of example change sites #####
e = extract(cc_72_20_m, ExCh, fun = mean, na.rm = TRUE, df = TRUE) # Adding weights = TRUE makes it take much longer
e$ID = c("Agriculture to Forest 1", "Agriculture to Forest 2", "Agriculture to Forest 3", "Agriculture to Forest 4", "Agriculture to Forest 5", 
         "Housing Development 1", "Housing Development 2", " Housing Development 3",
         "EAB 1",
         "Ice Storm 1", "Ice Storm 2", "Ice Storm 3", "Ice Storm 4", "Ice Storm 5", "Ice Storm 6", "Ice Storm 7", "Ice Storm 8", "Ice Storm 9")
colnames(e)  = c("Type", "1972", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                  "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                  "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

e2 = extract(cc_72_20_m, ExCh, fun = sd, na.rm = TRUE, df = TRUE)
e2$ID = c("Agriculture to Forest 1", "Agriculture to Forest 2", "Agriculture to Forest 3", "Agriculture to Forest 4", "Agriculture to Forest 5", 
         "Housing Development 1", "Housing Development 2", " Housing Development 3",
         "EAB 1",
         "Ice Storm 1", "Ice Storm 2", "Ice Storm 3", "Ice Storm 4", "Ice Storm 5", "Ice Storm 6", "Ice Storm 7", "Ice Storm 8", "Ice Storm 9")
colnames(e2)  = c("Type", "1972", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                  "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                  "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
#####
#write.csv(e, "ExampleChanges_mn.csv")
#write.csv(e2, "ExampleChanges_sd.csv")
