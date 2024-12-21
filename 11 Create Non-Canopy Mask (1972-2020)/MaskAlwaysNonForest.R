#### Create "always non-forest mask" to account for agriculture and similar areas ####
# Also mask water

# Vogeler et al., 2020
# If ever forest mask: forest for 3 years between 1973-2020

library(raster)

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/Spatiotemporal canopy change/MaskAlwaysNonForest")

##### Read in yearly forest masks #####
# 1 = Non-forest (<10)
# 2 = Forest (10+)

cc10_filepath = file.path("C:", "Users", "mitch", "Google Drive", "PHD", "Thesis Research", 
                          "Landsat Time-Series", "Validate", "CC Timeseries", "Data")

cc10_72 = raster(file.path(cc10_filepath, "1972", "Predictors", "cc10_1972.asc"))
plot(cc10_72)
cc10_74 = raster(file.path(cc10_filepath, "1974", "Predictors", "cc10_1974.asc"))
plot(cc10_74)
cc10_75 = raster(file.path(cc10_filepath, "1975", "Predictors", "cc10_1975.asc"))
plot(cc10_75)
cc10_76 = raster(file.path(cc10_filepath, "1976", "Predictors", "cc10_1976.asc"))
plot(cc10_76)
cc10_77 = raster(file.path(cc10_filepath, "1977", "Predictors", "cc10_1977.asc"))
plot(cc10_77)
cc10_78 = raster(file.path(cc10_filepath, "1978", "Predictors", "cc10_1978.asc"))
plot(cc10_78)
cc10_79 = raster(file.path(cc10_filepath, "1979", "Predictors", "cc10_1979.asc"))
plot(cc10_79)
cc10_80 = raster(file.path(cc10_filepath, "1980", "Predictors", "cc10_1980.asc"))
plot(cc10_80)
cc10_81 = raster(file.path(cc10_filepath, "1981", "Predictors", "cc10_1981.asc"))
plot(cc10_81)
cc10_82 = raster(file.path(cc10_filepath, "1982", "Predictors", "cc10_1982.asc"))
plot(cc10_82)
cc10_83 = raster(file.path(cc10_filepath, "1983", "Predictors", "cc10_1983.asc"))
plot(cc10_83)
cc10_84 = raster(file.path(cc10_filepath, "1984", "Predictors", "cc10_1984.asc"))
plot(cc10_84)
cc10_85 = raster(file.path(cc10_filepath, "1985", "Predictors", "cc10_1985.asc"))
plot(cc10_85)
cc10_86 = raster(file.path(cc10_filepath, "1986", "Predictors", "cc10_1986.asc"))
plot(cc10_86)
cc10_87 = raster(file.path(cc10_filepath, "1987", "Predictors", "cc10_1987.asc"))
plot(cc10_87)
cc10_88 = raster(file.path(cc10_filepath, "1988", "Predictors", "cc10_1988.asc"))
plot(cc10_88)
cc10_89 = raster(file.path(cc10_filepath, "1989", "Predictors", "cc10_1989.asc"))
plot(cc10_89)
cc10_90 = raster(file.path(cc10_filepath, "1990", "Predictors", "cc10_1990.asc"))
plot(cc10_90)
cc10_91 = raster(file.path(cc10_filepath, "1991", "Predictors", "cc10_1991.asc"))
plot(cc10_91)
cc10_92 = raster(file.path(cc10_filepath, "1992", "Predictors", "cc10_1992.asc"))
plot(cc10_92)
cc10_93 = raster(file.path(cc10_filepath, "1993", "Predictors", "cc10_1993.asc"))
plot(cc10_93)
cc10_94 = raster(file.path(cc10_filepath, "1994", "Predictors", "cc10_1994.asc"))
plot(cc10_94)
cc10_95 = raster(file.path(cc10_filepath, "1995", "Predictors", "cc10_1995.asc"))
plot(cc10_95)
cc10_96 = raster(file.path(cc10_filepath, "1996", "Predictors", "cc10_1996.asc"))
plot(cc10_96)
cc10_97 = raster(file.path(cc10_filepath, "1997", "Predictors", "cc10_1997.asc"))
plot(cc10_97)
cc10_98 = raster(file.path(cc10_filepath, "1998", "Predictors", "cc10_1998.asc"))
plot(cc10_98)
cc10_99 = raster(file.path(cc10_filepath, "1999", "Predictors", "cc10_1999.asc"))
plot(cc10_99)
cc10_00 = raster(file.path(cc10_filepath, "2000", "Predictors", "cc10_2000.asc"))
plot(cc10_00)
cc10_01 = raster(file.path(cc10_filepath, "2001", "Predictors", "cc10_2001.asc"))
plot(cc10_01)
cc10_02 = raster(file.path(cc10_filepath, "2002", "Predictors", "cc10_2002.asc"))
plot(cc10_02)
cc10_03 = raster(file.path(cc10_filepath, "2003", "Predictors", "cc10_2003.asc"))
plot(cc10_03)
cc10_04 = raster(file.path(cc10_filepath, "2004", "Predictors", "cc10_2004.asc"))
plot(cc10_04)
cc10_05 = raster(file.path(cc10_filepath, "2005", "Predictors", "cc10_2005.asc"))
plot(cc10_05)
cc10_06 = raster(file.path(cc10_filepath, "2006", "Predictors", "cc10_2006.asc"))
plot(cc10_06)
cc10_07 = raster(file.path(cc10_filepath, "2007", "Predictors", "cc10_2007.asc"))
plot(cc10_07)
cc10_08 = raster(file.path(cc10_filepath, "2008", "Predictors", "cc10_2008.asc"))
plot(cc10_08)
cc10_09 = raster(file.path(cc10_filepath, "2009", "Predictors", "cc10_2009.asc"))
plot(cc10_09)
cc10_10 = raster(file.path(cc10_filepath, "2010", "Predictors", "cc10_2010.asc"))
plot(cc10_10)
cc10_11 = raster(file.path(cc10_filepath, "2011", "Predictors", "cc10_2011.asc"))
plot(cc10_11)
cc10_12 = raster(file.path(cc10_filepath, "2012", "Predictors", "cc10_2012.asc"))
plot(cc10_12)
cc10_13 = raster(file.path(cc10_filepath, "2013", "Predictors", "cc10_2013.asc"))
plot(cc10_13)
cc10_14 = raster(file.path(cc10_filepath, "2014", "Predictors", "cc10_2014.asc"))
plot(cc10_14)
cc10_15 = raster(file.path(cc10_filepath, "2015", "Predictors", "cc10_2015.asc"))
plot(cc10_15)
cc10_16 = raster(file.path(cc10_filepath, "2016", "Predictors", "cc10_2016.asc"))
plot(cc10_16)
cc10_17 = raster(file.path(cc10_filepath, "2017", "Predictors", "cc10_2017.asc"))
plot(cc10_17)
cc10_18 = raster(file.path(cc10_filepath, "2018", "Predictors", "cc10_2018.asc"))
plot(cc10_18)
cc10_19 = raster(file.path(cc10_filepath, "2019", "Predictors", "cc10_2019.asc"))
plot(cc10_19)
cc10_20 = raster(file.path(cc10_filepath, "2020", "Predictors", "cc10_2020.asc"))
plot(cc10_20)
#####

# Landcover
#landcover = raster("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation/rf/predictors/landcover_2018_1.asc")
#plot(landcover) # Water already masked

##### Reclassify: Non-Forest = 1, Forest = 0 (was 2) #####
cc10_72 = reclassify(cc10_72, cbind(2,0))
plot(cc10_72)
cc10_74 = reclassify(cc10_74, cbind(2,0))
plot(cc10_74)
cc10_75 = reclassify(cc10_75, cbind(2,0))
plot(cc10_75)
cc10_76 = reclassify(cc10_76, cbind(2,0))
plot(cc10_76)
cc10_77 = reclassify(cc10_77, cbind(2,0))
plot(cc10_77)
cc10_78 = reclassify(cc10_78, cbind(2,0))
plot(cc10_78)
cc10_79 = reclassify(cc10_79, cbind(2,0))
plot(cc10_79)
cc10_80 = reclassify(cc10_80, cbind(2,0))
plot(cc10_80)
cc10_81 = reclassify(cc10_81, cbind(2,0))
plot(cc10_81)
cc10_82 = reclassify(cc10_82, cbind(2,0))
plot(cc10_82)
cc10_83 = reclassify(cc10_83, cbind(2,0))
plot(cc10_83)
cc10_84 = reclassify(cc10_84, cbind(2,0))
plot(cc10_84)
cc10_85 = reclassify(cc10_85, cbind(2,0))
plot(cc10_85)
cc10_86 = reclassify(cc10_86, cbind(2,0))
plot(cc10_86)
cc10_87 = reclassify(cc10_87, cbind(2,0))
plot(cc10_87)
cc10_88 = reclassify(cc10_88, cbind(2,0))
plot(cc10_88)
cc10_89 = reclassify(cc10_89, cbind(2,0))
plot(cc10_89)
cc10_90 = reclassify(cc10_90, cbind(2,0))
plot(cc10_90)
cc10_91 = reclassify(cc10_91, cbind(2,0))
plot(cc10_91)
cc10_92 = reclassify(cc10_92, cbind(2,0))
plot(cc10_92)
cc10_93 = reclassify(cc10_93, cbind(2,0))
plot(cc10_93)
cc10_94 = reclassify(cc10_94, cbind(2,0))
plot(cc10_94)
cc10_95 = reclassify(cc10_95, cbind(2,0))
plot(cc10_95)
cc10_96 = reclassify(cc10_96, cbind(2,0))
plot(cc10_96)
cc10_97 = reclassify(cc10_97, cbind(2,0))
plot(cc10_97)
cc10_98 = reclassify(cc10_98, cbind(2,0))
plot(cc10_98)
cc10_99 = reclassify(cc10_99, cbind(2,0))
plot(cc10_99)
cc10_00 = reclassify(cc10_00, cbind(2,0))
plot(cc10_00)
cc10_01 = reclassify(cc10_01, cbind(2,0))
plot(cc10_01)
cc10_02 = reclassify(cc10_02, cbind(2,0))
plot(cc10_02)
cc10_03 = reclassify(cc10_03, cbind(2,0))
plot(cc10_03)
cc10_04 = reclassify(cc10_04, cbind(2,0))
plot(cc10_04)
cc10_05 = reclassify(cc10_05, cbind(2,0))
plot(cc10_05)
cc10_06 = reclassify(cc10_06, cbind(2,0))
plot(cc10_06)
cc10_07 = reclassify(cc10_07, cbind(2,0))
plot(cc10_07)
cc10_08 = reclassify(cc10_08, cbind(2,0))
plot(cc10_08)
cc10_09 = reclassify(cc10_09, cbind(2,0))
plot(cc10_09)
cc10_10 = reclassify(cc10_10, cbind(2,0))
plot(cc10_10)
cc10_11 = reclassify(cc10_11, cbind(2,0))
plot(cc10_11)
cc10_12 = reclassify(cc10_12, cbind(2,0))
plot(cc10_12)
cc10_13 = reclassify(cc10_13, cbind(2,0))
plot(cc10_13)
cc10_14 = reclassify(cc10_14, cbind(2,0))
plot(cc10_14)
cc10_15 = reclassify(cc10_15, cbind(2,0))
plot(cc10_15)
cc10_16 = reclassify(cc10_16, cbind(2,0))
plot(cc10_16)
cc10_17 = reclassify(cc10_17, cbind(2,0))
plot(cc10_17)
cc10_18 = reclassify(cc10_18, cbind(2,0))
plot(cc10_18)
cc10_19 = reclassify(cc10_19, cbind(2,0))
plot(cc10_19)
cc10_20 = reclassify(cc10_20, cbind(2,0))
plot(cc10_20)
#####

##### By decade: If non-forest at least once = 1, never = 0 #####
cc10_70s = cc10_72 + cc10_74 + cc10_75 + cc10_76 + cc10_77 + cc10_78 + cc10_79 + cc10_80
plot(cc10_70s)
cc10_70s = reclassify(cc10_70s, cbind(1,8,1))
plot(cc10_70s)

cc10_80s = cc10_81 + cc10_82 + cc10_83 + cc10_84 + cc10_85 + cc10_86 + cc10_87 + cc10_88 + cc10_89 +
  cc10_90
plot(cc10_80s)
cc10_80s = reclassify(cc10_80s, cbind(1,10,1))
plot(cc10_80s)

cc10_90s = cc10_91 + cc10_92 + cc10_93 + cc10_94 + cc10_95 + cc10_96 + cc10_97 + cc10_98 + cc10_99 +
  cc10_00
plot(cc10_90s)
cc10_90s = reclassify(cc10_90s, cbind(1,10,1))
plot(cc10_90s)

cc10_00s = cc10_01 + cc10_02 + cc10_03 + cc10_04 + cc10_05 + cc10_06 + cc10_07 + cc10_08 + cc10_09 +
  cc10_10
plot(cc10_00s)
cc10_00s = reclassify(cc10_00s, cbind(1,10,1))
plot(cc10_00s)

cc10_10s = cc10_11 + cc10_12 + cc10_13 + cc10_14 + cc10_15 + cc10_16 + cc10_17 + cc10_18 + cc10_19 +
  cc10_20
plot(cc10_10s)
cc10_10s = reclassify(cc10_10s, cbind(1,10,1))
plot(cc10_10s)
#####

##### If non-forest at least once each decade = non_forest in analysis #####
nonforest_10 = cc10_70s + cc10_80s + cc10_90s + cc10_00s + cc10_10s
plot(nonforest_10)
nonforest_10 = reclassify(nonforest_10, cbind(0,4,0))
nonforest_10 = reclassify(nonforest_10, cbind(5,1))
plot(nonforest_10)
#####

crs(nonforest_10) = "EPSG:32617" # WGS84 UTM Zone 17N

#writeRaster(nonforest_10, "always_nonforest10.tif")

##### By 5-year: If non-forest at least once = 1, never = 0 #####
#cc10_72_76 = cc10_72 + cc10_74 + cc10_75 + cc10_76
#plot(cc10_72_76)
#cc10_72_76 = reclassify(cc10_72_76, cbind(1,4,1))
#plot(cc10_72_76)

#cc10_77_80 = cc10_77 + cc10_78 + cc10_79 + cc10_80
#plot(cc10_77_80)
#cc10_77_80 = reclassify(cc10_77_80, cbind(1,4,1))
#plot(cc10_77_80)

#cc10_81_85 = cc10_81 + cc10_82 + cc10_83 + cc10_84 + cc10_85
#plot(cc10_81_85)
#cc10_81_85 = reclassify(cc10_81_85, cbind(1,5,1))
#plot(cc10_81_85)

#cc10_86_90 = cc10_86 + cc10_87 + cc10_88 + cc10_89 + cc10_90
#plot(cc10_86_90)
#cc10_86_90 = reclassify(cc10_86_90, cbind(1,5,1))
#plot(cc10_86_90)

#cc10_91_95 = cc10_91 + cc10_92 + cc10_93 + cc10_94 + cc10_95
#plot(cc10_91_95)
#cc10_91_95 = reclassify(cc10_91_95, cbind(1,5,1))
#plot(cc10_91_95)

#cc10_96_00 = cc10_96 + cc10_97 + cc10_98 + cc10_99 + cc10_00
#plot(cc10_96_00)
#cc10_96_00 = reclassify(cc10_96_00, cbind(1,5,1))
#plot(cc10_96_00)

#cc10_01_05 = cc10_01 + cc10_02 + cc10_03 + cc10_04 + cc10_05
#plot(cc10_01_05)
#cc10_01_05 = reclassify(cc10_01_05, cbind(1,5,1))
#plot(cc10_01_05)

#cc10_06_10 = cc10_06 + cc10_07 + cc10_08 + cc10_09 + cc10_10
#plot(cc10_06_10)
#cc10_06_10 = reclassify(cc10_06_10, cbind(1,5,1))
#plot(cc10_06_10)

#cc10_11_15 = cc10_11 + cc10_12 + cc10_13 + cc10_14 + cc10_15
#plot(cc10_11_15)
#cc10_11_15 = reclassify(cc10_11_15, cbind(1,5,1))
#plot(cc10_11_15)

#cc10_16_20 = cc10_16 + cc10_17 + cc10_18 + cc10_19 + cc10_20
#plot(cc10_16_20)
#cc10_16_20 = reclassify(cc10_16_20, cbind(1,5,1))
#plot(cc10_16_20)
#####

##### If non-forest at least once per ~5 years = non_forest in analysis #####
#nonforest_5 = cc10_72_76 + cc10_77_80 + cc10_81_85 + cc10_86_90 + cc10_91_95 + cc10_96_00 + 
#  cc10_01_05 + cc10_06_10 + cc10_11_15 + cc10_16_20
#plot(nonforest_5)
#nonforest_5 = reclassify(nonforest_5, cbind(0,9,0))
#nonforest_5 = reclassify(nonforest_5, cbind(10,1))
#plot(nonforest_5)
#####

#crs(nonforest_5) = "EPSG:32617" # WGS84 UTM Zone 17N

#writeRaster(nonforest_5, "always_nonforest5.tif")