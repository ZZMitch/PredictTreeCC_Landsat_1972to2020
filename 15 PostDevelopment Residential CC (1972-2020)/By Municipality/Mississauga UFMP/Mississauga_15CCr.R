##### Assess Key Objective of Mississauga's UFMP #####
##### What % of "residential areas" meet 15% threshold through time (1972-2020) #####
##### By pixel, by DA, by population-weighted DA #####

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(tidyr)

##### Input Data ######
cc = stack("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/CC Timeseries/cc_72to20_res1.tif") # CCr time-series raster (1972-2020)
#plot(cc$cc_72to20_res1.48)

da_shp = readOGR("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change/StudyArea/DA_Peel_prj.shp") # 1600 DAs
#plot(da_shp)
da = read.csv("DA_data1.csv")
da_shp = merge(da_shp, da, by = "DAUID") # Add Transition year
#plot(da_shp, col = da_shp$Transition)

cc_da = read.csv("cc_postdev.csv")

### Bring in population data from 2016 census ###
pop2016 = read.csv("Pop2016.csv")
pop2016 = subset(pop2016, DAUID %in% da$DAUID) # Reduce to just 1600 included DAs
da$Population = pop2016$Pop2016

### Create population tables through time ###
cc_da_pop = cc_da
cc_da_pop[,2:50] = pop2016$Pop2016
cc_da_pop[is.na(cc_da)] = NA

tbl = data.frame(matrix(ncol = 4, nrow = 49)) # Blank tbl to fill for plotting
colnames(tbl) = c("Year", "Pixel", "DA", "DApop")
tbl$Year = 1972:2020
#####

##### By Pixel #####
### 1972 ###
cc72 = crop(cc$cc_72to20_res1.1, subset(da_shp, Municipality == "Mississauga" & Transition <= 1972))
cc72 = mask(cc72, subset(da_shp, Municipality == "Mississauga" & Transition <= 1972))
plot(cc72)

cc72_15 = reclassify(cc72, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc72_15)

tbl$Pixel[1] = (freq(cc72_15)[2,2] / (freq(cc72_15)[2,2] + freq(cc72_15)[1,2])) * 100
tbl$Pixel[1]

### 1973 ###
# No Data #

### 1974 ###
cc74 = crop(cc$cc_72to20_res1.2, subset(da_shp, Municipality == "Mississauga" & Transition <= 1974))
cc74 = mask(cc74, subset(da_shp, Municipality == "Mississauga" & Transition <= 1974))
plot(cc74)

cc74_15 = reclassify(cc74, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc74_15)

tbl$Pixel[3] = (freq(cc74_15)[2,2] / (freq(cc74_15)[2,2] + freq(cc74_15)[1,2])) * 100
tbl$Pixel[3]

### 1975 ###
cc75 = crop(cc$cc_72to20_res1.3, subset(da_shp, Municipality == "Mississauga" & Transition <= 1975))
cc75 = mask(cc75, subset(da_shp, Municipality == "Mississauga" & Transition <= 1975))
plot(cc75)

cc75_15 = reclassify(cc75, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc75_15)

tbl$Pixel[4] = (freq(cc75_15)[2,2] / (freq(cc75_15)[2,2] + freq(cc75_15)[1,2])) * 100
tbl$Pixel[4]

### 1976 ###
cc76 = crop(cc$cc_72to20_res1.4, subset(da_shp, Municipality == "Mississauga" & Transition <= 1976))
cc76 = mask(cc76, subset(da_shp, Municipality == "Mississauga" & Transition <= 1976))
plot(cc76)

cc76_15 = reclassify(cc76, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc76_15)

tbl$Pixel[5] = (freq(cc76_15)[2,2] / (freq(cc76_15)[2,2] + freq(cc76_15)[1,2])) * 100
tbl$Pixel[5]

### 1977 ###
cc77 = crop(cc$cc_72to20_res1.5, subset(da_shp, Municipality == "Mississauga" & Transition <= 1977))
cc77 = mask(cc77, subset(da_shp, Municipality == "Mississauga" & Transition <= 1977))
plot(cc77)

cc77_15 = reclassify(cc77, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc77_15)

tbl$Pixel[6] = (freq(cc77_15)[2,2] / (freq(cc77_15)[2,2] + freq(cc77_15)[1,2])) * 100
tbl$Pixel[6]

### 1978 ###
cc78 = crop(cc$cc_72to20_res1.6, subset(da_shp, Municipality == "Mississauga" & Transition <= 1978))
cc78 = mask(cc78, subset(da_shp, Municipality == "Mississauga" & Transition <= 1978))
plot(cc78)

cc78_15 = reclassify(cc78, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc78_15)

tbl$Pixel[7] = (freq(cc78_15)[2,2] / (freq(cc78_15)[2,2] + freq(cc78_15)[1,2])) * 100
tbl$Pixel[7]

### 1979 ###
cc79 = crop(cc$cc_72to20_res1.7, subset(da_shp, Municipality == "Mississauga" & Transition <= 1979))
cc79 = mask(cc79, subset(da_shp, Municipality == "Mississauga" & Transition <= 1979))
plot(cc79)

cc79_15 = reclassify(cc79, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc79_15)

tbl$Pixel[8] = (freq(cc79_15)[2,2] / (freq(cc79_15)[2,2] + freq(cc79_15)[1,2])) * 100
tbl$Pixel[8]

### 1980 ###
cc80 = crop(cc$cc_72to20_res1.8, subset(da_shp, Municipality == "Mississauga" & Transition <= 1980))
cc80 = mask(cc80, subset(da_shp, Municipality == "Mississauga" & Transition <= 1980))
plot(cc80)

cc80_15 = reclassify(cc80, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc80_15)

tbl$Pixel[9] = (freq(cc80_15)[2,2] / (freq(cc80_15)[2,2] + freq(cc80_15)[1,2])) * 100
tbl$Pixel[9]

### 1981 ###
cc81 = crop(cc$cc_72to20_res1.9, subset(da_shp, Municipality == "Mississauga" & Transition <= 1981))
cc81 = mask(cc81, subset(da_shp, Municipality == "Mississauga" & Transition <= 1981))
plot(cc81)

cc81_15 = reclassify(cc81, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc81_15)

tbl$Pixel[10] = (freq(cc81_15)[2,2] / (freq(cc81_15)[2,2] + freq(cc81_15)[1,2])) * 100
tbl$Pixel[10]

### 1982 ###
cc82 = crop(cc$cc_72to20_res1.10, subset(da_shp, Municipality == "Mississauga" & Transition <= 1982))
cc82 = mask(cc82, subset(da_shp, Municipality == "Mississauga" & Transition <= 1982))
plot(cc82)

cc82_15 = reclassify(cc82, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc82_15)

tbl$Pixel[11] = (freq(cc82_15)[2,2] / (freq(cc82_15)[2,2] + freq(cc82_15)[1,2])) * 100
tbl$Pixel[11]

### 1983 ###
cc83 = crop(cc$cc_72to20_res1.11, subset(da_shp, Municipality == "Mississauga" & Transition <= 1983))
cc83 = mask(cc83, subset(da_shp, Municipality == "Mississauga" & Transition <= 1983))
plot(cc83)

cc83_15 = reclassify(cc83, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc83_15)

tbl$Pixel[12] = (freq(cc83_15)[2,2] / (freq(cc83_15)[2,2] + freq(cc83_15)[1,2])) * 100
tbl$Pixel[12]

### 1984 ###
cc84 = crop(cc$cc_72to20_res1.12, subset(da_shp, Municipality == "Mississauga" & Transition <= 1984))
cc84 = mask(cc84, subset(da_shp, Municipality == "Mississauga" & Transition <= 1984))
plot(cc84)

cc84_15 = reclassify(cc84, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc84_15)

tbl$Pixel[13] = (freq(cc84_15)[2,2] / (freq(cc84_15)[2,2] + freq(cc84_15)[1,2])) * 100
tbl$Pixel[13]

### 1985 ###
cc85 = crop(cc$cc_72to20_res1.13, subset(da_shp, Municipality == "Mississauga" & Transition <= 1985))
cc85 = mask(cc85, subset(da_shp, Municipality == "Mississauga" & Transition <= 1985))
plot(cc85)

cc85_15 = reclassify(cc85, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc85_15)

tbl$Pixel[14] = (freq(cc85_15)[2,2] / (freq(cc85_15)[2,2] + freq(cc85_15)[1,2])) * 100
tbl$Pixel[14]

### 1986 ###
cc86 = crop(cc$cc_72to20_res1.14, subset(da_shp, Municipality == "Mississauga" & Transition <= 1986))
cc86 = mask(cc86, subset(da_shp, Municipality == "Mississauga" & Transition <= 1986))
plot(cc86)

cc86_15 = reclassify(cc86, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc86_15)

tbl$Pixel[15] = (freq(cc86_15)[2,2] / (freq(cc86_15)[2,2] + freq(cc86_15)[1,2])) * 100
tbl$Pixel[15]

### 1987 ###
cc87 = crop(cc$cc_72to20_res1.15, subset(da_shp, Municipality == "Mississauga" & Transition <= 1987))
cc87 = mask(cc87, subset(da_shp, Municipality == "Mississauga" & Transition <= 1987))
plot(cc87)

cc87_15 = reclassify(cc87, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc87_15)

tbl$Pixel[16] = (freq(cc87_15)[2,2] / (freq(cc87_15)[2,2] + freq(cc87_15)[1,2])) * 100
tbl$Pixel[16]

### 1988 ###
cc88 = crop(cc$cc_72to20_res1.16, subset(da_shp, Municipality == "Mississauga" & Transition <= 1988))
cc88 = mask(cc88, subset(da_shp, Municipality == "Mississauga" & Transition <= 1988))
plot(cc88)

cc88_15 = reclassify(cc88, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc88_15)

tbl$Pixel[17] = (freq(cc88_15)[2,2] / (freq(cc88_15)[2,2] + freq(cc88_15)[1,2])) * 100
tbl$Pixel[17]

### 1989 ###
cc89 = crop(cc$cc_72to20_res1.17, subset(da_shp, Municipality == "Mississauga" & Transition <= 1989))
cc89 = mask(cc89, subset(da_shp, Municipality == "Mississauga" & Transition <= 1989))
plot(cc89)

cc89_15 = reclassify(cc89, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc89_15)

tbl$Pixel[18] = (freq(cc89_15)[2,2] / (freq(cc89_15)[2,2] + freq(cc89_15)[1,2])) * 100
tbl$Pixel[18]

### 1990 ###
cc90 = crop(cc$cc_72to20_res1.18, subset(da_shp, Municipality == "Mississauga" & Transition <= 1990))
cc90 = mask(cc90, subset(da_shp, Municipality == "Mississauga" & Transition <= 1990))
plot(cc90)

cc90_15 = reclassify(cc90, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc90_15)

tbl$Pixel[19] = (freq(cc90_15)[2,2] / (freq(cc90_15)[2,2] + freq(cc90_15)[1,2])) * 100
tbl$Pixel[19]

### 1991 ###
cc91 = crop(cc$cc_72to20_res1.19, subset(da_shp, Municipality == "Mississauga" & Transition <= 1991))
cc91 = mask(cc91, subset(da_shp, Municipality == "Mississauga" & Transition <= 1991))
plot(cc91)

cc91_15 = reclassify(cc91, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc91_15)

tbl$Pixel[20] = (freq(cc91_15)[2,2] / (freq(cc91_15)[2,2] + freq(cc91_15)[1,2])) * 100
tbl$Pixel[20]

### 1992 ###
cc92 = crop(cc$cc_72to20_res1.20, subset(da_shp, Municipality == "Mississauga" & Transition <= 1992))
cc92 = mask(cc92, subset(da_shp, Municipality == "Mississauga" & Transition <= 1992))
plot(cc92)

cc92_15 = reclassify(cc92, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc92_15)

tbl$Pixel[21] = (freq(cc92_15)[2,2] / (freq(cc92_15)[2,2] + freq(cc92_15)[1,2])) * 100
tbl$Pixel[21]

### 1993 ###
cc93 = crop(cc$cc_72to20_res1.21, subset(da_shp, Municipality == "Mississauga" & Transition <= 1993))
cc93 = mask(cc93, subset(da_shp, Municipality == "Mississauga" & Transition <= 1993))
plot(cc93)

cc93_15 = reclassify(cc93, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc93_15)

tbl$Pixel[22] = (freq(cc93_15)[2,2] / (freq(cc93_15)[2,2] + freq(cc93_15)[1,2])) * 100
tbl$Pixel[22]

### 1994 ###
cc94 = crop(cc$cc_72to20_res1.22, subset(da_shp, Municipality == "Mississauga" & Transition <= 1994))
cc94 = mask(cc94, subset(da_shp, Municipality == "Mississauga" & Transition <= 1994))
plot(cc94)

cc94_15 = reclassify(cc94, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc94_15)

tbl$Pixel[23] = (freq(cc94_15)[2,2] / (freq(cc94_15)[2,2] + freq(cc94_15)[1,2])) * 100
tbl$Pixel[23]

### 1995 ###
cc95 = crop(cc$cc_72to20_res1.23, subset(da_shp, Municipality == "Mississauga" & Transition <= 1995))
cc95 = mask(cc95, subset(da_shp, Municipality == "Mississauga" & Transition <= 1995))
plot(cc95)

cc95_15 = reclassify(cc95, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc95_15)

tbl$Pixel[24] = (freq(cc95_15)[2,2] / (freq(cc95_15)[2,2] + freq(cc95_15)[1,2])) * 100
tbl$Pixel[24]

### 1996 ###
cc96 = crop(cc$cc_72to20_res1.24, subset(da_shp, Municipality == "Mississauga" & Transition <= 1996))
cc96 = mask(cc96, subset(da_shp, Municipality == "Mississauga" & Transition <= 1996))
plot(cc96)

cc96_15 = reclassify(cc96, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc96_15)

tbl$Pixel[25] = (freq(cc96_15)[2,2] / (freq(cc96_15)[2,2] + freq(cc96_15)[1,2])) * 100
tbl$Pixel[25]

### 1997 ###
cc97 = crop(cc$cc_72to20_res1.25, subset(da_shp, Municipality == "Mississauga" & Transition <= 1997))
cc97 = mask(cc97, subset(da_shp, Municipality == "Mississauga" & Transition <= 1997))
plot(cc97)

cc97_15 = reclassify(cc97, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc97_15)

tbl$Pixel[26] = (freq(cc97_15)[2,2] / (freq(cc97_15)[2,2] + freq(cc97_15)[1,2])) * 100
tbl$Pixel[26]

### 1998 ###
cc98 = crop(cc$cc_72to20_res1.26, subset(da_shp, Municipality == "Mississauga" & Transition <= 1998))
cc98 = mask(cc98, subset(da_shp, Municipality == "Mississauga" & Transition <= 1998))
plot(cc98)

cc98_15 = reclassify(cc98, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc98_15)

tbl$Pixel[27] = (freq(cc98_15)[2,2] / (freq(cc98_15)[2,2] + freq(cc98_15)[1,2])) * 100
tbl$Pixel[27]

### 1999 ###
cc99 = crop(cc$cc_72to20_res1.27, subset(da_shp, Municipality == "Mississauga" & Transition <= 1999))
cc99 = mask(cc99, subset(da_shp, Municipality == "Mississauga" & Transition <= 1999))
plot(cc99)

cc99_15 = reclassify(cc99, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc99_15)

tbl$Pixel[28] = (freq(cc99_15)[2,2] / (freq(cc99_15)[2,2] + freq(cc99_15)[1,2])) * 100
tbl$Pixel[28]

### 2000 ##
cc00 = crop(cc$cc_72to20_res1.28, subset(da_shp, Municipality == "Mississauga" & Transition <= 2000))
cc00 = mask(cc00, subset(da_shp, Municipality == "Mississauga" & Transition <= 2000))
plot(cc00)

cc00_15 = reclassify(cc00, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc00_15)

tbl$Pixel[29] = (freq(cc00_15)[2,2] / (freq(cc00_15)[2,2] + freq(cc00_15)[1,2])) * 100
tbl$Pixel[29]

### 2001 ###
cc01 = crop(cc$cc_72to20_res1.29, subset(da_shp, Municipality == "Mississauga" & Transition <= 2001))
cc01 = mask(cc01, subset(da_shp, Municipality == "Mississauga" & Transition <= 2001))
plot(cc01)

cc01_15 = reclassify(cc01, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc01_15)

tbl$Pixel[30] = (freq(cc01_15)[2,2] / (freq(cc01_15)[2,2] + freq(cc01_15)[1,2])) * 100
tbl$Pixel[30]

### 2002 ###
cc02 = crop(cc$cc_72to20_res1.30, subset(da_shp, Municipality == "Mississauga" & Transition <= 2002))
cc02 = mask(cc02, subset(da_shp, Municipality == "Mississauga" & Transition <= 2002))
plot(cc02)

cc02_15 = reclassify(cc02, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc02_15)

tbl$Pixel[31] = (freq(cc02_15)[2,2] / (freq(cc02_15)[2,2] + freq(cc02_15)[1,2])) * 100
tbl$Pixel[31]

### 2003 ###
cc03 = crop(cc$cc_72to20_res1.31, subset(da_shp, Municipality == "Mississauga" & Transition <= 2003))
cc03 = mask(cc03, subset(da_shp, Municipality == "Mississauga" & Transition <= 2003))
plot(cc03)

cc03_15 = reclassify(cc03, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc03_15)

tbl$Pixel[32] = (freq(cc03_15)[2,2] / (freq(cc03_15)[2,2] + freq(cc03_15)[1,2])) * 100
tbl$Pixel[32]

### 2004 ###
cc04 = crop(cc$cc_72to20_res1.32, subset(da_shp, Municipality == "Mississauga" & Transition <= 2004))
cc04 = mask(cc04, subset(da_shp, Municipality == "Mississauga" & Transition <= 2004))
plot(cc04)

cc04_15 = reclassify(cc04, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc04_15)

tbl$Pixel[33] = (freq(cc04_15)[2,2] / (freq(cc04_15)[2,2] + freq(cc04_15)[1,2])) * 100
tbl$Pixel[33]

### 2005 ###
cc05 = crop(cc$cc_72to20_res1.33, subset(da_shp, Municipality == "Mississauga" & Transition <= 2005))
cc05 = mask(cc05, subset(da_shp, Municipality == "Mississauga" & Transition <= 2005))
plot(cc05)

cc05_15 = reclassify(cc05, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc05_15)

tbl$Pixel[34] = (freq(cc05_15)[2,2] / (freq(cc05_15)[2,2] + freq(cc05_15)[1,2])) * 100
tbl$Pixel[34]

### 2006 ###
cc06 = crop(cc$cc_72to20_res1.34, subset(da_shp, Municipality == "Mississauga" & Transition <= 2006))
cc06 = mask(cc06, subset(da_shp, Municipality == "Mississauga" & Transition <= 2006))
plot(cc06)

cc06_15 = reclassify(cc06, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc06_15)

tbl$Pixel[35] = (freq(cc06_15)[2,2] / (freq(cc06_15)[2,2] + freq(cc06_15)[1,2])) * 100
tbl$Pixel[35]

### 2007 ###
cc07 = crop(cc$cc_72to20_res1.35, subset(da_shp, Municipality == "Mississauga" & Transition <= 2007))
cc07 = mask(cc07, subset(da_shp, Municipality == "Mississauga" & Transition <= 2007))
plot(cc07)

cc07_15 = reclassify(cc07, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc07_15)

tbl$Pixel[36] = (freq(cc07_15)[2,2] / (freq(cc07_15)[2,2] + freq(cc07_15)[1,2])) * 100
tbl$Pixel[36]

### 2008 ###
cc08 = crop(cc$cc_72to20_res1.36, subset(da_shp, Municipality == "Mississauga" & Transition <= 2008))
cc08 = mask(cc08, subset(da_shp, Municipality == "Mississauga" & Transition <= 2008))
plot(cc08)

cc08_15 = reclassify(cc08, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc08_15)

tbl$Pixel[37] = (freq(cc08_15)[2,2] / (freq(cc08_15)[2,2] + freq(cc08_15)[1,2])) * 100
tbl$Pixel[37]

### 2009 ###
cc09 = crop(cc$cc_72to20_res1.37, subset(da_shp, Municipality == "Mississauga" & Transition <= 2009))
cc09 = mask(cc09, subset(da_shp, Municipality == "Mississauga" & Transition <= 2009))
plot(cc09)

cc09_15 = reclassify(cc09, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc09_15)

tbl$Pixel[38] = (freq(cc09_15)[2,2] / (freq(cc09_15)[2,2] + freq(cc09_15)[1,2])) * 100
tbl$Pixel[38]

### 2010 ###
cc10 = crop(cc$cc_72to20_res1.38, subset(da_shp, Municipality == "Mississauga" & Transition <= 2010))
cc10 = mask(cc10, subset(da_shp, Municipality == "Mississauga" & Transition <= 2010))
plot(cc10)

cc10_15 = reclassify(cc10, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc10_15)

tbl$Pixel[39] = (freq(cc10_15)[2,2] / (freq(cc10_15)[2,2] + freq(cc10_15)[1,2])) * 100
tbl$Pixel[39]

### 2011 ###
cc11 = crop(cc$cc_72to20_res1.39, subset(da_shp, Municipality == "Mississauga" & Transition <= 2011))
cc11 = mask(cc11, subset(da_shp, Municipality == "Mississauga" & Transition <= 2011))
plot(cc11)

cc11_15 = reclassify(cc11, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc11_15)

tbl$Pixel[40] = (freq(cc11_15)[2,2] / (freq(cc11_15)[2,2] + freq(cc11_15)[1,2])) * 100
tbl$Pixel[40]

### 2012 ###
cc12 = crop(cc$cc_72to20_res1.40, subset(da_shp, Municipality == "Mississauga" & Transition <= 2012))
cc12 = mask(cc12, subset(da_shp, Municipality == "Mississauga" & Transition <= 2012))
plot(cc12)

cc12_15 = reclassify(cc12, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc12_15)

tbl$Pixel[41] = (freq(cc12_15)[2,2] / (freq(cc12_15)[2,2] + freq(cc12_15)[1,2])) * 100
tbl$Pixel[41]

### 2013 ###
cc13 = crop(cc$cc_72to20_res1.41, subset(da_shp, Municipality == "Mississauga" & Transition <= 2013))
cc13 = mask(cc13, subset(da_shp, Municipality == "Mississauga" & Transition <= 2013))
plot(cc13)

cc13_15 = reclassify(cc13, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc13_15)

tbl$Pixel[42] = (freq(cc13_15)[2,2] / (freq(cc13_15)[2,2] + freq(cc13_15)[1,2])) * 100
tbl$Pixel[42]

### 2014 ###
cc14 = crop(cc$cc_72to20_res1.42, subset(da_shp, Municipality == "Mississauga" & Transition <= 2014))
cc14 = mask(cc14, subset(da_shp, Municipality == "Mississauga" & Transition <= 2014))
plot(cc14)

cc14_15 = reclassify(cc14, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc14_15)

tbl$Pixel[43] = (freq(cc14_15)[2,2] / (freq(cc14_15)[2,2] + freq(cc14_15)[1,2])) * 100
tbl$Pixel[43]

### 2015 ###
cc15 = crop(cc$cc_72to20_res1.43, subset(da_shp, Municipality == "Mississauga" & Transition <= 2015))
cc15 = mask(cc15, subset(da_shp, Municipality == "Mississauga" & Transition <= 2015))
plot(cc15)

cc15_15 = reclassify(cc15, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc15_15)

tbl$Pixel[44] = (freq(cc15_15)[2,2] / (freq(cc15_15)[2,2] + freq(cc15_15)[1,2])) * 100
tbl$Pixel[44]

### 2016 ###
cc16 = crop(cc$cc_72to20_res1.44, subset(da_shp, Municipality == "Mississauga" & Transition <= 2016))
cc16 = mask(cc16, subset(da_shp, Municipality == "Mississauga" & Transition <= 2016))
plot(cc16)

cc16_15 = reclassify(cc16, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc16_15)

tbl$Pixel[45] = (freq(cc16_15)[2,2] / (freq(cc16_15)[2,2] + freq(cc16_15)[1,2])) * 100
tbl$Pixel[45]

### 2017 ###
cc17 = crop(cc$cc_72to20_res1.45, subset(da_shp, Municipality == "Mississauga" & Transition <= 2017))
cc17 = mask(cc17, subset(da_shp, Municipality == "Mississauga" & Transition <= 2017))
plot(cc17)

cc17_15 = reclassify(cc17, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc17_15)

tbl$Pixel[46] = (freq(cc17_15)[2,2] / (freq(cc17_15)[2,2] + freq(cc17_15)[1,2])) * 100
tbl$Pixel[46]

### 2018 ###
cc18 = crop(cc$cc_72to20_res1.46, subset(da_shp, Municipality == "Mississauga" & Transition <= 2018))
cc18 = mask(cc18, subset(da_shp, Municipality == "Mississauga" & Transition <= 2018))
plot(cc18)

cc18_15 = reclassify(cc18, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc18_15)

tbl$Pixel[47] = (freq(cc18_15)[2,2] / (freq(cc18_15)[2,2] + freq(cc18_15)[1,2])) * 100
tbl$Pixel[47]

### 2019 ###
cc19 = crop(cc$cc_72to20_res1.47, subset(da_shp, Municipality == "Mississauga" & Transition <= 2019))
cc19 = mask(cc19, subset(da_shp, Municipality == "Mississauga" & Transition <= 2019))
plot(cc19)

cc19_15 = reclassify(cc19, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc19_15)

tbl$Pixel[48] = (freq(cc19_15)[2,2] / (freq(cc19_15)[2,2] + freq(cc19_15)[1,2])) * 100
tbl$Pixel[48]

### 2020 ###
cc20 = crop(cc$cc_72to20_res1.48, subset(da_shp, Municipality == "Mississauga" & Transition <= 2020))
cc20 = mask(cc20, subset(da_shp, Municipality == "Mississauga" & Transition <= 2020))
plot(cc20)

cc20_15 = reclassify(cc20, c(-Inf,0.15,0, 0.15,Inf,1))
plot(cc20_15)

tbl$Pixel[49] = (freq(cc20_15)[2,2] / (freq(cc20_15)[2,2] + freq(cc20_15)[1,2])) * 100
tbl$Pixel[49]

# 1973 fix
tbl$Pixel[2] = (tbl$Pixel[1] + tbl$Pixel[3]) / 2
#####
plot(tbl$Year, tbl$Pixel, type = "l", ylim = c(0,100))

##### By DA #####
for (i in 1:nrow(tbl)) {
  tbl$DA[i] = (nrow(subset(cc_da, cc_da[i + 1] >= 15)) / nrow(subset(cc_da, cc_da[i + 1] >= 0))) * 100
}
#####
plot(tbl$Year, tbl$DA, type = "l", ylim = c(0,100))

##### By population-weighted DA #####
### 1972 ###
test = subset(cc_da, cc_da$X1972 >= 15)
test1 = subset(cc_da_pop, DAUID %in% test$DAUID)
sum(test1$X1972)
sum(cc_da_pop$X1972, na.rm = TRUE)
(sum(test1$X1972) / sum(cc_da_pop$X1972, na.rm = TRUE)) * 100

for (i in 1:nrow(tbl)) {
  test = subset(cc_da, cc_da[i + 1] >= 15) # DAs with CC >= 15
  test1 = subset(cc_da_pop, DAUID %in% test$DAUID) # Population of DAs with CC > 15
  tbl$DApop[i] = (sum(test1[i + 1]) / sum(cc_da_pop[i + 1], na.rm = TRUE)) * 100 # % of Population in a DA with CC >= 15
}
#####
plot(tbl$Year, tbl$DApop, type = "l", ylim = c(0,100))

##### Create Plot #####
# Create long-form table
tbl1 = pivot_longer(tbl, cols = c("Pixel", "DA", "DApop"), names_to = "Type")
tbl1$Type = factor(tbl1$Type, levels = c("Pixel", "DA", "DApop"), labels = c("Pixels", "DAs", "Population within DAs"))

axt_sz = 10
axn_sz = 8

# Wide format...
#cc15.p = ggplot(data = tbl) +
#  geom_line(aes(x = Year, y = Pixel), color = "blue", lty = 3) +
#  geom_line(aes(x = Year, y = DA), color = "blue", lty = 2) +
#  geom_line(aes(x = Year, y = DApop), color = "blue", lty = 1) +
#  scale_x_continuous(limits = c(1972,2020), expand = c(0,0), name  = "Year") + 
#  scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100,10), name = expression("Percentage with %CC"["r"]* " >= 15")) +
#  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
#        axis.text.x = element_text(size = axn_sz, color = "black"),
#        axis.title.y = element_text(size = axt_sz, color = "black"),
#        axis.text.y = element_text(size = axn_sz, color = "black"),
#        axis.ticks = element_line(color = "black"),
#        panel.background = element_blank(),
#        panel.border = element_rect(color = "black", fill = NA),
#        plot.margin = margin(0.2, 0.4, 0.1, 0.1, "cm"))
#cc15.p

# Long format...
cc15.p = ggplot(data = tbl1, aes(x = Year, y = value, group = Type)) +
  geom_rect(data = NULL, aes(ymin = 95, ymax = 100, xmin = 1972, xmax = 2020), fill = "gray50") +
  geom_rect(data = NULL, aes(ymin = 75, ymax = 95, xmin = 1972, xmax = 2020), fill = "gray70") +
  geom_rect(data = NULL, aes(ymin = 50, ymax = 75, xmin = 1972, xmax = 2020), fill = "gray90") +
  geom_rect(data = NULL, aes(ymin = 0, ymax = 50, xmin = 1972, xmax = 2020), fill = "white") +
  geom_line(aes(linetype = Type), col = "blue") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid")) +
  #scale_x_continuous(limits = c(1972,2020), expand = c(0,0), name  = "Year") + 
  coord_cartesian(xlim = c(1972,2020), expand = FALSE, clip = "off") +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100,10), name = "% with Residential CC >= 15%") +
  annotate(geom = "text", x = 2024, y = 97.5, label = "Optimal", size = 3) +
  annotate(geom = "text", x = 2023, y = 85, label = "Good", size = 3) +
  annotate(geom = "text", x = 2024.8, y = 62.5, label = "Moderate", size = 3) +
  annotate(geom = "text", x = 2022.3, y = 25, label = "Low", size = 3) +
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = margin(0.2, 1.4, 0.1, 0.1, "cm"),
        legend.position = c(0.3, 0.25),
        legend.title = element_blank())
cc15.p

tiff("MississaugaUFMP_15goal_72to20_1.tif", units = "cm", width = 10, height = 7, res = 300)
cc15.p
dev.off()
#####