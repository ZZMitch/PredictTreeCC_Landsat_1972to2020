setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")
library(segmented)
library(ggplot2)
library(patchwork)

# Add CC / TCA DA mean time-series (created from LandsatLinkr + LandTrendr + randomforest + GEE reduceRegions)
cc_res = read.csv("cc_72to20_mn_byDA_res.csv") # Residential only CC model
tca = read.csv("tca_72to20_mn_byDA_res.csv")

# Add 1973 (mean of 1972 and 1974?)
cc_res$X1973 = (cc_res$X1972 + cc_res$X1974) / 2 # Add 1973 column
cc_res = cc_res[,c(1,2,50,3:49)]# Put it in the right place
tca$X1973 = (tca$X1972 + tca$X1974) / 2 
tca = tca[,c(1,2,50,3:49)] 

# Add in DA data
da_data = read.csv("DA_data.csv") 

# Comparison plot single DA
#da = 35211612
da = 35211737 # example DA

cc_da = subset(cc, DAUID == da) # Find single DA
cc_res_da = subset(cc_res, DAUID == da)
tca_da = subset(tca, DAUID == da)

plot(1972:2020, cc_da[1,2:50], type = "l", lwd = 2, col = "darkgreen", xaxs = "i", yaxs = "i",
     ylim = c(0,0.7), ylab = "%CC", xlab = "") # CC all
grid(48,NULL)
grid(NULL,NULL, col = "black")
lines(1972:2020, cc_res_da[1,2:50], type = "l", lwd = 2, col = "gray48") # CC residential
par(new = TRUE)
plot(1972:2020, tca_da[1,2:50], type = "l", lwd = 2, col = "red3",
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", ylim = c(0,4000), xaxs = "i", yaxs = "i") # TCA
axis(side = 4)

# Later... if no Dev and most common land cover = Agriculture, Forest, Natural Vegetation Cover then all PreDev
# If most common land cover = Barren, Commercial-Industrial, Residential, Rural? then all PostDev

##### Segmented Regression Option #####
#year = 1972:2020

#tca1 = setNames(data.frame(t(tca[,-1])), tca[,1]) # Sets DAUID to column name
#tca1[1]
#tca1$year = 1972:2020
#tca1 = tca1[,c(1651,1:1650)]

#lm.da = lm(`35210002` ~ year, data = tca1)
#summary(lm.da)

#test = selgmented(lm.da, Kmax = 2)
#test
#test$psi
#test$coefficients

#test$fitted.values
#plot(year, test$fitted.values) # GOal, table of all fitted values, and then table of difference values - ID loss above 100 TCA / year (to start)

#test$id.group
#test$indexU

#segloop = function(colname, data){
#  data$y = data[,colname]
#  model = lm(y ~ x, data)
#  seg_model = selgmented(model, Kmax = 2)
#  fitted = as.matrix(seg_model$fitted.values)
#  
#  return(fitted)
#}

#segloop(3521001, tca1)

#lapply(names(tca1)[2:ncol(tca1)], function(x)segloop(year,tca1))

#tca_test = data.frame(matrix(nrow = 49, ncol = 0))

#tca_test$x = 1972:2020
#tca_test$y1 = tca1$`35210001`
#tca_test$y2 = tca1$`35210002`
#tca_test$y3 = tca1$`35210003`

#segloop(tca$ y1, tca_test)

#run_mod = function(varname, data) {
#  data$Y = data[,varname]
#  model = lm(Y ~ year, data)
# seg_model = segmented(model, seg.Z = ~year)
#  breakpoint = as.matrix(seg_model$psi.history[[5]])
#  coefficients = as.matrix(seg_model$coefficients)
#  summary_curve1 = as.data.frame(rbind(breakpoint, coefficients))
#  colnames(summary_curve1) = varname
  
#  return(summary_curve1)
#}

#lapply(names(tca1)[2:ncol(tca1)], function(year)run_mod(year,tca1))
#####

##### LandTrendr GEE Biggest Loss Option #####
biglossyears = read.csv("DA Peel Construction Dates/GEE Development Finders/Loss1000_Years4/DA_EndYearMix1.csv",
                        check.names = FALSE) # use as.numeric to turn into numbers later

biglossyears1 = 100*prop.table(as.matrix(biglossyears[,2:49]), 1) 
# Percentage by row (e.g., what % of each DA has DevEnd in given year?)

# Estimated DevEnd Year by DA (LandTrendr method)
#DevEnd = data.frame(matrix(nrow = 1650, ncol = 0))
#DevEnd$DAUID = cc$DAUID
#DevEnd$DevEndYear = as.numeric(colnames(biglossyears1[,2:48])[max.col(biglossyears1[,2:48], ties.method = "last")]) #first
# For each DA, finds year (besides 0/no year) with most pixels ending a big loss segment, with ties going to first/last year
#DevEnd$DevEndPer = apply(biglossyears1[,2:48], 1, max) # % of year selected in DevEndYear

#for (i in 1:nrow(DevEnd)) {
#  if(biglossyears1[i,1] >= 70) { # Looking into "0" column
#    DevEnd$DevEndYear[i] = NA
#  }
#} # If at least 70% of pixels have no big loss, then set DA to no DevEnd

#write.csv(DevEnd, "DevEnd_landtrendr.csv") # Solid, need to complete true dates to compare

# MaxYear must have at least X% of pixels, otherwise NA
#for (i in 1:nrow(DevEnd)) {
#  if(DevEnd$DevEndPer[i] < 8) { # 10?
#    DevEnd$DevEndYear[i] = NA
#  }
#} # Sometimes clusters around a couple years without one hitting threshold... how to find that if needed?

# Possible change: Most recent year with at least X% of pixels (may help with multi-dev DAs)
biglossyears8 = biglossyears1 #[,2:48]
biglossyears8[biglossyears8 < 8] = NA # Turn all years (besides 0) below 8% of pixels to NA
#write.csv(biglossyears8, "biglossyears8_byda.csv")

DevEnd = data.frame(matrix(nrow = 1650, ncol = 0))
DevEnd$DAUID = cc$DAUID
DevEnd$DevEndYear = as.numeric(colnames(biglossyears8)[max.col(!is.na(biglossyears8), ties.method = 'last')])
DevEnd$DevEndYear[DevEnd$DevEndYear == 0] = NA

#write.csv(DevEnd, "DevEnd_landtrendr4.csv") # Solid, need to complete true dates to compare
# Landtrendr1 = original (offset, error in lake front DAs)
# LandTrendr2 = snap2fit, fix lake front DAs)
# LandTrendr3 = ties.method = last (instead of first)
# LandTrendr4 = most recent year above 8% rather than maxyear (5,6,7,9,10,15 worse)

# Conceptual model figure
pt_sz = 2
pt_sz_sm = 1
big_ln = 1.5
sm_ln = 1
eq_sz = 3
eq_sz_sm = 2.5
axt_sz = 10
axn_sz = 8
anno_sz = 6

da = 35211737

cc_da = as.data.frame(t(subset(cc[,2:50], cc$DAUID == da)))
cc_da$year = 1972:2020
colnames(cc_da) = c("CC", "Year")
cc_da$CC = cc_da$CC * 100

cc_res_da = as.data.frame(t(subset(cc_res[,2:50], cc_res$DAUID == da)))
cc_res_da$year = 1972:2020
colnames(cc_res_da) = c("CC", "Year")
cc_res_da$CC = cc_res_da$CC * 100

#tca_da = subset(tca, DAUID == da)

cc_devend.p = ggplot() +
  geom_line(data = cc_da, aes(x = Year, y = CC), color = "black") +
  geom_line(data = cc_res_da, aes(x = Year, y = CC), color = "red3") +
  geom_vline(xintercept = 1998, linetype = "dashed", color = "gray48") +
  scale_y_continuous(expand = c(0,0), limits = c(0,40), name = "%CC") + 
  scale_x_continuous(expand = c(0,0), limits = c(1972,2020)) +
  annotate(geom = "text", x = 1973.5, y = 5, label = "A", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_devend.p

da_biggestlossyears = read.csv("DA Peel Construction Dates/GEE Development Finders/Loss1000_Years4/ExampleDA_LTRendyear.csv")

ltr_devend.p = ggplot() +
  geom_bar(data = da_biggestlossyears, aes(x = Year, y = Percent),
           stat = "identity") +
  geom_hline(yintercept = 8, linetype = "dashed", color = "gray48") +
  scale_y_continuous(expand = c(0,0), limits = c(0,40)) +
  annotate(geom = "text", x = 2, y = 35, label = "B", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black",
                                   angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
ltr_devend.p

tiff("DevEnd_interp.tif", units = "cm", width = 16.5, height = 10, res = 300)
cc_devend.p / ltr_devend.p
dev.off()

#####



#Other option... start with fire code, (year to year difference, ID big drop years etc. )

##### Compare Human and LTR Biggest Loss DevEnd Years #####
# Percent exact agreement
da_data_res = subset(da_data, MostCommonLandcover == "Residential")

nrow(subset(da_data_res, Match == "Yes")) / nrow(da_data_res) * 100 #69%

# All most common residential DAs
all.p = ggplot(da_data_res, aes(x = Match)) +
  geom_bar(fill = "gray48") +
  scale_x_discrete(limits = c("Yes", "No"), name = "Agreement") +
  scale_y_continuous(limits = c(0,1000), expand = c(0,0), name = "Residential DAs",
                     breaks = seq(0,1000,100)) +
  annotate(geom = "text", x = 2.4, y = 950, label = "A", fontface = "bold", size = anno_sz) + 
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
all.p

#plot.margin = unit(c(0,0.1,0.1,0), "cm"),

# DAs where both human and LTR gave a year
# Histogram of Agreement... 
#da_data_res[is.na(da_data_res)] = 0 # Turn NAs to 0s

devend_dif = da_data_res$DevEnd - da_data_res$DevEnd_LTR # Human minus LTR
devend_dif = na.omit(data.frame(devend_dif))
#hist(devend_dif)
mean(devend_dif$devend_dif) #0.73 (human IDs transition less than one year later than LTR)
nrow(subset(devend_dif, devend_dif == 0)) / nrow(devend_dif) * 100 #65% same year
nrow(subset(devend_dif, devend_dif >= -1 & devend_dif <= 1)) / nrow(devend_dif) * 100 #81% within 1 year
nrow(subset(devend_dif, devend_dif >= -3 & devend_dif <= 3)) / nrow(devend_dif) * 100 #91% within 3 years
nrow(subset(devend_dif, devend_dif >= -5 & devend_dif <= 5)) / nrow(devend_dif) * 100 #94% within 5 years
nrow(subset(devend_dif, devend_dif >= -10 & devend_dif <= 10)) / nrow(devend_dif) * 100 #99% within 10 years

#devend_dif[devend_dif < -10] = -27 # Human is NA, LTR has a year
#devend_dif[devend_dif > 26] = 27 # Human has a year, LTR is NA
#ggplot(devend_dif, aes(x = devend_dif)) +
#  geom_histogram(breaks = c(-27,-26,-25,-24,-23,-22,-21,-20,-19,-18,-17,-16,-15,
#                            -14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,
#                            4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
#                            25,26,27))

num.p = ggplot(devend_dif, aes(x = devend_dif)) +
  geom_histogram(data = subset(devend_dif, devend_dif > 0), binwidth = 1, fill = "blue") +
  geom_histogram(data = subset(devend_dif, devend_dif < 0), binwidth = 1, fill = "red3") +
  geom_histogram(data = subset(devend_dif, devend_dif == 0), binwidth = 1, fill = "gray48") +
  scale_y_continuous(limits = c(0,1000), expand = c(0,0), breaks = seq(0,1000,100)) +
  scale_x_continuous(name = "Difference (human - model)", expand = c(0,0)) +
  annotate(geom = "text", x = 23, y = 950, label = "B", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
num.p

#ggplot(da_data_res, aes(x = DevEnd_LTR, y = DevEnd)) + # predicted x,  observed y
#  geom_point() +
#  geom_abline()

#summary(lm(da_data_res$DevEnd ~ da_data_res$DevEnd_LTR))
# R2 = 0.92****, y = -9.38 + 1.005

# DAs where at least one gave NA (no DevEnd)
#da_data_resNA = subset(da_data_res, is.na(DevEnd) == TRUE | is.na(DevEnd_LTR) == TRUE)
# Isolate DAs where at least one method did not find a dev year
nrow(subset(da_data_res, is.na(DevEnd) == TRUE & is.na(DevEnd_LTR) == TRUE)) / 
  nrow(subset(da_data_res, is.na(DevEnd) == TRUE | is.na(DevEnd_LTR) == TRUE)) #77%


na.p = ggplot() +
  geom_bar(data = subset(da_data_res, is.na(DevEnd) == TRUE & is.na(DevEnd_LTR) == TRUE),
           aes(x = Match), fill = "gray48") + # Both have no dev year
  geom_bar(data = subset(da_data_res, is.na(DevEnd) == TRUE & is.na(DevEnd_LTR) == FALSE),
          aes(x = Match), fill = "red3") + # Human picked no dev year but LTR did
  geom_bar(data = subset(da_data_res, is.na(DevEnd) == FALSE & is.na(DevEnd_LTR) == TRUE),
           aes(x = MostCommonLandcover), fill = "blue") + # LTR picked no dev year but human did
  scale_x_discrete(limits = c("No", "Yes", "Residential"),
                  labels = c("No" = "Only human", 
                             "Yes" = "Both", 
                             "Residential" = "Only model"),
                   name = "Agreement") +
  scale_y_continuous(limits = c(0,1000), expand = c(0,0), breaks = seq(0,1000,100)) +
  annotate(geom = "text", x = 3.3, y = 950, label = "C", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
na.p

tiff("Human_LTRModel.tif", units = "cm", width = 16.5, height = 10, res = 300)
all.p + num.p + na.p
dev.off()
#####

##### Compare byPixel, byDA, and byDA (Area weighted) #####
# By Pixel
cc_mun_bypixel = read.csv("cc_72to20_mn_bymun.csv", check.names = FALSE)
cc_mun_bypixel = as.data.frame(t(cc_mun_bypixel[,2:50])) # transpose
colnames(cc_mun_bypixel) = c("Mississauga", "Brampton", "Caledon")
cc_mun_bypixel = cc_mun_bypixel * 100 # %

ggplot(cc_mun_bypixel) + # By Pixel
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln)

# By DA
cc$Municipality = da_data$Municipality # Add DA municipality to cc time-series
cc_mun_byda = cc_mun_bypixel
cc_mun_byda[1:3] = NA
cc_mun_byda$Mississauga = apply(subset(cc[2:50],cc$Municipality == "Mississauga"), 
                                2, mean) * 100
cc_mun_byda$Brampton = apply(subset(cc[2:50],cc$Municipality == "Brampton"), 
                             2, mean) * 100
cc_mun_byda$Caledon = apply(subset(cc[2:50],cc$Municipality == "Caledon"), 
                            2, mean) * 100

ggplot(cc_mun_byda) + # By DA
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln)

# Compare Pixel and DA
ggplot() +
  geom_line(aes(x = 1972:2020, y = cc_mun_bypixel$Mississauga), 
            color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = cc_mun_bypixel$Brampton), 
            color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = cc_mun_bypixel$Caledon), 
            color = "darkgreen", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = cc_mun_byda$Mississauga), 
            color = "blue", size = big_ln, linetype = "dashed") +
  geom_line(aes(x = 1972:2020, y = cc_mun_byda$Brampton), 
            color = "red3", size = big_ln, linetype = "dashed") +
  geom_line(aes(x = 1972:2020, y = cc_mun_byda$Caledon), 
            color = "darkgreen", size = big_ln, linetype = "dashed")

# By DA (weighted mean by area)
cc$Area_m2 = da_data$Area_m2 # Add DA municipality to cc time-series
cc_mun_byda_w = cc_mun_bypixel
cc_mun_byda_w[1:3] = NA

for(i in 1:nrow(cc_mun_byda_w)) { # Mississauga weighted mean CC by Area
  cc_mun_byda_w[i,1] = weighted.mean(subset(cc[,i+1], cc$Municipality == "Mississauga"), 
                                     subset(cc$Area_m2, cc$Municipality == "Mississauga"))
}
for(i in 1:nrow(cc_mun_byda_w)) { # Brampton weighted mean CC by Area
  cc_mun_byda_w[i,2] = weighted.mean(subset(cc[,i+1], cc$Municipality == "Brampton"), 
                                     subset(cc$Area_m2, cc$Municipality == "Brampton"))
}
for(i in 1:nrow(cc_mun_byda_w)) { # Caledon weighted mean CC by Area
  cc_mun_byda_w[i,3] = weighted.mean(subset(cc[,i+1], cc$Municipality == "Caledon"), 
                                     subset(cc$Area_m2, cc$Municipality == "Caledon"))
}
cc_mun_byda_w = cc_mun_byda_w * 100 # %

# Compare Pixel and weighted DA
ggplot() +
  geom_line(aes(x = 1972:2020, y = cc_mun_bypixel$Mississauga), 
            color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = cc_mun_bypixel$Brampton), 
            color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = cc_mun_bypixel$Caledon), 
            color = "darkgreen", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = cc_mun_byda_w$Mississauga), 
            color = "blue", size = big_ln, linetype = "dashed") +
  geom_line(aes(x = 1972:2020, y = cc_mun_byda_w$Brampton), 
            color = "red3", size = big_ln, linetype = "dashed") +
  geom_line(aes(x = 1972:2020, y = cc_mun_byda_w$Caledon), 
            color = "darkgreen", size = big_ln, linetype = "dashed")
# Almost exactly the same
#####
# cc_mun_byda_w = cc_mun_bypixel

##### %CC by Municipality/DA type (Area weighted DA) #####
# Also compare postDev with just residential %CC #
cc_mun.p = ggplot(cc_mun_byda_w) +
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln) +
  scale_y_continuous(limits = c(5,55), expand = c(0,0), breaks = seq(10,50,10),
                     name = "%CC") +
  scale_x_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1975, y = 50, label = "A", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_mun.p

#cc_mun_bypixel_res = read.csv("cc_72to20_mn_bymun_res.csv", check.names = FALSE) # by pixel
cc_mun_byda_w_res = cc_mun_byda_w
cc_mun_byda_w_res[1:3] = NA

# Subset cc_res to only DAs where residential is most common land cover
#cc_res$Municipality = da_data$Municipality
#cc_res$Area_m2 = da_data$Area_m2
#cc_res$MostCommonLandcover = da_data$MostCommonLandcover
#cc_res$PercentResidential = da_data$PercentResidential / 100 #proportion
#cc_res$Area_res = cc_res$Area_m2 * cc_res$PercentResidential #Weight
#cc_res = subset(cc_res, MostCommonLandcover == "Residential") #Only Residential DAs

#for(i in 1:nrow(cc_mun_byda_w_res)) { # Mississauga weighted mean CC by Area
#  cc_mun_byda_w_res[i,1] = weighted.mean(subset(cc_res[,i+1], cc_res$Municipality == "Mississauga"), 
#                                     subset(cc_res$Area_res, cc_res$Municipality == "Mississauga"))
#}
#for(i in 1:nrow(cc_mun_byda_w_res)) { # Brampton weighted mean CC by Area
#  cc_mun_byda_w_res[i,2] = weighted.mean(subset(cc_res[,i+1], cc_res$Municipality == "Brampton"), 
#                                         subset(cc_res$Area_res, cc_res$Municipality == "Brampton"))
#}
#for(i in 1:nrow(cc_mun_byda_w_res)) { # Caledon weighted mean CC by Area
#  cc_mun_byda_w_res[i,3] = weighted.mean(subset(cc_res[,i+1], cc_res$Municipality == "Caledon"), 
#                                         subset(cc_res$Area_res, cc_res$Municipality == "Caledon"))
#}
#cc_mun_byda_w_res = cc_mun_byda_w_res * 100 # %

#cc_mun_res.p = ggplot(cc_mun_byda_w_res) +
#  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
#  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
#  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln)
#cc_mun_res.p

# Create residential/rural/green space/gray space subets of general cc
cc$Municipality = da_data$Municipality
cc$Area_m2 = da_data$Area_m2
cc$MostCommonLandcover = da_data$MostCommonLandcover

# Residential
cc_res1 = subset(cc, MostCommonLandcover == "Residential") # Subset general cc to residential DAs

for(i in 1:nrow(cc_mun_byda_w_res)) { # Mississauga weighted mean CC by Area
  cc_mun_byda_w_res[i,1] = weighted.mean(subset(cc_res1[,i+1], cc_res1$Municipality == "Mississauga"), 
                                         subset(cc_res1$Area_m2, cc_res1$Municipality == "Mississauga"))
}
for(i in 1:nrow(cc_mun_byda_w_res)) { # Brampton weighted mean CC by Area
  cc_mun_byda_w_res[i,2] = weighted.mean(subset(cc_res1[,i+1], cc_res1$Municipality == "Brampton"), 
                                         subset(cc_res1$Area_m2, cc_res1$Municipality == "Brampton"))
}
for(i in 1:nrow(cc_mun_byda_w_res)) { # Caledon weighted mean CC by Area
  cc_mun_byda_w_res[i,3] = weighted.mean(subset(cc_res1[,i+1], cc_res1$Municipality == "Caledon"), 
                                         subset(cc_res1$Area_m2, cc_res1$Municipality == "Caledon"))
}
cc_mun_byda_w_res = cc_mun_byda_w_res * 100 # %

cc_mun_res.p = ggplot(cc_mun_byda_w_res) +
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln) +
  scale_y_continuous(limits = c(5,55), expand = c(0,0), breaks = seq(10,50,10),
                     name = "%CC") +
  scale_x_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1975, y = 50, label = "B", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 1985, y = 50, label = "Residential", size = eq_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_mun_res.p

# Rural
cc_rur = subset(cc, MostCommonLandcover == "Agriculture" |
                    MostCommonLandcover == "Rural") # Subset cc to rural DAs
cc_mun_byda_w_rur = cc_mun_byda_w_res
cc_mun_byda_w_rur[1:3] = NA

for(i in 1:nrow(cc_mun_byda_w_rur)) { # Mississauga weighted mean CC by Area
  cc_mun_byda_w_rur[i,1] = weighted.mean(subset(cc_rur[,i+1], cc_rur$Municipality == "Mississauga"), 
                                         subset(cc_rur$Area_m2, cc_rur$Municipality == "Mississauga"))
}
for(i in 1:nrow(cc_mun_byda_w_rur)) { # Brampton weighted mean CC by Area
  cc_mun_byda_w_rur[i,2] = weighted.mean(subset(cc_rur[,i+1], cc_rur$Municipality == "Brampton"), 
                                         subset(cc_rur$Area_m2, cc_rur$Municipality == "Brampton"))
}
for(i in 1:nrow(cc_mun_byda_w_rur)) { # Caledon weighted mean CC by Area
  cc_mun_byda_w_rur[i,3] = weighted.mean(subset(cc_rur[,i+1], cc_rur$Municipality == "Caledon"), 
                                         subset(cc_rur$Area_m2, cc_rur$Municipality == "Caledon"))
}
cc_mun_byda_w_rur = cc_mun_byda_w_rur * 100 # %

cc_mun_rur.p = ggplot(cc_mun_byda_w_rur) +
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln) +
  scale_y_continuous(limits = c(5,55), expand = c(0,0), breaks = seq(10,50,10)) +
  scale_x_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1985, y = 50, label = "Rural", size = eq_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_mun_rur.p

# Green space
cc_gs = subset(cc, MostCommonLandcover == "Forest" |
                  MostCommonLandcover == "Natural Vegetation Cover" |
                 MostCommonLandcover == "Open Space") # Subset cc to green space DAs
cc_mun_byda_w_gs = cc_mun_byda_w_res
cc_mun_byda_w_gs[1:3] = NA

for(i in 1:nrow(cc_mun_byda_w_gs)) { # Mississauga weighted mean CC by Area
  cc_mun_byda_w_gs[i,1] = weighted.mean(subset(cc_gs[,i+1], cc_gs$Municipality == "Mississauga"), 
                                         subset(cc_gs$Area_m2, cc_gs$Municipality == "Mississauga"))
}
for(i in 1:nrow(cc_mun_byda_w_gs)) { # Brampton weighted mean CC by Area
  cc_mun_byda_w_gs[i,2] = weighted.mean(subset(cc_gs[,i+1], cc_gs$Municipality == "Brampton"), 
                                         subset(cc_gs$Area_m2, cc_gs$Municipality == "Brampton"))
}
for(i in 1:nrow(cc_mun_byda_w_gs)) { # Caledon weighted mean CC by Area
  cc_mun_byda_w_gs[i,3] = weighted.mean(subset(cc_gs[,i+1], cc_gs$Municipality == "Caledon"), 
                                         subset(cc_gs$Area_m2, cc_gs$Municipality == "Caledon"))
}
cc_mun_byda_w_gs = cc_mun_byda_w_gs * 100 # %

cc_mun_gs.p = ggplot(cc_mun_byda_w_gs) +
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln) +
  scale_y_continuous(limits = c(5,55), expand = c(0,0), breaks = seq(10,50,10),
                     name = "%CC") +
  scale_x_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1985, y = 50, label = "Green Space", size = eq_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axt_sz, color = "black"),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_mun_gs.p

# Gray space
cc_gray = subset(cc, MostCommonLandcover == "Commercial-Industrial" |
                 MostCommonLandcover == "Barren") # Subset cc to gray space DAs
cc_mun_byda_w_gray = cc_mun_byda_w_res
cc_mun_byda_w_gray[1:3] = NA

for(i in 1:nrow(cc_mun_byda_w_gray)) { # Mississauga weighted mean CC by Area
  cc_mun_byda_w_gray[i,1] = weighted.mean(subset(cc_gray[,i+1], cc_gray$Municipality == "Mississauga"), 
                                        subset(cc_gray$Area_m2, cc_gray$Municipality == "Mississauga"))
}
for(i in 1:nrow(cc_mun_byda_w_gray)) { # Brampton weighted mean CC by Area
  cc_mun_byda_w_gray[i,2] = weighted.mean(subset(cc_gray[,i+1], cc_gray$Municipality == "Brampton"), 
                                        subset(cc_gray$Area_m2, cc_gray$Municipality == "Brampton"))
}
for(i in 1:nrow(cc_mun_byda_w_gray)) { # Caledon weighted mean CC by Area
  cc_mun_byda_w_gray[i,3] = weighted.mean(subset(cc_gray[,i+1], cc_gray$Municipality == "Caledon"), 
                                        subset(cc_gray$Area_m2, cc_gray$Municipality == "Caledon"))
}
cc_mun_byda_w_gray = cc_mun_byda_w_gray * 100 # %

cc_mun_gray.p = ggplot(cc_mun_byda_w_gray) +
  geom_line(aes(x = 1972:2020, y = Mississauga), color = "blue", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Brampton), color = "red3", size = big_ln) +
  geom_line(aes(x = 1972:2020, y = Caledon), color = "darkgreen", size = big_ln) +
  scale_y_continuous(limits = c(5,55), expand = c(0,0), breaks = seq(10,50,10)) +
  scale_x_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1985, y = 50, label = "Gray Space", size = eq_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axt_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_mun_gray.p

tiff("CC_mun.tif", units = "cm", width = 16.5, height = 20, res = 300)
cc_mun.p /
  ((cc_mun_res.p + cc_mun_rur.p) /
  (cc_mun_gs.p + cc_mun_gray.p))
dev.off()
#####

##### Use DevEnd (human) to manipulate cc tables (cut off before, set DevEnd to 0) #####
da_data_res = subset(da_data, MostCommonLandcover == "Residential") #1430 DAs
cc_res_g = subset(cc[,1:50], cc$MostCommonLandcover == "Residential")
# Average CC by residential DA, general CC model
cc_res_r = cc_res[,1:50] # Average CC by residential DA, residential CC model

# Use DevEnd (human) from da_data_res to cut time-series
# Transpose, add year as column
#cc_res_r = as.data.frame(t(cc_res_r))
# cc_res_r[1,1] #3510024
#cc_res_r =cc_res_r[-1,] # Remove DAUID for now
#cc_res_r$year = 1972:2020

#for (i in 1:ncol(cc_res_r) -1) {
#  for (j in 1:nrow(cc_res_r)) {
#    if(cc_res_r$year[j] < da_data_res$DevEnd[i]) {
#      cc_res_r[i,j] = NA
#    }
#  }
#}

# Build dummy DevEnd table 
DevEnd_dum = cc_res_r
DevEnd_dum[,2:50] = NA
DevEnd_dum[,2:50] = da_data_res$DevEnd # All years for each DA set as DevEnd year
DevEnd_dum[is.na(DevEnd_dum)] = -9999 # NAs to -9999 (turn back to NA later)
#DevEnd_dum[nrow(DevEnd_dum) +1,] = 1971:2020 # Add dummy years

# Build dummy years table
Years_dum = cc_res_r
Years_dum[,2:50] = NA
for (i in 1:nrow(Years_dum)) {
  Years_dum[i,2:50] = 1972:2020
}

# Build YearsDevEnd dummy table
YearsDevEnd_dum = cc_res_r
YearsDevEnd_dum[,2:50] = NA
YearsDevEnd_dum[,2:50] = Years_dum[,2:50] - DevEnd_dum[,2:50] 
# 0 = year of transition, negative = preDev, positive = residential

# If value < 0, NA (i.e., preDev residential = NA)
cc_res_r[YearsDevEnd_dum < 0] = NA
cc_res_r[,2:50] = cc_res_r[,2:50] * 100 # %

cc_res_g[,2:50] = cc_res_g[,2:50] * 100 # %

# Shift non-NA values to beginning of table. NAs to end
cc_res_r1 = as.data.frame(t(apply(cc_res_r,1,function(x) {
  return(c(x[!is.na(x)], x[is.na(x)]))
})))
colnames(cc_res_r1) = c("DAUID", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                        "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                        "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
                        "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                        "40", "41", "42", "43", "44", "45", "46", "47", "48")

# Normalize (e.g., Subtract year 0 from all years)
cc_res_r2 = cc_res_r1
cc_res_r2[,2:50] = cc_res_r1[,2:50] - cc_res_r1[,2]

# CC_res_g (DevEnd = 0), but keeping full time-series
# If value <0, NA
cc_res_g1 = cc_res_g
cc_res_g1[YearsDevEnd_dum < 0] = NA # Just postDev

# Shift non-NA values to beginning of table, NAs to end
cc_res_g2 = as.data.frame(t(apply(cc_res_g1,1,function(x) {
  return(c(x[!is.na(x)], x[is.na(x)]))
})))
colnames(cc_res_g2) = c("DAUID", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                        "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                        "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
                        "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                        "40", "41", "42", "43", "44", "45", "46", "47", "48")

#If value > -1, NA
cc_res_g3 = cc_res_g
cc_res_g3[YearsDevEnd_dum > -1] = NA # Just preDev
cc_res_g3 = subset(cc_res_g3, select = -c(DAUID)) # Remove DAUID for now

# Shift non-NA values to end of table, NAs to beginning
cc_res_g4 = as.data.frame(t(apply(cc_res_g3,1,function(x) {
  return(c(x[is.na(x)], x[!is.na(x)]))
})))
colnames(cc_res_g4) = c("-49", "-48", "-47", "-46", "-45", "-44", "-43", "-42", "-41", "-40",
                        "-39", "-38", "-37", "-36", "-35", "-34", "-33", "-32", "-31", "-30",
                        "-29", "-28", "-27", "-26", "-25", "-24", "-23", "-22", "-21", "-20",
                        "-19", "-18", "-17", "-16", "-15", "-14", "-13", "-12", "-11", "-10",
                        "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1")


# Fill blank table
cc_res_g5 = as.data.frame(matrix(nrow = nrow(da_data_res), ncol = 122))
colnames(cc_res_g5) = c("DAUID","-60","-59","-58","-57","-56","-55","-54","-53","-52","-51",
                        "-50","-49","-48","-47","-46","-45","-44","-43","-42","-41",
                        "-40","-39","-38","-37","-36","-35","-34","-33","-32","-31",
                        "-30","-29","-28","-27","-26","-25","-24","-23","-22","-21",
                        "-20","-19","-18","-17","-16","-15","-14","-13","-12","-11",
                        "-10","-9","-8","-7","-6","-5","-4","-3","-2","-1",
                        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                        "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                        "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
                        "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                        "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
                        "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60")
cc_res_g5$DAUID = da_data_res$DAUID

cc_res_g5[,13:61] = cc_res_g4 #PreDev
cc_res_g5[,62:110] = cc_res_g2[,2:50] #PostDev

# Normalize (e.g., Subtract year 0 from all years)
cc_res_g6 = cc_res_g5
cc_res_g6[,2:122] = cc_res_g5[,2:122] - cc_res_g5[,62] # Dev Year 0 column

# ALSO: If Transition year = 1972, Keep 0 at 0
# If Transition year = 1960, Move 0 to 12 (assuming res at least 12 years old in 1972)
# Can do easily in Excel

# Outputs: 
# cc_res_r with years on X (i.e., NAs before DevEnd, keep in regular years format)
#write.csv(cc_res_r, "PostDevCC_res_r.csv")

# cc_res_r with postdev years on X (i.e., DevEnd = 0) + version where year 0 is norm to 0
write.csv(cc_res_r1, "Dev0CC_res_r.csv")
write.csv(cc_res_r2, "Dev0CC_res_r_norm0.csv")

# cc_res_g with postdev years on X (i.e., DevEnd = 0), but keep full time-series
write.csv(cc_res_g5, "Dev0CC_res_g.csv")
write.csv(cc_res_g6, "Dev0CC_res_g_norm0.csv")

# cc_res_g as is
#write.csv(cc_res_g, "CC_res_g.csv")

#####




    


