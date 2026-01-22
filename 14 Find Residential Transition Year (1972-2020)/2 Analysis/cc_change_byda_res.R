setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")
library(segmented)
library(ggplot2)
library(patchwork)

# Add CC / TCA DA mean time-series (created from LandsatLinkr + LandTrendr + randomforest + GEE reduceRegions)
cc = read.csv("cc_72to20_mn_byDA_res.csv") # Residential only CC model
tca = read.csv("tca_72to20_mn_byDA_res.csv") # Residential only TCA

# Add 1973 (mean of 1972 and 1974?)
cc$X1973 = (cc$X1972 + cc$X1974) / 2 # Add 1973 column
cc = cc[,c(1,2,50,3:49)]# Put it in the right place
cc[2:50] = cc[2:50] * 100 # Convert to %
tca$X1973 = (tca$X1972 + tca$X1974) / 2 
tca = tca[,c(1,2,50,3:49)] 

# Add in DA data
da_data = read.csv("DA_data1.csv") 

# Comparison plot single DA
da = 35212151
#da = 35211737 # example DA

cc_da = subset(cc, DAUID == da) # Find single DA
tca_da = subset(tca, DAUID == da)

plot(1972:2020, cc_da[1,2:50], type = "l", lwd = 2, col = "black", xaxs = "i", yaxs = "i",
     ylim = c(0,54), ylab = "%CCr", xlab = "") # CC residential
grid(48,NULL)
grid(NULL,NULL, col = "black")
par(new = TRUE)
plot(1972:2020, tca_da[1,2:50], type = "l", lwd = 2, col = "red3",
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", ylim = c(0,4000), xaxs = "i", yaxs = "i") # TCA residential
axis(side = 4)
# For figure... cut off CCr on transition year

##### LandTrendr GEE Biggest Loss Option #####
biglossyears = read.csv("Fitted TCA Biggest Loss End Year/DA_EndYearAreas.csv",
                        check.names = FALSE) # use as.numeric to turn into numbers later

biglossyears1 = 100*prop.table(as.matrix(biglossyears[,2:49]), 1) 
# Percentage by row (e.g., what % of each DA has DevEnd in given year?)

# Most recent year with at least X% of pixels (may help with multi-dev DAs)
biglossyearsX = biglossyears1 #[,2:48]
biglossyearsX[biglossyearsX < 5] = NA # Turn all years (besides 0) below X% of pixels to NA
#write.csv(biglossyearsX, "biglossyears8_byda.csv")

DevEnd = data.frame(matrix(nrow = 1600, ncol = 0))
DevEnd$DAUID = cc$DAUID
DevEnd$DevEndYear = as.numeric(colnames(biglossyearsX)[max.col(!is.na(biglossyearsX), ties.method = 'last')])
DevEnd$DevEndYear[DevEnd$DevEndYear == 0] = NA

#write.csv(DevEnd, "DevEnd_landtrendr5.csv") # Solid, need to complete true dates to compare
# Number on end indicates % deciding most recent year
# 4 = 77.4%
# 5 = 79.4% Best
# 6 = 78.2%
# 7 = 77.7%
# 8 = 75% agreement with old Human reference 
# 9 = 73.1%
# 10 = 70.6%
# 12 = 66.5%
# 15 = 61.3%
# 20 = 55.3%
# 25 = 49.8%

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
cc_da$CC[1:26] = NA # PreTransition

tca_da = as.data.frame(t(subset(tca[,2:50], cc$DAUID == da)))
tca_da$year = 1972:2020
colnames(tca_da) = c("TCA", "Year")

cc_devend.p = ggplot() +
  geom_line(data = cc_da, aes(x = Year, y = CC), color = "black") +
  geom_line(data = tca_da, aes(x = Year, y = TCA / 100), color = "red3") +
  geom_vline(xintercept = 1998, linetype = "dashed", color = "gray48") +
  scale_y_continuous(expand = c(0,0), limits = c(0,30), name = expression("CC"[r]*" (%)"), sec.axis = sec_axis(~.*100, name = "TCA")) + 
  scale_x_continuous(expand = c(0,0), limits = c(1972,2020)) +
  #annotate(geom = "text", x = 1973.5, y = 5, label = "D1", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y.left = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.title.y.right = element_text(size = axt_sz, color = "red3"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")
cc_devend.p

da_biggestlossyears = read.csv("Fitted TCA Biggest Loss End Year/ExampleDA_LTRendyear.csv")

ltr_devend.p = ggplot() +
  geom_bar(data = da_biggestlossyears, aes(x = Year, y = Percent),
           stat = "identity") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray48") +
  scale_y_continuous(expand = c(0,0), limits = c(0,59)) +
  #annotate(geom = "text", x = 2, y = 35, label = "D2", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black",
                                   angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0), "cm"),
        legend.position = "none")
ltr_devend.p

#tiff("DevEnd_interp.tif", units = "cm", width = 16.5, height = 10, res = 300)
#cc_devend.p / ltr_devend.p
ltr_devend.p / cc_devend.p
#dev.off()

#####

##### Compare Human and LTR Biggest Loss DevEnd Years #####
# Percent exact agreement
#da_data_res = subset(da_data, MostCommonLandcover == "Residential")
nrow(subset(da_data, Match == "Yes")) / nrow(da_data) * 100 # 80%
nrow(subset(da_data, Match == "Yes")) 
nrow(subset(da_data, Match == "No")) 

# All included DAs
all.p = ggplot(da_data, aes(x = Match)) +
  geom_bar(fill = "gray48") +
  scale_x_discrete(limits = c("Yes", "No"), name = "Agreement") +
  scale_y_continuous(limits = c(0,1300), expand = c(0,0), name = "Number of DAs",
                     breaks = seq(0,1300,100)) +
  annotate(geom = "text", x = 2.4, y = 1230, label = "A", fontface = "bold", size = anno_sz) + 
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

devend_dif = da_data$DevEnd_Hum - da_data$DevEnd_LTR5 # Human minus LTR
devend_dif = na.omit(data.frame(devend_dif))
#hist(devend_dif)
mean(devend_dif$devend_dif) # 0.46 (human IDs transition less than one year later than LTR)
nrow(subset(devend_dif, devend_dif == 0)) / nrow(devend_dif) * 100 # 77% same year
nrow(subset(devend_dif, devend_dif >= -1 & devend_dif <= 1)) / nrow(devend_dif) * 100 # 86% within 1 year
nrow(subset(devend_dif, devend_dif >= -3 & devend_dif <= 3)) / nrow(devend_dif) * 100 # 91% within 3 years
nrow(subset(devend_dif, devend_dif >= -5 & devend_dif <= 5)) / nrow(devend_dif) * 100 # 93% within 5 years
nrow(subset(devend_dif, devend_dif >= -10 & devend_dif <= 10)) / nrow(devend_dif) * 100 # 96% within 10 years

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
  scale_y_continuous(limits = c(0,1300), expand = c(0,0), breaks = seq(0,1300,100)) +
  scale_x_continuous(name = "Difference (human - model)", expand = c(0,0), limits = c(-20,20)) +
  annotate(geom = "text", x = 16, y = 1230, label = "B", fontface = "bold", size = anno_sz) +
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
nrow(subset(da_data, is.na(DevEnd_Hum) == TRUE & is.na(DevEnd_LTR5) == TRUE)) / 
  nrow(subset(da_data, is.na(DevEnd_Hum) == TRUE | is.na(DevEnd_LTR5) == TRUE)) # 87%

na.p = ggplot() +
  geom_bar(data = subset(da_data, is.na(DevEnd_Hum) == TRUE & is.na(DevEnd_LTR5) == TRUE),
           aes(x = Match), fill = "gray48") + # Both have no dev year
  geom_bar(data = subset(da_data, is.na(DevEnd_Hum) == TRUE & is.na(DevEnd_LTR5) == FALSE),
          aes(x = Match), fill = "red3") + # Human picked no dev year but LTR did
  geom_bar(data = subset(da_data, is.na(DevEnd_Hum) == FALSE & is.na(DevEnd_LTR5) == TRUE),
           aes(x = MostCommonLandcover), fill = "blue") + # LTR picked no dev year but human did
  scale_x_discrete(limits = c("No", "Yes", "Residential"),
                  labels = c("No" = "Only human", 
                             "Yes" = "Both", 
                             "Residential" = "Only model"),
                   name = "Agreement") +
  scale_y_continuous(limits = c(0,1300), expand = c(0,0), breaks = seq(0,1300,100)) +
  annotate(geom = "text", x = 3.3, y = 1230, label = "C", fontface = "bold", size = anno_sz) +
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
na.p

#tiff("Human_LTRModel.tif", units = "cm", width = 16.5, height = 10, res = 300)
#all.p + num.p + na.p
#dev.off()
#####

##### Use DevEnd (human) to manipulate cc tables (cut off before, set DevEnd to 0) #####
cc_res = cc

# Build dummy DevEnd table 
DevEnd_dum = cc_res
DevEnd_dum[,2:50] = NA
DevEnd_dum[,2:50] = da_data$DevEnd_Hum # All years for each DA set as DevEnd year (human)
DevEnd_dum[is.na(DevEnd_dum)] = -9999 # NAs to -9999 (turn back to NA later)
#DevEnd_dum[nrow(DevEnd_dum) +1,] = 1971:2020 # Add dummy years

# Build dummy years table
Years_dum = cc_res
Years_dum[,2:50] = NA
for (i in 1:nrow(Years_dum)) {
  Years_dum[i,2:50] = 1972:2020
}

# Build YearsDevEnd dummy table
YearsDevEnd_dum = cc_res
YearsDevEnd_dum[,2:50] = NA
YearsDevEnd_dum[,2:50] = Years_dum[,2:50] - DevEnd_dum[,2:50] 
# 0 = year of transition, negative = preDev, positive = residential

# If value < 0, NA (i.e., preDev residential = NA)
cc_res[YearsDevEnd_dum < 0] = NA

# Shift non-NA values to beginning of table. NAs to end
cc_res1 = as.data.frame(t(apply(cc_res,1,function(x) {
  return(c(x[!is.na(x)], x[is.na(x)]))
})))
colnames(cc_res1) = c("DAUID", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                        "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                        "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
                        "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
                        "40", "41", "42", "43", "44", "45", "46", "47", "48")

# Normalize (e.g., Subtract year 0 from all years)
cc_res2 = cc_res1
cc_res2[,2:50] = cc_res1[,2:50] - cc_res1[,2]

# Export
#write.csv(cc_res, "cc_postdev.csv")
#write.csv(cc_res1, "Dev0cc.csv")
#write.csv(cc_res2, "Dev0cc_norm0.csv")
#####