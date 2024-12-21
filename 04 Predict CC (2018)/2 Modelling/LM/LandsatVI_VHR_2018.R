setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation")

tbl = read.csv("LandsatVI_VHR_2018.csv")
tbl1 = subset(tbl, Include == 1) 
# Remaining sites
tbl0 = subset(tbl, Include == 0)
# Sites that were removed: 
# Classified as aquatic ecosystem
# More than 10% (~ 1/9 pixels) of land cover is aquatic ecosystem
# Agriculture sites where crops had been grown/harvested between Landsat and VHR dates

library(Hmisc)
library(caret)
library(ggplot2)
library(ggpmisc)
library(patchwork)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Overall
cor = rcorr(as.matrix(tbl1[2:8]), type = "pearson")
flattenCorrMatrix(round(cor$r^2, 3), round(cor$P, 4))
# %CC
# %CC vs. NDVI = 0.67**** # Best
# %CC vs. TCB = -0.39****
# %CC vs. TCG = 0.55****
# %CC vs. TCW = 0.57****
# %CC vs. TCA = 0.66**** 

# %VC
# %VC vs. NDVI = 0.96****
# %VC vs. TCB = -0.37****
# %VC vs. TCG = 0.91****
# %VC vs. TCW = 0.76****
# %VC vs. TCA = 0.96**** # Best

# Agriculture
cor_agr = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Agriculture")))
flattenCorrMatrix(round(cor_agr$r^2, 3), round(cor_agr$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Agriculture"), subset(tbl1$pVC, tbl1$LandCover == "Agriculture"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Agriculture"), subset(tbl1$pCC, tbl1$LandCover == "Agriculture"))
# %CC
# %CC vs. NDVI = 0.31****
# %CC vs. TCB = -0.34**** # Best
# %CC vs. TCG = 0.25***
# %CC vs. TCW = 0.30****
# %CC vs. TCA = 0.30****

# %VC
# %VC vs. NDVI = 0.95**** # Best
# %VC vs. TCB = -0.76****
# %VC vs. TCG = 0.91****
# %VC vs. TCW = 0.92****
# %VC vs. TCA = 0.95**** 

# Aquatic Ecosystem
#cor_aqu = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Aquatic Ecosystem")))
#flattenCorrMatrix(round(cor_aqu$r, 2), round(cor_aqu$P, 4))

# Barren
cor_bar = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Barren")))
flattenCorrMatrix(round(cor_bar$r^2, 3), round(cor_bar$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Barren"), subset(tbl1$pVC, tbl1$LandCover == "Barren"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Barren"), subset(tbl1$pCC, tbl1$LandCover == "Barren"))
# %CC
# %CC vs. NDVI = 0.79***
# %CC vs. TCB = -0.63**
# %CC vs. TCG = 0.77***
# %CC vs. TCW = 0.77***
# %CC vs. TCA = 0.81**** # Best

# %VC
# %VC vs. NDVI = 0.94****
# %VC vs. TCB = -0.75***
# %VC vs. TCG = 0.92****
# %VC vs. TCW = 0.85****
# %VC vs. TCA = 0.95**** # Best

# Commercial-Industrial
cor_com = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "CommercialIndustrial")))
flattenCorrMatrix(round(cor_com$r^2, 3), round(cor_com$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "CommercialIndustrial"), subset(tbl1$pVC, tbl1$LandCover == "CommercialIndustrial"))
plot(subset(tbl1$TCA, tbl1$LandCover == "CommercialIndustrial"), subset(tbl1$pCC, tbl1$LandCover == "CommercialIndustrial"))
# %CC
# %CC vs. NDVI = 0.62**** # Best
# %CC vs. TCB = -0.22*
# %CC vs. TCG = 0.52****
# %CC vs. TCW = 0.27**
# %CC vs. TCA = 0.61**** 

# %VC
# %VC vs. NDVI = 0.94**** # Best
# %VC vs. TCB = -0.25**
# %VC vs. TCG = 0.91****
# %VC vs. TCW = 0.33***
# %VC vs. TCA = 0.93**** 

# Forest
cor_for = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Forest")))
flattenCorrMatrix(round(cor_for$r^2, 3), round(cor_for$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Forest"), subset(tbl1$pVC, tbl1$LandCover == "Forest"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Forest"), subset(tbl1$pCC, tbl1$LandCover == "Forest"))
# %CC
# %CC vs. NDVI = 0.75**** # Best
# %CC vs. TCB = -0.07
# %CC vs. TCG = 0.40****
# %CC vs. TCW = 0.52****
# %CC vs. TCA = 0.74**** 

# %VC
# %VC vs. NDVI = 0.87****
# %VC vs. TCB = -0.03
# %VC vs. TCG = 0.50****
# %VC vs. TCW = 0.61****
# %VC vs. TCA = 0.90**** # Best

# Natural Vegetation Cover
cor_nvg = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Natural Vegetation Cover")))
flattenCorrMatrix(round(cor_nvg$r^2, 3), round(cor_nvg$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Natural Vegetation Cover"), subset(tbl1$pVC, tbl1$LandCover == "Natural Vegetation Cover"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Natural Vegetation Cover"), subset(tbl1$pCC, tbl1$LandCover == "Natural Vegetation Cover"))
# %CC
# %CC vs. NDVI = 0.53**** # Best
# %CC vs. TCB = -0.02
# %CC vs. TCG = 0.41****
# %CC vs. TCW = 0.36***
# %CC vs. TCA = 0.51**** 

# %VC
# %VC vs. NDVI = 0.89**** # Best
# %VC vs. TCB = 0.22*
# %VC vs. TCG = 0.79****
# %VC vs. TCW = 0.38***
# %VC vs. TCA = 0.88**** 

# Open Space
cor_os = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Open Space")))
flattenCorrMatrix(round(cor_os$r^2, 3), round(cor_os$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Open Space"), subset(tbl1$pVC, tbl1$LandCover == "Open Space"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Open Space"), subset(tbl1$pCC, tbl1$LandCover == "Open Space"))
# %CC
# %CC vs. NDVI = 0.39* 
# %CC vs. TCB = -0.42** 
# %CC vs. TCG = 0.25
# %CC vs. TCW = 0.54*** # Best
# %CC vs. TCA = 0.41*

# %VC
# %VC vs. NDVI = 0.92**** # Best
# %VC vs. TCB = 0.17
# %VC vs. TCG = 0.90****
# %VC vs. TCW = 0.56***
# %VC vs. TCA = 0.91**** 

# Residential
cor_res = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Residential")))
flattenCorrMatrix(round(cor_res$r^2, 4), round(cor_res$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Residential"), subset(tbl1$pVC, tbl1$LandCover == "Residential"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Residential"), subset(tbl1$pCC, tbl1$LandCover == "Residential"))
# %CC
# %CC vs. NDVI = 0.90**** # Best
# %CC vs. TCB = -0.03
# %CC vs. TCG = 0.87****
# %CC vs. TCW = 0.59****
# %CC vs. TCA = 0.90**** 

# %VC
# %VC vs. NDVI = 0.94**** # Best
# %VC vs. TCB = 0.04
# %VC vs. TCG = 0.94****
# %VC vs. TCW = 0.54****
# %VC vs. TCA = 0.94**** 

# Rural
cor_rur = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Rural")))
flattenCorrMatrix(round(cor_rur$r^2, 3), round(cor_rur$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Rural"), subset(tbl1$pVC, tbl1$LandCover == "Rural"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Rural"), subset(tbl1$pCC, tbl1$LandCover == "Rural"))
# %CC
# %CC vs. NDVI = 0.65**** 
# %CC vs. TCB = -0.52****
# %CC vs. TCG = 0.46***
# %CC vs. TCW = 0.67**** # Best
# %CC vs. TCA = 0.67**** 

# %VC
# %VC vs. NDVI = 0.83**** 
# %VC vs. TCB = -0.44***
# %VC vs. TCG = 0.73****
# %VC vs. TCW = 0.65****
# %VC vs. TCA = 0.84**** # Best

# Wetland
cor_wet = rcorr(as.matrix(subset(tbl1[2:8], tbl1$LandCover == "Wetland")))
flattenCorrMatrix(round(cor_wet$r^2, 3), round(cor_wet$P, 4))
plot(subset(tbl1$TCA, tbl1$LandCover == "Wetland"), subset(tbl1$pVC, tbl1$LandCover == "Wetland"))
plot(subset(tbl1$TCA, tbl1$LandCover == "Wetland"), subset(tbl1$pCC, tbl1$LandCover == "Wetland"))
# %CC
# %CC vs. NDVI = 0.72**** # Best
# %CC vs. TCB = -0.34*
# %CC vs. TCG = 0.16
# %CC vs. TCW = 0.57****
# %CC vs. TCA = 0.68**** 

# %VC
# %VC vs. NDVI = 0.64**** 
# %VC vs. TCB = -0.11
# %VC vs. TCG = 0.32*
# %VC vs. TCW = 0.42**
# %VC vs. TCA = 0.65*** # Best

# Figures
formula = y ~ x

agr = "#E6E600"
#aqu = "#005CE6"
bar = "#734C00"
com = "#FF0000"
fr = "#267300"
nvc = "#70A800"
os = "#E69800"
res = "#686868"
rur = "#CCCCCC"
wet = "#8400A8"

pt_sz = 2
pt_sz_sm = 1
big_ln = 1.5
sm_ln = 1
eq_sz = 3
eq_sz_sm = 2.5
axt_sz = 10
axn_sz = 8
anno_sz = 6

# Leave one out cross validation
#http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
train.control = trainControl(method = "LOOCV")

#%VC
TCA_mnvpVC.lm = lm(pVC ~ TCA_mn, tbl1)
summary(TCA_mnvpVC.lm)
plot(pVC ~ TCA_mn, tbl1)
abline(TCA_mnvpVC.lm)
# y = 0.027x - 11.1, R2 = 0.92****
TCA_mnvpVC.cv = train(pVC ~ TCA_mn, data = tbl1, method = "lm", trControl = train.control) 
print(TCA_mnvpVC.cv) 
# RMSE = 10.2
TCA_mnvpVC.cv$results$RMSE / mean(tbl1$pVC) * 100
# %RMSE = 16%

pVC.p = ggplot(tbl1, aes(x = TCA_mn, y = pVC, color = LandCover)) + 
  geom_point(size = pt_sz) +
  #geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = big_ln) +
  scale_color_manual(values = c(agr, bar, com, fr, nvc, os, res, rur, wet)) + 
  scale_x_continuous(name = "TCA", breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = "Vegetation Cover (%)", breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = -300, y = 95, label = "A", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 1100, y = 85, label = "y = 0.027x - 11.1, R� = 0.92***", size = eq_sz) +
  annotate(geom = "text", x = 1100, y = 80, label = "RMSE = 10.2", size = eq_sz) +
  theme(axis.title = element_text(size = axt_sz),
        axis.text = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none",
        plot.margin = unit(c(0, 0.25, 0.15, 0), "cm"))
pVC.p

# Individual ELC
TCA_mnvpVC.lm_agr = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Agriculture"))
summary(TCA_mnvpVC.lm_agr)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Agriculture"))
abline(TCA_mnvpVC.lm_agr)
# y = 0.031x - 22.8
# R2 = 0.90****
TCA_mnvpVC_agr.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Agriculture"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_agr.cv) 
# RMSE = 13.7
TCA_mnvpVC_agr.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Agriculture")) * 100
# %RMSE = 25%

pVC_agr.p = ggplot(subset(tbl1, LandCover == "Agriculture"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = agr) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = -300, y = 90, label = "B", fontface = "bold", size = anno_sz) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.031x - 22.8", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.90***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 13.7", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.05,0.05,0), "cm"),
        legend.position = "none")
pVC_agr.p 

TCA_mnvpVC.lm_bar = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Barren"))
summary(TCA_mnvpVC.lm_bar)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Barren"))
abline(TCA_mnvpVC.lm_bar)
# y = 0.023x - 2.24
# R2 = 0.90****
TCA_mnvpVC_bar.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Barren"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_bar.cv) 
# RMSE = 8.37
TCA_mnvpVC_bar.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Barren")) * 100
# %RMSE = 49%

pVC_bar.p = ggplot(subset(tbl1, LandCover == "Barren"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = bar) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.023x - 2.24", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.90***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 8.37", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.05,0.05,0.05), "cm"),
        legend.position = "none")
pVC_bar.p  

TCA_mnvpVC.lm_com = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "CommercialIndustrial"))
summary(TCA_mnvpVC.lm_com)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "CommercialIndustrial"))
abline(TCA_mnvpVC.lm_com)
# y = 0.022x + 0.16
# R2 = 0.87****
TCA_mnvpVC_com.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "CommercialIndustrial"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_com.cv) 
# RMSE = 5.66
TCA_mnvpVC_com.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "CommercialIndustrial")) * 100
# %RMSE = 27%

pVC_com.p = ggplot(subset(tbl1, LandCover == "CommercialIndustrial"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = com) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.022x + 0.16", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.87***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 5.66", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.25,0.05,0.05), "cm"),
        legend.position = "none")
pVC_com.p 

TCA_mnvpVC.lm_for = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Forest"))
summary(TCA_mnvpVC.lm_for)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Forest"))
abline(TCA_mnvpVC.lm_for)
# y = 0.022x + 8.91
# R2 = 0.81****
TCA_mnvpVC_for.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Forest"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_for.cv) 
# RMSE = 3.79
TCA_mnvpVC_for.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Forest")) * 100
# %RMSE = 4%

pVC_for.p = ggplot(subset(tbl1, LandCover == "Forest"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = fr) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = "Vegetation Cover (%)", breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.022x + 8.91", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.81***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 3.79", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0.05,0), "cm"),
        legend.position = "none")
pVC_for.p 

TCA_mnvpVC.lm_nvc = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Natural Vegetation Cover"))
summary(TCA_mnvpVC.lm_nvc)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Natural Vegetation Cover"))
abline(TCA_mnvpVC.lm_nvc)
# y = 0.024x + 4.77
# R2 = 0.77****
TCA_mnvpVC_nvc.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Natural Vegetation Cover"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_nvc.cv) 
# RMSE = 7.07
TCA_mnvpVC_nvc.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Natural Vegetation Cover")) * 100
# %RMSE = 8%

pVC_nvc.p = ggplot(subset(tbl1, LandCover == "Natural Vegetation Cover"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = nvc) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.024x + 4.77", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.77***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 7.07", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0.05,0.05), "cm"),
        legend.position = "none")
pVC_nvc.p 

TCA_mnvpVC.lm_os = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Open Space"))
summary(TCA_mnvpVC.lm_os)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Open Space"))
abline(TCA_mnvpVC.lm_os)
# y = 0.026x - 4.62
# R2 = 0.83****
TCA_mnvpVC_os.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Open Space"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_os.cv) 
# RMSE = 9.28
TCA_mnvpVC_os.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Open Space")) * 100
# %RMSE = 13%

pVC_os.p = ggplot(subset(tbl1, LandCover == "Open Space"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = os) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.026x - 4.62", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.83***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 9.28", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.25,0.05,0.05), "cm"),
        legend.position = "none")
pVC_os.p 

TCA_mnvpVC.lm_res = lm(pVC ~ TCA_mn_mn, subset(tbl1, LandCover == "Residential"))
summary(TCA_mnvpVC.lm_res)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Residential"))
abline(TCA_mnvpVC.lm_res)
# y = 0.022x - 5.26
# R2 = 0.88****
TCA_mnvpVC_res.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Residential"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_res.cv) 
# RMSE = 5.41
TCA_mnvpVC_res.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Residential")) * 100
# %RMSE = 13%

pVC_res.p = ggplot(subset(tbl1, LandCover == "Residential"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = res) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.022x - 5.26", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.88***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 5.41", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0,0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
pVC_res.p 

TCA_mnvpVC.lm_rur = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Rural"))
summary(TCA_mnvpVC.lm_rur)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Rural"))
abline(TCA_mnvpVC.lm_rur)
# y = 0.027x - 10.6
# R2 = 0.71****
TCA_mnvpVC_rur.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Rural"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_rur.cv) 
# RMSE = 9.20
TCA_mnvpVC_rur.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Rural")) * 100
# %RMSE = 13%

pVC_rur.p = ggplot(subset(tbl1, LandCover == "Rural"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = rur) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = "TCA", breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.027x - 10.6", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.71***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 9.20", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0,0.05), "cm"),
        legend.position = "none")
pVC_rur.p 

TCA_mnvpVC.lm_wet = lm(pVC ~ TCA_mn, subset(tbl1, LandCover == "Wetland"))
summary(TCA_mnvpVC.lm_wet)
plot(pVC ~ TCA_mn, subset(tbl1, LandCover == "Wetland"))
abline(TCA_mnvpVC.lm_wet)
# y = 0.010 + 58.7
# R2 = 0.42****
TCA_mnvpVC_wet.cv = train(pVC ~ TCA_mn, data = subset(tbl1, LandCover == "Wetland"), method = "lm", trControl = train.control) 
print(TCA_mnvpVC_wet.cv) 
# RMSE = 3.34
TCA_mnvpVC_wet.cv$results$RMSE / mean(subset(tbl1$pVC, tbl1$LandCover == "Wetland")) * 100
# %RMSE = 3%

pVC_wet.p = ggplot(subset(tbl1, LandCover == "Wetland"), aes(x = TCA_mn, y = pVC)) +
  geom_point(size = pt_sz_sm, color = wet) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.010x + 58.7", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.42***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 3.34", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.25,0,0.05), "cm"),
        legend.position = "none")
pVC_wet.p 

tiff("VIvVHR_VC.tif", units = "cm", width = 16.5, height = 20, res = 300)
pVC.p / # Big overall VC plot + individual plots for each ELC
  ((pVC_agr.p + pVC_bar.p + pVC_com.p) /
     (pVC_for.p + pVC_nvc.p + pVC_os.p) /
     (pVC_res.p + pVC_rur.p + pVC_wet.p))
dev.off()

#%CC
TCAvpCC.lm = lm(pCC ~ TCA, tbl1)
summary(TCAvpCC.lm)
plot(pCC ~ TCA, tbl1)
abline(TCAvpCC.lm)
# y = 0.016x - 15.6, R2 = 0.44****
TCAvpCC.cv = train(pCC ~ TCA, data = tbl1, method = "lm", trControl = train.control) 
print(TCAvpCC.cv) 
# RMSE = 23.7

pCC.p = ggplot(tbl1, aes(x = TCA_mn, y = pCC, color = LandCover)) + 
  geom_point(size = pt_sz) +
  #geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = big_ln) +
  scale_color_manual(values = c(agr, bar, com, fr, nvc, os, res, rur, wet),
                     labels = c("Agriculture", "Barren", "Commercial-Industrial",
                                "Forest", "Natural Vegetation Cover", "Open Space",
                                "Residential", "Rural Development", "Wetland")) + 
  scale_x_continuous(name = "TCA", breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = "Canopy Cover (%)", breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = -300, y = 95, label = "B", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 1700, y = 95, label = "y = 0.016x - 15.6, R� = 0.44***", size = eq_sz) +
  annotate(geom = "text", x = 1700, y = 90, label = "RMSE = 23.7", size = eq_sz) +
  theme(axis.title = element_text(size = axt_sz),
        axis.text = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = c(0.35,0.62),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, "cm"),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))
pCC.p

# Individual ELC
TCAvpCC.lm_agr = lm(pCC ~ TCA_mn, subset(tbl1, LandCover == "Agriculture"))
summary(TCAvpCC.lm_agr)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Agriculture"))
abline(TCAvpCC.lm_agr)
# y = 0.002x - 1.41
# R2 = 0.09****
TCAvpCC_agr.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Agriculture"), method = "lm", trControl = train.control) 
print(TCAvpCC_agr.cv) 
# RMSE = 9.64

pCC_agr.p = ggplot(subset(tbl1, LandCover == "Agriculture"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = agr) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = -300, y = 90, label = "B", fontface = "bold", size = anno_sz) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.002x - 1.41", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.09***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 9.64", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.05,0.05,0), "cm"),
        legend.position = "none")
pCC_agr.p 

TCAvpCC.lm_bar = lm(pCC ~ TCA, subset(tbl1, LandCover == "Barren"))
summary(TCAvpCC.lm_bar)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Barren"))
abline(TCAvpCC.lm_bar)
# y = 0.010x - 2.95
# R2 = 0.66****
TCAvpCC_bar.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Barren"), method = "lm", trControl = train.control) 
print(TCAvpCC_bar.cv) 
# RMSE = 8.56

pCC_bar.p = ggplot(subset(tbl1, LandCover == "Barren"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = bar) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.010x - 2.95", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.66***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 8.56", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.05,0.05,0.05), "cm"),
        legend.position = "none")
pCC_bar.p  

TCAvpCC.lm_com = lm(pCC ~ TCA, subset(tbl1, LandCover == "CommercialIndustrial"))
summary(TCAvpCC.lm_com)
plot(pCC ~ TCA, subset(tbl1, LandCover == "CommercialIndustrial"))
abline(TCAvpCC.lm_com)
# y = 0.007x - 1.21
# R2 = 0.38****
TCAvpCC_com.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "CommercialIndustrial"), method = "lm", trControl = train.control) 
print(TCAvpCC_com.cv) 
# RMSE = 6.02

pCC_com.p = ggplot(subset(tbl1, LandCover == "CommercialIndustrial"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = com) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.007x - 1.21", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.38***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 6.02", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.05,0.05), "cm"),
        legend.position = "none")
pCC_com.p 

TCAvpCC.lm_for = lm(pCC ~ TCA, subset(tbl1, LandCover == "Forest"))
summary(TCAvpCC.lm_for)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Forest"))
abline(TCAvpCC.lm_for)
# y = 0.032x - 47.9
# R2 = 0.55****
TCAvpCC_for.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Forest"), method = "lm", trControl = train.control) 
print(TCAvpCC_for.cv) 
# RMSE = 10.2

pCC_for.p = ggplot(subset(tbl1, LandCover == "Forest"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = fr) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = "Canopy Cover (%)", breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.032x - 47.9", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.55***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 10.2", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0.05,0), "cm"),
        legend.position = "none")
pCC_for.p 

TCAvpCC.lm_nvc = lm(pCC ~ TCA, subset(tbl1, LandCover == "Natural Vegetation Cover"))
summary(TCAvpCC.lm_nvc)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Natural Vegetation Cover"))
abline(TCAvpCC.lm_nvc)
# y = 0.016x - 33.2
# R2 = 0.26****
TCAvpCC_nvc.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Natural Vegetation Cover"), method = "lm", trControl = train.control) 
print(TCAvpCC_nvc.cv) 
# RMSE = 14.2

pCC_nvc.p = ggplot(subset(tbl1, LandCover == "Natural Vegetation Cover"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = nvc) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.016x - 33.2", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.26***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 14.2", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0.05,0.05), "cm"),
        legend.position = "none")
pCC_nvc.p 

TCAvpCC.lm_os = lm(pCC ~ TCA, subset(tbl1, LandCover == "Open Space"))
summary(TCAvpCC.lm_os)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Open Space"))
abline(TCAvpCC.lm_os)
# y = 0.006x - 6.40
# R2 = 0.17*
TCAvpCC_os.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Open Space"), method = "lm", trControl = train.control) 
print(TCAvpCC_os.cv) 
# RMSE = 11.0

pCC_os.p = ggplot(subset(tbl1, LandCover == "Open Space"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = os) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.006x - 6.40", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.17*", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 11.0", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0,0.05,0.05), "cm"),
        legend.position = "none")
pCC_os.p 

TCAvpCC.lm_res = lm(pCC ~ TCA_mn, subset(tbl1, LandCover == "Residential"))
summary(TCAvpCC.lm_res)
plot(pCC ~ TCA_mn, subset(tbl1, LandCover == "Residential"))
abline(TCAvpCC.lm_res)
# y = 0.020x - 22.2
# R2 = 0.81****
TCAvpCC_res.cv = train(pCC ~ TCA_mn, data = subset(tbl1, LandCover == "Residential"), method = "lm", trControl = train.control) 
print(TCAvpCC_res.cv) 
# RMSE = 6.43

pCC_res.p = ggplot(subset(tbl1, LandCover == "Residential"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = res) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.020x - 22.2", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.81***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 6.43", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0,0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
pCC_res.p 

TCAvpCC.lm_rur = lm(pCC ~ TCA, subset(tbl1, LandCover == "Rural"))
summary(TCAvpCC.lm_rur)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Rural"))
abline(TCAvpCC.lm_rur)
# y = 0.019x - 29.7
# R2 = 0.45****
TCAvpCC_rur.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Rural"), method = "lm", trControl = train.control) 
print(TCAvpCC_rur.cv) 
# RMSE = 11.4

pCC_rur.p = ggplot(subset(tbl1, LandCover == "Rural"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = rur) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = "TCA", breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.019x - 29.7", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.45***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 11.4", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0.05,0,0.05), "cm"),
        legend.position = "none")
pCC_rur.p 

TCAvpCC.lm_wet = lm(pCC ~ TCA, subset(tbl1, LandCover == "Wetland"))
summary(TCAvpCC.lm_wet)
plot(pCC ~ TCA, subset(tbl1, LandCover == "Wetland"))
abline(TCAvpCC.lm_wet)
# y = 0.007 - 193
# R2 = 0.46****
TCAvpCC_wet.cv = train(pCC ~ TCA, data = subset(tbl1, LandCover == "Wetland"), method = "lm", trControl = train.control) 
print(TCAvpCC_wet.cv) 
# RMSE = 20.4

pCC_wet.p = ggplot(subset(tbl1, LandCover == "Wetland"), aes(x = TCA_mn, y = pCC)) +
  geom_point(size = pt_sz_sm, color = wet) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 4000, 1000), limits = c(-650,4500), expand = c(0,0)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  #annotate(geom = "text", x = 1000, y = 90, label = "y = 0.007x - 193", size = eq_sz_sm) +
  annotate(geom = "text", x = 1300, y = 90, label = "R� = 0.46***", size = eq_sz_sm) +
  #annotate(geom = "text", x = 1000, y = 70, label = "RMSE = 20.4", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black", angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.05,0,0,0.05), "cm"),
        legend.position = "none")
pCC_wet.p 

tiff("VIvVHR_CC.tif", units = "cm", width = 16.5, height = 20, res = 300)
pCC.p / # Big overall VC plot + individual plots for each ELC
  ((pCC_agr.p + pCC_bar.p + pCC_com.p) /
     (pCC_for.p + pCC_nvc.p + pCC_os.p) /
     (pCC_res.p + pCC_rur.p + pCC_wet.p))
dev.off()

### SUPER COMBINED FIGURE ###
tiff("VIvVHR_VCCC_1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(pVC.p / # Big overall VC plot + individual plots for each ELC
  ((pVC_agr.p + pVC_bar.p + pVC_com.p) /
     (pVC_for.p + pVC_nvc.p + pVC_os.p) /
     (pVC_res.p + pVC_rur.p + pVC_wet.p))) |
(pCC.p / # Big overall VC plot + individual plots for each ELC
  ((pCC_agr.p + pCC_bar.p + pCC_com.p) /
     (pCC_for.p + pCC_nvc.p + pCC_os.p) /
     (pCC_res.p + pCC_rur.p + pCC_wet.p)))
dev.off()

# Strongest Linear Models (Table 5)
# TCA vs. %VC
TCAvpVC.lm = lm(pVC ~ TCA_mn, tbl1)
summary(TCAvpVC.lm)
plot(pVC ~ TCA_mn, tbl1)
abline(TCAvpVC.lm)
# y = 0.02742x - 11.16
# R2 = 0.92****
TCAvpVC.cv = train(pVC ~ TCA_mn, data = tbl1, method = "lm", trControl = train.control) 
print(TCAvpVC.cv) 
# RMSE = 10.2

# NDVI vs. %CC
NDVIvpCC.lm = lm(pCC ~ NDVI_mn, tbl1)
summary(NDVIvpCC.lm)
plot(pCC ~ NDVI_mn, tbl1)
abline(NDVIvpCC.lm)
# y = 85.807x - 22.288
# R2 = 0.45****
NDVIvpCC.cv = train(pCC ~ NDVI_mn, data = tbl1, method = "lm", trControl = train.control) 
print(NDVIvpCC.cv) 
# RMSE = 23.4

# NDVI vs. %CC (forest)
NDVIvpCC.lm_for = lm(pCC ~ NDVI_mn, subset(tbl1, LandCover == "Forest"))
summary(NDVIvpCC.lm_for)
plot(pCC ~ NDVI_mn, subset(tbl1, LandCover == "Forest"))
abline(NDVIvpCC.lm_for)
# y = 165.13x - 58.94
# R2 = 0.56****
NDVIvpCC_for.cv = train(pCC ~ NDVI_mn, data = subset(tbl1, LandCover == "Forest"), method = "lm", trControl = train.control) 
print(NDVIvpCC_for.cv) 
# RMSE = 10.1

