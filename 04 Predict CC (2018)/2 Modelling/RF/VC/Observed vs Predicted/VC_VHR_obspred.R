setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation")

tbl = read.csv("VC_VHR_obspred.csv")

library(caret)
library(ggplot2)
library(ggpmisc)
library(patchwork)

# Figures
formula = y ~ x

agr = "#E6E600"
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
ObsvPred.lm = lm(pVC ~ predicted, tbl)
summary(ObsvPred.lm)
plot(pVC ~ predicted, tbl)
abline(ObsvPred.lm)
# y = 1.01x - 0.38, R2 = 0.96****
ObsvPred.cv = train(pVC ~ predicted, data = tbl, method = "lm", trControl = train.control) 
print(ObsvPred.cv) 
# RMSE = 7.07

pVC.p = ggplot(tbl, aes(x = predicted, y = pVC, color = LandCover)) + 
  geom_point(size = pt_sz) +
  geom_abline(col = "gray48", lwd = big_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = big_ln) +
  scale_color_manual(values = c(agr, bar, com, fr, nvc, os, res, rur, wet)) + 
  scale_x_continuous(name = "OOB Predicted %VC", 
                     breaks = seq(0, 100, 10), limits = c(0,100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = "Observed %VC", breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 3, y = 95, label = "A", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 30, y = 95, label = "y = 1.01x - 0.38, R² = 0.96****", size = eq_sz) +
  annotate(geom = "text", x = 30, y = 90, label = "RMSE = 7.07", size = eq_sz) +
  theme(axis.title = element_text(size = axt_sz),
        axis.text = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
pVC.p

# Individual ELC
ObsvPred.lm_agr = lm(pVC ~ predicted, subset(tbl, LandCover == "Agriculture"))
summary(ObsvPred.lm_agr)
plot(pVC ~ predicted, subset(tbl, LandCover == "Agriculture"))
abline(ObsvPred.lm_agr)
# y = 1.02x - 0.95
# R2 = 0.95****
ObsvPred_agr.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Agriculture"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_agr.cv) 
# RMSE = 9.39

pVC_agr.p = ggplot(subset(tbl, LandCover == "Agriculture"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = agr) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 5, y = 90, label = "B", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 30, y = 90, label = "y = 1.02x - 0.95", size = eq_sz_sm) +
  annotate(geom = "text", x = 30, y = 80, label = "R² = 0.95****", size = eq_sz_sm) +
  annotate(geom = "text", x = 30, y = 70, label = "RMSE = 9.39", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0), "cm"),
        legend.position = "none")
pVC_agr.p 

ObsvPred.lm_bar = lm(pVC ~ predicted, subset(tbl, LandCover == "Barren"))
summary(ObsvPred.lm_bar)
plot(pVC ~ predicted, subset(tbl, LandCover == "Barren"))
abline(ObsvPred.lm_bar)
# y = 0.96x + 0.02
# R2 = 0.88****
ObsvPred_bar.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Barren"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_bar.cv) 
# RMSE = 9.00

pVC_bar.p = ggplot(subset(tbl, LandCover == "Barren"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = bar) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.96x + 0.02", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.88****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 9.00", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0.1), "cm"),
        legend.position = "none")
pVC_bar.p  

ObsvPred.lm_com = lm(pVC ~ predicted, subset(tbl, LandCover == "CommercialIndustrial"))
summary(ObsvPred.lm_com)
plot(pVC ~ predicted, subset(tbl, LandCover == "CommercialIndustrial"))
abline(ObsvPred.lm_com)
# y = 1.01x + 0.32
# R2 = 0.86****
ObsvPred_com.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "CommercialIndustrial"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_com.cv) 
# RMSE = 6.00

pVC_com.p = ggplot(subset(tbl, LandCover == "CommercialIndustrial"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = com) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.01x + 0.32", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.86****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 6.00", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0.1), "cm"),
        legend.position = "none")
pVC_com.p 

ObsvPred.lm_for = lm(pVC ~ predicted, subset(tbl, LandCover == "Forest"))
summary(ObsvPred.lm_for)
plot(pVC ~ predicted, subset(tbl, LandCover == "Forest"))
abline(ObsvPred.lm_for)
# y = 1.04x - 3.77
# R2 = 0.85****
ObsvPred_for.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Forest"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_for.cv) 
# RMSE = 3.35

pVC_for.p = ggplot(subset(tbl, LandCover == "Forest"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = fr) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = "Observed %VC", breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.04x - 3.77", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.85****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 3.35", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0.1,0), "cm"),
        legend.position = "none")
pVC_for.p 

ObsvPred.lm_nvc = lm(pVC ~ predicted, subset(tbl, LandCover == "Natural Vegetation Cover"))
summary(ObsvPred.lm_nvc)
plot(pVC ~ predicted, subset(tbl, LandCover == "Natural Vegetation Cover"))
abline(ObsvPred.lm_nvc)
# y = 0.95x + 5.19
# R2 = 0.87****
ObsvPred_nvc.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Natural Vegetation Cover"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_nvc.cv) 
# RMSE = 5.30

pVC_nvc.p = ggplot(subset(tbl, LandCover == "Natural Vegetation Cover"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = nvc) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.95x + 5.19", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.87****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 5.30", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = "none")
pVC_nvc.p 

ObsvPred.lm_os = lm(pVC ~ predicted, subset(tbl, LandCover == "Open Space"))
summary(ObsvPred.lm_os)
plot(pVC ~ predicted, subset(tbl, LandCover == "Open Space"))
abline(ObsvPred.lm_os)
# y = 0.96x + 2.76
# R2 = 0.80****
ObsvPred_os.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Open Space"), 
                       method = "lm", trControl = train.control) 
print(ObsvPred_os.cv) 
# RMSE = 10.2

pVC_os.p = ggplot(subset(tbl, LandCover == "Open Space"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = os) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.96x + 2.76", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.80****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 10.2", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0.1,0.1), "cm"),
        legend.position = "none")
pVC_os.p 

ObsvPred.lm_res = lm(pVC ~ predicted, subset(tbl, LandCover == "Residential"))
summary(ObsvPred.lm_res)
plot(pVC ~ predicted, subset(tbl, LandCover == "Residential"))
abline(ObsvPred.lm_res)
# y = 0.92x + 2.62
# R2 = 0.87****
ObsvPred_res.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Residential"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_res.cv) 
# RMSE = 5.84

pVC_res.p = ggplot(subset(tbl, LandCover == "Residential"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = res) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.92x + 2.62", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.87****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 5.84", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        legend.position = "none")
pVC_res.p 

ObsvPred.lm_rur = lm(pVC ~ predicted, subset(tbl, LandCover == "Rural"))
summary(ObsvPred.lm_rur)
plot(pVC ~ predicted, subset(tbl, LandCover == "Rural"))
abline(ObsvPred.lm_rur)
# y = 0.97x - 0.08
# R2 = 0.62****
ObsvPred_rur.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Rural"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_rur.cv) 
# RMSE = 10.4

pVC_rur.p = ggplot(subset(tbl, LandCover == "Rural"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = rur) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = "OOB Predicted %VC", 
                     breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.97x - 0.08", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.62****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 10.4", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        legend.position = "none")
pVC_rur.p 

ObsvPred.lm_wet = lm(pVC ~ predicted, subset(tbl, LandCover == "Wetland"))
summary(ObsvPred.lm_wet)
plot(pVC ~ predicted, subset(tbl, LandCover == "Wetland"))
abline(ObsvPred.lm_wet)
# y = 0.56 + 43.7
# R2 = 0.40****
ObsvPred_wet.cv = train(pVC ~ predicted, data = subset(tbl, LandCover == "Wetland"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_wet.cv) 
# RMSE = 3.50

pVC_wet.p = ggplot(subset(tbl, LandCover == "Wetland"), aes(x = predicted, y = pVC)) +
  geom_point(size = pt_sz_sm, color = wet) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.56x + 43.7", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.40****", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 3.50", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.1), "cm"),
        legend.position = "none")
pVC_wet.p 

tiff("VC_VHR_ObsPred.tif", units = "cm", width = 16.5, height = 20, res = 300)
pVC.p / # Big overall VC plot + individual plots for each ELC
  ((pVC_agr.p + pVC_bar.p + pVC_com.p) /
     (pVC_for.p + pVC_nvc.p + pVC_os.p) /
     (pVC_res.p + pVC_rur.p + pVC_wet.p))
dev.off()
