setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation")

tbl = read.csv("CC_VHR_obspred.csv")

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

#%CC
ObsvPred.lm = lm(pCC ~ predicted, tbl)
summary(ObsvPred.lm)
plot(pCC ~ predicted, tbl)
abline(ObsvPred.lm)
# y = 1.02x - 0.56, R2 = 0.89****
ObsvPred.cv = train(pCC ~ predicted, data = tbl, method = "lm", trControl = train.control) 
print(ObsvPred.cv) 
# RMSE = 10.2

pCC.p = ggplot(tbl, aes(x = predicted, y = pCC, color = LandCover)) + 
  geom_point(size = pt_sz) +
  geom_abline(col = "gray48", lwd = big_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = big_ln) +
  scale_color_manual(values = c(agr, bar, com, fr, nvc, os, res, rur, wet),
                     labels = c("Agriculture", "Barren", "Commercial-Industrial",
                                "Forest", "Natural Vegetation Cover", "Open Space",
                                "Residential", "Rural", "Wetland")) + 
  scale_x_continuous(name = "OOB Predicted %CC", 
                     breaks = seq(0, 100, 10), limits = c(0,100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = "Observed %CC", breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 3, y = 95, label = "A", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 30, y = 95, label = "y = 1.02x - 0.56, R² = 0.89***", size = eq_sz) +
  annotate(geom = "text", x = 30, y = 90, label = "RMSE = 10.2", size = eq_sz) +
  theme(axis.title = element_text(size = axt_sz),
        axis.text = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = c(0.82,0.25),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.background = element_blank(),
        legend.key.size = unit(0.35, "cm"))
pCC.p

# Individual ELC
ObsvPred.lm_agr = lm(pCC ~ predicted, subset(tbl, LandCover == "Agriculture"))
summary(ObsvPred.lm_agr)
plot(pCC ~ predicted, subset(tbl, LandCover == "Agriculture"))
abline(ObsvPred.lm_agr)
# y = 0.57x + 2.13
# R2 = 0.15****
ObsvPred_agr.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Agriculture"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_agr.cv) 
# RMSE = 9.35

pCC_agr.p = ggplot(subset(tbl, LandCover == "Agriculture"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = agr) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 5, y = 90, label = "B", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 30, y = 90, label = "y = 0.57x + 2.13", size = eq_sz_sm) +
  annotate(geom = "text", x = 30, y = 80, label = "R² = 0.15***", size = eq_sz_sm) +
  annotate(geom = "text", x = 30, y = 70, label = "RMSE = 9.35", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0), "cm"),
        legend.position = "none")
pCC_agr.p 

ObsvPred.lm_bar = lm(pCC ~ predicted, subset(tbl, LandCover == "Barren"))
summary(ObsvPred.lm_bar)
plot(pCC ~ predicted, subset(tbl, LandCover == "Barren"))
abline(ObsvPred.lm_bar)
# y = 1.11x + 0.21
# R2 = 0.49**
ObsvPred_bar.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Barren"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_bar.cv) 
# RMSE = 12.2

pCC_bar.p = ggplot(subset(tbl, LandCover == "Barren"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = bar) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.11x + 0.21", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.49**", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 12.2", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0.1), "cm"),
        legend.position = "none")
pCC_bar.p  

ObsvPred.lm_com = lm(pCC ~ predicted, subset(tbl, LandCover == "CommercialIndustrial"))
summary(ObsvPred.lm_com)
plot(pCC ~ predicted, subset(tbl, LandCover == "CommercialIndustrial"))
abline(ObsvPred.lm_com)
# y = 0.97x + 0.19
# R2 = 0.41****
ObsvPred_com.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "CommercialIndustrial"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_com.cv) 
# RMSE = 5.89

pCC_com.p = ggplot(subset(tbl, LandCover == "CommercialIndustrial"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = com) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.97x + 0.19", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.41***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 5.89", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0.1), "cm"),
        legend.position = "none")
pCC_com.p 

ObsvPred.lm_for = lm(pCC ~ predicted, subset(tbl, LandCover == "Forest"))
summary(ObsvPred.lm_for)
plot(pCC ~ predicted, subset(tbl, LandCover == "Forest"))
abline(ObsvPred.lm_for)
# y = 0.86x + 12.5
# R2 = 0.61****
ObsvPred_for.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Forest"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_for.cv) 
# RMSE = 9.53

pCC_for.p = ggplot(subset(tbl, LandCover == "Forest"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = fr) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = "Observed %CC", breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.86x + 12.5", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.61***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 9.53", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0.1,0), "cm"),
        legend.position = "none")
pCC_for.p 

ObsvPred.lm_nvc = lm(pCC ~ predicted, subset(tbl, LandCover == "Natural Vegetation Cover"))
summary(ObsvPred.lm_nvc)
plot(pCC ~ predicted, subset(tbl, LandCover == "Natural Vegetation Cover"))
abline(ObsvPred.lm_nvc)
# y = 1.07x - 2.13
# R2 = 0.34****
ObsvPred_nvc.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Natural Vegetation Cover"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_nvc.cv) 
# RMSE = 13.4

pCC_nvc.p = ggplot(subset(tbl, LandCover == "Natural Vegetation Cover"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = nvc) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.07x - 2.13", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.34***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 13.4", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = "none")
pCC_nvc.p 

ObsvPred.lm_os = lm(pCC ~ predicted, subset(tbl, LandCover == "Open Space"))
summary(ObsvPred.lm_os)
plot(pCC ~ predicted, subset(tbl, LandCover == "Open Space"))
abline(ObsvPred.lm_os)
# y = 1.12x - 1.59
# R2 = 0.20**
ObsvPred_os.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Open Space"), 
                       method = "lm", trControl = train.control) 
print(ObsvPred_os.cv) 
# RMSE = 11.1

pCC_os.p = ggplot(subset(tbl, LandCover == "Open Space"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = os) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.12x - 1.59", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.20**", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 11.1", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0.1,0.1), "cm"),
        legend.position = "none")
pCC_os.p 

ObsvPred.lm_res = lm(pCC ~ predicted, subset(tbl, LandCover == "Residential"))
summary(ObsvPred.lm_res)
plot(pCC ~ predicted, subset(tbl, LandCover == "Residential"))
abline(ObsvPred.lm_res)
# y = 1.08x - 1.00
# R2 = 0.81****
ObsvPred_res.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Residential"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_res.cv) 
# RMSE = 6.42

pCC_res.p = ggplot(subset(tbl, LandCover == "Residential"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = res) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.08x - 1.00", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.81***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 6.42", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        legend.position = "none")
pCC_res.p 

ObsvPred.lm_rur = lm(pCC ~ predicted, subset(tbl, LandCover == "Rural"))
summary(ObsvPred.lm_rur)
plot(pCC ~ predicted, subset(tbl, LandCover == "Rural"))
abline(ObsvPred.lm_rur)
# y = 0.99x + 1.28
# R2 = 0.44****
ObsvPred_rur.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Rural"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_rur.cv) 
# RMSE = 11.6

pCC_rur.p = ggplot(subset(tbl, LandCover == "Rural"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = rur) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = "OOB Predicted %CC", 
                     breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.99x + 1.28", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.44***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 11.6", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        legend.position = "none")
pCC_rur.p 

ObsvPred.lm_wet = lm(pCC ~ predicted, subset(tbl, LandCover == "Wetland"))
summary(ObsvPred.lm_wet)
plot(pCC ~ predicted, subset(tbl, LandCover == "Wetland"))
abline(ObsvPred.lm_wet)
# y = 0.99 - 0.6803
# R2 = 0.47****
ObsvPred_wet.cv = train(pCC ~ predicted, data = subset(tbl, LandCover == "Wetland"), 
                        method = "lm", trControl = train.control) 
print(ObsvPred_wet.cv) 
# RMSE = 20.3

pCC_wet.p = ggplot(subset(tbl, LandCover == "Wetland"), aes(x = predicted, y = pCC)) +
  geom_point(size = pt_sz_sm, color = wet) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.99x + 2.5", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.47***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 20.3", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.1), "cm"),
        legend.position = "none")
pCC_wet.p 

tiff("CC_VHR_ObsPred.tif", units = "cm", width = 16.5, height = 20, res = 300)
pCC.p / # Big overall VC plot + individual plots for each ELC
  ((pCC_agr.p + pCC_bar.p + pCC_com.p) /
     (pCC_for.p + pCC_nvc.p + pCC_os.p) /
     (pCC_res.p + pCC_rur.p + pCC_wet.p))
dev.off()
