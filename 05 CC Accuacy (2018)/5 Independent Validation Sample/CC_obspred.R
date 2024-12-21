#setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation")
setwd("C:/Users/Mitchell/My Drive/Work/Research/Projects/MTB/Active/2018/Landsat CC across ROP-CRW since 1972/5 CC Accuacy (2018)/5 Independent Validation Sample")

tbl = read.csv("Sample214_details.csv")

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

#%CC
ObsvPred.lm = lm(pCC_90 ~ pCC_90_pred, tbl)
summary(ObsvPred.lm)
plot(pCC_90 ~ pCC_90_pred, tbl)
abline(ObsvPred.lm)
# y = 1.03x - 3.54, R2 = 0.86***
sqrt(mean((tbl$pCC_90 - tbl$pCC_90_pred)^2))
# RMSE = 11.4

pCC.p = ggplot(tbl, aes(x = pCC_90_pred, y = pCC_90, color = InterpLandcover1_90)) + 
  geom_point(size = pt_sz) +
  geom_abline(col = "gray48", lwd = big_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = big_ln) +
  scale_color_manual(values = c(agr, bar, com, fr, nvc, os, res, rur, wet),
                     labels = c("Agriculture", "Barren", "Commercial-Industrial",
                                "Forest", "Natural Vegetation Cover", "Open Space",
                                "Residential", "Rural Development", "Wetland")) + 
  scale_x_continuous(name = "RF Predicted %CC", 
                     breaks = seq(0, 100, 10), limits = c(0,100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = "Observed %CC", breaks = seq(0, 100, 10), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 3, y = 95, label = "A", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 30, y = 95, label = "y = 1.03x - 3.54, R² = 0.86***", size = eq_sz) +
  annotate(geom = "text", x = 30, y = 90, label = "RMSE = 11.4", size = eq_sz) +
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
sub = subset(tbl, InterpLandcover1_90 == "Agriculture")
ObsvPred.lm_agr = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_agr)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_agr)
# y = 0.53x - 0.36
# R2 = 0.34***
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2))
# RMSE = 10.3

pCC_agr.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = agr) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 5, y = 90, label = "B", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 30, y = 90, label = "y = 0.53x - 0.36", size = eq_sz_sm) +
  annotate(geom = "text", x = 30, y = 80, label = "R² = 0.34***", size = eq_sz_sm) +
  annotate(geom = "text", x = 30, y = 70, label = "RMSE = 10.3", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Barren")
ObsvPred.lm_bar = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_bar)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_bar)
# y = 0.04x + 2.00
# R2 = 0.01
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 14.1

pCC_bar.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = bar) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.04x + 2.00", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.01", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 14.1", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Commercial-Industrial")
ObsvPred.lm_com = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_com)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_com)
# y = 0.47x + 1.31
# R2 = 0.24*
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 5.83

pCC_com.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = com) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.47x + 1.31", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.24*", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 5.83", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Forest")
ObsvPred.lm_for = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_for)
plot(pCC ~ predicted, subset(tbl, LandCover == "Forest"))
abline(ObsvPred.lm_for)
# y = 0.71x + 22.7
# R2 = 0.62***
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 9.72

pCC_for.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = fr) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = "Observed %CC", breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.71x + 22.7", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.62***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 9.72", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Natural Vegetation Cover")
ObsvPred.lm_nvc = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_nvc)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_nvc)
# y = 0.77x + 5.05
# R2 = 0.40**
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 14.6

pCC_nvc.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = nvc) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 0.77x + 5.05", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.40**", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 14.6", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Open Space")
ObsvPred.lm_os = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_os)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_os)
# y = -0.00x + 13.3
# R2 = 0.00
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 11.2

pCC_os.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = os) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = -0.00x + 13.3", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.00", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 11.2", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Residential")
ObsvPred.lm_res = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_res)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_res)
# y = 1.23x - 5.66
# R2 = 0.93***
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 6.27

pCC_res.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = res) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.23x - 5.66", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.93***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 6.27", size = eq_sz_sm) +
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

sub = subset(tbl, InterpLandcover1_90 == "Rural Development")
ObsvPred.lm_rur = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_rur)
plot(pCC_90 ~ pCC_90_pred, sub)
abline(ObsvPred.lm_rur)
# y = 1.20x - 2.24
# R2 = 0.68***
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 12.5

pCC_rur.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = rur) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = "OOB Predicted %CC", 
                     breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.20x - 2.24", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.68***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 12.5", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
pCC_rur.p 

sub = subset(tbl, InterpLandcover1_90 == "Wetland")
ObsvPred.lm_wet = lm(pCC_90 ~ pCC_90_pred, sub)
summary(ObsvPred.lm_wet)
plot(pCC ~ predicted, subset(tbl, LandCover == "Wetland"))
abline(ObsvPred.lm_wet)
# y = 1.52x - 44.6
# R2 = 0.83***
sqrt(mean((sub$pCC_90 - sub$pCC_90_pred)^2)) 
# RMSE = 21.1

pCC_wet.p = ggplot(sub, aes(x = pCC_90_pred, y = pCC_90)) +
  geom_point(size = pt_sz_sm, color = wet) +
  geom_abline(col = "gray48", lwd = sm_ln) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, level = 0.95, col = "black", lwd = sm_ln) +
  scale_x_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  scale_y_continuous(name = NULL, breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0.01,0.01)) +
  annotate(geom = "text", x = 22, y = 90, label = "y = 1.52x - 44.6", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 80, label = "R² = 0.83***", size = eq_sz_sm) +
  annotate(geom = "text", x = 22, y = 70, label = "RMSE = 21.1", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
pCC_wet.p 

tiff("CC_ObsPred.tif", units = "cm", width = 16.5, height = 20, res = 300)
pCC.p / # Big overall VC plot + individual plots for each ELC
  ((pCC_agr.p + pCC_bar.p + pCC_com.p) /
     (pCC_for.p + pCC_nvc.p + pCC_os.p) /
     (pCC_res.p + pCC_rur.p + pCC_wet.p))
dev.off()
