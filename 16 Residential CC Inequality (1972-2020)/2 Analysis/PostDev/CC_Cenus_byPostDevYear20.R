##### Create scatterplots with GAM lines for post-dev year 20 #####

library(ggplot2)
library(patchwork)
library(mgcv)
library(weights)
library(ggpubr)
library(scales) 

##### Bring in Data #####
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

tbl = read.csv("CensusCC Compare/By PostDevelopment Year/cc_yr20_all.csv")

tbl$PopDen[tbl$PopDen >= 20000] = sample(15000:20000,100)
tbl$MedInc[tbl$MedInc >= 250000] = 245000

k = 50

alpha = 0.2
big_ln = 2
pt_mn1 = 0.5 
pt_mx1 = 6 
eq_sz = 3
axt_sz = 10
axn_sz = 8
#####

##### Plots ######
### Linear models ###
# Transition year
trans.lm = lm(CC ~ Trans_yr, tbl)
summary(trans.lm) # R2 = 0.16***
plot(CC ~ Trans_yr, tbl)
abline(trans.lm)

# Population Density (< 20000)
popden.lm = lm(CC ~ PopDen, subset(tbl, PopDen < 20000))
summary(popden.lm) # R2 = 0.35***
plot(CC ~ PopDen, subset(tbl, PopDen < 20000))
abline(popden.lm)

# Median Income
medinc.lm = lm(CC ~ MedInc, tbl)
summary(medinc.lm) # R2 = 0.11***
plot(CC ~ MedInc, tbl)
abline(medinc.lm)

# % Racial Minorities
pmin.lm = lm(CC ~ pMin, tbl)
summary(pmin.lm) # R2 = 0.39***
plot(CC ~ pMin, tbl)
abline(pmin.lm)

### Weighted GAM Figures ###
# Transition Year
trans.gam = gam(CC ~ s(Trans_yr, bs = "cs", k = 27), data = tbl, weights = Pop, method = "REML")
summary(trans.gam) # R2 = 0.15***

trans_pred = data.frame(Trans_yr = tbl$Trans_yr, CC = tbl$CC, Pop = tbl$Pop, pred = predict(trans.gam, newdata = tbl))

trans.p = ggplot(trans_pred, aes(x = Trans_yr, y = CC, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) +
  scale_x_continuous(name = "Transition Year", limits = c(1973, 2001), expand = c(0,0)) +
  scale_y_continuous(name = "Residential CC (%) - Year 20", limits = c(0, 45), expand = c(0,0)) + 
  scale_size_continuous(limits = c(min(tbl$Pop), max(tbl$Pop)), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 1994, y = 42, label = "R� = 0.15**", size = eq_sz) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
trans.p

# Population Density (< 20000)
popden.gam = gam(CC ~ s(PopDen, bs = "cs", k = k), data = tbl, weights = Pop, method = "REML")
summary(popden.gam) # R2 = 0.52***

popden_pred = data.frame(PopDen = tbl$PopDen, CC = tbl$CC, Pop = tbl$Pop, pred = predict(popden.gam, newdata = tbl))

popden.p = ggplot(popden_pred, aes(x = PopDen, y = CC, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) +
  scale_x_continuous(name = "Population Density", limits = c(0, 20000), expand = c(0,0)) +
  scale_y_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) + 
  scale_size_continuous(limits = c(min(tbl$Pop), max(tbl$Pop)), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 15000, y = 42, label = "R� = 0.49***", size = eq_sz) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
popden.p

# Most Common Dwelling Type (90+%) - Boxplot
wtd.t.test(x = subset(tbl$CC, tbl$pDet >= 90), y = subset(tbl$CC, tbl$pAtt >= 90),
           weight = subset(tbl$Pop, tbl$pDet >= 90), weighty = subset(tbl$Pop, tbl$pAtt >= 90), bootse = TRUE)
# Diff = 5.9***
wtd.t.test(x = subset(tbl$CC, tbl$pApt >= 90), y = subset(tbl$CC, tbl$pAtt >= 90),
           weight = subset(tbl$Pop, tbl$pApt >= 90), weighty = subset(tbl$Pop, tbl$pAtt >= 90), bootse = TRUE)
# Diff = 1.2
wtd.t.test(x = subset(tbl$CC, tbl$pDet >= 90), y = subset(tbl$CC, tbl$pApt >= 90),
           weight = subset(tbl$Pop, tbl$pDet >= 90), weighty = subset(tbl$Pop, tbl$pApt >= 90), bootse = TRUE)
# Diff = 4.7***

dwty.p = ggboxplot(subset(tbl, pDet >= 90 | pAtt >= 90 | pApt >= 90), x = "MC_DwTy", y = "CC", weight = "Pop") +
  geom_bracket(xmin = c("Detached", "Attached", "Detached"), xmax = c("Attached", "Apartment", "Apartment"), y.position = c(4, 4, 1), 
               label = c("*** (6.0)", " ns (1.6)", "*** (4.4)"), tip.length = -0.01, label.size = eq_sz) +
  scale_y_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) +
  scale_x_discrete(name = "Dwelling Type (90+%)") +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
dwty.p

# Median Income
medinc.gam = gam(CC ~ s(MedInc, bs = "cs", k = k), data = tbl, weights = Pop, method = "REML")
summary(medinc.gam) # R2 = 0.11***

medinc_pred = data.frame(MedInc = tbl$MedInc, CC = tbl$CC, Pop = tbl$Pop, pred = predict(medinc.gam, newdata = tbl))

medinc.p = ggplot(medinc_pred, aes(x = MedInc / 1000, y = CC, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) +
  scale_x_continuous(name = "Median Income (1000s $ CAD)", limits = c(20, 250), expand = c(0,0), labels = comma) +
  scale_y_continuous(name = "Residential CC (%) - Year 20", limits = c(0, 45), expand = c(0,0)) + 
  scale_size_continuous(limits = c(min(tbl$Pop), max(tbl$Pop)), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 210, y = 42, label = "R� = 0.10**", size = eq_sz) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
medinc.p

# % Racial Minorities
pmin.gam = gam(CC ~ s(pMin, bs = "cs", k = k), data = tbl, weights = Pop, method = "REML")
summary(pmin.gam) # R2 = 0.38***

pmin_pred = data.frame(pMin = tbl$pMin, CC = tbl$CC, Pop = tbl$Pop, pred = predict(pmin.gam, newdata = tbl))

pmin.p = ggplot(pmin_pred, aes(x = pMin, y = CC, size = Pop)) +
  geom_point(alpha = alpha) +
  geom_line(aes(y = pred), col = "red3", lwd = big_ln) +
  scale_x_continuous(name = "Visible Minorities (%)", limits = c(0, 100), expand = c(0,0)) +
  scale_y_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) + 
  scale_size_continuous(limits = c(min(tbl$Pop), max(tbl$Pop)), range = c(pt_mn1, pt_mx1)) + 
  annotate(geom = "text", x = 83, y = 42, label = "R� = 0.37***", size = eq_sz) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
pmin.p
#####

tiff("PostDev20_census_1.tif", units = "cm", width = 16.5, height = 16.5, res = 300)
(trans.p + popden.p + dwty.p) /
  (medinc.p + pmin.p)
dev.off()