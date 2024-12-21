library(raster)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyverse)
library(patchwork)

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Ground Validation")

# ROP shapefile
ROP = raster("ROP/ROP_NoWater.tif")
plot(ROP) # Need to use as mask

# Load rasters,  convert to dataframes and build tables
landcover = raster("rf/predictors/tif/Landcover_2018_14_NoWater.tif")
plot(landcover)

landcover = crop(landcover, ROP)
landcover = mask(landcover, ROP)
plot(landcover)

landcover = as.data.frame(landcover, na.rm = TRUE)

landcover1 = data.frame(matrix(nrow = nrow(landcover), ncol = 0))
landcover1$landcover = landcover$Landcover_2018_14_NoWater
landcover1 = landcover1 %>% mutate(landcover = recode(landcover,
                                        '1' = "Agriculture",
                                        '2' = "Barren",
                                        '3' = "Commercial-Industrial",
                                        '4' = "Forest",
                                        '5' = "Natural Vegetation Cover",
                                        '6' = "Open Space",
                                        '7' = "Residential",
                                        '8' = "Rural",
                                        '9' = "Wetland"))

cc_vhrdhp_rf = raster("rf/cc_dhpvhr/ccdhpvhr_2018_rf_1.tif")
plot(cc_vhrdhp_rf)

cc = crop(cc_vhrdhp_rf, ROP)
cc = mask(cc, ROP)
plot(cc)

cc = as.data.frame(cc, na.rm = TRUE)
colnames(cc) = "pCC"
cc$landcover = landcover1$landcover

ROP = as.data.frame(ROP, na.rm = TRUE) 

cc$municipality = ROP$ROP_NoWater
cc = cc %>% mutate(municipality = recode(municipality,
                                 '1' = "Mississauga",
                                 '2' = "Brampton",
                                 '3' = "Caledon"))

# change order for plotting
cc = cc %>% mutate(municipality = fct_relevel(municipality, levels = "Mississauga", "Brampton", "Caledon"))

# Means / Median
# ROP
mean(cc$pCC) # 26.3 +- 27.6
sd(cc$pCC)
median(cc$pCC) # 14.3

mean(cc[cc$municipality == "Caledon", "pCC"]) # 34.0
mean(cc[cc$municipality == "Brampton", "pCC"]) # 15.9
mean(cc[cc$municipality == "Mississauga", "pCC"]) # 17.5

# CC VHRDHP
mean(cc[cc$landcover == "Residential", "pCC"]) # 19.8 +- 11.8
sd(cc[cc$landcover == "Residential", "pCC"])
mean(cc[cc$municipality == "Caledon" & cc$landcover == "Residential", "pCC"]) # 26.4 +- 11.5
sd(cc[cc$municipality == "Caledon" & cc$landcover == "Residential", "pCC"])
median(cc[cc$municipality == "Caledon" & cc$landcover == "Residential", "pCC"]) # 28.9
mean(cc[cc$municipality == "Brampton" & cc$landcover == "Residential", "pCC"]) # 15.3 +- 9.7
sd(cc[cc$municipality == "Brampton" & cc$landcover == "Residential", "pCC"])
median(cc[cc$municipality == "Brampton" & cc$landcover == "Residential", "pCC"]) # 12.4
mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Residential", "pCC"]) # 22.3 +- 12.1
sd(cc[cc$municipality == "Mississauga" & cc$landcover == "Residential", "pCC"])
median(cc[cc$municipality == "Mississauga" & cc$landcover == "Residential", "pCC"]) # 18.7

# % Residential pixels above 15% 
(nrow(subset(cc, pCC >= 15 & landcover == "Residential")) / nrow(subset(cc, landcover == "Residential"))) * 100 
# 59% overall
(nrow(subset(cc, pCC >= 15 & landcover == "Residential" & municipality == "Caledon")) / 
    nrow(subset(cc, landcover == "Residential" & municipality == "Caledon"))) * 100 
# 80% in Caledon
(nrow(subset(cc, pCC >= 15 & landcover == "Residential" & municipality == "Brampton")) / 
    nrow(subset(cc, landcover == "Residential" & municipality == "Brampton"))) * 100 
# 43% in Brampton
(nrow(subset(cc, pCC >= 15 & landcover == "Residential" & municipality == "Mississauga")) / 
    nrow(subset(cc, landcover == "Residential" & municipality == "Mississauga"))) * 100 
# 69% in Mississauga

# FOR RESIDENTIAL FOCUS USE SPECIALIZED CC VHR RES MODEL
cc_vhr_res = raster("rf/cc_vhr/residential/ccvhr_res_2018_rf_2.tif")
plot(cc_vhr_res)

cc_vhr_res = crop(cc_vhr_res, ROP)
cc_vhr_res = mask(cc_vhr_res, ROP)
plot(cc_vhr_res)

ROP_res = crop(ROP, cc_vhr_res)
ROP_res = mask(ROP, cc_vhr_res)
plot(ROP_res)

cc_vhr_res = as.data.frame(cc_vhr_res, na.rm = TRUE)
colnames(cc_vhr_res) = "pCC"

ROP_res = as.data.frame(ROP_res, na.rm = TRUE) 

cc_vhr_res$municipality = ROP_res$ROP_NoWater
cc_vhr_res = cc_vhr_res %>% mutate(municipality = recode(municipality,
                                         '1' = "Mississauga",
                                         '2' = "Brampton",
                                         '3' = "Caledon"))

mean(cc_vhr_res$pCC) # 20.1 +- 13.1
sd(cc_vhr_res$pCC)
mean(cc_vhr_res[cc_vhr_res$municipality == "Caledon", "pCC"]) # 28.9 +- 15.0
sd(cc_vhr_res[cc_vhr_res$municipality == "Caledon", "pCC"])
mean(cc_vhr_res[cc_vhr_res$municipality == "Brampton", "pCC"]) # 14.7 +- 11.0
sd(cc_vhr_res[cc_vhr_res$municipality == "Brampton", "pCC"])
mean(cc_vhr_res[cc_vhr_res$municipality == "Mississauga", "pCC"]) # 22.9 +- 12.7
sd(cc_vhr_res[cc_vhr_res$municipality == "Mississauga", "pCC"])

# % Residential pixels above 15% 
(nrow(subset(cc_vhr_res, pCC >= 15)) / nrow(cc_vhr_res)) * 100 
# 62% overall
(nrow(subset(cc_vhr_res, pCC >= 15 & municipality == "Caledon")) / 
    nrow(subset(cc_vhr_res, municipality == "Caledon"))) * 100 
# 76% in Caledon
(nrow(subset(cc_vhr_res, pCC >= 15 & municipality == "Brampton")) / 
    nrow(subset(cc_vhr_res, municipality == "Brampton"))) * 100 
# 44% in Brampton
(nrow(subset(cc_vhr_res, pCC >= 15 & municipality == "Mississauga")) / 
    nrow(subset(cc_vhr_res, municipality == "Mississauga"))) * 100 
# 73% in Mississauga

cc_vhr_res = cc_vhr_res %>% mutate(municipality = fct_relevel(municipality, levels = "Mississauga", "Brampton", "Caledon"))
pCC_res1.p = ggplot(cc_vhr_res, aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.01, scale = 2, alpha = 0.5, fill = "black",
                      quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(name = "Canopy Cover (%)", breaks = seq(0, 100, 10), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        legend.position = "none")
pCC_res1.p

### %CC by land cover ###

# Caledon
nrow(subset(cc, municipality == "Caledon")) * 9e-04 # 686 km2

# Agriculture
nrow(subset(cc, municipality == "Caledon" & landcover == "Agriculture")) * 9e-04 # 286 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Agriculture", "pCC"]) # 8.4 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Agriculture")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 41.7% of area

# Barren
nrow(subset(cc, municipality == "Caledon" & landcover == "Barren")) * 9e-04 # 11 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Barren", "pCC"]) # 13.1 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Barren")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 1.5% of area

# Commercial-Industrial
nrow(subset(cc, municipality == "Caledon" & landcover == "Commercial-Industrial")) * 9e-04 # 11 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Commercial-Industrial", "pCC"]) # 6.2 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Commercial-Industrial")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 1.7% of area

# Forest
nrow(subset(cc, municipality == "Caledon" & landcover == "Forest")) * 9e-04 # 170 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Forest", "pCC"]) # 78.5 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Forest")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 24.8% of area

# Natural Vegetation Cover
nrow(subset(cc, municipality == "Caledon" & landcover == "Natural Vegetation Cover")) * 9e-04 # 67 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Natural Vegetation Cover", "pCC"]) # 24.2 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Natural Vegetation Cover")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 9.7% of area

# Open Space
nrow(subset(cc, municipality == "Caledon" & landcover == "Open Space")) * 9e-04 # 24 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Open Space", "pCC"]) # 19.1 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Open Space")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 3.5% of area

# Residential
nrow(subset(cc, municipality == "Caledon" & landcover == "Residential")) * 9e-04 # 15 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Residential", "pCC"]) # 26.4 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Residential")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 2.2% of area

# Rural
nrow(subset(cc, municipality == "Caledon" & landcover == "Rural")) * 9e-04 # 51 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Rural", "pCC"]) # 30.1 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Rural")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 7.4% of area

# Wetland
nrow(subset(cc, municipality == "Caledon" & landcover == "Wetland")) * 9e-04 # 51 km2

mean(cc[cc$municipality == "Caledon" & cc$landcover == "Wetland", "pCC"]) # 66.4 %CC
nrow(subset(cc, municipality == "Caledon" & landcover == "Wetland")) / 
  nrow(subset(cc, municipality == "Caledon")) * 100 # 7.5% of area



# Brampton
nrow(subset(cc, municipality == "Brampton")) * 9e-04 # 266 km2

# Agriculture
nrow(subset(cc, municipality == "Brampton" & landcover == "Agriculture")) * 9e-04 # 39 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Agriculture", "pCC"]) # 5.2 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Agriculture")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 14.7% of area

# Barren
nrow(subset(cc, municipality == "Brampton" & landcover == "Barren")) * 9e-04 # 10 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Barren", "pCC"]) # 6.5 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Barren")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 3.7% of area

# Commercial-Industrial
nrow(subset(cc, municipality == "Brampton" & landcover == "Commercial-Industrial")) * 9e-04 # 50 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Commercial-Industrial", "pCC"]) # 5.1 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Commercial-Industrial")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 18.9% of area

# Forest
nrow(subset(cc, municipality == "Brampton" & landcover == "Forest")) * 9e-04 # 17 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Forest", "pCC"]) # 67.1 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Forest")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 6.4% of area

# Natural Vegetation Cover
nrow(subset(cc, municipality == "Brampton" & landcover == "Natural Vegetation Cover")) * 9e-04 # 25 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Natural Vegetation Cover", "pCC"]) # 18.5 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Natural Vegetation Cover")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 9.6% of area

# Open Space
nrow(subset(cc, municipality == "Brampton" & landcover == "Open Space")) * 9e-04 # 20 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Open Space", "pCC"]) # 14.2 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Open Space")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 7.6% of area

# Residential
nrow(subset(cc, municipality == "Brampton" & landcover == "Residential")) * 9e-04 # 83 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Residential", "pCC"]) # 15.3 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Residential")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 31.2% of area

# Rural
nrow(subset(cc, municipality == "Brampton" & landcover == "Rural")) * 9e-04 # 16 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Rural", "pCC"]) # 19.2 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Rural")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 6.2% of area

# Wetland
nrow(subset(cc, municipality == "Brampton" & landcover == "Wetland")) * 9e-04 # 5 km2

mean(cc[cc$municipality == "Brampton" & cc$landcover == "Wetland", "pCC"]) # 50.1 %CC
nrow(subset(cc, municipality == "Brampton" & landcover == "Wetland")) / 
  nrow(subset(cc, municipality == "Brampton")) * 100 # 1.7% of area



# Mississauga
nrow(subset(cc, municipality == "Mississauga")) * 9e-04 # 289 km2

# Agriculture
nrow(subset(cc, municipality == "Mississauga" & landcover == "Agriculture")) * 9e-04 # 8 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Agriculture", "pCC"]) # 9.2 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Agriculture")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 2.6% of area

# Barren
nrow(subset(cc, municipality == "Mississauga" & landcover == "Barren")) * 9e-04 # 2 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Barren", "pCC"]) # 9.0 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Barren")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 0.8% of area

# Commercial-Industrial
nrow(subset(cc, municipality == "Mississauga" & landcover == "Commercial-Industrial")) * 9e-04 # 96 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Commercial-Industrial", "pCC"]) # 4.9 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Commercial-Industrial")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 33.2% of area

# Forest
nrow(subset(cc, municipality == "Mississauga" & landcover == "Forest")) * 9e-04 # 17 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Forest", "pCC"]) # 66.5 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Forest")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 6.0% of area

# Natural Vegetation Cover
nrow(subset(cc, municipality == "Mississauga" & landcover == "Natural Vegetation Cover")) * 9e-04 # 25 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Natural Vegetation Cover", "pCC"]) # 15.7 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Natural Vegetation Cover")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 8.8% of area

# Open Space
nrow(subset(cc, municipality == "Mississauga" & landcover == "Open Space")) * 9e-04 # 22 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Open Space", "pCC"]) # 14.7 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Open Space")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 7.6% of area

# Residential
nrow(subset(cc, municipality == "Mississauga" & landcover == "Residential")) * 9e-04 # 110 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Residential", "pCC"]) # 22.3 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Residential")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 38.1% of area

# Rural
nrow(subset(cc, municipality == "Mississauga" & landcover == "Rural")) * 9e-04 # 7 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Rural", "pCC"]) # 13.0 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Rural")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 2.4% of area

# Wetland
nrow(subset(cc, municipality == "Mississauga" & landcover == "Wetland")) * 9e-04 # 1 km2

mean(cc[cc$municipality == "Mississauga" & cc$landcover == "Wetland", "pCC"]) # 48.5 %CC
nrow(subset(cc, municipality == "Mississauga" & landcover == "Wetland")) / 
  nrow(subset(cc, municipality == "Mississauga")) * 100 # 0.5% of area

# Geom_density_ridges
agr = "#E6E600"
bar = "#734C00"
com = "#FF0000"
fr = "#267300"
nvc = "#70A800"
os = "#E69800"
res = "#686868"
rur = "#CCCCCC"
wet = "#8400A8"

pt_sz = 4.5
pt_sz_sm = 4
big_ln = 1.5
sm_ln = 1
eq_sz = 3
eq_sz_sm = 2.5
axt_sz = 10
axn_sz = 8
anno_sz = 6

### CC ###
# All pixels
all_mean = cc %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

#hist(subset(cc$pCC, cc$municipality == "Caledon"), breaks = seq(0,100,1))

pCC.p = ggplot(cc, aes(x = pCC, y = municipality)) +
  geom_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 0.65, fill = "black", bandwidth = 1) +
  #geom_point(aes(x = pCC, y = municipality), data = all_mean, inherit.aes = FALSE, color = "black", size = pt_sz) +
  geom_segment(data = all_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
                                    inherit.aes = FALSE, size = 2) +
  scale_x_continuous(name = "Canopy Cover (%)", breaks = seq(0, 100, 10), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = "By-pixel Distribution (scaled)", expand = c(0,0)) + 
  annotate(geom = "text", x = 3, y = 3.80, label = "A", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 50, y = 3.2, label = "686 km²", size = eq_sz) +
  annotate(geom = "text", x = 50, y = 2.2, label = "266 km²", size = eq_sz) +
  annotate(geom = "text", x = 50, y = 1.2, label = "289 km²", size = eq_sz) +
  theme(axis.title = element_text(size = axt_sz),
        axis.text = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
pCC.p
#quantile_lines = TRUE, quantiles = 2 To add median lines

# Agriculture
agr_mean = subset(cc, landcover == "Agriculture") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_agr.p = ggplot(subset(cc, landcover == "Agriculture"), aes(x = pCC, y = municipality)) +
  geom_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = agr, bandwidth = 1) +
  geom_segment(data = agr_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 18, y = 3.6, label = "B", fontface = "bold", size = anno_sz) +
  annotate(geom = "text", x = 80, y = 3.7, label = "Agriculture", size = eq_sz) +
  annotate(geom = "text", x = 50, y = 3.2, label = "286 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 2.2, label = "39 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 1.2, label = "8 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.15,0.1,0), "cm"),
        legend.position = "none")
pCC_agr.p

# Barren
bar_mean = subset(cc, landcover == "Barren") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_bar.p = ggplot(subset(cc, landcover == "Barren"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = bar, bandwidth = 1) +
  geom_segment(data = bar_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 85, y = 3.7, label = "Barren", size = eq_sz) +
  annotate(geom = "text", x = 50, y = 3.2, label = "11 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 2.2, label = "10 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 1.2, label = "2 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.15,0.1,0.15), "cm"),
        legend.position = "none")
pCC_bar.p

# Commercial / Industrial
com_mean = subset(cc, landcover == "Commercial-Industrial") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_com.p = ggplot(subset(cc, landcover == "Commercial-Industrial"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = com, bandwidth = 1) +
  geom_segment(data = com_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 63, y = 3.7, label = "Commercial-Industrial", size = eq_sz) +
  annotate(geom = "text", x = 50, y = 3.2, label = "11 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 2.2, label = "50 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 1.2, label = "96 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0.15), "cm"),
        legend.position = "none")
pCC_com.p 

# Forest
for_mean = subset(cc, landcover == "Forest") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_for.p = ggplot(subset(cc, landcover == "Forest"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = fr, bandwidth = 1) +
  geom_segment(data = for_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = "By-pixel Distribution (scaled)", expand = c(0,0)) + 
  annotate(geom = "text", x = 13, y = 3.7, label = "Forest", size = eq_sz) +
  annotate(geom = "text", x = 20, y = 3.2, label = "170 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 20, y = 2.2, label = "17 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 20, y = 1.2, label = "17 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.15,0.1,0), "cm"),
        legend.position = "none")
pCC_for.p 

# Natural Vegetation Cover
nvc_mean = subset(cc, landcover == "Natural Vegetation Cover") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_nvc.p = ggplot(subset(cc, landcover == "Natural Vegetation Cover"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = nvc, bandwidth = 1) +
  geom_segment(data = nvc_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 65, y = 3.7, label = "Natural Vegetation Cover", size = eq_sz_sm) +
  annotate(geom = "text", x = 60, y = 3.2, label = "67 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 60, y = 2.2, label = "25 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 60, y = 1.2, label = "25 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.15,0.1,0.15), "cm"),
        legend.position = "none")
pCC_nvc.p 

# Open Space
os_mean = subset(cc, landcover == "Open Space") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_os.p = ggplot(subset(cc, landcover == "Open Space"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = os, bandwidth = 1) +
  geom_segment(data = os_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 77, y = 3.7, label = "Open Space", size = eq_sz) +
  annotate(geom = "text", x = 50, y = 3.2, label = "24 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 2.2, label = "20 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 50, y = 1.2, label = "22 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0.1,0.15), "cm"),
        legend.position = "none")
pCC_os.p 

# Residential
res_mean = subset(cc, landcover == "Residential") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_res.p = ggplot(subset(cc, landcover == "Residential"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = res, bandwidth = 1) +
  geom_segment(data = res_mean, aes(x = pCC, xend = pCC, 
                                   y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 80, y = 3.7, label = "Residential", size = eq_sz) +
  annotate(geom = "text", x = 70, y = 3.2, label = "15 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 70, y = 2.2, label = "83 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 70, y = 1.2, label = "110 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.background = element_blank(),
        plot.margin = unit(c(0.1,0.15,0,0), "cm"),
        legend.position = "none")
pCC_res.p 

# Rural
rur_mean = subset(cc, landcover == "Rural") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_rur.p = ggplot(subset(cc, landcover == "Rural"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = rur, bandwidth = 1) +
  geom_segment(data = rur_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(name = "Canopy Cover (%)", breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 67, y = 3.7, label = "Rural Development", size = eq_sz) +
  annotate(geom = "text", x = 70, y = 3.2, label = "51 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 70, y = 2.2, label = "16 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 70, y = 1.2, label = "7 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.15,0,0.15), "cm"),
        legend.position = "none",
        plot.background = element_blank())
pCC_rur.p 

# Wetland
wet_mean = subset(cc, landcover == "Wetland") %>% group_by(municipality) %>% summarize(pCC = mean(pCC))

pCC_wet.p = ggplot(subset(cc, landcover == "Wetland"), aes(x = pCC, y = municipality)) +
  stat_density_ridges(rel_min_height = 0.00, scale = 1, alpha = 1, fill = wet, bandwidth = 1) +
  geom_segment(data = wet_mean, aes(x = pCC, xend = pCC, 
                                    y = as.numeric(municipality), yend = as.numeric(municipality) + 1), 
               inherit.aes = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0,100), expand = c(0,0)) +
  scale_y_discrete(name = NULL, expand = c(0,0)) + 
  annotate(geom = "text", x = 16, y = 3.7, label = "Wetland", size = eq_sz) +
  annotate(geom = "text", x = 10, y = 3.2, label = "51 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 8, y = 2.2, label = "5 km²", size = eq_sz_sm) +
  annotate(geom = "text", x = 8, y = 1.2, label = "1 km²", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.15), "cm"),
        legend.position = "none",
        plot.background = element_blank())
pCC_wet.p 

tiff("CC_ELCmunicipality2.tif", units = "cm", width = 16.5, height = 20, res = 300)
pCC.p / # Big overall CC plot + individual plots for each ELC
  ((pCC_agr.p + pCC_bar.p + pCC_com.p) /
     (pCC_for.p + pCC_nvc.p + pCC_os.p) /
     (pCC_res.p + pCC_rur.p + pCC_wet.p))
dev.off()