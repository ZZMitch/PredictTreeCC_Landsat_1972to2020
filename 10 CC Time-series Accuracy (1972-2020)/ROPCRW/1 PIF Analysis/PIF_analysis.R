### Analyze CC time-series from stable CC impervious and agricultural sites ###

library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

##### Initialize Data #####
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/Spatiotemporal canopy change/MaskAlwaysNonForest/PIF Analysis/PIF_120samples")

### Year-to-Year change ###
# Impervious sites
cc_imp = read.csv("cc_comindpts.csv", fileEncoding="UTF-8-BOM", check.names = FALSE) 
# File encoding removes weird syntax for first column name, check names removes X in front of years
cc_imp1 = gather(cc_imp, Year, CC, '1972':'2020') # Convert to long format
cc_imp1$Year = as.numeric(cc_imp1$Year) # Convert Year column from chr to num
cc_imp1$CC = cc_imp1$CC * 100 # Multiply CC by 100 to get %
cc_imp1 = cc_imp1 %>% mutate(Landsat = 
                  case_when(Year <= 1983 ~ "MSS",
                            Year <= 2012 ~ "TM/ETM+",
                            Year <= 2020 ~ "OLI")) # Add Landsat sensor column depending on years
cc_imp1$Landsat = factor(cc_imp1$Landsat, levels = c("MSS", "TM/ETM+", "OLI")) # Reorder sensors for plotting

# Agriculture sites: do the same as above
cc_agr = read.csv("cc_agriculturepts.csv", fileEncoding="UTF-8-BOM", check.names = FALSE)
cc_agr1 = gather(cc_agr, Year, CC, '1972':'2020')
cc_agr1$Year = as.numeric(cc_agr1$Year)
cc_agr1$CC = cc_agr1$CC * 100
cc_agr1 = cc_agr1 %>% mutate(Landsat = 
                               case_when(Year <= 1983 ~ "MSS",
                                         Year <= 2012 ~ "TM/ETM+",
                                         Year <= 2020 ~ "OLI"))
cc_agr1$Landsat = factor(cc_agr1$Landsat, levels = c("MSS", "TM/ETM+", "OLI"))

### Multi-year trends ###
cc_trends_tchange = read.csv("cc_pif_trends1_segmenttotalchange.csv", check.names = FALSE) # Already x100 (%)
cc_trends_slope = read.csv("cc_pif_trends1_segmentslope.csv", check.names = FALSE)

# Impervious sites
cc_trends_tchange_imp = subset(cc_trends_tchange, SiteType == "Impervious")
cc_trends_tchange_imp1 = gather(cc_trends_tchange_imp, Year, CC, '1974':'2020')
cc_trends_tchange_imp1$Year = as.numeric(cc_trends_tchange_imp1$Year)
cc_trends_tchange_imp1 = cc_trends_tchange_imp1 %>% mutate(Landsat = 
                               case_when(Year <= 1983 ~ "MSS",
                                         Year <= 2012 ~ "TM/ETM+",
                                         Year <= 2020 ~ "OLI"))

cc_trends_slope_imp = subset(cc_trends_slope, SiteType == "Impervious")
cc_trends_slope_imp1 = gather(cc_trends_slope_imp, Year, CC, '1974':'2020')
cc_trends_slope_imp1$Year = as.numeric(cc_trends_slope_imp1$Year)
cc_trends_slope_imp1 = cc_trends_slope_imp1 %>% mutate(Landsat = 
                                                             case_when(Year <= 1983 ~ "MSS",
                                                                       Year <= 2012 ~ "TM/ETM+",
                                                                       Year <= 2020 ~ "OLI"))

# Agriculture sites
cc_trends_tchange_agr = subset(cc_trends_tchange, SiteType == "Agriculture")
cc_trends_tchange_agr1 = gather(cc_trends_tchange_agr, Year, CC, '1974':'2020')
cc_trends_tchange_agr1$Year = as.numeric(cc_trends_tchange_agr1$Year)
cc_trends_tchange_agr1 = cc_trends_tchange_agr1 %>% mutate(Landsat = 
                                                             case_when(Year <= 1983 ~ "MSS",
                                                                       Year <= 2012 ~ "TM/ETM+",
                                                                       Year <= 2020 ~ "OLI"))

cc_trends_slope_agr = subset(cc_trends_slope, SiteType == "Agriculture")
cc_trends_slope_agr1 = gather(cc_trends_slope_agr, Year, CC, '1974':'2020')
cc_trends_slope_agr1$Year = as.numeric(cc_trends_slope_agr1$Year)
cc_trends_slope_agr1 = cc_trends_slope_agr1 %>% mutate(Landsat = 
                                                         case_when(Year <= 1983 ~ "MSS",
                                                                   Year <= 2012 ~ "TM/ETM+",
                                                                   Year <= 2020 ~ "OLI"))
#####

##### CC through time #####
### Impervious ###
median(cc_imp1$CC) # Median CC (1972-2020) = 1.1%
median(subset(cc_imp1$CC, cc_imp1$Year < 1984)) # Median MSS CC (1972-1983) = 1.3%
median(subset(cc_imp1$CC, cc_imp1$Year > 1983 & cc_imp1$Year < 2013)) # Median TM/ETM+ CC (1984-2012) = 1.0%
median(subset(cc_imp1$CC, cc_imp1$Year > 2012)) # Median OLI CC (2013-2020) = 1.0%

# % of sites misclassifed as 10+% CC
(nrow(subset(cc_imp1, cc_imp1$CC >= 10)) / nrow(cc_imp1)) * 100 # 0 out of 2880 observations misclassifed overall

# Slope of medians
cc_imp_medians = cc_imp1 %>%
  group_by(Year) %>%
  summarize(Median = median(CC))
plot(cc_imp_medians$Year, cc_imp_medians$Median) # ~1.4 in 1972 to 1 by 2020 (downward slope)
summary(lm(cc_imp_medians$Median ~ cc_imp_medians$Year)) # R2 = 0.70***, -0.008% per Year

cc_imp.p = ggplot(cc_imp1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 10, lty = 2, lwd = 1, color = "gray48") +
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(name  = "CC (%)", expand = c(0, 0.5), limits = c(0, 40), breaks = seq(0, 40, 10)) +
  ggtitle(label = "Impervious PIFs") +
  annotate(geom = "text", x = 1975, y = 36.5, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0), "cm"),
        legend.position = c(0.8, 0.8),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
cc_imp.p

### Agriculture ###
median(cc_agr1$CC) # Median CC (1972-2020) = 2.4%
median(subset(cc_agr1$CC, cc_agr1$Year < 1984)) # Median MSS CC (1972-1983) = 2.8%
median(subset(cc_agr1$CC, cc_agr1$Year > 1983 & cc_agr1$Year < 2013)) # Median TM/ETM+ CC (1984-2012) = 2.4%
median(subset(cc_agr1$CC, cc_agr1$Year > 2012)) # Median OLI CC (2013-2020) = 1.7%

# % of sites misclassifed as 10+% CC
(nrow(subset(cc_agr1, cc_agr1$CC >= 10)) / nrow(cc_agr1)) * 100 # 3.2% overall (2/60 sites misclassified each year on average)
(nrow(subset(cc_agr1, cc_agr1$CC >= 10 & cc_agr1$Year < 1984)) / nrow(subset(cc_agr1, cc_agr1$Year < 1984))) * 100 # 4.8% during MSS
(nrow(subset(cc_agr1, cc_agr1$CC >= 10 & cc_agr1$Year > 1983 &  cc_agr1$Year < 2013)) / nrow(subset(cc_agr1, cc_agr1$Year > 1983 &  cc_agr1$Year < 2013))) * 100 
# 2.7% during TM/ETM+
(nrow(subset(cc_agr1, cc_agr1$CC >= 10 & cc_agr1$Year > 2012)) / nrow(subset(cc_agr1, cc_agr1$Year > 2012))) * 100 # 2.9% during OLI

# Slope of medians
cc_agr_medians = cc_agr1 %>%
  group_by(Year) %>%
  summarize(Median = median(CC))
plot(cc_agr_medians$Year, cc_agr_medians$Median) # ~3.5 in early 1970s to ~1.5 by 2019 (downward slope)
summary(lm(cc_agr_medians$Median ~ cc_agr_medians$Year)) # R2 = 0.71***, -0.026% per Year

cc_agr.p = ggplot(cc_agr1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 10, lty = 2, lwd = 1, color = "gray48") +
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(expand = c(0, 0.5), limits = c(0, 40), breaks = seq(0, 40, 10)) +
  ggtitle(label = "Agriculture PIFs") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"),
        legend.position = "none",
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
cc_agr.p
#####
cc_imp.p | cc_agr.p

##### CC year-to-year change through time #####
### Impervious ###
cc_imp_change = cc_imp
cc_imp_change$`1972`= NULL # Remove 1972 since we are doing year-to-year change
cc_imp_change[,4:50] = NA # Set years column to NA
cc_imp_change[4:50] = cc_imp[5:51] - cc_imp[4:50] # Creates year-to-year difference table
cc_imp_change1 = gather(cc_imp_change, Year, CC, '1974':'2020') # Convert to long format
cc_imp_change1$Year = as.numeric(cc_imp_change1$Year) # Convert Year column from chr to num
cc_imp_change1$CC = cc_imp_change1$CC * 100 # Multiply CC by 100 to get %
cc_imp_change1 = cc_imp_change1 %>% mutate(Landsat = 
                               case_when(Year <= 1983 ~ "MSS",
                                         Year <= 2012 ~ "TM/ETM+",
                                         Year <= 2020 ~ "OLI")) # Add Landsat sensor column depending on years
cc_imp_change1$Landsat = factor(cc_imp_change1$Landsat, levels = c("MSS", "TM/ETM+", "OLI")) # Reorder sensors for plotting

# ID signal-noise border (90th percentile) - positive change
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC > 0), 0.9) # Overall = 0.35%
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC > 0 & cc_imp_change1$Year < 1984), 0.9) # MSS = 0.49%
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC > 0 & cc_imp_change1$Year > 1983 & cc_imp_change1$Year < 2013), 0.9) # TM/ETM+ = 0.32%
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC > 0 & cc_imp_change1$Year > 2012), 0.9) # OLI = 0.33%

# ID signal-noise border (90th percentile) - negative change
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC < 0), 0.1) # Overall = -0.40%
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC < 0 & cc_imp_change1$Year < 1984), 0.1) # MSS = -0.58%
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC < 0 & cc_imp_change1$Year > 1983 & cc_imp_change1$Year < 2013), 0.1) # TM/ETM+ =  -0.38%
quantile(subset(cc_imp_change1$CC, cc_imp_change1$CC < 0 & cc_imp_change1$Year > 2012), 0.1) # OLI = -0.29%

# ID signal-noise border (90th percentile) - absolute change
cc_imp_change1$CCa = abs(cc_imp_change1$CC)

quantile(cc_imp_change1$CCa, 0.9) # Overall = 0.36%
quantile(subset(cc_imp_change1$CCa, cc_imp_change1$Year < 1984), 0.9) # MSS = 0.50%
quantile(subset(cc_imp_change1$CCa, cc_imp_change1$Year > 1983 & cc_imp_change1$Year < 2013), 0.9) # TM/ETM+ = 0.34%
quantile(subset(cc_imp_change1$CCa, cc_imp_change1$Year > 2012), 0.9) # OLI = 0.31%

cc_imp_change.p = ggplot(cc_imp_change1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 0.348712, lwd = 1, lty = 2, color = "gray48") + # 90th percentile positive
  geom_hline(yintercept = -0.400756, lwd = 1, lty = 2, color = "gray48") + # 90th percentile negative
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(name  = "Year-to-year CC Change (%/yr)", expand = c(0, 0), breaks = seq(-5, 5, 1)) + 
  coord_cartesian(ylim = c(-5.5, 5.5)) + # Zoom in (misses some outlier changes)
  annotate(geom = "text", x = 1975, y = 4.5, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        legend.position = "none")
cc_imp_change.p

### Agriculture ###
cc_agr_change = cc_agr
cc_agr_change$`1972`= NULL # Remove 1972 since we are doing year-to-year change
cc_agr_change[,4:50] = NA # Set years column to NA
cc_agr_change[4:50] = cc_agr[5:51] - cc_agr[4:50] # Creates year-to-year difference table
cc_agr_change1 = gather(cc_agr_change, Year, CC, '1974':'2020') # Convert to long format
cc_agr_change1$Year = as.numeric(cc_agr_change1$Year) # Convert Year column from chr to num
cc_agr_change1$CC = cc_agr_change1$CC * 100 # Multiply CC by 100 to get %
cc_agr_change1 = cc_agr_change1 %>% mutate(Landsat = 
                                             case_when(Year <= 1983 ~ "MSS",
                                                       Year <= 2012 ~ "TM/ETM+",
                                                       Year <= 2020 ~ "OLI")) # Add Landsat sensor column depending on years
cc_agr_change1$Landsat = factor(cc_agr_change1$Landsat, levels = c("MSS", "TM/ETM+", "OLI")) # Reorder sensors for plotting

# ID signal-noise border (nth percentile) - positive change
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC > 0), 0.9) # Overall = 1.22%
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC > 0 & cc_agr_change1$Year < 1984), 0.9) # MSS = 0.92%
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC > 0 & cc_agr_change1$Year > 1983 & cc_agr_change1$Year < 2013), 0.9) # TM/ETM+ = 1.21%
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC > 0 & cc_agr_change1$Year > 2012), 0.9) # OLI = 1.77%

# ID signal-noise border (nth percentile) - negative change
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC < 0), 0.1) # -1.31%
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC < 0 & cc_agr_change1$Year < 1984), 0.1) # MSS = -1.47%
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC < 0 & cc_agr_change1$Year > 1983 & cc_agr_change1$Year < 2013), 0.1) # TM/ETM+ = -1.15%
quantile(subset(cc_agr_change1$CC, cc_agr_change1$CC < 0 & cc_agr_change1$Year > 2012), 0.1) # OLI = -1.42%

# ID signal-noise border (nth percentile) - absolute change
cc_agr_change1$CCa = abs(cc_agr_change1$CC)

quantile(cc_agr_change1$CCa, 0.9) # Overall = 1.27%
quantile(subset(cc_agr_change1$CCa, cc_agr_change1$Year < 1984), 0.9) # MSS = 1.15%
quantile(subset(cc_agr_change1$CCa, cc_agr_change1$Year > 1983 & cc_agr_change1$Year < 2013), 0.9) # TM/ETM+ = 1.19%
quantile(subset(cc_agr_change1$CCa, cc_agr_change1$Year > 2012), 0.9) # OLI = 1.59%

cc_agr_change.p = ggplot(cc_agr_change1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 1.217257 , lwd = 1, lty = 2, color = "gray48") + # 90th percentile positive
  geom_hline(yintercept = -1.311754, lwd = 1, lty = 2, color = "gray48") + # 90th percentile negative
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(name  = "CC (%)", expand = c(0, 0), breaks = seq(-5, 5, 1)) + 
  coord_cartesian(ylim = c(-5.5, 5.5)) + # Zoom in (misses some outlier changes)
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        legend.position = "none")
cc_agr_change.p

# Overall 90th percentile - absolute change
agr_imp_change = rbind(cc_imp_change1, cc_agr_change1)
quantile(agr_imp_change$CCa, 0.9) # Overall - 0.76%
quantile(subset(agr_imp_change$CCa, agr_imp_change$Year < 1984), 0.9) # MSS = 0.83%
quantile(subset(agr_imp_change$CCa, agr_imp_change$Year > 1983 & agr_imp_change$Year < 2013), 0.9) # TM/ETM+ = 0.69%
quantile(subset(agr_imp_change$CCa, agr_imp_change$Year > 2012), 0.9) # OLI = 0.95%

#Signal-noise border: 1% CC/year (round up from overall 90th percentile)
# % of PIF year-to-year changes above 1% CC/year
(nrow(subset(cc_agr_change1, CCa >= 1)) / nrow(cc_agr_change1)) * 100 # 13.1%
(nrow(subset(cc_imp_change1, CCa >= 1)) / nrow(cc_imp_change1)) * 100 # 0.96%
(nrow(subset(agr_imp_change, CCa >= 1)) / nrow(agr_imp_change)) * 100 # 7.0% of year-to-year CC changes across all PIFs was at least 1%

#####
cc_imp_change.p | cc_agr_change.p

# Final Figure 1
tiff("PIFvalidation_pt1.tiff", units = "in", width = 6.5, height = 6.5, res = 300)
(cc_imp.p | cc_agr.p) / 
  (cc_imp_change.p | cc_agr_change.p)
dev.off()

##### CC multi-year trends through time (total CC change) #####
### Impervious ###
# ID signal-noise border (90th percentile) - positive change
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC > 0), 0.9) # Overall = 0.96%
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC > 0 & cc_trends_tchange_imp1$Year < 1984), 0.9) # MSS = 0.91%
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC > 0 & cc_trends_tchange_imp1$Year > 1983 & cc_trends_tchange_imp1$Year < 2013), 0.9)
# TM/ETM+ = 0.96%
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC > 0 & cc_trends_tchange_imp1$Year > 2012), 0.9) # OLI = 0.98%

# ID signal-noise border (90th percentile) - negative change
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC < 0), 0.1) # Overall = -1.10%
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC < 0 & cc_trends_tchange_imp1$Year < 1984), 0.1) # MSS = -1.26%
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC < 0 & cc_trends_tchange_imp1$Year > 1983 & cc_trends_tchange_imp1$Year < 2013), 0.1) 
# TM/ETM+ =  -0.83%
quantile(subset(cc_trends_tchange_imp1$CC, cc_trends_tchange_imp1$CC < 0 & cc_trends_tchange_imp1$Year > 2012), 0.1) # OLI = -0.83%

# ID signal-noise border (90th percentile) - absolute change
cc_trends_tchange_imp1$CCa = abs(cc_trends_tchange_imp1$CC)

quantile(cc_trends_tchange_imp1$CCa, 0.9) # Overall = 1.03%
quantile(subset(cc_trends_tchange_imp1$CCa, cc_trends_tchange_imp1$Year < 1984), 0.9) # MSS = 1.15%
quantile(subset(cc_trends_tchange_imp1$CCa, cc_trends_tchange_imp1$Year > 1983 & cc_trends_tchange_imp1$Year < 2013), 0.9) # TM/ETM+ = 1.03%
quantile(subset(cc_trends_tchange_imp1$CCa, cc_trends_tchange_imp1$Year > 2012), 0.9) # OLI = 0.84%

cc_imp_tchange.p = ggplot(cc_trends_tchange_imp1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 0.96, lwd = 1, lty = 2, color = "gray48") + # 90th percentile positive
  geom_hline(yintercept = -1.104, lwd = 1, lty = 2, color = "gray48") + # 90th percentile negative
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(name  = "Total change of CC segments (%)", expand = c(0, 0.5), limits = c(-7, 7), breaks = seq(-6, 6, 2)) +
  ggtitle(label = "Impervious PIFs") +
  annotate(geom = "text", x = 1975, y = 6, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0), "cm"),
        legend.position = c(0.8, 0.8),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
cc_imp_tchange.p

### Agriculture ###
# ID signal-noise border (90th percentile) - positive change
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC > 0), 0.9) # Overall = 3.12%
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC > 0 & cc_trends_tchange_agr1$Year < 1984), 0.9) # MSS = 3.02%
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC > 0 & cc_trends_tchange_agr1$Year > 1983 & cc_trends_tchange_agr1$Year < 2013), 0.9)
# TM/ETM+ = 3.23%
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC > 0 & cc_trends_tchange_agr1$Year > 2012), 0.9) # OLI = 3.30%

# ID signal-noise border (90th percentile) - negative change
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC < 0), 0.1) # Overall = -3.89%
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC < 0 & cc_trends_tchange_agr1$Year < 1984), 0.1) # MSS = -3.21%
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC < 0 & cc_trends_tchange_agr1$Year > 1983 & cc_trends_tchange_agr1$Year < 2013), 0.1) 
# TM/ETM+ =  -4.08%
quantile(subset(cc_trends_tchange_agr1$CC, cc_trends_tchange_agr1$CC < 0 & cc_trends_tchange_agr1$Year > 2012), 0.1) # OLI = -4.09%

# ID signal-noise border (90th, 99th percentile) - absolute change
cc_trends_tchange_agr1$CCa = abs(cc_trends_tchange_agr1$CC)

quantile(cc_trends_tchange_agr1$CCa, 0.9) # Overall = 3.50%
quantile(subset(cc_trends_tchange_agr1$CCa, cc_trends_tchange_agr1$Year < 1984), 0.9) # MSS = 3.21%
quantile(subset(cc_trends_tchange_agr1$CCa, cc_trends_tchange_agr1$Year > 1983 & cc_trends_tchange_agr1$Year < 2013), 0.9) # TM/ETM+ = 3.61%
quantile(subset(cc_trends_tchange_agr1$CCa, cc_trends_tchange_agr1$Year > 2012), 0.9) # OLI = 3.89%

cc_agr_tchange.p = ggplot(cc_trends_tchange_agr1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 3.12, lwd = 1, lty = 2, color = "gray48") + # 90th percentile positive
  geom_hline(yintercept = -3.89, lwd = 1, lty = 2, color = "gray48") + # 90th percentile negative
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(expand = c(0, 0.5), limits = c(-7, 7), breaks = seq(-6, 6, 2)) +
  ggtitle(label = "Agriculture PIFs") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"),
        legend.position = "none",
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
cc_agr_tchange.p

# Overall 90th percentile - absolute change
agr_imp_tchange = rbind(cc_trends_tchange_agr1, cc_trends_tchange_imp1)
quantile(agr_imp_tchange$CCa, 0.9) # Overall - 2.72%
quantile(subset(agr_imp_tchange$CCa, agr_imp_tchange$Year < 1984), 0.9) # MSS = 2.54%
quantile(subset(agr_imp_tchange$CCa, agr_imp_tchange$Year > 1983 & agr_imp_tchange$Year < 2013), 0.9) # TM/ETM+ = 2.78%
quantile(subset(agr_imp_tchange$CCa, agr_imp_tchange$Year > 2012), 0.9) # OLI = 2.97%

#Signal-noise border: segments with a total change at least 3% CC (round up from overall 90th percentile)
# % of PIF segments that change at least 3%
(nrow(subset(cc_trends_tchange_agr1, CCa >= 3)) / nrow(cc_trends_tchange_agr1)) * 100 # 14.8%
(nrow(subset(cc_trends_tchange_imp1, CCa >= 3)) / nrow(cc_trends_tchange_imp1)) * 100 # 0.57%
(nrow(subset(agr_imp_tchange, CCa >= 3)) / nrow(agr_imp_tchange)) * 100 # 7.7% of segments had a total change of at least 3%
#####
cc_imp_tchange.p | cc_agr_tchange.p

##### CC multi-year trends through time (slope) #####
### Impervious ###
# ID signal-noise border (90th percentile) - positive change
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC > 0), 0.9) # Overall = 0.16%
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC > 0 & cc_trends_slope_imp1$Year < 1984), 0.9) # MSS = 0.22%
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC > 0 & cc_trends_slope_imp1$Year > 1983 & cc_trends_slope_imp1$Year < 2013), 0.9)
# TM/ETM+ = 0.14%
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC > 0 & cc_trends_slope_imp1$Year > 2012), 0.9) # OLI = 0.20%

# ID signal-noise border (90th percentile) - negative change
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC < 0), 0.1) # Overall = -0.26%
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC < 0 & cc_trends_slope_imp1$Year < 1984), 0.1) # MSS = -32%
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC < 0 & cc_trends_slope_imp1$Year > 1983 & cc_trends_slope_imp1$Year < 2013), 0.1) 
# TM/ETM+ =  -0.24%
quantile(subset(cc_trends_slope_imp1$CC, cc_trends_slope_imp1$CC < 0 & cc_trends_slope_imp1$Year > 2012), 0.1) # OLI = -0.22%

# ID signal-noise border (90th percentile) - absolute change
cc_trends_slope_imp1$CCa = abs(cc_trends_slope_imp1$CC)

quantile(cc_trends_slope_imp1$CCa, 0.9) # Overall = 0.22%
quantile(subset(cc_trends_slope_imp1$CCa, cc_trends_slope_imp1$Year < 1984), 0.9) # MSS = 0.27%
quantile(subset(cc_trends_slope_imp1$CCa, cc_trends_slope_imp1$Year > 1983 & cc_trends_slope_imp1$Year < 2013), 0.9) # TM/ETM+ = 0.19%
quantile(subset(cc_trends_slope_imp1$CCa, cc_trends_slope_imp1$Year > 2012), 0.9) # OLI = 0.19%

cc_imp_slope.p = ggplot(cc_trends_slope_imp1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 0.1633, lwd = 1, lty = 2, color = "gray48") + # 90th percentile positive
  geom_hline(yintercept = -0.26, lwd = 1, lty = 2, color = "gray48") + # 90th percentile negative
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(name  = "CC segment slope (%/yr)", expand = c(0, 0), breaks = seq(-3, 3, 1)) + 
  coord_cartesian(ylim = c(-3.5, 3.5)) + # Zoom in (misses some outlier changes)
  annotate(geom = "text", x = 1975, y = 2.7, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        legend.position = "none")
cc_imp_slope.p

### Agriculture ###
# ID signal-noise border (90th percentile) - positive change
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC > 0), 0.9) # Overall = 0.62%
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC > 0 & cc_trends_slope_agr1$Year < 1984), 0.9) # MSS = 0.50%
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC > 0 & cc_trends_slope_agr1$Year > 1983 & cc_trends_slope_agr1$Year < 2013), 0.9)
# TM/ETM+ = 0.57%
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC > 0 & cc_trends_slope_agr1$Year > 2012), 0.9) # OLI = 0.79%

# ID signal-noise border (90th percentile) - negative change
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC < 0), 0.1) # Overall = -0.56%
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC < 0 & cc_trends_slope_agr1$Year < 1984), 0.1) # MSS = -0.46%
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC < 0 & cc_trends_slope_agr1$Year > 1983 & cc_trends_slope_agr1$Year < 2013), 0.1) 
# TM/ETM+ =  -0.58%
quantile(subset(cc_trends_slope_agr1$CC, cc_trends_slope_agr1$CC < 0 & cc_trends_slope_agr1$Year > 2012), 0.1) # OLI = -0.64%

# ID signal-noise border (90th, 99th percentile) - absolute change
cc_trends_slope_agr1$CCa = abs(cc_trends_slope_agr1$CC)

quantile(cc_trends_slope_agr1$CCa, 0.9) # Overall = 0.59%
quantile(subset(cc_trends_slope_agr1$CCa, cc_trends_slope_agr1$Year < 1984), 0.9) # MSS = 0.47%
quantile(subset(cc_trends_slope_agr1$CCa, cc_trends_slope_agr1$Year > 1983 & cc_trends_slope_agr1$Year < 2013), 0.9) # TM/ETM+ = 0.57%
quantile(subset(cc_trends_slope_agr1$CCa, cc_trends_slope_agr1$Year > 2012), 0.9) # OLI = 0.74%

cc_agr_slope.p = ggplot(cc_trends_slope_agr1) + 
  geom_hline(yintercept = 0, lwd = 1, color = "gray48") +
  geom_hline(yintercept = 0.615, lwd = 1, lty = 2, color = "gray48") + # 90th percentile positive
  geom_hline(yintercept = -0.5628, lwd = 1, lty = 2, color = "gray48") + # 90th percentile negative
  geom_boxplot(aes(x = Year, y = CC, group = Year, fill = Landsat), outlier.size = 0.5, outlier.shape = 16, size = 0.25) +
  scale_x_continuous(expand = c(0, 0), limits = c(1971.35, 2020.65)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-3, 3, 1)) + 
  coord_cartesian(ylim = c(-3.5, 3.5)) + # Zoom in (misses some outlier changes)
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        legend.position = "none")
cc_agr_slope.p

# Overall 90th percentile - absolute change
agr_imp_slope = rbind(cc_trends_slope_agr1, cc_trends_slope_imp1)
quantile(agr_imp_slope$CCa, 0.9) # Overall - 0.43%
quantile(subset(agr_imp_slope$CCa, agr_imp_slope$Year < 1984), 0.9) # MSS = 0.39%
quantile(subset(agr_imp_slope$CCa, agr_imp_slope$Year > 1983 & agr_imp_slope$Year < 2013), 0.9) # TM/ETM+ = 0.37%
quantile(subset(agr_imp_slope$CCa, agr_imp_slope$Year > 2012), 0.9) # OLI = 0.59%

#Signal-noise border: segments with a CC slope of at least 0.5% CC per year (round up from overall 90th percentile)
# % of PIF segments with a slope of at least 0.5%
(nrow(subset(cc_trends_slope_agr1, CCa >= 0.5)) / nrow(cc_trends_slope_agr1)) * 100 # 13.7%
(nrow(subset(cc_trends_slope_imp1, CCa >= 0.5)) / nrow(cc_trends_slope_imp1)) * 100 # 2.52%
(nrow(subset(agr_imp_slope, CCa >= 0.5)) / nrow(agr_imp_slope)) * 100 # 8.1% of segments had a CC slope of at least 0.5% per year
#####
cc_imp_slope.p | cc_agr_slope.p

# Final Figure 2
tiff("PIFvalidation_pt2.tiff", units = "in", width = 6.5, height = 6.5, res = 300)
(cc_imp_tchange.p | cc_agr_tchange.p) / 
  (cc_imp_slope.p | cc_agr_slope.p)
dev.off()