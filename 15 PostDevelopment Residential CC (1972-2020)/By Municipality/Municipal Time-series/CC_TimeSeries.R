##### This code creates a figure of average CCr through time and by post-development year for all Peel DAs #####

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

library(ggplot2)
library(patchwork)

##### Bring in Data #####
### Base DA Data ###
da = read.csv("DA_data1.csv")

### Canopy Cover Data ###
cc_postdev = read.csv("cc_postdev.csv", check.names = FALSE) # CC by year (NA before transition)
cc_dev0 = read.csv("Dev0cc.csv", check.names = FALSE) # CC by post-development year (NA after time-series ends)
#cc_dev0n = read.csv("Dev0cc_norm0.csv", check.names = FALSE) # CC by post-development year, all years subtracted by year 0 (NA after time-series ends)
#####

##### Create population tables for weighting #####
### Bring in population data from 2016 census ###
pop2016 = read.csv("Pop2016.csv")
pop2016 = subset(pop2016, DAUID %in% da$DAUID) # Reduce to just 1600 included DAs
da$Population = pop2016$Pop2016

### Create population tables through time ###
cc_postdev_pop = cc_postdev
cc_postdev_pop[,2:50] = pop2016$Pop2016
cc_postdev_pop[is.na(cc_postdev)] = NA

cc_dev0_pop = cc_dev0
cc_dev0_pop[,2:50] = pop2016$Pop2016
cc_dev0_pop[is.na(cc_dev0)] = NA
#####

##### Subset cc_dev0 to only DAs with a transition year #####
### Add DevEnd_Hum to dev0 tables ###
# For subsetting in relation to census later (since we know what year "Year 0" is)
cc_dev0$DevEnd_Hum = da$DevEnd_Hum
#cc_dev0n$DevEnd_Hum = da$DevEnd_Hum
cc_dev0$Municipality = da$Municipality
cc_dev0$Pop = da$Population

cc_dev0 = cc_dev0[!is.na(cc_dev0$DevEnd_Hum),] # Remove DAs with no transition year (set by human)                          

cc_dev0_pop = subset(cc_dev0_pop, DAUID %in% cc_dev0$DAUID)
#####

##### Plot values #####
# Municipality colors
# Mississauga: blue
# Brampton: red3
# Caledon: darkgreen

setna = 7500 # If Population less than this (inc. through time), remove from plot

max_ss = sum(da$Population) # Max sample size (for plotting)

maxcc = 35 # Top CC value for all plots (0 always bottom)

# Plot values
alpha = 0.6
big_ln = 1.5
axt_sz = 10
axn_sz = 8
#####

##### postdev residential CC through time (1972 - 2020) #####
### Build subset data ###
cc_postdev_mn = data.frame(matrix(nrow = ncol(cc_postdev) - 1, ncol = 8))
colnames(cc_postdev_mn) = c("Year", "CC_m", "CC_b", "CC_c", "Pop", "Pop_m", "Pop_b", "Pop_c")

# Year or Year since Transition
cc_postdev_mn$Year = as.numeric(colnames(cc_postdev[2:ncol(cc_postdev)]))

# Mean by municipality, weighted by population
for(i in 1:nrow(cc_postdev_mn)) { 
  cc_postdev_mn[i,2] = weighted.mean(subset(cc_postdev[,i+1], da$Municipality == "Mississauga"), 
                               subset(da$Population, da$Municipality == "Mississauga"), na.rm = TRUE) 
}
for(i in 1:nrow(cc_postdev_mn)) { 
  cc_postdev_mn[i,3] = weighted.mean(subset(cc_postdev[,i+1], da$Municipality == "Brampton"), 
                               subset(da$Population, da$Municipality == "Brampton"), na.rm = TRUE)
}
for(i in 1:nrow(cc_postdev_mn)) { 
  cc_postdev_mn[i,4] = weighted.mean(subset(cc_postdev[,i+1], da$Municipality == "Caledon"), 
                               subset(da$Population, da$Municipality == "Caledon"), na.rm = TRUE)
}

# Population overall and for each municipality through time
cc_postdev_mn$Pop = colSums(cc_postdev_pop[2:ncol(cc_postdev_pop)], na.rm = TRUE)
cc_postdev_mn$Pop_m = colSums((subset(cc_postdev_pop[2:ncol(cc_postdev_pop)], da$Municipality == "Mississauga")), na.rm = TRUE)
cc_postdev_mn$Pop_b = colSums((subset(cc_postdev_pop[2:ncol(cc_postdev_pop)], da$Municipality == "Brampton")), na.rm = TRUE)
cc_postdev_mn$Pop_c = colSums((subset(cc_postdev_pop[2:ncol(cc_postdev_pop)], da$Municipality == "Caledon")), na.rm = TRUE)

# If Population < setna for any municipality, set to NA
for (i in 1:nrow(cc_postdev_mn)) {
  if(cc_postdev_mn$Pop_m[i] < setna) {
    cc_postdev_mn$CC_m[i] = NA # Mississauga
  }
  if(cc_postdev_mn$Pop_b[i] < setna) {
    cc_postdev_mn$CC_b[i] = NA # Brampton
  }
  if(cc_postdev_mn$Pop_c[i] < setna) {
    cc_postdev_mn$CC_c[i] = NA # Caledon
  }
}

### Build Plot ###
cc_postdev.p = ggplot(data = cc_postdev_mn) +
  geom_area(aes(x = Year, y = Pop_c / max_ss * maxcc + (Pop_m / max_ss * maxcc) + (Pop_b / max_ss * maxcc)), fill = "darkgreen") +
  geom_area(aes(x = Year, y = Pop_b / max_ss * maxcc + (Pop_m / max_ss * maxcc)), fill = "red3") +
  geom_area(aes(x = Year, y = Pop_m / max_ss * maxcc), fill = "blue") +
  geom_area(aes(x = Year, y = Pop / max_ss * maxcc), fill = "gray90", alpha = alpha) + 
  #geom_vline(xintercept = 1971, col = "gray48") + # Census years
  #geom_vline(xintercept = 1981, col = "gray48") + 
  #geom_vline(xintercept = 1986, col = "gray48") + 
  #geom_vline(xintercept = 1991, col = "gray48") +
  #geom_vline(xintercept = 1996, col = "gray48") + 
  #geom_vline(xintercept = 2001, col = "gray48") + 
  #geom_vline(xintercept = 2006, col = "gray48") + 
  #geom_vline(xintercept = 2011, col = "gray48", linetype = "dashed") + 
  #geom_vline(xintercept = 2016, col = "gray48") + 
  geom_line(aes(x = Year, y = CC_m), color = "blue", size = big_ln) +
  geom_line(aes(x = Year, y = CC_b), color = "red3", size = big_ln) +
  geom_line(aes(x = Year, y = CC_c), color = "darkgreen", size = big_ln) +
  annotate(geom = "text", x = 1975, y = 32, label = "A", fontface = "bold", size = 6) +
  scale_x_continuous(limits = c(1972, 2020), expand = c(0,0), name = "Year") +
  scale_y_continuous(name = "Residential CC (%, population weighted)", expand = c(0,0), limits = c(0,maxcc)) +
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_text(size = axt_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none",
        plot.margin = margin(0, 9, 0, 0))
cc_postdev.p
#####

##### postdev residential CC since transition year (0 to 48) #####
### Build subset data ###
cc_dev0_mn = data.frame(matrix(nrow = ncol(cc_dev0) - 4, ncol = 8))
colnames(cc_dev0_mn) = c("Year", "CC_m", "CC_b", "CC_c", "Pop", "Pop_m", "Pop_b", "Pop_c")

# Year or Year since Transition
cc_dev0_mn$Year = as.numeric(colnames(cc_dev0[2:50]))

# Mean by municipality, weighted by population
for(i in 1:nrow(cc_dev0_mn)) { 
  cc_dev0_mn[i,2] = weighted.mean(subset(cc_dev0[,i+1], cc_dev0$Municipality == "Mississauga"), 
                                 subset(cc_dev0$Pop, cc_dev0$Municipality == "Mississauga"), na.rm = TRUE)
}
for(i in 1:nrow(cc_dev0_mn)) { 
  cc_dev0_mn[i,3] = weighted.mean(subset(cc_dev0[,i+1], cc_dev0$Municipality == "Brampton"), 
                                 subset(cc_dev0$Pop, cc_dev0$Municipality == "Brampton"), na.rm = TRUE)
}
for(i in 1:nrow(cc_dev0_mn)) { 
  cc_dev0_mn[i,4] = weighted.mean(subset(cc_dev0[,i+1], cc_dev0$Municipality == "Caledon"), 
                                 subset(cc_dev0$Pop, cc_dev0$Municipality == "Caledon"), na.rm = TRUE)
}

# Population overall and for each municipality through time
cc_dev0_mn$Pop = colSums(cc_dev0_pop[2:ncol(cc_dev0_pop)], na.rm = TRUE)
cc_dev0_mn$Pop_m = colSums((subset(cc_dev0_pop[2:ncol(cc_dev0_pop)], cc_dev0$Municipality == "Mississauga")), na.rm = TRUE)
cc_dev0_mn$Pop_b = colSums((subset(cc_dev0_pop[2:ncol(cc_dev0_pop)], cc_dev0$Municipality == "Brampton")), na.rm = TRUE)
cc_dev0_mn$Pop_c = colSums((subset(cc_dev0_pop[2:ncol(cc_dev0_pop)], cc_dev0$Municipality == "Caledon")), na.rm = TRUE)

# If Population < setna for any municipality, set to NA
for (i in 1:nrow(cc_dev0_mn)) {
  if(cc_dev0_mn$Pop_m[i] < setna) {
    cc_dev0_mn$CC_m[i] = NA # Mississauga
  }
  if(cc_dev0_mn$Pop_b[i] < setna) {
    cc_dev0_mn$CC_b[i] = NA # Brampton
  }
  if(cc_dev0_mn$Pop_c[i] < setna) {
    cc_dev0_mn$CC_c[i] = NA # Caledon
  }
}

### Build Plot ###
cc_dev0.p = ggplot(data = cc_dev0_mn) +
  geom_area(aes(x = Year, y = Pop_c / max_ss * maxcc + (Pop_m / max_ss * maxcc) + (Pop_b / max_ss * maxcc)), fill = "darkgreen") +
  geom_area(aes(x = Year, y = Pop_b / max_ss * maxcc + (Pop_m / max_ss * maxcc)), fill = "red3") +
  geom_area(aes(x = Year, y = Pop_m / max_ss * maxcc), fill = "blue") +
  geom_area(aes(x = Year, y = Pop / max_ss * maxcc), fill = "gray90", alpha = alpha) + 
  geom_line(aes(x = Year, y = CC_m), color = "blue", size = big_ln) +
  geom_line(aes(x = Year, y = CC_b), color = "red3", size = big_ln) +
  geom_line(aes(x = Year, y = CC_c), color = "darkgreen", size = big_ln) +
  scale_x_continuous(expand = c(0,0), limits = c(0,46), name = "Years since Transition") +
  scale_y_continuous(expand = c(0,0), limits = c(0,maxcc)) +
  annotate(geom = "text", x = 4, y = 32, label = "B", fontface = "bold", size = 6) +
  theme(axis.title.x = element_text(size = axt_sz, color = "black"),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")
cc_dev0.p
#####

tiff("CC_mun_transitionfix2.tif", units = "cm", width = 16.5, height = 8, res = 300)
cc_postdev.p | cc_dev0.p
dev.off()

#summary(lm(subset(cc_dev0_mn$CC_c, cc_dev0_mn$Year < 35)~ subset(cc_dev0_mn$Year, cc_dev0_mn$Year < 35)))
#summary(lm(subset(cc_dev0_mn$CC_m, cc_dev0_mn$Year < 42)~ subset(cc_dev0_mn$Year, cc_dev0_mn$Year < 42)))
summary(lm(subset(cc_dev0_mn$CC_b, cc_dev0_mn$Year < 37)~ subset(cc_dev0_mn$Year, cc_dev0_mn$Year < 37)))
