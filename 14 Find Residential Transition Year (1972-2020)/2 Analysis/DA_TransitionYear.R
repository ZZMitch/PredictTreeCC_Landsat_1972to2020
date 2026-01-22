##### This code creates density plots of DA transition years + older DAs (transition before 1972) #####

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change")

library(ggplot2)
library(patchwork)

axt_sz = 10
axn_sz = 8
eq_sz = 3

# Municipality colors
# Mississauga: blue
# Brampton: red3
# Caledon: darkgreen


##### Bring in Data #####
### Base DA Data ###
da = read.csv("DA_data1.csv")
da$Municipality = factor(da$Municipality, levels = c("Caledon", "Brampton", "Mississauga"))

### Bring in population data from 2016 census ###
pop2016 = read.csv("Pop2016.csv")
pop2016 = subset(pop2016, DAUID %in% da$DAUID) # Reduce to just 1600 included DAs
da$Population = pop2016$Pop2016

# subset into pre/post 1972
da_trans = subset(da, Transition > 1971)
da_notrans = subset(da, Transition < 1971)
######

#da_notrans.p = ggplot(data = da_notrans, aes(x = Transition, y = Population, fill = Municipality)) +
#  geom_bar(position = "fill", stat = "identity")
#da_notrans.p

da_notrans.p = ggplot(data = da_notrans, aes(x = "", y = Population, fill = Municipality)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "red3", "blue")) +
  scale_x_discrete(name = "Transition Year (pre-1972, n = 368)", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_text(size = axn_sz - 1.75),
        axis.title.y = element_text(size = axn_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.position = "none")
da_notrans.p

da_trans.p = ggplot(data = da_trans, aes(x = Transition, weight = Population, fill = Municipality, after_stat(count))) +
  geom_density(position = "stack", adjust = 4/5) +
  scale_fill_manual(values = c("darkgreen", "red3", "blue")) +
  scale_x_continuous(name = "Transition Year (post-1972, n = 1232)", expand = c(0,0)) +
  scale_y_continuous(name = expression("Density (DA "["pop"]*")"), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.position = c(0.85,0.8))
da_trans.p

tiff("TransitionYear_density_bymun.tif", units = "cm", width = 16.5, height = 12, res = 300)
da_trans.p + inset_element(da_notrans.p, left = 0.03, bottom = 0.02, right = 0.37, top = 0.33)
dev.off()