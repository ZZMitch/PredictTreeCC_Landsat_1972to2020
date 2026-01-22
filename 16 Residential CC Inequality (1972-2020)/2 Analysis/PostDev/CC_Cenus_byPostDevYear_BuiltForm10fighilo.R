##### This code creates a figure of build form every 5 years post-development + split into hi / lo cc groups ######

library(ggplot2)
library(patchwork)
library(weights) # weighted t-test
citation("weights")

##### Bring in Data #####
cc_yr0 = read.csv("cc_yr0_hilo.csv") 
cc_yr0$cc_cat = factor(cc_yr0$cc_cat, levels = c("Low", "High"))
cc_yr0$PopDen[cc_yr0$PopDen >= 20000] = sample(15000:20000,100)
cc_yr10 = read.csv("cc_yr10_hilo.csv")
cc_yr10$cc_cat = factor(cc_yr10$cc_cat, levels = c("Low", "High"))
cc_yr10$PopDen[cc_yr10$PopDen >= 20000] = sample(15000:20000,100)
cc_yr20 = read.csv("cc_yr20_hilo.csv") 
cc_yr20$cc_cat = factor(cc_yr20$cc_cat, levels = c("Low", "High"))
cc_yr20$PopDen[cc_yr20$PopDen >= 20000] = sample(15000:20000,100)
cc_yr30 = read.csv("cc_yr30_hilo.csv") 
cc_yr30$cc_cat = factor(cc_yr30$cc_cat, levels = c("Low", "High"))
cc_yr30$PopDen[cc_yr30$PopDen >= 20000] = sample(15000:20000,100)
cc_yr40 = read.csv("cc_yr40_hilo.csv") 
cc_yr40$cc_cat = factor(cc_yr40$cc_cat, levels = c("Low", "High"))
cc_yr40$PopDen[cc_yr40$PopDen >= 20000] = sample(15000:20000,100)
#####

##### Create Plot #####
eq_sz = 3
eq_sz_sm = 2.5
axt_sz = 10
axn_sz = 8
alpha = 0.6

high = "darkgreen"
low = "sienna" 

##### Year 0 #####
### Transition Year (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr0$Trans_yr, cc_yr0$cc_cat == "High"),
           y = subset(cc_yr0$Trans_yr, cc_yr0$cc_cat == "Low"),
           weight = subset(cc_yr0$Pop, cc_yr0$cc_cat == "High"),
           weighty = subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low"),
           bootse = TRUE) # ns (Diff = 0.3)
t.test(x = subset(cc_yr0$Trans_yr, cc_yr0$cc_cat == "High"),
       y = subset(cc_yr0$Trans_yr, cc_yr0$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr0$Trans_yr, cc_yr0$cc_cat == "High"),
            y = subset(cc_yr0$Trans_yr, cc_yr0$cc_cat == "Low")) # Non-parametric - ns

trans_yr0.p = ggplot(cc_yr0, aes(x = Trans_yr, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(1972, 2020), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 0", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  geom_vline(aes(xintercept = 1992.7)) + # High: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = 1992.4), lty = 2) + # Low: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha, lty = 2) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha, lty = 2) +
  annotate(geom = "text", x = 2011, y = 0.08, label = "ns (0.4)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
trans_yr0.p

### Population Density (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr0$PopDen, cc_yr0$cc_cat == "High"),
           y = subset(cc_yr0$PopDen, cc_yr0$cc_cat == "Low"),
           weight = subset(cc_yr0$Pop, cc_yr0$cc_cat == "High"),
           weighty = subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -3.3k)
t.test(x = subset(cc_yr0$PopDen, cc_yr0$cc_cat == "High"),
       y = subset(cc_yr0$PopDen, cc_yr0$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr0$PopDen, cc_yr0$cc_cat == "High"),
            y = subset(cc_yr0$PopDen, cc_yr0$cc_cat == "Low")) # Non-parametric - ***

popden_yr0.p = ggplot(cc_yr0, aes(x = PopDen, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 20000), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.0003), expand = c(0,0), breaks = c(0, 0.0001, 0.0002, 0.0003)) +
  geom_vline(aes(xintercept = 5337)) + # High: Weighted Mean
  geom_vline(aes(xintercept = 7996), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 16000, y = 0.00025, label = "*** (-2.7k)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
popden_yr0.p

### Most Common Dwelling Type - 90%+ (High vs. Low) ###
mc_dwty90_yr0.p = ggplot(subset(cc_yr0, pDet >= 90 | pAtt >= 90 | pApt >= 90), aes(y = cc_cat, weight = Pop, fill = MC_DwTy)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(name = "", palette = "Set2") +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0, 0, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
mc_dwty90_yr0.p
#####

##### Year 10 #####
### Transition Year (High vs. Low) ###
wtd.t.test(x = subset(cc_yr10$Trans_yr, cc_yr10$cc_cat == "High"),
           y = subset(cc_yr10$Trans_yr, cc_yr10$cc_cat == "Low"),
           weight = subset(cc_yr10$Pop, cc_yr10$cc_cat == "High"),
           weighty = subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -11.6)
t.test(x = subset(cc_yr10$Trans_yr, cc_yr10$cc_cat == "High"),
       y = subset(cc_yr10$Trans_yr, cc_yr10$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr10$Trans_yr, cc_yr10$cc_cat == "High"),
            y = subset(cc_yr10$Trans_yr, cc_yr10$cc_cat == "Low")) # Non-parametric - ***

trans_yr10.p = ggplot(cc_yr10, aes(x = Trans_yr, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(1972, 2020), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 10", breaks = c(0, 0.05, 0.1)) +
  coord_cartesian(ylim = c(0, 0.1), expand = FALSE) +
  geom_vline(aes(xintercept = 1986.9)) + # High: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = 1998.8), lty = 2) + # Low: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha, lty = 2) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha, lty = 2) +
  annotate(geom = "text", x = 2011, y = 0.08, label = "*** (-11.8)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
trans_yr10.p

### Population Density (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr10$PopDen, cc_yr10$cc_cat == "High"),
           y = subset(cc_yr10$PopDen, cc_yr10$cc_cat == "Low"),
           weight = subset(cc_yr10$Pop, cc_yr10$cc_cat == "High"),
           weighty = subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -6.9k)
t.test(x = subset(cc_yr10$PopDen, cc_yr10$cc_cat == "High"),
       y = subset(cc_yr10$PopDen, cc_yr10$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr10$PopDen, cc_yr10$cc_cat == "High"),
            y = subset(cc_yr10$PopDen, cc_yr10$cc_cat == "Low")) # Non-parametric - ***

popden_yr10.p = ggplot(cc_yr10, aes(x = PopDen, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 20000), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.0003), expand = c(0,0), breaks = c(0, 0.0001, 0.0002, 0.0003)) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(PopDen, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mean
  geom_vline(aes(xintercept = weighted.mean(x = subset(PopDen , cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 16000, y = 0.00025, label = "*** (-4.2k)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
popden_yr10.p

### Most Common Dwelling Type - 90%+ (High vs. Low) ###
mc_dwty90_yr10.p = ggplot(subset(cc_yr10, pDet >= 90 | pAtt >= 90 | pApt >= 90), aes(y = cc_cat, weight = Pop, fill = MC_DwTy)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(name = "", palette = "Set2") +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
mc_dwty90_yr10.p
#####

##### Year 20  #####
### Transition Year (High vs. Low) ###
wtd.t.test(x = subset(cc_yr20$Trans_yr, cc_yr20$cc_cat == "High"),
           y = subset(cc_yr20$Trans_yr, cc_yr20$cc_cat == "Low"),
           weight = subset(cc_yr20$Pop, cc_yr20$cc_cat == "High"),
           weighty = subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -9.6)
t.test(x = subset(cc_yr20$Trans_yr, cc_yr20$cc_cat == "High"),
       y = subset(cc_yr20$Trans_yr, cc_yr20$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr20$Trans_yr, cc_yr20$cc_cat == "High"),
            y = subset(cc_yr20$Trans_yr, cc_yr20$cc_cat == "Low")) # Non-parametric - ***

trans_yr20.p = ggplot(cc_yr20, aes(x = Trans_yr, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(1972, 2020), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 20", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  geom_vline(aes(xintercept = 1983.5)) + # High: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = 1993.3), lty = 2) + # Low: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha, lty = 2) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha, lty = 2) +
  annotate(geom = "text", x = 2011, y = 0.08, label = "*** (-9.8)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
trans_yr20.p

### Population Density (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr20$PopDen, cc_yr20$cc_cat == "High"),
           y = subset(cc_yr20$PopDen, cc_yr20$cc_cat == "Low"),
           weight = subset(cc_yr20$Pop, cc_yr20$cc_cat == "High"),
           weighty = subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -11.3k)
t.test(x = subset(cc_yr20$PopDen, cc_yr20$cc_cat == "High"),
       y = subset(cc_yr20$PopDen, cc_yr20$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr20$PopDen, cc_yr20$cc_cat == "High"),
            y = subset(cc_yr20$PopDen, cc_yr20$cc_cat == "Low")) # Non-parametric - ***

popden_yr20.p = ggplot(cc_yr20, aes(x = PopDen, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 20000), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.0003), expand = c(0,0), breaks = c(0, 0.0001, 0.0002, 0.0003)) +
  geom_vline(aes(xintercept = 4906)) + # High: Weighted Mean
  geom_vline(aes(xintercept = 11051), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 16000, y = 0.00025, label = "*** (-6.1k)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
popden_yr20.p

### Most Common Dwelling Type - 90%+ (High vs. Low) ###
mc_dwty90_yr20.p = ggplot(subset(cc_yr20, pDet >= 90 | pAtt >= 90 | pApt >= 90), aes(y = cc_cat, weight = Pop, fill = MC_DwTy)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(name = "", palette = "Set2") +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
mc_dwty90_yr20.p
#####

##### Year 30  #####
### Transition Year (High vs. Low) ###
wtd.t.test(x = subset(cc_yr30$Trans_yr, cc_yr30$cc_cat == "High"),
           y = subset(cc_yr30$Trans_yr, cc_yr30$cc_cat == "Low"),
           weight = subset(cc_yr30$Pop, cc_yr30$cc_cat == "High"),
           weighty = subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -5.4)
t.test(x = subset(cc_yr30$Trans_yr, cc_yr30$cc_cat == "High"),
       y = subset(cc_yr30$Trans_yr, cc_yr30$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr30$Trans_yr, cc_yr30$cc_cat == "High"),
            y = subset(cc_yr30$Trans_yr, cc_yr30$cc_cat == "Low")) # Non-parametric - ***

trans_yr30.p = ggplot(cc_yr30, aes(x = Trans_yr, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(1972, 2020), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 30", breaks = c(0, 0.05, 0.1)) +
  coord_cartesian(ylim = c(0, 0.1), expand = FALSE) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "High" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Mississauga"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Mississauga"), na.rm = TRUE)), col = m, alpha = alpha, lty = 2) +
  #geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low" & Mun == "Brampton"), 
  #                                          w = subset(Pop, cc_cat == "Low" & Mun == "Brampton"), na.rm = TRUE)), col = b, alpha = alpha, lty = 2) +
  annotate(geom = "text", x = 2011, y = 0.08, label = "*** (-5.4)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
trans_yr30.p

### Population Density (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr30$PopDen, cc_yr30$cc_cat == "High"),
           y = subset(cc_yr30$PopDen, cc_yr30$cc_cat == "Low"),
           weight = subset(cc_yr30$Pop, cc_yr30$cc_cat == "High"),
           weighty = subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -21.0k)
t.test(x = subset(cc_yr30$PopDen, cc_yr30$cc_cat == "High"),
       y = subset(cc_yr30$PopDen, cc_yr30$cc_cat == "Low")) # Un-weighted - ***
wilcox.test(x = subset(cc_yr30$PopDen, cc_yr30$cc_cat == "High"),
            y = subset(cc_yr30$PopDen, cc_yr30$cc_cat == "Low")) # Non-parametric - ***

popden_yr30.p = ggplot(cc_yr30, aes(x = PopDen, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 20000), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.0003), expand = c(0,0), breaks = c(0, 0.0001, 0.0002, 0.0003)) +
  geom_vline(aes(xintercept = 4355)) + # High: Weighted Mean
  geom_vline(aes(xintercept = 11948), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 16000, y = 0.00025, label = "*** (-7.6k)", size = eq_sz_sm) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
popden_yr30.p

### Most Common Dwelling Type - 90%+ (High vs. Low) ###
mc_dwty90_yr30.p = ggplot(subset(cc_yr30, pDet >= 90 | pAtt >= 90 | pApt >= 90), aes(y = cc_cat, weight = Pop, fill = MC_DwTy)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(name = "", palette = "Set2") +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
mc_dwty90_yr30.p
#####

##### Year 40  #####
### Transition Year (High vs. Low) ###
wtd.t.test(x = subset(cc_yr40$Trans_yr, cc_yr40$cc_cat == "High"),
           y = subset(cc_yr40$Trans_yr, cc_yr40$cc_cat == "Low"),
           weight = subset(cc_yr40$Pop, cc_yr40$cc_cat == "High"),
           weighty = subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low"),
           bootse = TRUE) # ns (Diff = 0.7)
t.test(x = subset(cc_yr40$Trans_yr, cc_yr40$cc_cat == "High"),
       y = subset(cc_yr40$Trans_yr, cc_yr40$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr40$Trans_yr, cc_yr40$cc_cat == "High"),
            y = subset(cc_yr40$Trans_yr, cc_yr40$cc_cat == "Low")) # Non-parametric - ns

trans_yr40.p = ggplot(cc_yr40, aes(x = Trans_yr, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = "Transition Year", limits = c(1972, 2020), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 40", breaks = c(0, 0.05, 0.1)) +
  coord_cartesian(ylim = c(0, 0.1), expand = FALSE) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mean
  geom_vline(aes(xintercept = weighted.mean(x = subset(Trans_yr, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 2011, y = 0.08, label = "ns (0.7)", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
trans_yr40.p

### Population Density (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr40$PopDen, cc_yr40$cc_cat == "High"),
           y = subset(cc_yr40$PopDen, cc_yr40$cc_cat == "Low"),
           weight = subset(cc_yr40$Pop, cc_yr40$cc_cat == "High"),
           weighty = subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -30.4k)
t.test(x = subset(cc_yr40$PopDen, cc_yr40$cc_cat == "High"),
       y = subset(cc_yr40$PopDen, cc_yr40$cc_cat == "Low")) # Un-weighted - **
wilcox.test(x = subset(cc_yr40$PopDen, cc_yr40$cc_cat == "High"),
            y = subset(cc_yr40$PopDen, cc_yr40$cc_cat == "Low")) # Non-parametric - ***

popden_yr40.p = ggplot(cc_yr40, aes(x = PopDen, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = "Population Density", limits = c(0, 20000), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.0003), expand = c(0,0), breaks = c(0, 0.0001, 0.0002, 0.0003)) +
  geom_vline(aes(xintercept = 3854)) + # High: Weighted Mean
  geom_vline(aes(xintercept = 13665), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 16000, y = 0.00025, label = "*** (-9.8k)", size = eq_sz_sm) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.background = element_blank())
popden_yr40.p

### Most Common Dwelling Type - 90%+ (High vs. Low) ###
mc_dwty90_yr40.p = ggplot(subset(cc_yr40, pDet >= 90 | pAtt >= 90 | pApt >= 90), aes(y = cc_cat, weight = Pop, fill = MC_DwTy)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(name = "", palette = "Set2") +
  scale_x_continuous(name = "Prop - Dwelling Type (90+%)",expand = c(0,0)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
mc_dwty90_yr40.p
#####

# Built form
tiff("Builtform_postdev10_1.tif", units = "cm", width = 16, height = 12, res = 300) # Add other built form variables
(trans_yr0.p + popden_yr0.p + mc_dwty90_yr0.p) / 
  (trans_yr10.p + popden_yr10.p + mc_dwty90_yr10.p) /
  (trans_yr20.p + popden_yr20.p + mc_dwty90_yr20.p) /
  (trans_yr30.p + popden_yr30.p + mc_dwty90_yr30.p) /
  (trans_yr40.p + popden_yr40.p + mc_dwty90_yr40.p) + plot_layout(guides = "collect") & theme(legend.position = 'bottom', 
                                                                                              legend.text = element_text(size = axn_sz),
                                                                                              legend.background = element_blank(),
                                                                                              legend.box.margin = unit(c(-0.5, 0, -0.3, 0), "cm"))
dev.off()
