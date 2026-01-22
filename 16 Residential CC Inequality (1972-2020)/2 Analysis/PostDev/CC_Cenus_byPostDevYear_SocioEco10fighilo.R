##### This code creates a figure of build form every 5 years post-development + split into hi / lo cc groups ######

library(ggplot2)
library(patchwork)
library(weights) # weighted t-test

##### Bring in Data #####
cc_yr0 = read.csv("cc_yr0_hilo.csv") 
cc_yr0$cc_cat = factor(cc_yr0$cc_cat, levels = c("Low", "High"))
cc_yr10 = read.csv("cc_yr10_hilo.csv")
cc_yr10$cc_cat = factor(cc_yr10$cc_cat, levels = c("Low", "High"))
cc_yr20 = read.csv("cc_yr20_hilo.csv") 
cc_yr20$cc_cat = factor(cc_yr20$cc_cat, levels = c("Low", "High"))
cc_yr20$MedInc[cc_yr20$MedInc >= 250000] = 245000
cc_yr30 = read.csv("cc_yr30_hilo.csv") 
cc_yr30$cc_cat = factor(cc_yr30$cc_cat, levels = c("Low", "High"))
cc_yr30$MedInc[cc_yr30$MedInc >= 250000] = 245000
cc_yr40 = read.csv("cc_yr40_hilo.csv") 
cc_yr40$cc_cat = factor(cc_yr40$cc_cat, levels = c("Low", "High"))
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
### Median Income (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr0$MedInc, cc_yr0$cc_cat == "High"),
           y = subset(cc_yr0$MedInc, cc_yr0$cc_cat == "Low"),
           weight = subset(cc_yr0$Pop, cc_yr0$cc_cat == "High"),
           weighty = subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = 8k)
t.test(x = subset(cc_yr0$MedInc, cc_yr0$cc_cat == "High"),
       y = subset(cc_yr0$MedInc, cc_yr0$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr0$MedInc, cc_yr0$cc_cat == "High"),
            y = subset(cc_yr0$MedInc, cc_yr0$cc_cat == "Low")) # Non-parametric - ns

medinc_yr0.p = ggplot(cc_yr0, aes(x = MedInc / 1000, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(20, 250), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 0", limits = c(0, 0.031), expand = c(0,0)) +
  geom_vline(aes(xintercept = 105.269)) + # High: Weighted Mea
  geom_vline(aes(xintercept = 97.506), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 200, y = 0.025, label = "** (8k)", size = eq_sz_sm) +
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
medinc_yr0.p

### % Racial Minorities (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr0$pMin, cc_yr0$cc_cat == "High"),
           y = subset(cc_yr0$pMin, cc_yr0$cc_cat == "Low"),
           weight = subset(cc_yr0$Pop, cc_yr0$cc_cat == "High"),
           weighty = subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -24%)
t.test(x = subset(cc_yr0$pMin, cc_yr0$cc_cat == "High"),
       y = subset(cc_yr0$pMin, cc_yr0$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr0$pMin, cc_yr0$cc_cat == "High"),
            y = subset(cc_yr0$pMin, cc_yr0$cc_cat == "Low")) # Non-parametric - ns

pmin_yr0.p = ggplot(cc_yr0, aes(x = pMin, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 100), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.037), expand = c(0,0)) +
  geom_vline(aes(xintercept = 27.7)) + # High: Weighted Mea
  geom_vline(aes(xintercept = 51.8), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 75, y = 0.031, label = "*** (-24%)", size = eq_sz_sm) +
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
pmin_yr0.p
#####

##### Year 10 #####
### Median Income (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr10$MedInc, cc_yr10$cc_cat == "High"),
           y = subset(cc_yr10$MedInc, cc_yr10$cc_cat == "Low"),
           weight = subset(cc_yr10$Pop, cc_yr10$cc_cat == "High"),
           weighty = subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = 14k)
t.test(x = subset(cc_yr10$MedInc, cc_yr10$cc_cat == "High"),
       y = subset(cc_yr10$MedInc, cc_yr10$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr10$MedInc, cc_yr10$cc_cat == "High"),
            y = subset(cc_yr10$MedInc, cc_yr10$cc_cat == "Low")) # Non-parametric - ns

medinc_yr10.p = ggplot(cc_yr10, aes(x = MedInc / 1000, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(20, 250), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 10", limits = c(0, 0.031), expand = c(0,0)) +
  geom_vline(aes(xintercept = 109.864)) + # High: Weighted Mea
  geom_vline(aes(xintercept = 95.590), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 200, y = 0.025, label = "*** (14k)", size = eq_sz_sm) +
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
medinc_yr10.p

### % Racial Minorities (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr10$pMin, cc_yr10$cc_cat == "High"),
           y = subset(cc_yr10$pMin, cc_yr10$cc_cat == "Low"),
           weight = subset(cc_yr10$Pop, cc_yr10$cc_cat == "High"),
           weighty = subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -50%)
t.test(x = subset(cc_yr10$pMin, cc_yr10$cc_cat == "High"),
       y = subset(cc_yr10$pMin, cc_yr10$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr10$pMin, cc_yr10$cc_cat == "High"),
            y = subset(cc_yr10$pMin, cc_yr10$cc_cat == "Low")) # Non-parametric - ns

pmin_yr10.p = ggplot(cc_yr10, aes(x = pMin, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 100), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.037), expand = c(0,0)) +
  geom_vline(aes(xintercept = 23.3)) + # High: Weighted Mea
  geom_vline(aes(xintercept = 73.9), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 50, y = 0.031, label = "*** (-51%)", size = eq_sz_sm) +
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
pmin_yr10.p
#####

##### Year 20 #####
### Median Income (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr20$MedInc, cc_yr20$cc_cat == "High"),
           y = subset(cc_yr20$MedInc, cc_yr20$cc_cat == "Low"),
           weight = subset(cc_yr20$Pop, cc_yr20$cc_cat == "High"),
           weighty = subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = 31k)
t.test(x = subset(cc_yr20$MedInc, cc_yr20$cc_cat == "High"),
       y = subset(cc_yr20$MedInc, cc_yr20$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr20$MedInc, cc_yr20$cc_cat == "High"),
            y = subset(cc_yr20$MedInc, cc_yr20$cc_cat == "Low")) # Non-parametric - ns

medinc_yr20.p = ggplot(cc_yr20, aes(x = MedInc / 1000, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(20, 250), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 20", limits = c(0, 0.031), expand = c(0,0)) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(MedInc / 1000, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mea
  geom_vline(aes(xintercept = weighted.mean(x = subset(MedInc / 1000, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 200, y = 0.025, label = "*** (31k)", size = eq_sz_sm) +
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
medinc_yr20.p

### % Racial Minorities (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr20$pMin, cc_yr20$cc_cat == "High"),
           y = subset(cc_yr20$pMin, cc_yr20$cc_cat == "Low"),
           weight = subset(cc_yr20$Pop, cc_yr20$cc_cat == "High"),
           weighty = subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -50%)
t.test(x = subset(cc_yr20$pMin, cc_yr20$cc_cat == "High"),
       y = subset(cc_yr20$pMin, cc_yr20$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr20$pMin, cc_yr20$cc_cat == "High"),
            y = subset(cc_yr20$pMin, cc_yr20$cc_cat == "Low")) # Non-parametric - ns

pmin_yr20.p = ggplot(cc_yr20, aes(x = pMin, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 100), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.037), expand = c(0,0)) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(pMin, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mea
  geom_vline(aes(xintercept = weighted.mean(x = subset(pMin, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 50, y = 0.031, label = "*** (-50%)", size = eq_sz_sm) +
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
pmin_yr20.p
#####

##### Year 30 #####
### Median Income (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr30$MedInc, cc_yr30$cc_cat == "High"),
           y = subset(cc_yr30$MedInc, cc_yr30$cc_cat == "Low"),
           weight = subset(cc_yr30$Pop, cc_yr30$cc_cat == "High"),
           weighty = subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = 41k)
t.test(x = subset(cc_yr30$MedInc, cc_yr30$cc_cat == "High"),
       y = subset(cc_yr30$MedInc, cc_yr30$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr30$MedInc, cc_yr30$cc_cat == "High"),
            y = subset(cc_yr30$MedInc, cc_yr30$cc_cat == "Low")) # Non-parametric - ns

medinc_yr30.p = ggplot(cc_yr30, aes(x = MedInc / 1000, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(20, 250), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 30", limits = c(0, 0.031), expand = c(0,0)) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(MedInc / 1000, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mea
  geom_vline(aes(xintercept = weighted.mean(x = subset(MedInc / 1000, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 200, y = 0.025, label = "*** (41k)", size = eq_sz_sm) +
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
medinc_yr30.p

### % Racial Minorities (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr30$pMin, cc_yr30$cc_cat == "High"),
           y = subset(cc_yr30$pMin, cc_yr30$cc_cat == "Low"),
           weight = subset(cc_yr30$Pop, cc_yr30$cc_cat == "High"),
           weighty = subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -49%)
t.test(x = subset(cc_yr30$pMin, cc_yr30$cc_cat == "High"),
       y = subset(cc_yr30$pMin, cc_yr30$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr30$pMin, cc_yr30$cc_cat == "High"),
            y = subset(cc_yr30$pMin, cc_yr30$cc_cat == "Low")) # Non-parametric - ns

pmin_yr30.p = ggplot(cc_yr30, aes(x = pMin, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = NULL, limits = c(0, 100), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.037), expand = c(0,0)) +
  geom_vline(aes(xintercept = 27.7)) + # High: Weighted Mea
  geom_vline(aes(xintercept = 76.8), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 50, y = 0.031, label = "*** (-49%)", size = eq_sz_sm) +
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
pmin_yr30.p
#####

##### Year 40 #####
### Median Income (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr40$MedInc, cc_yr40$cc_cat == "High"),
           y = subset(cc_yr40$MedInc, cc_yr40$cc_cat == "Low"),
           weight = subset(cc_yr40$Pop, cc_yr40$cc_cat == "High"),
           weighty = subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = 56k)
t.test(x = subset(cc_yr40$MedInc, cc_yr40$cc_cat == "High"),
       y = subset(cc_yr40$MedInc, cc_yr40$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr40$MedInc, cc_yr40$cc_cat == "High"),
            y = subset(cc_yr40$MedInc, cc_yr40$cc_cat == "Low")) # Non-parametric - ns

medinc_yr40.p = ggplot(cc_yr40, aes(x = MedInc / 1000, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = "Median Income (1000s $ CAD)", limits = c(20, 250), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 40", limits = c(0, 0.031), expand = c(0,0)) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(MedInc / 1000, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mea
  geom_vline(aes(xintercept = weighted.mean(x = subset(MedInc / 1000, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 200, y = 0.025, label = "*** (56k)", size = eq_sz_sm) +
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
medinc_yr40.p

### % Racial Minorities (High vs. Low) ###
# Significant Difference of weighted means: High vs. Low - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_yr40$pMin, cc_yr40$cc_cat == "High"),
           y = subset(cc_yr40$pMin, cc_yr40$cc_cat == "Low"),
           weight = subset(cc_yr40$Pop, cc_yr40$cc_cat == "High"),
           weighty = subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low"),
           bootse = TRUE) # *** (Diff = -42%)
t.test(x = subset(cc_yr40$pMin, cc_yr40$cc_cat == "High"),
       y = subset(cc_yr40$pMin, cc_yr40$cc_cat == "Low")) # Un-weighted - ns
wilcox.test(x = subset(cc_yr40$pMin, cc_yr40$cc_cat == "High"),
            y = subset(cc_yr40$pMin, cc_yr40$cc_cat == "Low")) # Non-parametric - ns

pmin_yr40.p = ggplot(cc_yr40, aes(x = pMin, weight = Pop, fill = cc_cat)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "CC (%)", values = c(low, high)) + 
  scale_x_continuous(name = "Visible Minorities (%)", limits = c(0, 100), expand = c(0,0)) + 
  scale_y_continuous(name = NULL, limits = c(0, 0.037), expand = c(0,0)) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(pMin, cc_cat == "High"), w = subset(Pop, cc_cat == "High"), na.rm = TRUE))) + # High: Weighted Mea
  geom_vline(aes(xintercept = weighted.mean(x = subset(pMin, cc_cat == "Low"), w = subset(Pop, cc_cat == "Low"), na.rm = TRUE)), lty = 2) + # Low: Weighted Mean
  annotate(geom = "text", x = 50, y = 0.031, label = "*** (-42%)", size = eq_sz_sm) +
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
pmin_yr40.p
#####

tiff("SocioEco_postdev10_1.tif", units = "cm", width = 10, height = 12, res = 300) # Finalize and done :)
(medinc_yr0.p + pmin_yr0.p) / 
  (medinc_yr10.p + pmin_yr10.p) /
  (medinc_yr20.p + pmin_yr20.p) /
  (medinc_yr30.p + pmin_yr30.p) /
  (medinc_yr40.p + pmin_yr40.p) + plot_layout(guides = "collect") & theme(legend.position = 'bottom', 
                                                           legend.text = element_text(size = axn_sz), 
                                                           legend.background = element_blank(),
                                                           legend.box.margin = unit(c(-0.5, 0, -0.3, 0), "cm"))
dev.off()
