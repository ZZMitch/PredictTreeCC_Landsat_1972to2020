##### This code creates a figure of CC every 10 years post-development + high vs. low CC population distribution ######

library(ggplot2) # Plotting
library(patchwork)
library(weights) # weighted t-test

##### Bring in Data #####
cc_dev0_pop5 = read.csv("cc_dev0_pop5.csv") # CC every 5 years post-development for all transition DAs + actual year + population of closest census year

# CC every 5 years post-development + census data for just DAs with CC +1 SD above / -1 SD below the weighted mean
cc_yr0 = read.csv("cc_yr0_hilo.csv") 
cc_yr0$Mun = factor(cc_yr0$Mun, levels = c("Caledon", "Brampton", "Mississauga"))
cc_yr0$cc_cat = factor(cc_yr0$cc_cat, levels = c("Low", "High"))
cc_yr10 = read.csv("cc_yr10_hilo.csv")
cc_yr10$Mun = factor(cc_yr10$Mun, levels = c("Caledon", "Brampton", "Mississauga"))
cc_yr10$cc_cat = factor(cc_yr10$cc_cat, levels = c("Low", "High"))
cc_yr20 = read.csv("cc_yr20_hilo.csv") 
cc_yr20$Mun = factor(cc_yr20$Mun, levels = c("Caledon", "Brampton", "Mississauga"))
cc_yr20$cc_cat = factor(cc_yr20$cc_cat, levels = c("Low", "High"))
cc_yr30 = read.csv("cc_yr30_hilo.csv") 
cc_yr30$Mun = factor(cc_yr30$Mun, levels = c("Caledon", "Brampton", "Mississauga"))
cc_yr30$cc_cat = factor(cc_yr30$cc_cat, levels = c("Low", "High"))
cc_yr40 = read.csv("cc_yr40_hilo.csv") 
cc_yr40$Mun = factor(cc_yr40$Mun, levels = c("Caledon", "Brampton", "Mississauga"))
cc_yr40$cc_cat = factor(cc_yr40$cc_cat, levels = c("Low", "High"))
#####

##### Create Plot #####
eq_sz = 3
eq_sz_sm = 2.5
axt_sz = 10
axn_sz = 8
alpha = 0.6

m = "blue"
b = "red3"
c = "darkgreen"

##### Year 0 #####
# cc_yr0: Confirm population of high/low and mun subsets > 7500 (DOES NOT APPLY TO CANOPY COVER)
sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "High")) # 123,617
sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low")) # 92,487

sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "High" & cc_yr0$Mun == "Mississauga")) # 79,915
sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "High" & cc_yr0$Mun == "Brampton")) # 29,709
sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "High" & cc_yr0$Mun == "Caledon")) # 13,993 - Do not include (other is below 7500)

sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low" & cc_yr0$Mun == "Mississauga")) # 27,235
sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low" & cc_yr0$Mun == "Brampton")) # 64,262
sum(subset(cc_yr0$Pop, cc_yr0$cc_cat == "Low" & cc_yr0$Mun == "Caledon")) # 990 - Do not include

### Canopy Cover (All DAs) ###
# Significant Difference of weighted means: Mississauga vs. Brampton - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$Mun == "Mississauga"),
           y = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$Mun == "Brampton"),
           weight = subset(cc_dev0_pop5$yr0_pop, cc_dev0_pop5$Mun == "Mississauga"),
           weighty = subset(cc_dev0_pop5$yr0_pop, cc_dev0_pop5$Mun == "Brampton"),
           bootse = TRUE) # *** (Diff = 3.0)
t.test(x = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$Mun == "Mississauga"),
       y = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$Mun == "Brampton")) # Un-weighted - ***
wilcox.test(x = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$Mun == "Mississauga"),
            y = subset(cc_dev0_pop5$yr0_cc, cc_dev0_pop5$Mun == "Brampton")) # Non-parametric - ***

cc_yr0.p = ggplot(cc_dev0_pop5, aes(x = yr0_cc, weight = yr0_pop)) +
  geom_density(fill = "gray70") + 
  geom_vline(aes(xintercept = weighted.mean(x = yr0_cc, w = yr0_pop, na.rm = TRUE))) + # Weighted Mean
  geom_vline(aes(xintercept = min(subset(cc_yr0$CC, cc_yr0$cc_cat == "High"))), lty = 2) + # Weighted Mean + 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = max(subset(cc_yr0$CC, cc_yr0$cc_cat == "Low"))), lty = 2) + # Weighted Mean - 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = 10.24), col = m, alpha = alpha) +
  geom_vline(aes(xintercept = 7.58), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(yr0_cc, Mun == "Caledon"), w = subset(yr0_pop, Mun == "Caledon"), na.rm = TRUE)), col = c, alpha = alpha) +
  scale_x_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 0", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  annotate(geom = "text", x = 38, y = 0.08, label = "** (2.7)", size = eq_sz) +
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
cc_yr0.p

### High vs. Low CC Population distribution ###
pop_yr0.p = ggplot(cc_yr0, aes(x = cc_cat, y = Pop / 1000, fill = Mun)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(c, b, m)) +
  scale_y_continuous(name = "", limits = c(0, 163), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
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
pop_yr0.p
#####

##### Year 10 #####
# cc_yr10: Confirm population of high/low and mun subsets > 7500 (DOES NOT APPLY TO CANOPY COVER)
sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "High")) # 144,004
sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low")) # 154.284

sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "High" & cc_yr10$Mun == "Mississauga")) # 97,798
sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "High" & cc_yr10$Mun == "Brampton")) # 26,225
sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "High" & cc_yr10$Mun == "Caledon")) # 19,981 - Do not include (other is below 7500)

sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low" & cc_yr10$Mun == "Mississauga")) # 51,193
sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low" & cc_yr10$Mun == "Brampton")) # 100,688
sum(subset(cc_yr10$Pop, cc_yr10$cc_cat == "Low" & cc_yr10$Mun == "Caledon")) # 2,403 - Do not include

### Canopy Cover (All DAs)
# Significant Difference of weighted means: Mississauga vs. Brampton - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$Mun == "Mississauga"),
           y = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$Mun == "Brampton"),
           weight = subset(cc_dev0_pop5$yr10_pop, cc_dev0_pop5$Mun == "Mississauga"),
           weighty = subset(cc_dev0_pop5$yr10_pop, cc_dev0_pop5$Mun == "Brampton"),
           bootse = TRUE) # *** (Diff = 3.4)
t.test(x = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$Mun == "Mississauga"),
       y = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$Mun == "Brampton"),) # Un-weighted - ***
wilcox.test(x = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$Mun == "Mississauga"),
            y = subset(cc_dev0_pop5$yr10_cc, cc_dev0_pop5$Mun == "Brampton")) # Non-parametric - ***

cc_yr10.p = ggplot(cc_dev0_pop5, aes(x = yr10_cc, weight = yr10_pop)) +
  geom_density(fill = "gray70") + 
  geom_vline(aes(xintercept = weighted.mean(x= yr10_cc, w = yr10_pop, na.rm = TRUE))) + # Weighted Mean
  geom_vline(aes(xintercept = min(subset(cc_yr10$CC, cc_yr10$cc_cat == "High"))), lty = 2) + # Weighted Mean + 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = max(subset(cc_yr10$CC, cc_yr10$cc_cat == "Low"))), lty = 2) + # Weighted Mean - 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = 14.52), col = m, alpha = alpha) +
  geom_vline(aes(xintercept = 11.66), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(yr10_cc, Mun == "Caledon"), w = subset(yr10_pop, Mun == "Caledon"), na.rm = TRUE)), col = c, alpha = alpha) +
  scale_x_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 10", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  annotate(geom = "text", x = 38, y = 0.08, label = "** (2.9)", size = eq_sz) +
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
cc_yr10.p

### High vs. Low CC Population distribution ###
pop_yr10.p = ggplot(cc_yr10, aes(x = cc_cat, y = Pop / 1000, fill = Mun)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(c, b, m)) +
  scale_y_continuous(name = "", limits = c(0, 163), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
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
pop_yr10.p
#####

##### Year 20 #####
# cc_yr20: Confirm population of high/low and mun subsets > 7500 (DOES NOT APPLY TO CANOPY COVER)
sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "High")) # 103,043
sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low")) # 89,046

sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "High" & cc_yr20$Mun == "Mississauga")) # 73,628
sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "High" & cc_yr20$Mun == "Brampton")) # 15,605
sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "High" & cc_yr20$Mun == "Caledon")) # 13,810 - Do not include (other is below 7500)

sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low" & cc_yr20$Mun == "Mississauga")) # 30,995
sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low" & cc_yr20$Mun == "Brampton")) # 55,982
sum(subset(cc_yr20$Pop, cc_yr20$cc_cat == "Low" & cc_yr20$Mun == "Caledon")) # 2,069 - Do not include

### Canopy Cover (All DAs)
# Significant Difference of weighted means: Mississauga vs. Brampton - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$Mun == "Mississauga"),
           y = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$Mun == "Brampton"),
           weight = subset(cc_dev0_pop5$yr20_pop, cc_dev0_pop5$Mun == "Mississauga"),
           weighty = subset(cc_dev0_pop5$yr20_pop, cc_dev0_pop5$Mun == "Brampton"),
           bootse = TRUE) # *** (Diff = 3.2)
t.test(x = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$Mun == "Mississauga"),
       y = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$Mun == "Brampton"),) # Un-weighted - ***
wilcox.test(x = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$Mun == "Mississauga"),
            y = subset(cc_dev0_pop5$yr20_cc, cc_dev0_pop5$Mun == "Brampton")) # Non-parametric - ***

cc_yr20.p = ggplot(cc_dev0_pop5, aes(x = yr20_cc, weight = yr20_pop)) +
  geom_density(fill = "gray70") + 
  geom_vline(aes(xintercept = weighted.mean(x= yr20_cc, w = yr20_pop, na.rm = TRUE))) + # Weighted Mean
  geom_vline(aes(xintercept = min(subset(cc_yr20$CC, cc_yr20$cc_cat == "High"))), lty = 2) + # Weighted Mean + 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = max(subset(cc_yr20$CC, cc_yr20$cc_cat == "Low"))), lty = 2) + # Weighted Mean - 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = 17.67), col = m, alpha = alpha) +
  geom_vline(aes(xintercept = 14.90), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(yr20_cc, Mun == "Caledon"), w = subset(yr20_pop, Mun == "Caledon"), na.rm = TRUE)), col = c, alpha = alpha) +
  scale_x_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 20", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  annotate(geom = "text", x = 38, y = 0.08, label = "** (2.8)", size = eq_sz) +
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
cc_yr20.p

### High vs. Low CC Population distribution ###
pop_yr20.p = ggplot(cc_yr20, aes(x = cc_cat, y = Pop / 1000, fill = Mun)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(c, b, m)) +
  scale_y_continuous(name = "Population (1000s)", limits = c(0, 163), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
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
pop_yr20.p
#####

##### Year 30 #####
# cc_yr30: Confirm population of high/low and mun subsets > 7500 (DOES NOT APPLY TO CANOPY COVER)
sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "High")) # 72.519
sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low")) # 50,626

sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "High" & cc_yr30$Mun == "Mississauga")) # 50,993
sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "High" & cc_yr30$Mun == "Brampton")) # 14,437
sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "High" & cc_yr30$Mun == "Caledon")) # 7,089 - Do not include (both below 7500)

sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low" & cc_yr30$Mun == "Mississauga")) # 18,526
sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low" & cc_yr30$Mun == "Brampton")) # 32,100
sum(subset(cc_yr30$Pop, cc_yr30$cc_cat == "Low" & cc_yr30$Mun == "Caledon")) # 0 - Do not include (both below 7500)

### Canopy Cover (All DAs)
# Significant Difference of weighted means: Mississauga vs. Brampton - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$Mun == "Mississauga"),
           y = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$Mun == "Brampton"),
           weight = subset(cc_dev0_pop5$yr30_pop, cc_dev0_pop5$Mun == "Mississauga"),
           weighty = subset(cc_dev0_pop5$yr30_pop, cc_dev0_pop5$Mun == "Brampton"),
           bootse = TRUE) # *** (Diff = 3.3)
t.test(x = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$Mun == "Mississauga"),
       y = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$Mun == "Brampton"),) # Un-weighted - ***
wilcox.test(x = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$Mun == "Mississauga"),
            y = subset(cc_dev0_pop5$yr30_cc, cc_dev0_pop5$Mun == "Brampton")) # Non-parametric - ***

cc_yr30.p = ggplot(cc_dev0_pop5, aes(x = yr30_cc, weight = yr30_pop)) +
  geom_density(fill = "gray70") + 
  geom_vline(aes(xintercept = weighted.mean(x= yr30_cc, w = yr30_pop, na.rm = TRUE))) + # Weighted Mean
  geom_vline(aes(xintercept = min(subset(cc_yr30$CC, cc_yr30$cc_cat == "High"))), lty = 2) + # Weighted Mean + 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = max(subset(cc_yr30$CC, cc_yr30$cc_cat == "Low"))), lty = 2) + # Weighted Mean - 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = 19.81), col = m, alpha = alpha) +
  geom_vline(aes(xintercept = 17.01), col = b, alpha = alpha) +
  geom_vline(aes(xintercept = weighted.mean(x = subset(yr30_cc, Mun == "Caledon"), w = subset(yr30_pop, Mun == "Caledon"), na.rm = TRUE)), col = c, alpha = alpha) +
  scale_x_continuous(name = NULL, limits = c(0, 45), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 30", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  annotate(geom = "text", x = 38, y = 0.08, label = "** (2.7)", size = eq_sz) +
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
cc_yr30.p

### High vs. Low CC Population distribution ###
pop_yr30.p = ggplot(cc_yr30, aes(x = cc_cat, y = Pop / 1000, fill = Mun)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(c, b, m)) +
  scale_y_continuous(name = "", limits = c(0, 163), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = axt_sz),
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
pop_yr30.p
#####

##### Year 40 #####
# cc_yr40: Confirm population of high/low and mun subsets > 7500 (DOES NOT APPLY TO CANOPY COVER)
sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "High")) # 30,703
sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low")) # 22,020

sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "High" & cc_yr40$Mun == "Mississauga")) # 24,872 - Do not include (other below 7500)
sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "High" & cc_yr40$Mun == "Brampton")) # 1,873 - Do not include (below 7500)
sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "High" & cc_yr40$Mun == "Caledon")) # 3,958 - Do not include (both below 7500)

sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low" & cc_yr40$Mun == "Mississauga")) # 6,801 - Do not include (below 7500)
sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low" & cc_yr40$Mun == "Brampton")) # 15,219 - Do not include (other below 7500)
sum(subset(cc_yr40$Pop, cc_yr40$cc_cat == "Low" & cc_yr40$Mun == "Caledon")) # 0 - Do not include (both below 7500)

### Canopy Cover (All DAs)
# Significant Difference of weighted means: Mississauga vs. Brampton - Two Sample weighted t-test (Welch)
wtd.t.test(x = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$Mun == "Mississauga"),
           y = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$Mun == "Brampton"),
           weight = subset(cc_dev0_pop5$yr40_pop, cc_dev0_pop5$Mun == "Mississauga"),
           weighty = subset(cc_dev0_pop5$yr40_pop, cc_dev0_pop5$Mun == "Brampton"),
           bootse = TRUE) # *** (Diff = 5.5)
t.test(x = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$Mun == "Mississauga"),
       y = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$Mun == "Brampton"),) # Un-weighted - ***
wilcox.test(x = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$Mun == "Mississauga"),
            y = subset(cc_dev0_pop5$yr40_cc, cc_dev0_pop5$Mun == "Brampton")) # Non-parametric - ***

cc_yr40.p = ggplot(cc_dev0_pop5, aes(x = yr40_cc, weight = yr40_pop)) +
  geom_density(fill = "gray70") + 
  geom_vline(aes(xintercept = weighted.mean(x= yr40_cc, w = yr40_pop, na.rm = TRUE))) + # Weighted Mean
  geom_vline(aes(xintercept = min(subset(cc_yr40$CC, cc_yr40$cc_cat == "High"))), lty = 2) + # Weighted Mean + 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = max(subset(cc_yr40$CC, cc_yr40$cc_cat == "Low"))), lty = 2) + # Weighted Mean - 1 Weighted Standard Deviation
  geom_vline(aes(xintercept = 22.77), col = m, alpha = alpha) +
  geom_vline(aes(xintercept = 17.76), col = b, alpha = alpha) +
  scale_x_continuous(name = "A. CC (%, population weighted)", limits = c(0, 45), expand = c(0,0)) + 
  scale_y_continuous(name = "Year 40", limits = c(0, 0.1), expand = c(0,0), breaks = c(0, 0.05, 0.1)) +
  annotate(geom = "text", x = 38, y = 0.08, label = "*** (5.0)", size = eq_sz) +
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
cc_yr40.p

### High vs. Low CC Population distribution ###
pop_yr40.p = ggplot(cc_yr40, aes(x = cc_cat, y = Pop / 1000, fill = Mun)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(c, b, m)) +
  scale_x_discrete(name = " B. CC Group") +
  scale_y_continuous(name = "", limits = c(0, 163), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = axt_sz),
        axis.title.y = element_text(size = axt_sz),
        axis.text.x = element_text(size = axn_sz, color = "black"),
        axis.text.y = element_text(size = axn_sz, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.1, "cm"),
        axis.ticks.length.x = unit(0.1, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        plot.background = element_blank(),
        legend.position = "none")
pop_yr40.p
#####

tiff("CC_postdev10_1.tif", units = "cm", width = 10, heigh = 12, res = 300) 
(cc_yr0.p + pop_yr0.p + plot_layout(widths = c(1.75, 1))) / 
  (cc_yr10.p + pop_yr10.p + plot_layout(widths = c(1.75, 1))) /
  (cc_yr20.p + pop_yr20.p + plot_layout(widths = c(1.75, 1))) /
  (cc_yr30.p + pop_yr30.p + plot_layout(widths = c(1.75, 1))) /
  (cc_yr40.p + pop_yr40.p + plot_layout(widths = c(1.75, 1)))  + plot_layout(guides = "collect") & theme(legend.position = 'bottom', 
                                                                                                         legend.text = element_text(size = axn_sz), 
                                                                                                         legend.title = element_blank(),
                                                                                                         legend.box.margin = unit(c(-0.5, 0, -0.3, 0), "cm"),
                                                                                                         legend.background = element_blank())
dev.off()