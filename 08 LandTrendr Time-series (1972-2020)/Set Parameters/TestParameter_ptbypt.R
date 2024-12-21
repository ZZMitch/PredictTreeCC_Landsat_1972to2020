# Read in TCA, TCB, TCG, TCW time-series for 100 random points in study area from GEE
# Plot median and fitted time-series together for all TC indices
# Compare - give 1 if fitted is good representation of median, 0 if not
# Test different parameter settings

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Build/LandTrendR/ParameterTests")

# Read in time-series tables
# Median (same for all paramaters)
tca_med = read.csv("TCA_med_tbl.csv")
tcb_med = read.csv("TCB_med_tbl.csv")
tcg_med = read.csv("TCG_med_tbl.csv")
tcw_med = read.csv("TCW_med_tbl.csv")

# Multiply tca, tbg, and tcw med by -1 (LandTrendr)
tca_med[,2:49] = tca_med[,2:49] * -1
tcg_med[,2:49] = tcg_med[,2:49] * -1
tcw_med[,2:49] = tcw_med[,2:49] * -1

# Parameters (Parameters 1)
tca_fit = read.csv("Parameters 1/TCA_fit_tbl.csv")
tcb_fit = read.csv("Parameters 1/TCB_fit_tbl.csv")
tcg_fit = read.csv("Parameters 1/TCG_fit_tbl.csv")
tcw_fit = read.csv("Parameters 1/TCW_fit_tbl.csv")

# Parameters 2 (MaxSegments 6)
#tca_fit2 = read.csv("Parameters 2/TCA_fit_tbl.csv")
#tcb_fit2 = read.csv("Parameters 2/TCB_fit_tbl.csv")
#tcg_fit2 = read.csv("Parameters 2/TCG_fit_tbl.csv")
#tcw_fit2 = read.csv("Parameters 2/TCW_fit_tbl.csv")

# Parameters 3 (+ spikeThreshold 0.75)
#tca_fit3 = read.csv("Parameters 3/TCA_fit_tbl.csv")
#tcb_fit3 = read.csv("Parameters 3/TCB_fit_tbl.csv")
#tcg_fit3 = read.csv("Parameters 3/TCG_fit_tbl.csv")
#tcw_fit3 = read.csv("Parameters 3/TCW_fit_tbl.csv")

# Parameters 4 (+ vertexCountOvershoot = 0)
#tca_fit4 = read.csv("Parameters 4/TCA_fit_tbl.csv")
#tcb_fit4 = read.csv("Parameters 4/TCB_fit_tbl.csv")
#tcg_fit4 = read.csv("Parameters 4/TCG_fit_tbl.csv")
#tcw_fit4 = read.csv("Parameters 4/TCW_fit_tbl.csv")

# Best: Parameters 5 (+ preventOneYearRecovery = false)
tca_fit5 = read.csv("Parameters 5/TCA_fit_tbl.csv")
tcb_fit5 = read.csv("Parameters 5/TCB_fit_tbl.csv")
tcg_fit5 = read.csv("Parameters 5/TCG_fit_tbl.csv")
tcw_fit5 = read.csv("Parameters 5/TCW_fit_tbl.csv")

# Parameters 6 (+ recoveryThreshold = 0.5)
#tca_fit6 = read.csv("Parameters 6/TCA_fit_tbl.csv")
#tcb_fit6 = read.csv("Parameters 6/TCB_fit_tbl.csv")
#tcg_fit6 = read.csv("Parameters 6/TCG_fit_tbl.csv")
#tcw_fit6 = read.csv("Parameters 6/TCW_fit_tbl.csv")

# Parameters 7 (+ pval = 0.1)
#tca_fit7 = read.csv("Parameters 7/TCA_fit_tbl.csv")
#tcb_fit7 = read.csv("Parameters 7/TCB_fit_tbl.csv")
#tcg_fit7 = read.csv("Parameters 7/TCG_fit_tbl.csv")
#tcw_fit7 = read.csv("Parameters 7/TCW_fit_tbl.csv")

# Parameters 8 (+ bestModelProportion = 1.25)
#tca_fit8 = read.csv("Parameters 8/TCA_fit_tbl.csv")
#tcb_fit8 = read.csv("Parameters 8/TCB_fit_tbl.csv")
#tcg_fit8 = read.csv("Parameters 8/TCG_fit_tbl.csv")
#tcw_fit8 = read.csv("Parameters 8/TCW_fit_tbl.csv")

years = c(1972, 1974:2020)

# Point (1 = 0, 100 = 99)
pt = 58 # Change for each point

par(mfrow = c(2,2), mar = c(2,4,1,1))
plot(years, tca_med[pt,2:49], 
     xlab = "", ylab = "TCA", pch  = 16, cex  = 1.5)
lines(years, tca_fit5[pt,2:49], col = "red3", lwd = 3)
lines(years, tca_fit[pt,2:49], col = "blue", lwd = 3)
plot(years, tcb_med[pt,2:49], 
     xlab = "", ylab = "TCB", pch  = 16, cex  = 1.5)
lines(years, tcb_fit5[pt,2:49], col = "red3", lwd = 3)
lines(years, tcb_fit[pt,2:49], col = "blue", lwd = 3)
plot(years, tcg_med[pt,2:49], 
     xlab = "", ylab = "TCG", pch  = 16, cex  = 1.5)
lines(years, tcg_fit5[pt,2:49], col = "red3", lwd = 3)
lines(years, tcg_fit[pt,2:49], col = "blue", lwd = 3)
plot(years, tcw_med[pt,2:49], 
     xlab = "", ylab = "TCW", pch  = 16, cex  = 1.5)
lines(years, tcw_fit5[pt,2:49], col = "red3", lwd = 3)
lines(years, tcw_fit[pt,2:49], col = "blue", lwd = 3)

# Plot for Figure
tiff("ExamplePixel57_1.tiff", units = "in", width = 6.5, height = 8, res = 300)
layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
par(mar = c(2,4.5,1,1))
#A
plot(years, tca_med[pt,2:49], 
     xlab = "", ylab = "TCA", pch  = 16, cex  = 1.5, cex.lab = 1.25)
lines(years, tca_fit5[pt,2:49], col = "red3", lwd = 3)
#lines(years, tca_fit[pt,2:49], col = "blue", lwd = 3)
text(1973.5,450,"A", cex = 2)
#B
plot(years, tcb_med[pt,2:49], 
     xlab = "", ylab = "TCB", pch  = 16, cex  = 1.5, cex.lab = 1.25)
lines(years, tcb_fit5[pt,2:49], col = "red3", lwd = 3)
#lines(years, tcb_fit[pt,2:49], col = "blue", lwd = 3)
text(1973.5,1950,"B", cex = 2)
#C
plot(years, tcg_med[pt,2:49], 
     xlab = "", ylab = "TCG", pch  = 16, cex  = 1.5, cex.lab = 1.25)
lines(years, tcg_fit5[pt,2:49], col = "red3", lwd = 3)
#lines(years, tcg_fit[pt,2:49], col = "blue", lwd = 3)
text(1973.5,250,"C", cex = 2)
#D
plot(years, tcw_med[pt,2:49], 
     xlab = "", ylab = "TCW", pch  = 16, cex  = 1.5, cex.lab = 1.25)
lines(years, tcw_fit5[pt,2:49], col = "red3", lwd = 3)
#lines(years, tcw_fit[pt,2:49], col = "blue", lwd = 3)
text(1973.5,-1425,"D", cex = 2)

# E: CC Plot
cc57 = read.csv("pt57_cc_res.csv")
cc57[2:49] = cc57[2:49] * 100

plot(years, cc57[2:49], 
     xlab = "", ylab = expression("Residential CC (%)"), pch  = 16, cex  = 1.5, cex.lab = 1.25)
lines(years, cc57[2:49], col = "black", lwd = 3)
text(1971.5,15,"E", cex = 2)
dev.off()