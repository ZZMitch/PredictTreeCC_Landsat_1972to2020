# LandsatLinkr
#download.file("https://github.com/jdbcode/LandsatLinkr/releases/download/0.5.0-beta/LandsatLinkr_0.5.0.zip", "LandsatLinkr")
#install.packages("LandsatLinkr", repos=NULL, type="binary")
#install.packages(c("raster","SDMTools","doParallel","foreach","plyr","gdalUtils","rgdal","igraph","MASS"))

#SDMTools no longer available in CRAN
#install.packages("remotes")
#remotes::install_version("SDMTools", "1.1-221")

library(LandsatLinkr) # error related to R 4.0.0, downgraded to 3.6.3

# Check your package library path
#.libPaths()

run_landsatlinkr()
#Proj.4 string: +proj=utm +zone=17 +ellps=GRS80 +datum=WGS84 +units=m +no_defs

# Current progress:Step 6
# Step 4: MSS 017030 never calibrated? Address that? 