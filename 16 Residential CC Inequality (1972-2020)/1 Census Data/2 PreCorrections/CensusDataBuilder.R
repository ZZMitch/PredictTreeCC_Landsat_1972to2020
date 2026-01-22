##### Take original census data files from 20/20 tables and convert into useful Peel data #####
### Original Data: Past Census data (1971,1981,1986,1991,1996,2001,2006,2011) at 2016 DA geographies for Toronto CMA ###
### Data link: https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/QNO5JG ###

library(data.table)

##### Bring in Original Data #####
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change/Census Data")

Census1971_GTA = read.csv("dataverse_files/Census1971_TorontoCMA.csv", check.names = FALSE)
Census1981_GTA = read.csv("dataverse_files/Census1981_TorontoCMA.csv", check.names = FALSE)
Census1986_GTA = read.csv("dataverse_files/Census1986_TorontoCMA.csv", check.names = FALSE)
Census1991_GTA1 = read.csv("dataverse_files/Census1991_TorontoCMA1.csv", check.names = FALSE)
Census1991_GTA2 = read.csv("dataverse_files/Census1991_TorontoCMA2.csv", check.names = FALSE)
Census1996_GTA1 = read.csv("dataverse_files/Census1996_TorontoCMA1.csv", check.names = FALSE)
Census1996_GTA2 = read.csv("dataverse_files/Census1996_TorontoCMA2.csv", check.names = FALSE)
Census2001_GTA1 = read.csv("dataverse_files/Census2001_TorontoCMA1.csv", check.names = FALSE)
Census2001_GTA2 = read.csv("dataverse_files/Census2001_TorontoCMA2.csv", check.names = FALSE)
Census2006_GTA1 = read.csv("dataverse_files/Census2006_TorontoCMA1.csv", check.names = FALSE)
Census2006_GTA2 = read.csv("dataverse_files/Census2006_TorontoCMA2.csv", check.names = FALSE)
Census2011_GTA1 = read.csv("dataverse_files/Census2011_TorontoCMA1.csv", check.names = FALSE)
Census2011_GTA2 = read.csv("dataverse_files/Census2011_TorontoCMA2.csv", check.names = FALSE)
# 2016 already set
#####

##### Combine multi-part years into one table #####
Census1991_GTA = cbind(Census1991_GTA1, Census1991_GTA2[2:ncol(Census1991_GTA2)]) # Census 1991
Census1996_GTA = cbind(Census1996_GTA1, Census1996_GTA2[2:ncol(Census1996_GTA2)]) # Census 1996
Census2001_GTA = cbind(Census2001_GTA1, Census2001_GTA2[2:ncol(Census2001_GTA2)]) # Census 2001
Census2006_GTA = cbind(Census2006_GTA1, Census2006_GTA2[2:ncol(Census2006_GTA2)]) # Census 2006
Census2011_GTA = cbind(Census2011_GTA1, Census2011_GTA2[2:ncol(Census2011_GTA2)]) # Census 2011
#####

##### Reduce to only needed census questions #####
Census1971_GTA_r = Census1971_GTA[-c(2:61, 73:83, 103:113, 124:362, 371:376),] # Census 1971
Census1981_GTA_r = Census1981_GTA[-c(2:63, 77:86, 104:119, 131:389, 391:394, 403:405, 411:421),] # Census 1981
Census1986_GTA_r = Census1986_GTA[-c(2:75, 92:94, 118:130, 142:461, 463, 469:475, 482:500),] # Census 1986
Census1991_GTA_r = Census1991_GTA[-c(2:175, 212:214, 289:322, 336:814, 816, 826:836, 844:857),] # Census 1991
Census1996_GTA_r = Census1996_GTA[-c(2:164, 238:241, 354:375, 390:1012, 1014, 1024:1034, 1042:1055),] # Census 1996
Census2001_GTA_r = Census2001_GTA[-c(2:235, 273:304, 312:319, 440:467, 482:1191, 1193, 1203:1213, 1223:1236),] # Census 2001
Census2006_GTA_r = Census2006_GTA[-c(2:107, 127:470, 571:650, 660:669, 678:1339, 1382:1397, 1670:2105, 2107:2169),] # Census 2006
Census2011_GTA_r = Census2011_GTA[-c(2:215, 226:1268),] # Census 2011
#####

##### Remove Toronto CMA #####
Census1971_GTA_r = Census1971_GTA_r[,-c(2)]
Census1981_GTA_r = Census1981_GTA_r[,-c(2)]
Census1986_GTA_r = Census1986_GTA_r[,-c(2)]
Census1991_GTA_r = Census1991_GTA_r[,-c(2)]
Census1996_GTA_r = Census1996_GTA_r[,-c(2)]
Census2001_GTA_r = Census2001_GTA_r[,-c(2)]
Census2006_GTA_r = Census2006_GTA_r[,-c(2)]
Census2011_GTA_r = Census2011_GTA_r[,-c(2)]
#####

##### Reduce to only post-transition (for that census year) Peel DAs #####
PeelDA = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change/IncludedDAs1650.csv")
PeelDAInclude = subset(PeelDA, Include == 1)
DA_data = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Analyze/PostDevelopment Residential Change/DA_data1.csv")
PeelDAInclude$Transition = DA_data$Transition

# Census 1971
Census1971_GTA_r = setNames(as.data.frame(t(Census1971_GTA_r[,-1])), Census1971_GTA_r[,1]) # Transpose, setting first column as row headers
Census1971_GTA_r = cbind(rownames(Census1971_GTA_r), data.frame(Census1971_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census1971_GTA_r)[names(Census1971_GTA_r) == "rownames(Census1971_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census1971_GTA_r = as.data.frame(sapply(Census1971_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census1971_Peel_r = setDT(Census1971_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census1971_Peel_r$Total.Population.by.Sex.and.Age.Groups..33..Sample.Data.) # Population Included
#nrow(Census1971_Peel_r) # DAs Included
#Census1971_Peel_r = setDT(Census1971_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census1971_Peel_r$Total.Population.by.Sex.and.Age.Groups..33..Sample.Data.) # Population Included
#nrow(Census1971_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 1971) Peel DAs
Census1971_Peel_r = setDT(Census1971_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 1972)] 
#sum(Census1971_Peel_r$Total.Population.by.Sex.and.Age.Groups..33..Sample.Data.) # Population Included
#nrow(Census1971_Peel_r) # DAs Included

# Census 1981
Census1981_GTA_r = setNames(as.data.frame(t(Census1981_GTA_r[,-1])), Census1981_GTA_r[,1]) # Transpose, setting first column as row headers
Census1981_GTA_r = cbind(rownames(Census1981_GTA_r), data.frame(Census1981_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census1981_GTA_r)[names(Census1981_GTA_r) == "rownames(Census1981_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census1981_GTA_r = as.data.frame(sapply(Census1981_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census1981_Peel_r = setDT(Census1981_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census1981_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1981_Peel_r) # DAs Included
#Census1981_Peel_r = setDT(Census1981_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census1981_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1981_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 1981) Peel DAs
Census1981_Peel_r = setDT(Census1981_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 1982)] 
#sum(Census1981_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1981_Peel_r) # DAs Included

# Census 1986
Census1986_GTA_r = setNames(as.data.frame(t(Census1986_GTA_r[,-1])), Census1986_GTA_r[,1]) # Transpose, setting first column as row headers
Census1986_GTA_r = cbind(rownames(Census1986_GTA_r), data.frame(Census1986_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census1986_GTA_r)[names(Census1986_GTA_r) == "rownames(Census1986_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census1986_GTA_r = as.data.frame(sapply(Census1986_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census1986_Peel_r = setDT(Census1986_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census1986_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1986_Peel_r) # DAs Included
#Census1986_Peel_r = setDT(Census1986_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census1986_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1986_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 1986) Peel DAs
Census1986_Peel_r = setDT(Census1986_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 1987)] 
#sum(Census1986_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1986_Peel_r) # DAs Included

# Census 1991
Census1991_GTA_r = setNames(as.data.frame(t(Census1991_GTA_r[,-1])), Census1991_GTA_r[,1]) # Transpose, setting first column as row headers
Census1991_GTA_r = cbind(rownames(Census1991_GTA_r), data.frame(Census1991_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census1991_GTA_r)[names(Census1991_GTA_r) == "rownames(Census1991_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census1991_GTA_r = as.data.frame(sapply(Census1991_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census1991_Peel_r = setDT(Census1991_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census1991_Peel_r$X..Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1991_Peel_r) # DAs Included
#Census1991_Peel_r = setDT(Census1991_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census1991_Peel_r$X..Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1991_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 1991) Peel DAs
Census1991_Peel_r = setDT(Census1991_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 1992)] 
#sum(Census1991_Peel_r$X..Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1991_Peel_r) # DAs Included

# Census 1996
Census1996_GTA_r = setNames(as.data.frame(t(Census1996_GTA_r[,-1])), Census1996_GTA_r[,1]) # Transpose, setting first column as row headers
Census1996_GTA_r = cbind(rownames(Census1996_GTA_r), data.frame(Census1996_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census1996_GTA_r)[names(Census1996_GTA_r) == "rownames(Census1996_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census1996_GTA_r = as.data.frame(sapply(Census1996_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census1996_Peel_r = setDT(Census1996_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census1996_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1996_Peel_r) # DAs Included
#Census1996_Peel_r = setDT(Census1996_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census1996_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1996_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 1996) Peel DAs
Census1996_Peel_r = setDT(Census1996_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 1997)] 
#sum(Census1996_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census1996_Peel_r) # DAs Included

# Census 2001
Census2001_GTA_r = setNames(as.data.frame(t(Census2001_GTA_r[,-1])), Census2001_GTA_r[,1]) # Transpose, setting first column as row headers
Census2001_GTA_r = cbind(rownames(Census2001_GTA_r), data.frame(Census2001_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census2001_GTA_r)[names(Census2001_GTA_r) == "rownames(Census2001_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census2001_GTA_r = as.data.frame(sapply(Census2001_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census2001_Peel_r = setDT(Census2001_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census2001_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census2001_Peel_r) # DAs Included
#Census2001_Peel_r = setDT(Census2001_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census2001_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census2001_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 2001) Peel DAs
Census2001_Peel_r = setDT(Census2001_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 2002)] 
#sum(Census2001_Peel_r$Total.Population.by.Sex.and.Age.Groups..20..Sample.Data.) # Population Included
#nrow(Census2001_Peel_r) # DAs Included

# Census 2006
Census2006_GTA_r = setNames(as.data.frame(t(Census2006_GTA_r[,-1])), Census2006_GTA_r[,1]) # Transpose, setting first column as row headers
Census2006_GTA_r = cbind(rownames(Census2006_GTA_r), data.frame(Census2006_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census2006_GTA_r)[names(Census2006_GTA_r) == "rownames(Census2006_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census2006_GTA_r = as.data.frame(sapply(Census2006_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census2006_Peel_r = setDT(Census2006_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census2006_Peel_r$Total.population.by.sex.and.age.groups...20..sample.data) # Population Included
#nrow(Census2006_Peel_r) # DAs Included
#Census2006_Peel_r = setDT(Census2006_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census2006_Peel_r$Total.population.by.sex.and.age.groups...20..sample.data) # Population Included
#nrow(Census2006_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 2006) Peel DAs
Census2006_Peel_r = setDT(Census2006_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 2007)] 
#sum(Census2006_Peel_r$Total.population.by.sex.and.age.groups...20..sample.data) # Population Included
#nrow(Census2006_Peel_r) # DAs Included

# Census 2011
Census2011_GTA_r = setNames(as.data.frame(t(Census2011_GTA_r[,-1])), Census2011_GTA_r[,1]) # Transpose, setting first column as row headers
Census2011_GTA_r = cbind(rownames(Census2011_GTA_r), data.frame(Census2011_GTA_r, row.names = NULL)) # Convert row names to first column
names(Census2011_GTA_r)[names(Census2011_GTA_r) == "rownames(Census2011_GTA_r)"] <- "DAUID" # Rename first column to DAUID
Census2011_GTA_r = as.data.frame(sapply(Census2011_GTA_r, as.numeric)) # Convert data.frame from chr to num

#Census2011_Peel_r = setDT(Census2011_GTA_r)[DAUID %in% PeelDA$DAUID] # Match DAUID in both tables to reduce to only Peel DAs
#sum(Census2011_Peel_r$Total.population.by.age.groups) # Population Included
#nrow(Census2011_Peel_r) # DAs Included
#Census2011_Peel_r = setDT(Census2011_GTA_r)[DAUID %in% PeelDAInclude$DAUID] # Match DAUID in both tables to reduce to only included Peel DAs
#sum(Census2011_Peel_r$Total.population.by.age.groups) # Population Included
#nrow(Census2011_Peel_r) # DAs Included

# Match DAUID in both tables to reduce to only include developed (in 2011) Peel DAs
Census2011_Peel_r = setDT(Census2011_GTA_r)[DAUID %in% subset(PeelDAInclude$DAUID, PeelDAInclude$Transition < 2012)] 
#sum(Census2011_Peel_r$Total.population.by.age.groups) # Population Included
#nrow(Census2011_Peel_r) # DAs Included
#####

##### Create useful/stable Census data through time #####
# Will but much easier to export raw Census data and complete in Excel...
#write.csv(Census1971_Peel_r, "Census1971_Peel_raw.csv")
#write.csv(Census1981_Peel_r, "Census1981_Peel_raw.csv")
#write.csv(Census1986_Peel_r, "Census1986_Peel_raw.csv")
#write.csv(Census1991_Peel_r, "Census1991_Peel_raw.csv")
#write.csv(Census1996_Peel_r, "Census1996_Peel_raw.csv")
#write.csv(Census2001_Peel_r, "Census2001_Peel_raw.csv")
#write.csv(Census2006_Peel_r, "Census2006_Peel_raw.csv")
#write.csv(Census2011_Peel_r, "Census2011_Peel_raw.csv")
#####