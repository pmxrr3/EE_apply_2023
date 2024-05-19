packages <- c("tidyverse","data.table","matrixStats","geoR","installr","pracma","reshape2","cowplot","ggpattern","ggcorrplot","pwr", "rmetalog", "FME")
lapply(packages, library, character.only = TRUE)

# fieldRFRSensMatrix <- read.table(file = "C:/Users/Rik/OneDrive - RMIT University/zzz_Personal/PhD papers/Paper ApplyEE/Supplementary material/Outputs and processed data/EE Analysis/fieldRFRSensMatrix.txt")
# assCOtwoSensMatrix <- read.table(file = "C:/Users/Rik/OneDrive - RMIT University/zzz_Personal/PhD papers/Paper ApplyEE/Supplementary material/Outputs and processed data/EE Analysis/assCOtwoSensMatrix.txt")
# fAbsSensMatrix <- read.table(file = "C:/Users/Rik/OneDrive - RMIT University/zzz_Personal/PhD papers/Paper ApplyEE/Supplementary material/Outputs and processed data/EE Analysis/fAbsSensMatrix.txt")
BiomSensMatrix <- read.table(file = "C:/Users/Rik/OneDrive - RMIT University/zzz_Personal/PhD papers/Paper ApplyEE/Supplementary material/Outputs and processed data/EE Analysis/BiomSensMatrix.txt")
YieldSensMatrix <- read.table(file = "C:/Users/Rik/OneDrive - RMIT University/zzz_Personal/PhD papers/Paper ApplyEE/Supplementary material/Outputs and processed data/EE Analysis/YieldSensMatrix.txt")
LAISensMatrix <- read.table(file = "C:/Users/Rik/OneDrive - RMIT University/zzz_Personal/PhD papers/Paper ApplyEE/Supplementary material/Outputs and processed data/EE Analysis/LAISensMatrix.txt")

# Only take 12 most important parameters at final time (14 for LAI), plus all parameters that have S_chi > 0.1 at some point, into account
Biomindices <- sort(unique(c(27, 30, 48, 3, 2, 4, 6, 27, 36, 44, 45, 47, 48, 51, 52)))
Yieldindices <- sort(unique(c(51, 48, 3, 51, 48, 47, 45, 44, 27, 20, 19, 6, 4, 3, 2)))
LAIindices <- sort(unique(c(19, 6, 48, 52, 3, 6, 11, 12, 19, 22, 26, 27, 44, 45, 47, 48, 51, 52)))

BiomSensMatrixRestricted <- BiomSensMatrix[,Biomindices]
YieldSensMatrixRestricted <- YieldSensMatrix[,Yieldindices]
# YieldSensMatrixRestricted[is.na(YieldSensMatrixRestricted)] <- 0
LAISensMatrixRestricted <- LAISensMatrix[,LAIindices]