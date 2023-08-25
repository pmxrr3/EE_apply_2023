#install.packages('matrixStats')
library(matrixStats)
field_Rep_1 <- 'C:/Users/***/Output/OATSims/field_Paper2_OATsims_Core_1.txt'
field_Rep_2 <- 'C:/Users/***/Output/OATSims/field_Paper2_OATsims_Core_2.txt'
field_Rep_3 <- 'C:/Users/***/Output/OATSims/field_Paper2_OATsims_Core_3.txt'
field_Rep_4 <- 'C:/Users/***/Output/OATSims/field_Paper2_OATsims_Core_4.txt'
field_Rep_5 <- 'C:/Users/***/Output/OATSims/field_Paper2_OATsims_Core_5.txt'
field_Rep_6 <- 'C:/Users/***/Output/OATSims/field_Paper2_OATsims_Core_6.txt'

# Read in each of the 3 output data files for each full replicate; make 3 new data files with only harvest day outputs, adding which replicate they came from
df_field_Rep_1 <- read.table(field_Rep_1, header = TRUE)
df_field_Rep_2 <- read.table(field_Rep_2, header = TRUE)
df_field_Rep_3 <- read.table(field_Rep_3, header = TRUE)
df_field_Rep_4 <- read.table(field_Rep_4, header = TRUE)
df_field_Rep_5 <- read.table(field_Rep_5, header = TRUE)
df_field_Rep_6 <- read.table(field_Rep_6, header = TRUE)

Rep_1_harvest_159 <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == "159", ])
colnames(Rep_1_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
Rep_2_harvest_159 <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == "159", ])
colnames(Rep_2_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
Rep_3_harvest_159 <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == "159", ])
colnames(Rep_3_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
Rep_4_harvest_159 <- as.matrix(df_field_Rep_4[df_field_Rep_4[ ,1] == "159", ])
colnames(Rep_4_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
Rep_5_harvest_159 <- as.matrix(df_field_Rep_5[df_field_Rep_5[ ,1] == "159", ])
colnames(Rep_5_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")
Rep_6_harvest_159 <- as.matrix(df_field_Rep_6[df_field_Rep_6[ ,1] == "159", ])
colnames(Rep_6_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR")

Rep_1_harvest_159[,c(3,7,8)] # nitro[maize]
Rep_2_harvest_159[,c(3,7,8)] # tb[maize]
Rep_3_harvest_159[,c(3,7,8)] # fCO2_SA
Rep_4_harvest_159[,c(3,7,8)] # shapeCoeff[maize]
Rep_5_harvest_159[,c(3,7,8)] # phyllochron[maize]
Rep_6_harvest_159[,c(3,7,8)] # reflectancePAR