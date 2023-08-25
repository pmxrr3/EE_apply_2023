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

# Find day where LAI reaches maximum for each simulation; save those days and line numbers in vectors
maxLAIRep1 <- sapply(1:101, function(x) max(df_field_Rep_1[(x-1)*159+1:159, 3]) ) #as.matrix to make sure it's in global environment
maxLAIRep2 <- sapply(1:101, function(x) max(df_field_Rep_2[(x-1)*159+1:159, 3]) )
maxLAIRep3 <- sapply(1:101, function(x) max(df_field_Rep_3[(x-1)*159+1:159, 3]) )
maxLAIRep4 <- sapply(1:101, function(x) max(df_field_Rep_4[(x-1)*159+1:159, 3]) ) #as.matrix to make sure it's in global environment
maxLAIRep5 <- sapply(1:101, function(x) max(df_field_Rep_5[(x-1)*159+1:159, 3]) )
maxLAIRep6 <- sapply(1:101, function(x) max(df_field_Rep_6[(x-1)*159+1:159, 3]) )
maxLAIvalues <- cbind(maxLAIRep1,maxLAIRep2,maxLAIRep3,maxLAIRep4,maxLAIRep5,maxLAIRep6)
maxLAIindexRep1 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_1[(x-1)*159+1:159, 3]))) #Unlist to list->matrix
maxLAIindexRep2 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_2[(x-1)*159+1:159, 3])))
maxLAIindexRep3 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_3[(x-1)*159+1:159, 3])))
maxLAIindexRep4 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_4[(x-1)*159+1:159, 3]))) #Unlist to list->matrix
maxLAIindexRep5 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_5[(x-1)*159+1:159, 3])))
maxLAIindexRep6 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_6[(x-1)*159+1:159, 3])))
maxLAIindices <- cbind(maxLAIindexRep1,maxLAIindexRep2,maxLAIindexRep3,maxLAIindexRep4,maxLAIindexRep5,maxLAIindexRep6)
# Find day where biom reaches maximum for each simulation; save those days and line numbers in vectors
maxbiomRep1 <- sapply(1:101, function(x) max(df_field_Rep_1[(x-1)*159+1:159, 7]) ) #as.matrix to make sure it's in global environment
maxbiomRep2 <- sapply(1:101, function(x) max(df_field_Rep_2[(x-1)*159+1:159, 7]) )
maxbiomRep3 <- sapply(1:101, function(x) max(df_field_Rep_3[(x-1)*159+1:159, 7]) )
maxbiomRep4 <- sapply(1:101, function(x) max(df_field_Rep_4[(x-1)*159+1:159, 7]) ) #as.matrix to make sure it's in global environment
maxbiomRep5 <- sapply(1:101, function(x) max(df_field_Rep_5[(x-1)*159+1:159, 7]) )
maxbiomRep6 <- sapply(1:101, function(x) max(df_field_Rep_6[(x-1)*159+1:159, 7]) )
maxbiomvalues <- cbind(maxbiomRep1,maxbiomRep2,maxbiomRep3,maxbiomRep4,maxbiomRep5,maxbiomRep6)
maxbiomindexRep1 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_1[(x-1)*159+1:159, 7]))) #Unlist to list->matrix
maxbiomindexRep2 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_2[(x-1)*159+1:159, 7])))
maxbiomindexRep3 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_3[(x-1)*159+1:159, 7])))
maxbiomindexRep4 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_4[(x-1)*159+1:159, 7]))) #Unlist to list->matrix
maxbiomindexRep5 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_5[(x-1)*159+1:159, 7])))
maxbiomindexRep6 <- unlist(sapply(1:101, function(x) (x-1)*159+which.max(df_field_Rep_6[(x-1)*159+1:159, 7])))
maxbiomindices <- cbind(maxbiomindexRep1,maxbiomindexRep2,maxbiomindexRep3,maxbiomindexRep4,maxbiomindexRep5,maxbiomindexRep6)

Rep_1_harvest_159 <- as.matrix(df_field_Rep_1[df_field_Rep_1[ ,1] == "159", ])
Rep_1_harvest_159[,3] <- df_field_Rep_1[maxLAIindexRep1, 3]  # change LAI outputs to the max values
Rep_1_harvest_159[,7] <- df_field_Rep_1[maxbiomindexRep1, 7]  # change biom outputs to the max values
Rep_1_harvest_159 <- cbind(Rep_1_harvest_159, rep = rep(1,nrow(Rep_1_harvest_159))) #add rep number
colnames(Rep_1_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_2_harvest_159 <- as.matrix(df_field_Rep_2[df_field_Rep_2[ ,1] == "159", ])
Rep_2_harvest_159[,3] <- df_field_Rep_2[maxLAIindexRep2, 3]
Rep_2_harvest_159[,7] <- df_field_Rep_2[maxbiomindexRep2, 7]  
Rep_2_harvest_159 <- cbind(Rep_2_harvest_159, rep = rep(2,nrow(Rep_2_harvest_159)))
colnames(Rep_2_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_3_harvest_159 <- as.matrix(df_field_Rep_3[df_field_Rep_3[ ,1] == "159", ])
Rep_3_harvest_159[,3] <- df_field_Rep_3[maxLAIindexRep3, 3]
Rep_3_harvest_159[,7] <- df_field_Rep_3[maxbiomindexRep3, 7]  
Rep_3_harvest_159 <- cbind(Rep_3_harvest_159, rep = rep(3,nrow(Rep_3_harvest_159)))
colnames(Rep_3_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_4_harvest_159 <- as.matrix(df_field_Rep_4[df_field_Rep_4[ ,1] == "159", ])
Rep_4_harvest_159[,3] <- df_field_Rep_4[maxLAIindexRep4, 3]  # change LAI outputs to the max values
Rep_4_harvest_159[,7] <- df_field_Rep_4[maxbiomindexRep4, 7]  # change biom outputs to the max values
Rep_4_harvest_159 <- cbind(Rep_4_harvest_159, rep = rep(1,nrow(Rep_4_harvest_159))) #add rep number
colnames(Rep_4_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_5_harvest_159 <- as.matrix(df_field_Rep_5[df_field_Rep_5[ ,1] == "159", ])
Rep_5_harvest_159[,3] <- df_field_Rep_5[maxLAIindexRep5, 3]
Rep_5_harvest_159[,7] <- df_field_Rep_5[maxbiomindexRep5, 7]  
Rep_5_harvest_159 <- cbind(Rep_5_harvest_159, rep = rep(2,nrow(Rep_5_harvest_159)))
colnames(Rep_5_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")
#
Rep_6_harvest_159 <- as.matrix(df_field_Rep_6[df_field_Rep_6[ ,1] == "159", ])
Rep_6_harvest_159[,3] <- df_field_Rep_6[maxLAIindexRep6, 3]
Rep_6_harvest_159[,7] <- df_field_Rep_6[maxbiomindexRep6, 7]  
Rep_6_harvest_159 <- cbind(Rep_6_harvest_159, rep = rep(3,nrow(Rep_6_harvest_159)))
colnames(Rep_6_harvest_159) <- c("time(d)",	"species",	"LAI"	,"nrShoots",	"fAbs",	"assCO2",	"biomAbove",	"yield",	"harvestIndex",	"leafArea",	"fieldRFR", "Rep")

Rep_1_harvest_159[,c(3,7,8)] # nitro[maize]
Rep_2_harvest_159[,c(3,7,8)] # tb[maize]
Rep_3_harvest_159[,c(3,7,8)] # fCO2_SA
Rep_4_harvest_159[,c(3,7,8)] # shapeCoeff[maize]
Rep_5_harvest_159[,c(3,7,8)] # phyllochron[maize]
Rep_6_harvest_159[,c(3,7,8)] # reflectancePAR