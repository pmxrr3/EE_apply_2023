# Calculate collinearity for all subsets (combinations of 2-all parameters)
BiomCollin <- collin(BiomSensMatrixRestricted)
YieldCollin <- collin(YieldSensMatrixRestricted[-(1:71),])
LAICollin <- collin(LAISensMatrixRestricted[-(1:5),])

# Find largest parameter set that still has collinearity <= 15 for all combinations
BiomLargestIdentifiableSet <- min(first(BiomCollin[BiomCollin$collinearity>15,]$N)-1,length(Biomindices))
YieldLargestIdentifiableSet <- min(first(YieldCollin[YieldCollin$collinearity>15,]$N)-1,length(Yieldindices))
LAILargestIdentifiableSet <- min(first(LAICollin[LAICollin$collinearity>15,]$N)-1,length(LAIindices))


# Plots
# Boxplots of all collinearity values for the 3 outputs
df <- data.frame(Output = c(rep("LAI", nrow(LAICollin)), rep("Aboveground biomass", nrow(BiomCollin)), rep("Yield", nrow(YieldCollin))),
                 Collinearity = c(LAICollin$collinearity, BiomCollin$collinearity, YieldCollin$collinearity))
df$Output <- factor(df$Output, levels=unique(df$Output))
xticks <- c("LAI", "Aboveground biomass", "Yield")
ggplot(df, aes(x=Output, y=Collinearity, color = Output, fill=Output)) + 
        geom_boxplot(alpha = 0.6, notch=FALSE, varwidth=FALSE) +
        stat_summary(fun=mean, geom="point", shape=4, size=2, stroke=1.2, position = position_dodge(.75)) + 
        geom_hline(yintercept=20, lty=1, col='darkgrey') + geom_hline(yintercept=15, lty=1, col='darkgrey') + #geom_hline(yintercept=10, lty=2) +
        theme_bw() + 
        labs(x = "Output", y = "Collinearity") + 
        scale_x_discrete(labels = xticks) +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplots of collinearity values per parameter set size, for each of the 3 outputs
df <- data.frame(Output = c(rep("LAI", nrow(LAICollin)), rep("Biomass", nrow(BiomCollin)), rep("Yield", nrow(YieldCollin))),
                 Collinearity = c(LAICollin$collinearity, BiomCollin$collinearity, YieldCollin$collinearity),
                 N = c(LAICollin$N, BiomCollin$N, YieldCollin$N))
df$N <- factor(df$N, levels=unique(df$N))
df$Output <- factor(df$Output, levels=unique(df$Output))
xticks <- 2:14
ggplot(df, aes(x=N, y=Collinearity, color = Output, fill=Output)) + 
  geom_boxplot(alpha = 0.6, notch=FALSE, varwidth=FALSE) +
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke=1.2, position = position_dodge(.75)) + 
  geom_hline(yintercept=20, lty=1, col='darkgrey') + geom_hline(yintercept=15, lty=1, col='darkgrey') + #geom_hline(yintercept=10, lty=2) +
  geom_vline(xintercept=c(-0.5+2:13), colour="lightgrey") +
  theme_bw() + 
  labs(x = "Parameter set size", y = "Collinearity") + 
  scale_x_discrete(labels = xticks) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

