## Validation of BEEHAVE model with data from honey bee large-scale colony feeding studies
## SChmolke et al., submitted
## Script for plotting goodness-of-fit indicators
## Creator: Amelie Schmolke

rm(list = ls())

library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# All scripts and data are located in the same folder: the path may have to adjusted 
home.dir <- 'C:/BEEHAVEModelValidation/Validation/'
setwd(home.dir)

# Reading goodness-of-fit indicator values from file (produced by the script 'Calc_GoodnessOfFitIndicators.R')
# Data set without correction for bias in BEEHAVE outputs
gofi <- read.csv(file = "GoodnessOfFitIndicators_LSCFS.csv")
# Renaming to fit terms used in the manuscript, including 'LSCFS' instead of 'CFS'
gofi$Data <- as.character(gofi$Data)
gofi$CCAs <- as.character(gofi$CCAs)
for(i in 1:length(gofi$CCAs)){
  if(gofi$CCAs[i] == "Fall"){gofi$CCAs[i] <- "October"}
  if(gofi$CCAs[i] == "Season"){gofi$CCAs[i] <- "First year"}
}
for(i in 1:length(gofi$Data)){
  if(gofi$Data[i] == "CFS_2013_1"){gofi$Data[i] <- "LSCFS_2013_1"}
  if(gofi$Data[i] == "CFS_2014_1"){gofi$Data[i] <- "LSCFS_2014_1"}
  if(gofi$Data[i] == "CFS_2014_2"){gofi$Data[i] <- "LSCFS_2014_2"}
  if(gofi$Data[i] == "CFS_2015_1"){gofi$Data[i] <- "LSCFS_2015_1"}
  if(gofi$Data[i] == "CFS_2015_2"){gofi$Data[i] <- "LSCFS_2015_2"}
  if(gofi$Data[i] == "CFS_2016_1"){gofi$Data[i] <- "LSCFS_2016_1"}
  if(gofi$Data[i] == "CFS_2016_2"){gofi$Data[i] <- "LSCFS_2016_2"}
}
gofi$Data <- factor(gofi$Data, levels=unique(gofi$Data))

# Reading goodness-of-fit indicator values from file (produced by the script 'Calc_GoodnessOfFitIndicators_biascorr.R')
# Data set with correction for bias in BEEHAVE outputs
gofi_bias <- read.csv(file = "GoodnessOfFitIndicators_LSCFS_biascorr.csv")
# Renaming to fit terms used in the manuscript, including 'LSCFS' instead of 'CFS'
gofi_bias$Data <- as.character(gofi_bias$Data)
gofi_bias$CCAs <- as.character(gofi_bias$CCAs)
for(i in 1:length(gofi_bias$CCAs)){
  if(gofi_bias$CCAs[i] == "Fall"){gofi_bias$CCAs[i] <- "October"}
  if(gofi_bias$CCAs[i] == "Season"){gofi_bias$CCAs[i] <- "First year"}
}
for(i in 1:length(gofi_bias$Data)){
  if(gofi_bias$Data[i] == "CFS_2013_1"){gofi_bias$Data[i] <- "LSCFS_2013_1"}
  if(gofi_bias$Data[i] == "CFS_2014_1"){gofi_bias$Data[i] <- "LSCFS_2014_1"}
  if(gofi_bias$Data[i] == "CFS_2014_2"){gofi_bias$Data[i] <- "LSCFS_2014_2"}
  if(gofi_bias$Data[i] == "CFS_2015_1"){gofi_bias$Data[i] <- "LSCFS_2015_1"}
  if(gofi_bias$Data[i] == "CFS_2015_2"){gofi_bias$Data[i] <- "LSCFS_2015_2"}
  if(gofi_bias$Data[i] == "CFS_2016_1"){gofi_bias$Data[i] <- "LSCFS_2016_1"}
  if(gofi_bias$Data[i] == "CFS_2016_2"){gofi_bias$Data[i] <- "LSCFS_2016_2"}
}
gofi_bias$Data <- factor(gofi_bias$Data, levels=unique(gofi_bias$Data))

# Plotting of data: corresponding to figures presented in the Supplemental Material
## NMAE (Figure S5)
plot_gofi_nmae <- ggplot(data=gofi, aes(x=Data, y=NMAE, colour="A")) + geom_point() + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=NMAE_bias, colour="B")) + 
  geom_point(data=gofi, mapping = aes(x=Data, y=NMAE_range, colour="C")) + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=NMAE_range_bias, colour="D")) + 
  geom_hline(yintercept=.5, linetype="dotted") + 
  scale_colour_manual(values=c("dodgerblue4","dodgerblue", "olivedrab", "olivedrab2"), 
                      aesthetics = "colour", name = "Indicator \ncalculation") + 
  ylim(0, 1.5) + ylab("NMAE") + facet_grid(CCAs~Measure) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot_gofi_nmae, file="Plot_NMAE_Fig_S5.jpg", width=7, height=5.2)

## NRMSE (Figure S6)
plot_gofi_nrmse <- ggplot(data=gofi, aes(x=Data, y=NRMSE, colour="A")) + geom_point() + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=NRMSE_bias, colour="B")) + 
  geom_point(data=gofi, mapping = aes(x=Data, y=NRMSE_range, colour="C")) + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=NRMSE_range_bias, colour="D")) + 
  geom_hline(yintercept=.5, linetype="dotted") + 
             scale_colour_manual(values=c("dodgerblue4","dodgerblue", "olivedrab", "olivedrab2"), 
                                 aesthetics = "colour", name = "Indicator \ncalculation") + 
             ylim(0, 1.5) + ylab("NRMSE") + facet_grid(CCAs~Measure) +  
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot_gofi_nrmse, file="Plot_NRMSE_Fig_S6.jpg", width=7, height=5.2)

## RSR (Figure S7)
plot_gofi_rsr <- ggplot(gofi, aes(x=Data, y=RSR, colour="A")) + geom_point() + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=RSR_bias, colour="B")) + 
  geom_point(data=gofi, mapping = aes(x=Data, y=RSR_range, colour="C")) + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=RSR_range_bias, colour="D")) + 
  geom_hline(yintercept=.7, linetype="dotted") + 
             scale_colour_manual(values=c("dodgerblue4","dodgerblue", "olivedrab", "olivedrab2"), 
                                 aesthetics = "colour", name = "Indicator \ncalculation") + 
             ylim(0, 3.1) + ylab("RSR") + facet_grid(CCAs~Measure) + 
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot_gofi_rsr, file="Plot_RSR_Fig_S7.jpg", width=7, height=5.2)
  
## Adequacy (Figure S8)
plot_gofi_adequacy <- ggplot(data=gofi, aes(x=Data, y=Adequacy, colour = "a")) + geom_point() + 
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=Adequacy_bias, colour = "b")) + 
             scale_colour_manual(values=c("orchid4", "orchid1"), aesthetics = "colour",
                                 name = "", breaks=c("a","b"), labels = c("Without correction", "Bias corrected")) + 
             ylim(0, 1) + ylab("Adequacy") + facet_grid(CCAs~Measure) + 
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot_gofi_adequacy, file="Plot_Adequacy_Fig_S8.jpg", width=7, height=5.2)
  
## Reliability (Figure S9)
plot_gofi_reliability <- ggplot(data=gofi, aes(x=Data, y=Reliability, color="a")) + geom_point() +  
  geom_point(data=gofi_bias, mapping = aes(x=Data, y=Reliability_bias, colour="b")) + 
             scale_colour_manual(values=c("goldenrod4", "goldenrod"), aesthetics = "colour",
                                 name = "", breaks=c("a","b"), labels = c("Without correction", "Bias corrected")) + 
             ylim(0, 1) + ylab("Reliability") + facet_grid(CCAs~Measure) + 
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot_gofi_reliability, file="Plot_Reliability_Fig_S9.jpg", width=7, height=5.2)
  