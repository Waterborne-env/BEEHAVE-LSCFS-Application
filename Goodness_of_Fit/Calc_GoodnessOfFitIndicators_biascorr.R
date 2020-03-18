## Validation of BEEHAVE model with data from honey bee large-scale colony feeding studies
## SChmolke et al., submitted
## Script for the application of goodness-of-fit indicators to BEEHAVE simulation outputs
## Creator: Amelie Schmolke

rm(list = ls())

# All scripts and data are located in the same folder: the path may have to adjusted 
home.dir <- 'C:/BEEHAVEModelValidation/Validation/'
setwd(home.dir)

# Script defining the functions for the calculations of goodness-of-fit measures with correction for bias in BEEHAVE outputs
source(file = "GoodnessOfFitIndicators_functions_biascorr.R")

# Reading data that combines BEEHAVE outputs and measurements from LSCFS
# Note study IDs in the data set: 'CFS_<year>_<number>' instead of 'LSCFS_<year>_<number>' 
BHout <- read.csv("Validation_BEEHAVE_LSCFS_data.csv", header = TRUE)
BHout$AdultBees_CFS_mean[is.na(BHout$AdultBees_CFS_mean)] <- 0
BHout <- BHout[BHout$AdultBees_CFS_mean > 0,]
BHout$AdultBees_CFS_mean[is.na(BHout$Honey_CFS_mean)] <- 0
BHout <- BHout[BHout$Honey_CFS_mean > 0,]

# Analysis of all data (combined from all studies)
# Temporal subset: fall only
fall <- subset(BHout, DOY > 284 & DOY < 365)
# Temporal subset: first year (data set without spring following overwintering)
season <- subset(BHout, DOY > 190)
# Temporal subset: only spring following overwintering
spring <- subset(BHout, DOY < 190)

# Goodness-of-fit measures for adult bees
# Complete data set (no temporal subset)
prefmeasures <- data.frame()
prefmeasures <- rbind(prefmeasures, c("All", "All", "AdultBees", mean(BHout$AdultBees_CFS_mean), mean(BHout$AdultBees_BH_mean),
                           GOFM_bias(BHout$AdultBees_CFS_mean, BHout$AdultBees_BH_mean,
                                BHout$AdultBees_CFS_low, BHout$AdultBees_CFS_high, 
                                BHout$AdultBees_BH_low, BHout$AdultBees_BH_high)))
colnames(prefmeasures) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                            "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                            "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias", 
                            "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
# First-year subset 
row <- data.frame()
row <- rbind(row, c("All", "Season", "AdultBees", mean(season$AdultBees_CFS_mean), mean(season$AdultBees_BH_mean), 
            GOFM_bias(season$AdultBees_CFS_mean, season$AdultBees_BH_mean,
             season$AdultBees_CFS_low, season$AdultBees_CFS_high, 
             season$AdultBees_BH_low, season$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                            "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                            "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                            "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Fall subset 
row <- data.frame()
row <- rbind(row, c("All", "Fall", "AdultBees", mean(fall$AdultBees_CFS_mean), mean(fall$AdultBees_BH_mean),
                                      GOFM_bias(fall$AdultBees_CFS_mean, fall$AdultBees_BH_mean,
                                           fall$AdultBees_CFS_low, fall$AdultBees_CFS_high, 
                                           fall$AdultBees_BH_low, fall$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Spring subset 
row <- data.frame()
row <- rbind(row, c("All", "Spring", "AdultBees", mean(spring$AdultBees_CFS_mean), mean(spring$AdultBees_BH_mean),
                                      GOFM_bias(spring$AdultBees_CFS_mean, spring$AdultBees_BH_mean,
                                           spring$AdultBees_CFS_low, spring$AdultBees_CFS_high, 
                                           spring$AdultBees_BH_low, spring$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)

# Goodness-of-fit measures for honey
# Complete data set (no temporal subset)
row <- data.frame()
row <- rbind(row, c("All", "All", "Honey", mean(BHout$Honey_CFS_mean), mean(BHout$Honey_BH_mean),
                           GOFM_bias(BHout$Honey_CFS_mean, BHout$Honey_BH_mean,
                                BHout$Honey_CFS_low, BHout$Honey_CFS_high, 
                                BHout$Honey_BH_low, BHout$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# First-year subset 
row <- data.frame()
row <- rbind(row, c("All", "Season", "Honey", mean(season$Honey_CFS_mean), mean(season$Honey_BH_mean),
                                      GOFM_bias(season$Honey_CFS_mean, season$Honey_BH_mean,
                                           season$Honey_CFS_low, season$Honey_CFS_high, 
                                           season$Honey_BH_low, season$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Fall subset 
row <- data.frame()
row <- rbind(row, c("All", "Fall", "Honey", mean(fall$Honey_CFS_mean), mean(fall$Honey_BH_mean),
                                      GOFM_bias(fall$Honey_CFS_mean, fall$Honey_BH_mean,
                                           fall$Honey_CFS_low, fall$Honey_CFS_high, 
                                           fall$Honey_BH_low, fall$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Spring subset 
row <- data.frame()
row <- rbind(row, c("All", "Spring", "Honey", mean(spring$Honey_CFS_mean), mean(spring$Honey_BH_mean),
                                      GOFM_bias(spring$Honey_CFS_mean, spring$Honey_BH_mean,
                                           spring$Honey_CFS_low, spring$Honey_CFS_high, 
                                           spring$Honey_BH_low, spring$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)

# Validation studies (studies not included in calibration): LSCFS_2013_1, LSCFS_2014_1, LSCFS_2014_2, LSCFS_2016_1, LSCFS_2016_2)
BHout_val <- subset(BHout, CFS != "CFS_2015_1" & CFS != "CFS_2015_2")
# Temporal subset: fall only
fall_val <- subset(fall, CFS != "CFS_2015_1" & CFS != "CFS_2015_2")
# Temporal subset: first year (data set without spring following overwintering)
season_val <- subset(season, CFS != "CFS_2015_1" & CFS != "CFS_2015_2")
# Temporal subset: only spring following overwintering
spring_val <- subset(spring, CFS != "CFS_2015_1" & CFS != "CFS_2015_2")

# Goodness-of-fit measures for adult bees
# Complete data set (no temporal subset)
row <- data.frame()
row <- rbind(row, c("Validation", "All", "AdultBees", mean(BHout_val$AdultBees_CFS_mean), mean(BHout_val$AdultBees_BH_mean),
                                      GOFM_bias(BHout_val$AdultBees_CFS_mean, BHout_val$AdultBees_BH_mean,
                                           BHout_val$AdultBees_CFS_low, BHout_val$AdultBees_CFS_high, 
                                           BHout_val$AdultBees_BH_low, BHout_val$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                            "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                            "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                            "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# First-year subset 
row <- data.frame()
row <- rbind(row, c("Validation", "Season", "AdultBees", mean(season_val$AdultBees_CFS_mean), mean(season_val$AdultBees_BH_mean), 
                    GOFM_bias(season_val$AdultBees_CFS_mean, season_val$AdultBees_BH_mean,
                         season_val$AdultBees_CFS_low, season_val$AdultBees_CFS_high, 
                         season_val$AdultBees_BH_low, season_val$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Fall subset 
row <- data.frame()
row <- rbind(row, c("Validation", "Fall", "AdultBees", mean(fall_val$AdultBees_CFS_mean), mean(fall_val$AdultBees_BH_mean),
                    GOFM_bias(fall_val$AdultBees_CFS_mean, fall_val$AdultBees_BH_mean,
                         fall_val$AdultBees_CFS_low, fall_val$AdultBees_CFS_high, 
                         fall_val$AdultBees_BH_low, fall_val$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Spring subset 
row <- data.frame()
row <- rbind(row, c("Validation", "Spring", "AdultBees", mean(spring_val$AdultBees_CFS_mean), mean(spring_val$AdultBees_BH_mean),
                    GOFM_bias(spring_val$AdultBees_CFS_mean, spring_val$AdultBees_BH_mean,
                         spring_val$AdultBees_CFS_low, spring_val$AdultBees_CFS_high, 
                         spring_val$AdultBees_BH_low, spring_val$AdultBees_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)

# Goodness-of-fit measures for honey
# Complete data set (no temporal subset)
row <- data.frame()
row <- rbind(row, c("Validation", "All", "Honey", mean(BHout_val$Honey_CFS_mean), mean(BHout_val$Honey_BH_mean),
                    GOFM_bias(BHout_val$Honey_CFS_mean, BHout_val$Honey_BH_mean,
                         BHout_val$Honey_CFS_low, BHout_val$Honey_CFS_high, 
                         BHout_val$Honey_BH_low, BHout_val$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# First-year subset 
row <- data.frame()
row <- rbind(row, c("Validation", "Season", "Honey", mean(season_val$Honey_CFS_mean), mean(season_val$Honey_BH_mean),
                    GOFM_bias(season_val$Honey_CFS_mean, season_val$Honey_BH_mean,
                         season_val$Honey_CFS_low, season_val$Honey_CFS_high, 
                         season_val$Honey_BH_low, season_val$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Fall subset 
row <- data.frame()
row <- rbind(row, c("Validation", "Fall", "Honey", mean(fall_val$Honey_CFS_mean), mean(fall_val$Honey_BH_mean),
                    GOFM_bias(fall_val$Honey_CFS_mean, fall_val$Honey_BH_mean,
                         fall_val$Honey_CFS_low, fall_val$Honey_CFS_high, 
                         fall_val$Honey_BH_low, fall_val$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)
# Spring subset 
row <- data.frame()
row <- rbind(row, c("Validation", "Spring", "Honey", mean(spring_val$Honey_CFS_mean), mean(spring_val$Honey_BH_mean),
                    GOFM_bias(spring_val$Honey_CFS_mean, spring_val$Honey_BH_mean,
                         spring_val$Honey_CFS_low, spring_val$Honey_CFS_high, 
                         spring_val$Honey_BH_low, spring_val$Honey_BH_high)))
colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                   "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                   "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                   "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
prefmeasures <- rbind(prefmeasures, row)

## Study-specific calculations
study_names <- c("CFS_2013_1","CFS_2014_1","CFS_2014_2","CFS_2015_1","CFS_2015_2","CFS_2016_1","CFS_2016_2")
for(i in 1:length(study_names)){
  CFS_name <- study_names[i]
  full_dataset <- subset(BHout, CFS == CFS_name)
  season_dataset <- subset(season, CFS == CFS_name)
  fall_dataset <- subset(fall, CFS == CFS_name)
  spring_dataset <- subset(spring, CFS == CFS_name)
  
  # Goodness-of-fit measures for adult bees
  # Complete data set (no temporal subset)
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "All", "AdultBees", mean(full_dataset$AdultBees_CFS_mean), mean(full_dataset$AdultBees_BH_mean),
                                        GOFM_bias(full_dataset$AdultBees_CFS_mean, full_dataset$AdultBees_BH_mean,
                                             full_dataset$AdultBees_CFS_low, full_dataset$AdultBees_CFS_high, 
                                             full_dataset$AdultBees_BH_low, full_dataset$AdultBees_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  # First-year subset
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "Season", "AdultBees", mean(season_dataset$AdultBees_CFS_mean), mean(season_dataset$AdultBees_BH_mean),
                                        GOFM_bias(season_dataset$AdultBees_CFS_mean, season_dataset$AdultBees_BH_mean,
                                             season_dataset$AdultBees_CFS_low, season_dataset$AdultBees_CFS_high, 
                                             season_dataset$AdultBees_BH_low, season_dataset$AdultBees_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  # Fall subset
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "Fall", "AdultBees", mean(fall_dataset$AdultBees_CFS_mean), mean(fall_dataset$AdultBees_BH_mean),
                                        GOFM_bias(fall_dataset$AdultBees_CFS_mean, fall_dataset$AdultBees_BH_mean,
                                             fall_dataset$AdultBees_CFS_low, fall_dataset$AdultBees_CFS_high, 
                                             fall_dataset$AdultBees_BH_low, fall_dataset$AdultBees_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  # Spring subset
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "Spring", "AdultBees", mean(spring_dataset$AdultBees_CFS_mean), mean(spring_dataset$AdultBees_BH_mean),
                                        GOFM_bias(spring_dataset$AdultBees_CFS_mean, spring_dataset$AdultBees_BH_mean,
                                             spring_dataset$AdultBees_CFS_low, spring_dataset$AdultBees_CFS_high, 
                                             spring_dataset$AdultBees_BH_low, spring_dataset$AdultBees_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  
  # Goodness-of-fit measures for honey
  # Complete data set (no temporal subset)
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "All", "Honey", mean(full_dataset$Honey_CFS_mean), mean(full_dataset$Honey_BH_mean),
                                        GOFM_bias(full_dataset$Honey_CFS_mean, full_dataset$Honey_BH_mean,
                                             full_dataset$Honey_CFS_low, full_dataset$Honey_CFS_high, 
                                             full_dataset$Honey_BH_low, full_dataset$Honey_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  # First-year subset
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "Season", "Honey", mean(season_dataset$Honey_CFS_mean), mean(season_dataset$Honey_BH_mean),
                                        GOFM_bias(season_dataset$Honey_CFS_mean, season_dataset$Honey_BH_mean,
                                             season_dataset$Honey_CFS_low, season_dataset$Honey_CFS_high, 
                                             season_dataset$Honey_BH_low, season_dataset$Honey_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  # Fall subset
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "Fall", "Honey", mean(fall_dataset$Honey_CFS_mean), mean(fall_dataset$Honey_BH_mean),
                                        GOFM_bias(fall_dataset$Honey_CFS_mean, fall_dataset$Honey_BH_mean,
                                             fall_dataset$Honey_CFS_low, fall_dataset$Honey_CFS_high, 
                                             fall_dataset$Honey_BH_low, fall_dataset$Honey_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel",
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
  # Spring subset
  row <- data.frame()
  row <- rbind(row, c(CFS_name, "Spring", "Honey", mean(spring_dataset$Honey_CFS_mean), mean(spring_dataset$Honey_BH_mean),
                                        GOFM_bias(spring_dataset$Honey_CFS_mean, spring_dataset$Honey_BH_mean,
                                             spring_dataset$Honey_CFS_low, spring_dataset$Honey_CFS_high, 
                                             spring_dataset$Honey_BH_low, spring_dataset$Honey_BH_high)))
  colnames(row) <- c("Data", "CCAs", "Measure","CFS_mean", "BH_mean", "Bias", "Bias_rel", 
                     "MAE_bias", "NMAE_bias", "RMSE_bias", "NRMSE_bias", "NSE_bias", "RSR_bias",
                     "Bias_range", "MAE_range_bias", "NMAE_range_bias", "RMSE_range_bias", "NRMSE_range_bias", "NSE_range_bias", "RSR_range_bias",
                     "Overlap_range_bias", "Adequacy_bias", "Reliability_bias")
  prefmeasures <- rbind(prefmeasures, row)
}

write.csv(prefmeasures, file = "GoodnessOfFitIndicators_LSCFS_biascorr.csv")
