## Publication Plot: Figure 1
# sim_fp = file path to output from BEEHAVE runs
# fun_fp = file path to read_beehave.R
# rmeas_fp = file path to measured_data_range.csv

## Input Values ---------------------------------------------------------------
sim_fp = ''
fun_fp = 'read_beehave.R'
rmeas_fp = 'measured_data_range.csv'

## Required Packages, constants, etc ------------------------------------------
library(plyr)
source(fun_fp) # read beehave output function

## Measured data --------------------------------------------------------------
rmeas_df = read.csv(rmeas_fp) # read measured ranges
rmeas_df$date = as.Date(rmeas_df$date) # correct date
colnames(rmeas_df) = c('colony', 'hive', 'cca', 'date', 'doy',
  'AdultBees_CFS_low', 'AdultBees_CFS_mean', 'AdultBees_CFS_high',
  'Honey_CFS_low', 'Honey_CFS_mean', 'Honey_CFS_high',
  'Pupae_CFS_low', 'Pupae_CFS_mean', 'Pupae_CFS_high')

## Modeled Data ---------------------------------------------------------------
plt_colonies = c('LSCFS_2013_1', 'LSCFS_2014_1', 'LSCFS_2014_2',
  'LSCFS_2015_1', 'LSCFS_2015_2', 'LSCFS_2016_1', 'LSCFS_2016_2')
sim_df = data.frame(colony = character(), hive = character(), # empty df
  date = as.Date(character()), adults = numeric(), honey_kg = numeric(), 
  stringsAsFactors = FALSE)

sim_df = data.frame(colony = character(), hive = character(), # empty df
  date = as.Date(character()), ihbees = numeric(), foragers = numeric(),
  adults = numeric(), eggs = numeric(), larvae = numeric(), pupae = numeric(),
  honey_kg = numeric(), pollen_g = numeric(), stringsAsFactors = FALSE)
for(colony in plt_colonies){
  for(hive in LETTERS[1:12]){
    tmp_df = 
      read_beehave(file.path(sim_fp, paste0(colony, '-', hive, '.csv')))
    tmp_df$colony = colony # site label
    tmp_df$hive = hive # site label
    yr = as.numeric(strsplit(colony[1], '_')[[1]][2])
    tmp_df$date = as.Date(paste0(yr-1, '-12-31')) + tmp_df$step
    
    sim_df = rbind(sim_df, tmp_df[colnames(sim_df)]) # bind together
    rm(yr, tmp_df) # remove temp data
  }
}
rm(colony, hive, read_beehave)

# summarize
sim_df = ddply(sim_df, .(colony, hive, date), summarize,
  AdultBees_BH_min = min(adults, na.rm = TRUE), 
  AdultBees_BH_mean = mean(adults, na.rm = TRUE), 
  AdultBees_BH_max = max(adults, na.rm = TRUE), 
  Honey_BH_min = min(honey_kg, na.rm = TRUE),
  Honey_BH_mean = mean(honey_kg, na.rm = TRUE),
  Honey_BH_max = max(honey_kg, na.rm = TRUE),
  Pupae_BH_min = min(pupae, na.rm = TRUE),
  Pupae_BH_mean = mean(pupae, na.rm = TRUE),
  Pupae_BH_max = max(pupae, na.rm = TRUE))
sim_df = sim_df[order(sim_df$date),] # ensure ordered by date

## sim_df Data ---------------------------------------------------------------
comb_df = merge(rmeas_df, sim_df)
rm(rmeas_df, sim_df)

write.csv(comb_df, 'Validation_BEEHAVE_LSCFS_data.csv', row.names = FALSE)
