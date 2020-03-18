## Publication Plot: Figure 1
# outp_fp = file path where image will be written
# sim_fp = file path to output from BEEHAVE runs
# fun_fp = file path to read_beehave.R
# meas_fp = file path to measured_data.csv
# rmeas_fp = file path to measured_data_range.csv

## Input Values ---------------------------------------------------------------
outp_fp = ''
sim_fp = ''
fun_fp = 'read_beehave.R'
meas_fp = 'measured_data.csv'
rmeas_fp = 'measured_data_range.csv'

## Required Packages, constants, etc ------------------------------------------
library(plyr)
library(scales)
source(fun_fp) # read beehave output function

## Measured data --------------------------------------------------------------
meas_df = read.csv(meas_fp) # read measured data
meas_df$date = as.Date(meas_df$date) # correct date
meas_df$pollen_kg = meas_df$pollen_g * 0.001 # convert pollen units

rmeas_df = read.csv(rmeas_fp) # read measured ranges
rmeas_df$date = as.Date(rmeas_df$date) # correct date
rmeas_df = ddply(rmeas_df, .(colony, cca), summarize,
  date = mean(date, na.rm = TRUE), # summarize by study
  adult_num_high = max(adult_num_high, na.rm = TRUE),
  adult_num_low = min(adult_num_low, na.rm = TRUE),
  pupae_num_high = max(pupae_num_high, na.rm = TRUE),
  pupae_num_low = min(pupae_num_low, na.rm = TRUE),
  honey_kg_high = max(honey_kg_high, na.rm = TRUE),
  honey_kg_low = min(honey_kg_low, na.rm = TRUE))
rmeas_df = rmeas_df[order(rmeas_df$date),] # ensure ordered by date

## Modeled Data ---------------------------------------------------------------
plt_colonies = c('LSCFS_2013_1', 'LSCFS_2014_1', 'LSCFS_2014_2',
  'LSCFS_2016_1', 'LSCFS_2016_2')
sim_df = data.frame(colony = character(), hive = character(), # empty df
  date = as.Date(character()), adults = numeric(), honey_kg = numeric(), 
  stringsAsFactors = FALSE)
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
sim_df = ddply(sim_df, .(colony, date), summarize,
  adults_max = max(adults, na.rm = TRUE), 
  adults_min = min(adults, na.rm = TRUE), 
  honey_kg_max = max(honey_kg, na.rm = TRUE),
  honey_kg_min = min(honey_kg, na.rm = TRUE))
sim_df = sim_df[order(sim_df$date),] # ensure ordered by date

## Plotting -------------------------------------------------------------------
col_plt = c('Adults'='#7570b3', 'Honey (kg)'='#1b9e77')

# Initialize plot
jpeg(file.path(outp_fp, 'Figure1.jpeg'), 
  width = 7, height = 7, units = 'in', res = 300)
layout(matrix(c(1:10), 5, 2, byrow = TRUE))
par(oma = c(1, 3, 0, 0.25), mar = c(1, 3.25, 0.5, 0.5))

for(colony in plt_colonies){
  tmp_sim_df = sim_df[sim_df$colony == colony,] # subset simulated data
  tmp_sim_df = tmp_sim_df[order(tmp_sim_df$date),] # ensure ordered by date
  
  tmp_meas_df = meas_df[meas_df$colony == colony,] # subset measured data
  tmp_meas_df = tmp_meas_df[order(tmp_meas_df$date),] # ensure ordered by date
  
  tmp_rmeas_df = rmeas_df[rmeas_df$colony == colony,] # subset meas range data
  tmp_rmeas_df = tmp_rmeas_df[order(tmp_rmeas_df$date),] # ensure order by date
  
  cal_date = tmp_meas_df$date[which(tmp_meas_df$cca == 
      ifelse(tmp_meas_df$colony %in% c('LSCFS_2016_1', 'LSCFS_2016_2'),
        'CCA2', 'CCA3'))]

  # x axis info
  lim_x_plt = seq(as.Date(paste0(strsplit(colony[1], '_')[[1]][2], '-05-01')), 
    by = '1 year', length.out=2)
  tck_x_plt = seq(lim_x_plt[1], lim_x_plt[2], by = '1 month')

  # Adult Bees
  plot(NA, ylim = c(0, 60000), xlim = lim_x_plt, xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  mtext(colony, side = 2, line = 4.75, cex = 0.75, font = 2) # label
  mtext('# Adult Bees', side = 2, line = 3.5, cex = 0.5) # y axis label
  axis(2, at = seq(0, 60000, 10000), las = 2, cex = 0.75) # y axis marker
  if(colony == 'LSCFS_2016_2'){ # x only on bottom
    axis.Date(1, at = lim_x_plt, format = '%b\n', tick = FALSE)
    mtext('Months', side = 1, line = 1, cex = 0.75) # x axis label
  }
  abline(h = seq(10000, 50000, 10000), col = '#D3D3D3', lty = 3) # grid
  abline(v = tck_x_plt[2:(length(tck_x_plt)-1)], col = '#D3D3D3', lty = 3)
  
  abline(v = cal_date, col = '#000000', lty = 3) # calibration start
  
  # simulated ranges
  with(tmp_sim_df, polygon(c(date, rev(date)), c(adults_max, rev(adults_min)),
    col = alpha(col_plt[['Adults']], 0.5), border = NA))

  # measured range
  with(tmp_rmeas_df, segments(date, adult_num_low, 
    date, adult_num_high, col_plt[['Adults']]))
  with(tmp_rmeas_df, segments(date-3, adult_num_low, 
    date+3, adult_num_low, col_plt[['Adults']]))
  with(tmp_rmeas_df, segments(date-3, adult_num_high, 
    date+3, adult_num_high, col_plt[['Adults']]))
  
  # measured points
  with(tmp_meas_df, points(date, adult_num, 
    type = 'p', pch = 16, col = col_plt[['Adults']]))
  
  # Honey Storage
  plot(NA, ylim = c(0, 60), xlim = lim_x_plt, xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  axis(2, at = seq(0, 60, 10), las = 2, cex = 0.75) # y axis marker
  mtext('Honey (kg)', side = 2, line = 2.5, cex = 0.75) # y axis label
  if(colony == 'LSCFS_2016_2'){ # x only on bottom
    axis.Date(1, at = lim_x_plt, format = '%b\n', tick = FALSE)
    mtext('Months', side = 1, line = 1, cex = 0.75) # x axis label
  } # x axis label
  abline(h = seq(10, 50, 10), col = '#D3D3D3', lty = 3) # y grid
  abline(v = tck_x_plt[2:(length(tck_x_plt)-1)], col = '#D3D3D3', lty = 3)
  
  abline(v = cal_date, col = '#000000', lty = 3) # start of calibration
  
  # simulated ranges
  with(sim_df, polygon(c(date, rev(date)), c(honey_kg_max, rev(honey_kg_min)),
    col = alpha(col_plt[['Honey (kg)']], 0.5), border = NA))
  
  # measured range
  with(tmp_rmeas_df, segments(date, honey_kg_low, 
    date, honey_kg_high, col_plt[['Honey (kg)']]))
  with(tmp_rmeas_df, segments(date-3, honey_kg_low, 
    date+3, honey_kg_low, col_plt[['Honey (kg)']]))
  with(tmp_rmeas_df, segments(date-3, honey_kg_high, 
    date+3, honey_kg_high, col_plt[['Honey (kg)']]))
  
  # measured dots
  with(tmp_meas_df, points(date, honey_kg, 
    type = 'p', pch = 16, col = col_plt[['Honey (kg)']]))
}
dev.off()
