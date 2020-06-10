## Publication Plot: Figure 3
# outp_fp = file path where image will be written
# meas_fp = file path to measured_data.csv
# val_fp = file path to Validation_BEEHAVE_LSCFS_data.csv

## Input Values ---------------------------------------------------------------
outp_fp = ''
meas_fp = 'measured_data.csv'
val_fp = 'Validation_BEEHAVE_LSCFS_data.csv'

## Required Packages, constants, etc ------------------------------------------
library(plyr)

## Read CCA Data --------------------------------------------------------------
meas_df = read.csv(meas_fp) # read measured data
meas_df = meas_df[,c('colony', 'hive', 'cca', 'adult_num', 'honey_kg')]
meas_df$hive = substr(meas_df$hive, 1, 1)
meas_df = na.omit(meas_df)

## Read Validation Output -----------------------------------------------------
val_df = read.csv(val_fp, header = TRUE, stringsAsFactors = FALSE)
val_df = val_df[,c('CFS', 'Apiary', 'CCA', 'DOY',
  'AdultBees_BH_mean', 'Honey_BH_mean')]
colnames(val_df) = c('colony', 'hive', 'cca', 'doy', 'adult_num', 'honey_kg')
val_df$colony = mapvalues(val_df$colony,
  paste0('CFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')), 
  paste0('LSCFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')))
val_df = val_df[val_df$colony %in% c('LSCFS_2013_1', 'LSCFS_2014_1', 
  'LSCFS_2014_2', 'LSCFS_2016_1', 'LSCFS_2016_2'),]
val_df$cca_adult = val_df$cca_honey_kg = 0

## fill in CCA data for simulations
for(i in 1:nrow(val_df)){
  tmp_df = meas_df[meas_df$colony == val_df$colony[i] &
      meas_df$hive == val_df$hive[i] & meas_df$cca == val_df$cca[i],]
  
  if(nrow(tmp_df) > 0){
    val_df$cca_adult[i] = mean(tmp_df$adult)
    val_df$cca_honey_kg[i] = mean(tmp_df$honey_kg)
  }
  
  rm(tmp_df)
}

# remove zero and na values
val_df$cca_adult[is.na(val_df$cca_adult)] = 0
val_df = val_df[val_df$cca_adult > 0,]
val_df$cca_honey_kg[is.na(val_df$cca_honey_kg)] = 0
val_df = val_df[val_df$cca_honey_kg > 0,]

## Plotting -------------------------------------------------------------------
doys_plt = list('All dates' = NA, 'First year' = 191:365, 
  'October' = 285:364, 'Spring' = 1:189)

lim_a_plt = c(0, 44)
brk_a_plt = seq(-44, 44, 2)*1000
lab_a_plt = seq(-40, 40, 4)
tck_a_plt =  match(lab_a_plt*1000, brk_a_plt)-1

brk_h_plt = seq(-34, 34, 2)
tck_h_plt = seq(2, 34, 4)
lab_h_plt = brk_h_plt[tck_h_plt]

# Initialize plot
jpeg(file.path(outp_fp, 'Figure3.jpeg'), 
  width = 7, height = 7, units = 'in', res = 300)
layout(matrix(c(1:8), 4, 2, byrow = TRUE))
par(oma = c(3, 3, 2, 0.25), mar = c(1, 3.25, 0.5, 0.5))

for(doys in names(doys_plt)){
  if(doys != 'All dates'){
    tmp_df = val_df[val_df$doy %in% doys_plt[[doys]],]
  } else {
    tmp_df = val_df
  }
  
  # Adult Bees 
  tmp_plt = with(tmp_df, 
    hist(adult_num-cca_adult, breaks = brk_a_plt, plot = F))
  plot(NA, ylim = c(0, 50), xlim = lim_a_plt, xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  mtext(doys, side = 2, line = 4.75, cex = 0.75, font = 2) # label
  if(doys == 'Spring'){ # x axis
    mtext('Deviation of adult bees x1000', side = 1, line = 2.25, cex = 0.75)
    axis(1, at = tck_a_plt, labels = lab_a_plt, las = 2, cex = 0.75, las = 1)
  }
  mtext('Frequency', side = 2, line = 2.5, cex = 0.75) # y axis label
  axis(2, at = seq(0, 5)*10, las = 2, cex = 0.75) # y axis marker
  abline(h = seq(1, 4)*10, col = '#D3D3D3', lty = 3) # y grid
  abline(v = tck_a_plt, col = '#D3D3D3', lty = 3) # x 
  abline(v = 22, col = '#000000', lty = 3)
  barplot(tmp_plt$counts, width = 1, space = 0, col = 'white', add = TRUE,
    xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  
  # Honey 
  tmp_plt = with(tmp_df, hist(honey_kg-cca_honey_kg, 
    breaks = brk_h_plt, plot = FALSE))
  plot(NA, ylim = c(0, 50), xlim = c(1, 35), xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  if(doys == 'Spring'){ # x axis
    mtext('Deviation of honey (kg)', side = 1, line = 2.25, cex = 0.75)
    axis(1, at = tck_h_plt, labels = lab_h_plt, las = 2, cex = 0.75, las = 1)
  }
  mtext('Frequency', side = 2, line = 2.5, cex = 0.75) # y axis label
  axis(2, at = seq(0, 5)*10, las = 2, cex = 0.75) # y axis marker
  abline(h = seq(1, 4)*10, col = '#D3D3D3', lty = 3) # y grid
  abline(v = tck_h_plt, col = '#D3D3D3', lty = 3) # x 
  abline(v = 18, col = '#000000', lty = 3)
  barplot(tmp_plt$counts, width = 1, space = 0, col = 'white', add = TRUE,
    xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
}

dev.off()
