## Publication Plot: Figure 2
# outp_fp = file path where image will be written
# meas_fp = file path to measured_data.csv
# val_fp = file path to Validation_BEEHAVE_LSCFS_data.csv

## Input Values ---------------------------------------------------------------
outp_fp = ''
meas_fp = 'measured_data.csv'
val_fp = 'Validation_BEEHAVE_LSCFS_data.csv'

## Required Packages, constants, etc ------------------------------------------
library(plyr)

## Measured Data --------------------------------------------------------------
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
col_plt = colorRampPalette(c('#ADD8E6', '#00008B'))
col_plt = col_plt(length(min(val_df$doy):max(val_df$doy)))
names(col_plt) = min(val_df$doy):max(val_df$doy)

doys_plt = list('All dates' = NA, 'First year' = 191:365, 
  'October' = 285:364, 'Spring' = 1:189)

# Initialize plot
jpeg(file.path(outp_fp, 'Figure2.jpeg'), 
  width = 7, height = 7, units = 'in', res = 300)
layout(matrix(c(1:5,3,6:7,3,8:9,3), 4, 3, byrow = TRUE), widths = c(1, 1, 0.25))
par(oma = c(3, 3, 2, 0.25), mar = c(1, 3.25, 0.5, 0.5))

for(doys in names(doys_plt)){
  if(doys != 'All dates'){
    tmp_df = val_df[val_df$doy %in% doys_plt[[doys]],]
  } else {
    tmp_df = val_df
  }
  
  # Adult Bees 
  plot(NA, ylim = c(0, 6)*10000, xlim = c(0, 6)*10000, xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  mtext(doys, side = 2, line = 4.75, cex = 0.75, font = 2) # label
  if(doys == 'Spring'){
    mtext('# Adult bees in BEEHAVE', side = 1, line = 2.25, cex = 0.75) # x axis
    axis(1, at = seq(0, 6)*10000, las = 2, cex = 0.75, las = 1) # x marker
  }
  mtext('# Adult bees in LSCFS', side = 2, line = 3.5, cex = 0.75) # y label
  axis(2, at = seq(0, 6)*10000, las = 2, cex = 0.75) # y axis marker
  abline(h = seq(1, 5)*10000, col = '#D3D3D3', lty = 3) # y grid
  abline(v = seq(1, 5)*10000, col = '#D3D3D3', lty = 3) # x grid
  abline(a = 0, b = 1) # 1:1 line
  with(tmp_df, points(adult, cca_adult, # data
    type = 'p', pch = 16, col = col_plt[as.character(tmp_df$doy)]))
  
  # Honey 
  plot(NA, ylim = c(0, 6)*10, xlim = c(0, 60), xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  if(doys == 'Spring'){
    mtext('Honey (kg) in BEEHAVE', side = 1, line = 2.25, cex = 0.75) # x axis
    axis(1, at = seq(0, 6)*10, las = 2, cex = 0.75, las = 1) # x marker
  }
  mtext('Honey (kg) in LSCFS', side = 2, line = 2.5, cex = 0.75) # y axis label
  axis(2, at = seq(0, 6)*10, las = 2, cex = 0.75) # y axis marker
  abline(h = seq(1, 5)*10, col = '#D3D3D3', lty = 3) # y grid
  abline(v = seq(1, 5)*10, col = '#D3D3D3', lty = 3) # x grid
  abline(a = 0, b = 1) # 1:1 line
  with(tmp_df, points(honey_kg, cca_honey_kg, # data
    type = 'p', pch = 16, col = col_plt[as.character(tmp_df$doy)]))
  
  if(doys == 'All dates'){ # Legend
    par(mai = c(0.25, 0, 0.25, 0)) # new margins
    
    plot(c(0, 2), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '',
      main = 'DOY')
    text(x = 1.5, y = seq(0, 1, l = 5), 
      labels = round(seq(max(val_df$doy), min(val_df$doy), l = 5)))
    rasterImage(as.raster(matrix(col_plt, ncol = 1)), 0, 0, 1, 1)
    
    par(mar = c(1, 3.25, 0.5, 0.5)) # reset margins
  }
}

dev.off()
