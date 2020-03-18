## Publication Plot: Figure 4
# outp_fp = file path where image will be written
# gofi_fp = file path to output from Calc_GoodnessOfFitIndicators.R
# bias_fp = file path to output from Calc_GoodnessOfFitIndicators_biascorr.R

## Input Values ---------------------------------------------------------------
outp_fp = ''
gofi_fp = 'GoodnessOfFitIndicators_LSCFS.csv'
bias_fp = 'GoodnessOfFitIndicators_LSCFS_biascorr.csv'

## Required Packages, constants, etc ------------------------------------------
library(plyr)

## Process Measured Data ------------------------------------------------------
gofi_df = read.csv(gofi_fp, stringsAsFactors = FALSE)
gofi_df$Data = mapvalues(gofi_df$Data,
  paste0('CFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')), 
  paste0('LSCFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')))
gofi_df = gofi_df[gofi_df$Data != 'All' & gofi_df$CCAs == 'Season',]

bias_df = read.csv(bias_fp, stringsAsFactors = FALSE)
bias_df$Data = mapvalues(bias_df$Data,
  paste0('CFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')), 
  paste0('LSCFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')))
bias_df = bias_df[bias_df$Data != 'All' & bias_df$CCAs == 'Season',]

gofi_df = merge(gofi_df, bias_df, by = c('Data', 'CCAs', 'Measure'))
gofi_df$Data = factor(gofi_df$Data, ordered = TRUE, 
  levels = c('Validation', 'LSCFS_2013_1', 'LSCFS_2014_1', 'LSCFS_2014_2', 
    'LSCFS_2015_1', 'LSCFS_2015_2', 'LSCFS_2016_1', 'LSCFS_2016_2'))
rm(bias_df)

## Plotting -------------------------------------------------------------------
col_plt = c('Ia'='#104E8B', 'Ib'='#1E90FF', 'IIa'='#6B8E23', 'IIb'='#B3EE3A')

# Initialize plot
jpeg(file.path(outp_fp, 'Figure4.jpeg'), 
  width = 7, height = 5, units = 'in', res = 300)
layout(matrix(c(1:7,7), 4, 2, byrow = TRUE))
par(oma = c(0, 3, 2, 0.25), mar = c(0.5, 3.25, 0.5, 0.5))

for(i in c('NMAE', 'NRMSE', 'RSR')){ # plot each stat
  # y axis limits
  if(i == 'RSR'){
    lim_y_plt = c(-0.05, 2.55)
    tck_y_plt = seq(0, 2.5, 0.5)
  } else {
    lim_y_plt = c(-0.05, 1.05)
    tck_y_plt = seq(0, 1.05, 0.2)
  }
  
  # get data
  adt_gofi_df = gofi_df[gofi_df$Measure == 'AdultBees',
    c('Data', paste0(i, c('', '_bias', '_range', '_range_bias')))]
  colnames(adt_gofi_df) = c('Data', 'stat', 'bias', 'range', 'range_bias')
  hon_gofi_df = gofi_df[gofi_df$Measure == 'Honey',
    c('Data', paste0(i, c('', '_bias', '_range', '_range_bias')))]
  colnames(hon_gofi_df) = c('Data', 'stat', 'bias', 'range', 'range_bias')
  
  # Adult Bees
  plot(NA, ylim = lim_y_plt, xlim = c(0.5, 8.5), xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  if(i == 'NMAE'){
    mtext('# Adult Bees', side = 3, line = 1, cex = 0.75, font = 2)} # label
  axis(2, at = tck_y_plt, las = 2, cex = 0.75) # y axis marker
  mtext(i, side = 2, line = 3.5, cex = 0.75) # y axis label
  if(i == 'RSR') axis(1, at = 1:8, labels = levels(gofi_df$Data), las = 2)
  abline(h = tck_y_plt[2:(length(tck_y_plt)-1)], col = '#D3D3D3', lty = 3)
  abline(v = 2:7, col = '#D3D3D3', lty = 3) # x grid
  if(i == 'NRMSE') abline(h = 0.5, lty = 5) 
  if(i == 'RSR') abline(h = 0.7, lty = 5) 
  
  with(adt_gofi_df, points(Data, stat, 
    type = 'p', pch = 16, col = col_plt[['Ia']]))
  with(adt_gofi_df, points(Data, bias, 
    type = 'p', pch = 16, col = col_plt[['Ib']]))
  with(adt_gofi_df, points(Data, range, 
    type = 'p', pch = 16, col = col_plt[['IIa']]))
  with(adt_gofi_df, points(Data, range_bias, 
    type = 'p', pch = 16, col = col_plt[['IIb']]))
  
  # Honey
  plot(NA, ylim = lim_y_plt, xlim = c(0.5, 8.5), xaxt = 'n', yaxt = 'n',
    xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  if(i == 'NMAE'){
    mtext('Honey (kg)', side = 3, line = 1, cex = 0.75, font = 2)} # label
  axis(2, at = tck_y_plt, las = 2, cex = 0.75) # y axis marker
  if(i == 'RSR') axis(1, at = 1:8, labels = levels(gofi_df$Data), las = 2)
  abline(h = tck_y_plt[2:(length(tck_y_plt)-1)], col = '#D3D3D3', lty = 3)
  abline(v = 2:7, col = '#D3D3D3', lty = 3) # x grid
  if(i == 'NRMSE') abline(h = 0.5, lty = 5) 
  if(i == 'RSR') abline(h = 0.7, lty = 5) 
  
  with(hon_gofi_df, points(Data, stat, 
    type = 'p', pch = 16, col = col_plt[['Ia']]))
  with(hon_gofi_df, points(Data, bias, 
    type = 'p', pch = 16, col = col_plt[['Ib']]))
  with(hon_gofi_df, points(Data, range, 
    type = 'p', pch = 16, col = col_plt[['IIa']]))
  with(hon_gofi_df, points(Data, range_bias, 
    type = 'p', pch = 16, col = col_plt[['IIb']]))
  
  
}

# plot legend
par(mai = c(0,0,0,0))
plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
legend('bottom', bty = 'n', legend = names(col_plt),
  title = 'Indicator calculation',
  horiz = TRUE, fill = unname(col_plt), border = NA)

dev.off()
