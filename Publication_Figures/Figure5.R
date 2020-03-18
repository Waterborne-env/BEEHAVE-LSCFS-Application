## Publication Plot: Figure 5
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
gofi_df = gofi_df[,c('Data', 'CCAs', 'Measure', 'Adequacy', 'Reliability')]
gofi_df$Data = mapvalues(gofi_df$Data,
  paste0('CFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')), 
  paste0('LSCFS_20', c('13_1','14_1','14_2','15_1','15_2','16_1','16_2')))
gofi_df = gofi_df[gofi_df$Data != 'All' & gofi_df$CCAs == 'Season',]

bias_df = read.csv(bias_fp, stringsAsFactors = FALSE)
bias_df = 
  bias_df[,c('Data', 'CCAs', 'Measure', 'Adequacy_bias', 'Reliability_bias')]
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
col_plt = c('Aa'='#8B4789', 'Ab'='#FF83FA', 'Ra'='#8B6914', 'Rb'='#DAA520')

adt_gofi_df = gofi_df[gofi_df$Measure == 'AdultBees',]
hon_gofi_df = gofi_df[gofi_df$Measure == 'Honey',]

# Initialize plot
jpeg(file.path(outp_fp, 'Figure5.jpeg'), 
  width = 7, height = 3.15, units = 'in', res = 300)
layout(matrix(c(1:3,3), 2, 2, byrow = TRUE))
par(oma = c(0, 3, 1, 0.25), mar = c(0.5, 3.25, 0.5, 0.5))

# Adult bees
plot(NA, ylim = c(-0.05, 1.05), xlim = c(0.5, 8.5), xaxt = 'n', yaxt = 'n',
  xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
mtext('# Adult Bees', side = 3, line = 0.25, cex = 0.75, font = 2) # label
axis(2, at =seq(0, 1, 0.1), las = 2, cex = 0.75) # y axis marker
mtext('Adequacy / Reliability', side = 2, line = 3.5, cex = 0.75) # y axis label
axis(1, at = 1:8, labels = levels(gofi_df$Data), las = 2) # x axis label
abline(h = seq(0.1, 0.9, 0.1), col = '#D3D3D3', lty = 3) # y grid
abline(v = 2:7, col = '#D3D3D3', lty = 3) # x grid

with(adt_gofi_df, points(Data, Adequacy, 
  type = 'p', pch = 16, col = col_plt[['Aa']]))
with(adt_gofi_df, points(Data, Adequacy_bias, 
  type = 'p', pch = 16, col = col_plt[['Ab']]))
with(adt_gofi_df, points(Data, Reliability, 
  type = 'p', pch = 16, col = col_plt[['Ra']]))
with(adt_gofi_df, points(Data, Reliability_bias, 
  type = 'p', pch = 16, col = col_plt[['Rb']]))

# Honey 
plot(NA, ylim = c(-0.05, 1.05), xlim = c(0.5, 8.5), xaxt = 'n', yaxt = 'n',
  xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
mtext('Honey (kg)', side = 3, line = 0.25, cex = 0.75, font = 2) # label
axis(2, at = seq(0, 1, 0.1), las = 2, cex = 0.75) # y axis marker
axis(1, at = 1:8, labels = levels(gofi_df$Data), las = 2) # x axis label
abline(h = seq(0.1, 0.9, 0.1), col = '#D3D3D3', lty = 3) # y grid
abline(v = 2:7, col = '#D3D3D3', lty = 3) # x grid

with(hon_gofi_df, points(Data, Adequacy, 
  type = 'p', pch = 16, col = col_plt[['Aa']]))
with(hon_gofi_df, points(Data, Adequacy_bias, 
  type = 'p', pch = 16, col = col_plt[['Ab']]))
with(hon_gofi_df, points(Data, Reliability, 
  type = 'p', pch = 16, col = col_plt[['Ra']]))
with(hon_gofi_df, points(Data, Reliability_bias, 
  type = 'p', pch = 16, col = col_plt[['Rb']]))

# plot legend
par(mai = c(0,0,0,0))
plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
legend('bottom', bty = 'n', legend = names(col_plt),
  title = 'Area comparison statistics',
  horiz = TRUE, fill = unname(col_plt), border = NA)

dev.off()
