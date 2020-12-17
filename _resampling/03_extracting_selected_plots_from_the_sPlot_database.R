#
# 01 February 2017, updated on 11 December 2020
#
library(raster)
library(fBasics)
library(maptools)
#
data(wrld_simpl)
#
load("plot_data.RData")
load("plotToRemove.RData")
load("pca3.RData")
#
ls()
#
output[[1]] 
#
# 858 cells from the PC1-PC2 space have been resampled with a cutoff value of
# 50 plots maximum per cell with 50 being the median value of the total number 
# of plots across all grid cells of the PC1-PC2 space thus also being a good 
# compromise between quantity and quality in terms of extracting a subset of 
# sPlot that has a balanced sampling effort across the PC1-PC2 space
#
plotToRemove <- output[[2]]
rm(output)
class(plotToRemove) 
#
# A list of 100 vectors (100 different resampling iterations) that contains the 
# IDs of sPlot releves to remove from the plot_data object
#
length(plotToRemove[[1]]) 
#
# First iteration containing 700037 IDs
#
length(plotToRemove[[2]]) 
#
# Second iteration containing 700022 IDs
#
head(plotToRemove[[1]]) 
head(plotToRemove[[2]]) 
#
# First ID is NA for each vector in the list which is normal (cf. a property of 
# the resampling loop) we have to clean that (see below)
#
for (i in 1:100) {
  plotToRemove[[i]] <- na.omit(plotToRemove[[i]])
}
#
# One extraction exemple from the first vector in the plotToRemove list object
#
posit <- match(plotToRemove[[1]], plot_data$PlotID)
plot_sel <- plot_data[-posit, c("PlotID")]
length(plot_sel) 
#
# A total of 99364 plots seem to be selected which is a bit too much given that
# 50*858 grid cells gives only 42900 plots and even if some grid cells have less
# than 50 plots, the total should not be 99364??? This is far too much and thus
# something is wrong
#
length(which(is.na(plot_data$pc1_val)))
#
# It seems that 42878 plots in the plot_data object have NAs for PC1 and these
# mostly correspond to coastal pixels where data from SoilGrid are unavailable
#
posit <- which(is.na(plot_data$pc1_val))
plot_data <- plot_data[-posit, ]
dim(plot_data)[[1]]
#
# After removing rows with NAS for PC1, the plot_data object has 756522 plots
# instead of 799400 plots
#
posit <- match(plotToRemove[[1]], plot_data$PlotID)
plot_sel <- plot_data[-posit, c("PlotID")]
length(plot_sel) 
# 
# In the end, the true selection from the first resampling iteration is 56486 
# plots instead of 99364 plots
#
plot_sel <- list()
for (i in 1:100) {
  posit <- match(plotToRemove[[i]], plot_data$PlotID)
  plot_sel[[i]] <- plot_data[-posit, c("PlotID")]
}
save(plot_sel, file="plot_sel.RData")
#
