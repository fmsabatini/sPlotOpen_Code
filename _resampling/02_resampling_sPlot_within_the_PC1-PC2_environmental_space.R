#
# 01 February 2017
#
library(spdep)
library(raster)
library(maptools)
library(fBasics)
library(devtools)
library(parallelsugar)
library(colorRamps)
library(Rcpp)
library(bigmemory)
library(RcppArmadillo)
library(RcppParallel)
#
data(wrld_simpl)
#
# Loading the header data from the sPlot database v2.1
#
load("sPlot_header_20161124.RData")
ls() # header
dim(header) # 1121244 relevés and 51 variables
length(which(is.na(header$Longitude))) # 558 relevés without longitude coordinates
length(which(is.na(header$Latitude))) # 558 relevés without latitude coordinates
posit <- which(is.na(header$Longitude)&is.na(header$Latitude))
plot_data <- header[-posit, ]
dim(plot_data) # 1120686 relevés and 51 variables
rm(header)
#
# Make plot_data as a spatial dataframe
#
CRSlonlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
coords <- cbind(plot_data$Longitude, plot_data$Latitude)
coords <- SpatialPoints(coords, proj4string=CRSlonlat)
plot_data <- SpatialPointsDataFrame(coords, plot_data, proj4string=CRSlonlat)
class(plot_data)
#
# Compute the initial sampling effort across the geographical space per spatial unit of 2.5 arc minute
#
rgeo <- raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90) # raster at half a degree resolution (cf. 30 arc minute resolution)
rgeo <- disaggregate(rgeo, fact=12) # raster at 2.5 arc minute resolution hich is about 5 km at the equator (25 km2 is the approximate area of a spatial unit at the equator)
init_seff_rgeo <- rasterize(plot_data@data[, c("POINT_X", "POINT_Y")], rgeo, fun="count")
sum(getValues(init_seff_rgeo), na.rm=TRUE) # 1120686 relevés
#
# Remove plots for which location accuracy is above 2821 m (cf. radius of a buffer circle of 25 km2 around the plot location)
#
plot_data <- plot_data@data
buffer <- round(sqrt(25/pi)*1000)
posit <- which(plot_data[, "Location uncertainty (m)"]>buffer) # Be careful, there are 261 plots without location uncertainty but with coordinates that we will keep in the end
length(posit) # 280224 relevés for which location accuracy exceeds a radius of 2821 m
plot_data <- plot_data[-posit, ]
dim(plot_data) # 840462 relevés and 51 variables
#
# Remove plots from wetlands
#
posit <- which(is.na(plot_data$Grassland)&is.na(plot_data$Forest)&is.na(plot_data$Shrubland)&is.na(plot_data$Sparse.vegetation)&plot_data$Wetland==1)
length(posit) # 8323 relevés that are pure wetlands
plot_data <- plot_data[-posit, ]
dim(plot_data) # 832139 relevés and 51 variables
#
# Remove plots from anthropogenic vegetation types
#
posit <- which(plot_data$Naturalness==3)
length(posit) # 30942 relevés from anthropogenic vegetation types
plot_data <- plot_data[-posit, ]
dim(plot_data) # 801197 relevés and 51 variables
#
# Import DT2 which is the full dataset (free of non-vascular plants) with species composition
#
load("DT2_20161025.RData")
dim(DT2) # 22195966 species occurrences and 7 variables
#
# Match it with the plot_data dataframe
#
length(unique(DT2$PlotObservationID)) # 1117369 relevés
length(unique(DT2$PlotObservationID))-length(unique(plot_data$PlotObservationID)) # 316172 relevés in DT2 but missing from plot_data
posit <- match(DT2$PlotObservationID, plot_data$PlotObservationID)
any(is.na(posit)) # TRUE: some relevés (n = 316172) in DT2 are not in plot_data
length(DT2$PlotObservationID[is.na(posit)]) # 6168698 rows in DT2 corresponding to the species lists of the relevés missing from plot_data 
DT2 <- DT2[is.finite(posit), ]
length(unique(DT2$PlotObservationID)) # 799400 relevés
posit <- match(plot_data$PlotObservationID, DT2$PlotObservationID)
any(is.na(posit)) # TRUE: some relevés (n = 1797) in plot_data are not in DT2 (cf. plots with only non-vascular plants?)
length(plot_data$PlotObservationID[is.na(posit)]) # 1797 relevés in plot_data are not in DT2 (cf. plots with only non-vascular plants?)
plot_data <- plot_data[is.finite(posit), ]
length(unique(plot_data$PlotObservationID)) # 799400 relevés which matches with DT2
#
save(plot_data, file="plot_data.RData")
#
# Make plot_data as a spatial dataframe again
#
coords <- cbind(plot_data$Longitude, plot_data$Latitude)
coords <- SpatialPoints(coords, proj4string=CRSlonlat)
plot_data <- SpatialPointsDataFrame(coords, plot_data, proj4string=CRSlonlat)
class(plot_data)
#
# Check for relevés with identical spatial coordinates (just for information) 
#
coordID <- paste(plot_data@data$Longitude, plot_data@data$Latitude, sep=":")
length(coordID) # 799400 relevés
length(unique(coordID)) # 509977 relevés with unique coordinates (about 64% of the relevés have unique coordinates)
#
# Plot the global sampling effort per spatial unit of 2.5 arc minute
#
seff_rgeo <- rasterize(plot_data@data[, c("POINT_X", "POINT_Y")], rgeo, fun="count")
sum(getValues(seff_rgeo), na.rm=TRUE) # 799400 relevés
tiff(filename="Sampling_effort_at_2.5_arc_minute.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(seff_rgeo), legend=FALSE, asp=0, col=rev(divPalette(n=100, name="Spectral")), xlab="Longitude", ylab="Latitude")
plot(log(seff_rgeo), legend.only=TRUE, col=rev(divPalette(n=100, name="Spectral")), legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5), labels=round(exp(seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5))), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Number (log-scale) of plots \nper 2.5 arc-minute spatial unit")
dev.off()
#
# Plot the global sampling effort per spatial unit of 0.5 degree
#
seff_rgeo <- aggregate(seff_rgeo, fact=12, fun=sum)
sum(getValues(seff_rgeo), na.rm=TRUE) # 799400 relevés
tiff(filename="Sampling_effort_at_0.5_degree.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(seff_rgeo), legend=FALSE, asp=0, col=rev(divPalette(n=100, name="Spectral")), xlab="Longitude", ylab="Latitude")
plot(log(seff_rgeo), legend.only=TRUE, col=rev(divPalette(n=100, name="Spectral")), legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5), labels=round(exp(seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5))), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Number (log-scale) of plots \nper 30 arc-minute spatial unit")
dev.off()
#
# Plot the global sampling effort per spatial unit of 1 degree
#
seff_rgeo <- aggregate(seff_rgeo, fact=2, fun=sum)
sum(getValues(seff_rgeo), na.rm=TRUE) # 799400 relevés
tiff(filename="Sampling_effort_at_1_degree.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(seff_rgeo), legend=FALSE, asp=0, col=rev(divPalette(n=100, name="Spectral")), xlab="Longitude", ylab="Latitude")
plot(log(seff_rgeo), legend.only=TRUE, col=rev(divPalette(n=100, name="Spectral")), legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5), labels=round(exp(seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5))), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Number (log-scale) of plots \nper 1 degree spatial unit")
dev.off()
#
# Plot the global sampling effort per spatial unit of 2 degrees
#
seff_rgeo <- aggregate(seff_rgeo, fact=2, fun=sum)
sum(getValues(seff_rgeo), na.rm=TRUE) # 799400 relevés
tiff(filename="Sampling_effort_at_2_degrees.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(seff_rgeo), legend=FALSE, asp=0, col=rev(divPalette(n=100, name="Spectral")), xlab="Longitude", ylab="Latitude")
plot(log(seff_rgeo), legend.only=TRUE, col=rev(divPalette(n=100, name="Spectral")), legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5), labels=round(exp(seq(log(minValue(seff_rgeo)), log(maxValue(seff_rgeo)), length.out=5))), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Number (log-scale) of plots \nper 2 degrees spatial unit")
dev.off()
#
# Plot the difference with the initial sampling effort per spatial unit of 2 degrees
#
init_seff_rgeo <- aggregate(init_seff_rgeo, fact=48, fun=sum)
sum(getValues(init_seff_rgeo), na.rm=TRUE) # 1120686 relevés
diff_seff_rgeo <- init_seff_rgeo-seff_rgeo
sum(getValues(diff_seff_rgeo), na.rm=TRUE) # 316728 relevés lost after data filtering
tiff(filename="Plot_loss_at_2_degrees.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(diff_seff_rgeo+1), legend=FALSE, asp=0, col=c("grey", rev(divPalette(n=99, name="Spectral"))), xlab="Longitude", ylab="Latitude")
plot(log(diff_seff_rgeo+1), legend.only=TRUE, col=c("grey", rev(divPalette(n=99, name="Spectral"))), legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(0, log(maxValue(diff_seff_rgeo)+1), length.out=5), labels=round(seq(0, exp(log(maxValue(diff_seff_rgeo)+1)), length.out=5)), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Number (log-scale) of plots \nper 2 degrees spatial unit")
dev.off()
#
# Compute the global sampling effort across the bivariate (PC1-PC2) environmental space (not the geographical space)
#
load("PC1_r.RData")
load("PC2_r.RData")
plot_data@data$cellID <- cellFromXY(rgeo, cbind(plot_data@data$POINT_X, plot_data@data$POINT_Y))
plot_data@data$pc1_val <- extract(PC1_r, coordinates(plot_data))
plot_data@data$pc2_val <- extract(PC2_r, coordinates(plot_data)) 
load("pca3.RData")
res <- 100 # Setting the number of bins per PCA axis to 100
reco <- raster(nrows=res, ncols=res, xmn=min(pca3$x[, 1]), xmx=max(pca3$x[, 1]), ymn=min(pca3$x[, 2]), ymx=max(pca3$x[, 2]))
PC1_PC2_r <- rasterize(pca3$x[, 1:2], reco, fun="count") # Compute the density of geographic grid cells across the entire bivariate (PC1-PC2) environmental space
sPlot_reco <- rasterize(plot_data@data[, c("pc1_val", "pc2_val")], reco, fun="count") # Compute the sampling effort (number of vegetation plots) per environmental unit (cell) across the entire bivariate (PC1-PC2) environmental space
temp1 <- getValues(PC1_PC2_r)
temp1[!is.na(temp1)] <- 0 # Put zero values for the empty cells (cf. there is no existing terrestrial grid cell available on Earth for the focal PC1-PC2 grid cell condition) 
temp2 <- getValues(sPlot_reco)
temp2[which(temp1==0&is.na(temp2))] <- 0 # Put zero values for the empty cells (cf. there is no vegeteation plots available for those environmental conditions: gaps) 
sPlot_reco <- setValues(reco, temp2)
#
# Plot the number of 2.5 arc-minute cells for each cell of the PC1-PC2 space
#
tiff(filename="Global_availability_PC1-PC2.tiff", width=12, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(PC1_PC2_r), asp=0, col=rev(divPalette(n=100, name="RdBu")), xlab="PC1 (cold and seasonal to hot and stable)", ylab="PC2 (dry to wet)", legend=FALSE)
plot(log(PC1_PC2_r), asp=0, col=rev(divPalette(n=100, name="RdBu")), legend.only=TRUE, legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(log(minValue(PC1_PC2_r)), log(maxValue(PC1_PC2_r)), length.out=5), labels=round(seq(exp(log(minValue(PC1_PC2_r))), exp(log(maxValue(PC1_PC2_r))), length.out=5)), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
title(main="Number of 2.5 arc-minute spatial units \nper environmental cell (log scale)")
dev.off()
#
# Plot the number of sPlot relevés for each cell of the PC1-PC2 space
#
tiff(filename="Sampling_effort_PC1-PC2.tiff", width=12, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(log(sPlot_reco+1), asp=0, col=c("grey", rev(divPalette(n=99, name="RdBu"))), xlab="PC1 (cold and seasonal to hot and stable)", ylab="PC2 (dry to wet)", legend=FALSE)
plot(log(sPlot_reco+1), asp=0, col=c("grey", rev(divPalette(n=99, name="RdBu"))), legend.only=TRUE, legend.width=1, legend.shrink=0.75, axis.args=list(at=seq(0, log(maxValue(sPlot_reco)+1), length.out=5), labels=round(seq(0, exp(log(maxValue(sPlot_reco)+1)), length.out=5)), cex.axis=0.6), legend.args=list(text="N", side=3, font=2, line=0, cex=0.8))
title(main="Number of sPlot relevés \nper environmental cell (log scale)")
dev.off()
#
# Plot for each cell of the PC1-PC2 space the ratio between the relative proportion of sPlot relevés and the relative proportion of spatial units available worldwide
#
tiff(filename="Sampling_effort_ratio_PC1-PC2.tiff", width=12, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
ratio_reco <- (sPlot_reco/max(getValues(sPlot_reco), na.rm=T))/(PC1_PC2_r/max(getValues(PC1_PC2_r), na.rm=T))
plot(log(ratio_reco+1), asp=0, col=c("grey", rev(divPalette(n=99, name="Spectral"))), xlab="PC1 cold and seasonal to hot and stable)", ylab="PC2 (dry to wet)")
title(main="Oversampled (>0.69) versus \nundersampled (<0.69) PC1-PC2 cells")
dev.off()
#
# Run a sensitivity analysis to define the most appropriate resolution of the bivariate (PC1-PC2) environmental space
#
res <- seq(10, 500, 10)
ncell_disp <- c()
ncell_samp <- c()
seff_med <- c()
seff_mean <- c()
seff_max <- c()
seff_min <- c()
nbrel_sel <- c() 
for (i in 1:length(res)) {
  print(paste(i, "of", length(res), sep=" "))
  r <- raster(nrows=res[i], ncols=res[i], xmn=min(pca3$x[, 1]), xmx=max(pca3$x[, 1]), ymn=min(pca3$x[, 2]), ymx=max(pca3$x[, 2]))
  temp <- rasterize(pca3$x[, 1:2], r, fun="count")
  ncell_disp <- c(ncell_disp, length(which(getValues(temp)>0)))
  temp <- rasterize(plot_data@data[, c("pc1_val", "pc2_val")], r, fun="count")
  temp <- getValues(temp)
  temp <- na.omit(temp) 
  ncell_samp <- c(ncell_samp, length(which(temp)>0))
  seff_med <- c(seff_med, median(temp))
  seff_mean <- c(seff_mean, mean(temp))
  seff_max <- c(seff_max, max(temp))
  seff_min <- c(seff_min, min(temp))
  nbrel_sel <- c(nbrel_sel, length(temp[which(temp>median(temp)), ])*median(temp)+sum(temp[which(temp<=median(temp)), ]))  
}
plot(res, seff_med)
plot(res, seff_max)
plot(res, seff_mean)
plot(res, nbrel_sel)
plot(res, ncell_samp/ncell_disp, ylim=c(0, 1))
#
# Resample sPlot within the PC1-PC2 environmentral space to get an environmentally-balanced subset
#
plot_data <- plot_data@data
save(plot_data, file="plot_data.RData") # Save the latest version of plot_data
Sys.setenv("PKG_CXXFLAGS"="-fopenmp") # Set environment variables for other processes called from within R 	
setwd("~/Functions_TH") # Source C++ fonctions written by Tarek Hattab
sourceCpp("bray.part.OpenMP.cpp")
sourceCpp("bray.part.C_RcppParallel.cpp")
sourceCpp("hcr.C.cpp")
sourceCpp("cast_binary.cpp")
BigBrayPart <- function(bigMat) {
  zeros <- big.matrix(nrow=nrow(bigMat), ncol=nrow(bigMat), init=0, type=typeof(bigMat), shared=FALSE, backingfile=paste("BrayMatrix_",i,sep=""), backingpath=getwd(), descriptorfile=paste("BrayMatrix_",i,".desc",sep=""))
  bray_distance_OpenMP(bigMat@address, zeros@address)
  return(zeros)
}
res <- 100 # Set the resolution of the environmental space based on the sensitivity analysis
r <- raster(nrows=res, ncols=res, xmn=min(pca3$x[, 1]), xmx=max(pca3$x[, 1]), ymn=min(pca3$x[, 2]), ymx=max(pca3$x[, 2])) # Prepare the environmental space restricted to sPlot relevés only (not the entire environmental space available at a global extent)
pca_sPlot_r <- rasterize(plot_data[, c("pc1_val", "pc2_val")], r, fun="count")
cutoff <- median(values(pca_sPlot_r), na.rm=TRUE) # Compute the cutoff value above which relevés have to be resampled for a given cell
tempZoneOut <- coordinates(pca_sPlot_r)[which(values(pca_sPlot_r)>cutoff), ] # Select only the coordinates of the environmental cells for which the total number of sPlot relevés available exceeds the cutoff value
repet <- 100 # Set the number of repetitions for the HCR function
sp_data <- DT2[, c(1, 2, 7)] # Prepare the species data table that will be used by the HCR approach
names(sp_data) <- c("plot_id", "sp_name", "rel_cov")
save(sp_data, file="sp_data.RData") # Save the latest version of sp_data
plotToRemove <- as.list(rep(NA, repet)) # Prepare an empty object to store the IDs of the relevés to be removed
for (i in 1:nrow(tempZoneOut)) {
  print("--------")
  print(paste(i, "out of", nrow(tempZoneOut), "cells", sep=" "))
  plot(pca_sPlot_r, asp=0, xlab="PC1 (cold and seasonal to hot and stable)", ylab="PC2 (dry to wet)")
  points(tempZoneOut[, 1], tempZoneOut[, 2], cex=0.5)
  points(tempZoneOut[i, 1], tempZoneOut[i, 2], col="red", pch=19)
  sel.plot <- which(plot_data$pc1_val > tempZoneOut[i, 1]-(res(r)[1]/2) & 
                    plot_data$pc1_val < tempZoneOut[i, 1]+(res(r)[1]/2) &
                    plot_data$pc2_val > tempZoneOut[i, 2]-(res(r)[2]/2) & 
                    plot_data$pc2_val < tempZoneOut[i, 2]+(res(r)[2]/2)) 
  print(paste("This cell contains", length(sel.plot), "relevés", sep=" ")) 
  idZoneOut <- plot_data[sel.plot, "PlotID"]
  sel.comm <- sp_data[which(sp_data$plot_id%in%idZoneOut), c("plot_id", "sp_name", "rel_cov")]
  sel.comm <- na.omit(sel.comm)
  sel.comm [, 2]<- factor(sel.comm[, 2], labels=seq(1:length(unique(sel.comm[, 2])))) 
  sel.comm [, 2]<- as.numeric(sel.comm[, 2])
  comm.data <- castC(iD=sel.comm[, 1], sp=sel.comm[, 2], cov=sel.comm[, 3])
  rowNames <- comm.data[, 1]
  comm.data <- comm.data[, -1]
  print(paste("The total number of species is", dim(comm.data)[[2]], sep=" ")) 
  gc()
  if (nrow(comm.data) > 20000) {
    bigComMatrix <- as.big.matrix(comm.data,shared=FALSE, backingfile=paste("Matrix_",i,sep=""), backingpath=getwd(), descriptorfile=paste("Matrix_", i, ".desc", sep="")) ; brayBalDist <- BigBrayPart(bigComMatrix)
  } else {
    brayBalDist <- bray_distance_RcppParallel(comm.data); brayBalDist <- as.big.matrix(brayBalDist)
  }
  for (j in 1:repet) {
    selectedPlot <- HcrCPP(brayBalDist@address, nout=cutoff, nsampl=1000)  
    selectedPlot <- rowNames[selectedPlot] 
    selectedPlotIndex <- which(idZoneOut%in%selectedPlot)
    plotToRemove[[j]] <-  c(plotToRemove[[j]], idZoneOut[-selectedPlotIndex])
  }
  output <- list(i, plotToRemove)
  save(output, file="plotToRemove.RData")	
}
#
