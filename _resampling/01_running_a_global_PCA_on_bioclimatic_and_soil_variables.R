#
# 27 January 2017
#
library(raster)
library(fBasics)
library(factoextra)
library(maptools)
library(RColorBrewer)
library(gridExtra)
#
data(wrld_simpl)
#                                                                                                                              
# Load all bioclimatic and soil variables at 2.5 arc minute resolution across terrestrial Earth
#
load("splot.world2.RData")
ls() # splot.world2
#
# Run the PCA on the entire matrix of environmental variables (climate + soil)
#
pca3 <- prcomp(splot.world2[, c(5:15,17:35)], center=T, scale=T)
#
save(pca3, file="pca3.RData")
#
# Check summary statistics and interpret the PCA axes
#
summary(pca3) # Three first PCA axes account for 75% of the total inertia with 47% and 23% loadings on the first and second PCA axes, respectively
var <- get_pca_var(pca3)
var$coord[, 1:3]
var_cor_func <- function(var.loadings, comp.sdev){var.loadings*comp.sdev}
loadings <- pca3$rotation
sdev <- pca3$sdev
var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
var.cor[, 1:3]
var.cos2 <- var.cor^2
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2, 1, contrib, comp.cos2))
var.contrib[, 1:3]
#
# Map the first PCA axes in space at 2.5 arc minute resolution
#
rgeo <- raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90) # raster at half a degree resolution (cf. 30 arc minute resolution)
rgeo <- disaggregate(rgeo, fact=12) # raster at 2.5 arc minute resolution
splot.world2$cellID <- cellFromXY(rgeo, cbind(splot.world2$RAST_X, splot.world2$RAST_Y))
#
posit <- splot.world2$cellID
temp <- getValues(rgeo)
#
temp[posit] <- pca3$x[, 1]
PC1_r <- setValues(rgeo, temp)
ramp_PC1 <- colorRampPalette(brewer.pal(11, "Spectral"))
breaks_PC1 <- quantile(PC1_r, probs=seq(0, 1, 0.01))
#
save(PC1_r, file="PC1_r.RData")
#
temp[posit] <- pca3$x[, 2]
PC2_r <- setValues(rgeo, temp)
ramp_PC2 <- colorRampPalette(brewer.pal(11, "Spectral"))
breaks_PC2 <- quantile(PC2_r, probs=seq(0, 1, 0.01))
#
save(PC2_r, file="PC2_r.RData")
#
temp[posit] <- pca3$x[, 3]
PC3_r <- setValues(rgeo, temp)
ramp_PC3 <- colorRampPalette(brewer.pal(11, "Spectral"))
breaks_PC3 <- quantile(PC3_r, probs=seq(0, 1, 0.01))
#
save(PC3_r, file="PC3_r.RData")
#
# Plot PCA outputs
#
tiff(filename="PCA_outputs.tiff", width=24, height=8, res=300, unit="cm")
p1 <- fviz_screeplot(pca3, ncp=10)
p2 <- fviz_pca_var(pca3, axes=c(1, 2))
p3 <- fviz_pca_var(pca3, axes=c(2, 3))
grid.arrange(p1, p2, p3, ncol=3, nrow=1)
multiplot(p1, p2, p3, cols=3)
dev.off()
#
# Plot the global distribution of PCA axis 1 at 2.5 arc minute resolution
#
tiff(filename="PCA_axis1_at_2.5_arc_minute.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(PC1_r, breaks=breaks_PC1, col=rev(ramp_PC1(100)), legend=FALSE)
plot(PC1_r, breaks=breaks_PC1, col=rev(ramp_PC1(100)), legend.only=TRUE, legend.width=1, legend.shrink=0.75, axis.args=list(at=round(seq(min(breaks_PC1), max(breaks_PC1), length.out=10)), cex.axis=0.6), legend.args=list(text="PC1", side=3, font=2, line=0.4, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="First PCA axis (cold and seasonal to hot and stable) \nat 2.5 arc minute resolution")
dev.off()
#
# Plot the global distribution of PCA axis 2 at 2.5 arc minute resolution
#
tiff(filename="PCA_axis2_at_2.5_arc_minute.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(PC2_r, breaks=breaks_PC2, col=ramp_PC2(100), legend=FALSE)
plot(PC2_r, breaks=breaks_PC2, col=ramp_PC2(100), legend.only=TRUE, legend.width=1, legend.shrink=0.75, axis.args=list(at=round(seq(min(breaks_PC2), max(breaks_PC2), length.out=10)), cex.axis=0.6), legend.args=list(text="PC2", side=3, font=2, line=0.4, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Second PCA axis (dry to wet) \nat 2.5 arc minute resolution")
dev.off()
#
# Plot the global distribution of PCA axis 3 at 2.5 arc minute resolution
#
tiff(filename="PCA_axis3_at_2.5_arc_minute.tiff", width=20, height=12, res=300, unit="cm")
par(mar=c(4, 4, 4, 1)) 
plot(PC3_r, breaks=breaks_PC3, col=ramp_PC3(100), legend=FALSE)
plot(PC3_r, breaks=breaks_PC3, col=ramp_PC3(100), legend.only=TRUE, legend.width=1, legend.shrink=0.75, axis.args=list(at=round(seq(min(breaks_PC3), max(breaks_PC3), length.out=10)), cex.axis=0.6), legend.args=list(text="PC2", side=3, font=2, line=0.4, cex=0.8))
plot(wrld_simpl, add=T, border="darkgrey", lwd=0.1)
title(main="Third PCA axis (sandy to clayey) \nat  2.5 arc minute resolution")
dev.off()
#
