library(tidyverse)
library(viridis)

#library(raster)
library(sp)
library(sf)
library(rgdal)
library(rnaturalearth)
library(dggridR)
# library(rgeos)

#save temporary files
write("TMPDIR = /data/sPlot/users/Francesco/_tmp", file=file.path(Sys.getenv('TMPDIR'), '.Renviron'))
write("R_USER = /data/sPlot/users/Francesco/_tmp", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#rasterOptions(tmpdir="/data/sPlot/users/Francesco/_tmp")

## import data
load(file = "_output/sPlot_OPEN.RData")
load("/data/sPlot/releases/sPlot3.0/header_sPlot3.0.RData")

## Data Preparation for spatial plotting
header.oa.sf <- SpatialPointsDataFrame(coords= header.oa %>% 
                                      select(POINT_X, POINT_Y), 
                                    proj4string = CRS("+init=epsg:4326"), 
                                    data=data.frame(PlotObservationID=header.oa$PlotObservationID, 
                                                    Dataset=header.oa$Dataset)) %>% 
  st_as_sf() %>% 
  st_transform(crs = "+proj=eck4")

header <- header %>% 
  filter(!is.na(Latitude))
header.sf <- SpatialPointsDataFrame(coords= header %>% 
                                      
                                      select(Longitude, Latitude), 
                                    proj4string = CRS("+init=epsg:4326"), 
                                    data=data.frame(PlotObservationID=header$PlotObservationID, 
                                                    Dataset=header$Dataset)) %>% 
  st_as_sf() %>% 
  st_transform(crs = "+proj=eck4")



### Template of Global map
#download data from rnaturalearth package
countries <- ne_countries(returnclass = "sf") %>% 
  st_transform(crs = "+proj=eck4") %>% 
  st_geometry()
graticules <- ne_download(type = "graticules_15", category = "physical",
                          returnclass = "sf") %>% 
  st_transform(crs = "+proj=eck4") %>% 
  st_geometry()
bb <- ne_download(type = "wgs84_bounding_box", category = "physical",
                  returnclass = "sf") %>% 
  st_transform(crs = "+proj=eck4") %>% 
  st_geometry()

# create ggplot template of the world map
w3a <- ggplot() +
  geom_sf(data = bb, col = "grey20", fill = "white") +
  geom_sf(data = graticules, col = "grey20", lwd = 0.1) +
  #geom_sf(data = countries, fill = "grey90", col = NA, lwd = 0.3) +
  coord_sf(crs = "+proj=eck4") +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12),
        legend.background = element_rect(size=0.1, linetype="solid", colour = 1), 
        legend.key.height = unit(1.1, "cm"), 
        legend.key.width = unit(1.1, "cm")) +
  scale_fill_viridis()


### Map of plot distribution
### all plots from sPlot 3.0
w3all <- w3a + 
  geom_sf(data=header.sf, color=gray(0.5), pch="+", size=1, alpha=1/5) + # aes(col=Dataset),
  geom_sf(data = countries, col = "grey10", fill=NA, lwd = 0.3) + 
  theme(legend.position = "none")

### open.access plots from paper #02
w3oa <- w3all + 
  geom_sf(data=header.oa.sf, color="red", pch="+", size=1, alpha=1/4) + # aes(col=Dataset),
  geom_sf(data = countries, col = "grey10", fill=NA, lwd = 0.3) + 
  theme(legend.position = "none")

ggsave(filename="_output/sPlot3_distribution.png", device="png", width=8, height=5, dpi = 300, plot = w3all)
ggsave(filename="_output/sPlot21_oa_distribution.png", device="png", width=8, height=5, dpi = 300, plot = w3oa)




### Version 2  - hexagons
header2 <- header.oa %>% 
  select(PlotObservationID, POINT_Y, POINT_X) %>% 
  filter(!(abs(POINT_X) >171 & abs(POINT_Y>70)))
dggs <- dgconstruct(spacing=300, metric=T, resround='down')

#Get the corresponding grid cells for each plot
header2$cell <- dgGEO_to_SEQNUM(dggs, header2$POINT_X, header2$POINT_Y)$seqnum

#Calculate number of plots for each cell
header.dggs   <- header2 %>% 
  group_by(cell) %>% 
  summarise(value.out=log(n(), 10))

#Get the grid cell boundaries for cells 
grid   <- dgcellstogrid(dggs, header.dggs$cell, frame=F) %>%
  st_as_sf() %>% 
  mutate(cell = header.dggs$cell) %>% 
  mutate(value.out=header.dggs$value.out) %>% 
  st_transform("+proj=eck4") %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

## plotting
w3a + 
  geom_sf(data=grid, aes(fill=value.out),lwd=0, alpha=0.9)    +
  geom_sf(data = countries, col = "grey10", fill=NA, lwd = 0.3) + 
  scale_fill_viridis(
    name="# plots", breaks=0:5, labels = c("1", "10", "100",
                                           "1,000", "10,000", "100,000"), option="viridis" )

