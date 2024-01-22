require(tidyverse)
require(sf)
require(tigris)
require(geodata)
require(terra)
require(colorspace)
require(tidyterra)
require(ggnewscale)
require(tmap)
require(RColorBrewer)

siteData <- read.csv('Week 2/output.csv')
siteData$Month <- format(as.Date(siteData$Timestamp), "%m-%Y")
bombayBeach <- subset(subset(siteData, Lat>33.3), Lat<33.4)




siteSf <- st_as_sf(siteData, coords=c('Lon', 'Lat'), crs='+proj=longlat +ellips=WGS84')

studyArea <- st_bbox(siteSf)
studyArea <- studyArea + c(-0.13, -0.06, 0.05, 0.05)
studyArea <- st_as_sfc(studyArea)

bbSf <- st_as_sf(bombayBeach, coords=c('Lon', 'Lat'), crs='+proj=longlat +ellips=WGS84')

bbArea <- st_bbox(bbSf) + c(-0.005, -0.005, 0.005, 0.005)
bbArea <- st_as_sfc(bbArea)

bbAreaBig <- st_bbox(bbSf) + c(-0.02, -0.02, 0.02, 0.02)
bbAreaBig <- st_as_sfc(bbAreaBig)




california <- states() %>% 
  filter(NAME=='California')

california <- st_transform(california, st_crs(siteSf))
st_crs(california)$proj4string

usaWater <- landcover(var='water', country='USA', path=tempdir())

californiaWater <- crop(usaWater, california)
californiaWater <- mask(californiaWater, california)
californiaWater <- aggregate(californiaWater, fact=5, fun="mean")

plot(californiaWater)




raster <- rast('Week 2/NLCD_lCk6kYi4aKwW4nTdBDKJ/NLCD_2021_Land_Cover_L48_20230630_lCk6kYi4aKwW4nTdBDKJ.tiff')
raster[is.na(raster)] = 0
raster <- project(raster, crs(bbSf, proj=T))
rasterSa <- crop(raster, studyArea)
rasterBb <- crop(raster, bbArea)

rasterSa <- aggregate(rasterSa, fact=2, fun="mean")
plot(rasterSa)




tmap_options(max.categories = 256)

Cali <- tm_shape(california)+
  tm_polygons()+
  tm_shape(californiaWater)+
  tm_raster(palette = "Blues", legend.show = FALSE)+
  tm_shape(studyArea)+
  tm_borders(col = 'red')+
  tm_layout(frame = FALSE)

Cali




zoomedOut <- tm_shape(rasterSa)+
  tm_raster(palette = 'Set1', legend.show = FALSE)+
  tm_shape(siteSf)+ 
  tm_dots(size = 0.2, col = 'white', shape = 21)+
  tm_scale_bar(text.size = 0.7, position = 'left')+
  tm_shape(bbAreaBig)+
  tm_borders(col = 'black')+
  tm_layout(outer.margins = c(0, 0, 0, 0), inner.margins = c(0, 0, 0, 0))

zoomedOut




zoomedIn <- tm_shape(rasterBb)+
  tm_raster(alpha = 0.9, legend.show = FALSE)+
  tm_shape(bbSf)+
  tm_dots(shape = 21,
          size = 0.2,
          border.col = 1,
          col = 'Month',
          title = 'Sample Date',
          palette = 'Dark2',
          legend.show = FALSE)+
  tm_compass(position = 'left')+
  tm_scale_bar(text.size = 0.7, position = 'left')

zoomedIn




legend <- tm_shape(bbSf)+
  tm_dots(shape = 21,
          size = 0.2,
          border.col = 1,
          col = 'Month',
          title = 'Sample Date',
          palette = 'Dark2')+
  tm_legend(text.size = 1, title.size = 1.5)+
  tm_layout(legend.only = TRUE,
            title.position = c('center', 'center'))

legend




tmap_arrange(zoomedOut, Cali, zoomedIn, legend, ncol = 2, asp = NA)