#read srtm to read DEM band 1

library(raster)
library(rgdal)
library(tibble)
library(stringi)
library(stringdist)
library(stringr)
library(RStoolbox)
library(rjson)
library(geonames)
library(rgeos)
library(spatstat)
library(nlme)
library(spatialEco)

bel <- raster::getData('alt', country='IND', mask=TRUE, level=2)
belshp <- raster::getData('GADM', country='IND', level=2)
mlrdata1<read.xlsx("C:/Users/JKT/Desktop/TALUKA/BARDOLI/SAMPLE/SAMP_101.xlsx",sheetIndex = 1)
proj4string(mlrdata1) = CRS("+init=epsg:32643")
#
S <- SpatialPoints(data.frame(x = mlrdata1$Lon, y = mlrdata1$Lat),proj4string = CRS(proj4string(belshp)))
Sint <- gIntersection(S, belshp)

plot(bel)
plot(belshp, add=TRUE)
plot(Sint, add = TRUE, col = "red", pch = 19, cex = 0.2)


# elevation <- getData('alt', country='IND')
# x <- terrain(elevation, opt=c('slope', 'aspect'), unit='degrees')
# 
# plot(x)


predictorvar <- readGDAL("E:/RESEARCH/FINAL/KisanGIS/srtm_51_08/srtm_51_08.tif")
str(predictorvar)
ras2 <- as(predictorvar, "SpatialPixelsDataFrame")

proj4string(bel) = CRS("+init=epsg:32643")

mysrast <-stack("E:/RESEARCH/FINAL/KisanGIS/srtm_51_08/srtm_51_08.tif")
plot(mysrast)

proj4string(mysrast)
proj4string(mysrast) = CRS("+init=epsg:32643")

plot(mysrast$srtm_51_08)


newslope<-terrain(mysrast,"slope")
proj4string(newslope)= CRS("+init=epsg:32643")
plot(newslope)


newaspect <-terrain(mysrast,"aspect")
proj4string(newaspect)= CRS("+init=epsg:32643")
plot(newaspect)


newTPI <-terrain(mysrast,"TPI")
proj4string(newTPI)= CRS("+init=epsg:32643")
plot(newTPI)

newTRI <-terrain(mysrast,"TRI")
proj4string(newTRI)= CRS("+init=epsg:32643")
plot(newTRI)



mylay <-stack(newTPI,newTRI,newslope,newaspect)
nlayers(mylay)
mypca<-rasterPCA(mylay)
corlay <-layerStats(mylay,'cov')
  x<-mylay[[1]]
  y<-mylay[[2]]
corlay <-rasterCorrelation(x,y,3,"pearson","jkt")
mypca$model$sdev
  
  
newext<-c(20, 72, 22, 74)
gujarat<-crop(mylay,newext)
gujarat
cor<-layerStats(gujarat,'pearson')

mypca$map$PC1@data


ph<-mlrdata1$PH
ec<-mlrdata1$EC
oc<-mlrdata1$OC
SOC.lm = lm(oc ~ mypca$map$PC1@data,mlrdata1)
fSOC = step(SOC.lm)
summary(fSOC)


plot(predictorvar)
predictorvar
predictorvar$band1
dfdem <-as.data.frame(predictorvar)
mlrdata1<-read.xlsx("C:/Users/JKT/Desktop/Taluka.xlsx",sheetIndex = 2)

plot(hill, col=grey(0:100/100), legend=FALSE, main='guj')
plot(elevation, col=rainbow(25, alpha=0.35), add=TRUE)

myov=overlay(newslope,mlrdata1)

#http://viewfinderpanoramas.org/Coverage%20map%20viewfinderpanoramas_org3.htm

