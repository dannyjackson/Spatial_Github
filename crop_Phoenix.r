# crop AZ pc files to Tucson size

library(sf)
library(sp)

library(raster)

library(rgdal)

library(tidyverse)

library(rgeos)

library(scales)

library(fasterize)

library(maptools)

setwd("/scratch/dnjacks4/asks/smaller/")

# set up projection parameter for use throughout script

projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

r <- crop(reference_tend, e)




fileNames <- Sys.glob("dev/*.asc")

for (fileName in fileNames) {
# looped over buffered files already prepped for maxent

  print(fileName)

  # process reference file
  assign(paste0("raster_", "raw"), raster(paste0(fileName)))

  # phoenix extent
  e <- as(extent(-112.584727000, -111.425540000, 33.089419000, 33.885028000), 'SpatialPolygons')

  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r <- crop(raster_raw, extent(e))

  print("Writing to file")

  writeRaster(r, filename=paste0(fileName,".asc"), format="ascii", overwrite=TRUE)

  rgdal::writeGDAL(as(r, "SpatialGridDataFrame"),
   paste0(fileName,".phoenix.asc"),
   drivername = "AAIGrid")

  print("Done")
}



# correlations 

library("terra")
library("raster")
library("dismo")
library("rgeos")
library("knitr")
library("ENMTools")
library("parallel")

all.env.files<-list.files(path="./phoenix/", pattern = "*.asc$", full.names=TRUE)

print("stacking all files")

all.env<-stack(all.env.files)
names(all.env) <- c("ClimPC2", "ClimPC3", "DevPC1", "DevPC2", "Elev", "MedianHouseholdIncome", "Canopy", "NLCD_11", "NLCD_31", "NLCD_41", "NLCD_42", "NLCD_52", "NLCD_71", "NLCD_81", "NLCD_82", "NLCD_90")

print("dealing with NAs")

numbers <- seq(1, 16)

Scriptr = function(item){
all.env[[item]][is.na(all.env[[item]])] <- 0;
};


mclapply(numbers, Scriptr, mc.set.seed=FALSE)


print("making correlations matrix")

correlations<-raster.cor.matrix(all.env)

print("writing correlations matrix")
write.csv(correlations,"correlations_Phx_all.csv")

print("plotting correlations")
plot<-raster.cor.plot(all.env)

print("writing plot")
pdf(file="corr_plot_Phx_all.pdf",width=10,height=10,useDingbats=FALSE)
plot
dev.off()





















# correlations 

library("terra")
library("raster")
library("dismo")
library("rgeos")
library("knitr")
library("ENMTools")
library("parallel")

all.env.files<-list.files(path="./formaxent/", pattern = "*.asc$", full.names=TRUE)

print("stacking all files")

all.env<-stack(all.env.files)
names(all.env) <- c("ClimPC2", "ClimPC3", "DevPC1", "DevPC2", "Elev", "MedianHouseholdIncome", "Canopy", "NLCD_11", "NLCD_31", "NLCD_41", "NLCD_42", "NLCD_52", "NLCD_71", "NLCD_81", "NLCD_82", "NLCD_90")

print("dealing with NAs")

numbers <- seq(1, 16)

Scriptr = function(item){
all.env[[item]][is.na(all.env[[item]])] <- 0;
};


mclapply(numbers, Scriptr, mc.set.seed=FALSE)


print("making correlations matrix")

correlations<-raster.cor.matrix(all.env)

print("writing correlations matrix")
write.csv(correlations,"correlations_AZ_all.csv")

print("plotting correlations")
plot<-raster.cor.plot(all.env)

print("writing plot")
pdf(file="corr_plot_AZ_all.pdf",width=10,height=10,useDingbats=FALSE)
plot
dev.off()