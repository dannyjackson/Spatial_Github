#!/usr/bin/env Rscript
# Uses this tutorial https://www.azavea.com/blog/2018/10/09/preparing-data-for-maxent-species-distribution-modeling-using-r/

require(devtools)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(rgeos)
library(scales)
library(fasterize, lib='/home/dnjacks4/R/')
library(maptools)
library(parallel)

# for statewide
setwd("/home/dnjacks4/FilesToAgave/Tifs/")

# set up projection parameter for use throughout script

projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#set up extent parameter for use throughout script

# Arizona
ext <- extent(-114.8, -109.0, 31.3, 37.0)

# process reference file

assign(paste0("reference_", "raw"), raster('tifs/nlcd_continuous_clipped_r3_11.tif'))

reference_projected <- projectRaster(reference_raw, crs=projection)

rm(reference_raw)

# create variable equal to final raster

assign(paste0("reference_final"), reference_projected)

rm(reference_projected)




# Resample all datasets across pixels of reference file

# reference_final_re <- resample(reference_final, reference_final)


# Re-extend reference dataset to make sure that their shared extent was not influenced by the resampling

reference_tend <- extend(reference_final, ext, value=NA)

# arizona extent
e <- as(extent(-114.8, -109.0, 31.3, 37.0), 'SpatialPolygons')

# Arizona
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(reference_tend, e)
r <- crop(reference_tend, extent(e))

r[is.na(r[])] <- 0

#Write these environmental datasets out in .asc format

rgdal::writeGDAL(as(r, "SpatialGridDataFrame"),
 paste("reference_11.asc"),
 drivername = "AAIGrid")


# now repeat for all nonreference files

fileNames <- Sys.glob("/home/dnjacks4/FilesToAgave/Tifs/tifs/*.tif")
# fileNames <- Sys.glob("tifs/*.tif")


ScriptR = function(fileName){
# looped over buffered files already prepped for maxent

  print(fileName)

  # process reference file
  assign(paste0("raster_", "raw"), raster(paste0(fileName)))

  raster_projected <- projectRaster(raster_raw, reference_tend, value=NA)

  rm(raster_raw)

  # create variable equal to final raster

  assign(paste0("raster_final"), raster_projected)

  print("Resampling")

  # Resample all datasets across pixels of landcover file

  # raster_final_re <- resample(raster_final, reference_tend)

  # rm(raster_final)
  rm(raster_projected)
  print("Re-extending")

  # Re-extend dataets to make sure that their shared extent was not influenced by the resampling

  raster_tend <- extend(raster_final, ext, value=NA)

  # arizona extent
  e <- as(extent(-114.8, -109.0, 31.3, 37.0), 'SpatialPolygons')

  # Arizona
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r <- crop(raster_tend, extent(e))

  #Now all datasets are identical in extent, resolution, etc.

  r[is.na(r[])] <- 0

  #Write these environmental datasets out in .asc format

  print("Writing to file")

  rgdal::writeGDAL(as(r, "SpatialGridDataFrame"),
   paste0(fileName,".asc"),
   drivername = "AAIGrid")

  print("Done")
  rm(raster_final_re)
  rm(raster_tend)
  rm(r_raster)
  rm(r)
}

mclapply(fileNames, ScriptR, mc.set.seed=FALSE)
