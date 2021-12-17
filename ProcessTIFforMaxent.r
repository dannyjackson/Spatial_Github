# Version with no resampling
#!/usr/bin/env Rscript

library(sf)

library(raster)

library(rgdal)

library(tidyverse)

library(rgeos)

library(scales)

library(fasterize)

library(maptools)

# for tucson
# setwd("/Volumes/BackupPlus/GIS_files/TucsonEnvironmentalLayers/forMaxent_noNA_tucson/")

# for phoenix
setwd("/Volumes/BackupPlus/GIS_files/December2021/Phoenix")

# set up projection parameter for use throughout script

projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#set up extent parameter for use throughout script

# tucson
# ext <- extent(-111.4, -110.6, 31.7, 32.6)

# phoenix
ext <- extent(-112.8, -111.4, 33.1, 33.9)

# process reference file
assign(paste0("reference_", "raw"), raster('/Volumes/BackupPlus/GIS_files/December2021/TIF/Land.tif'))

reference_projected <- projectRaster(reference_raw, reference_raw)

rm(reference_raw)

# create variable equal to final raster

assign(paste0("reference_final"), reference_projected)

rm(reference_projected)




# Resample all datasets across pixels of reference file

# reference_final_re <- resample(reference_final, reference_final)


# Re-extend reference dataset to make sure that their shared extent was not influenced by the resampling

reference_tend <- extend(reference_final, ext, value=NA)

# tucson extent
# e <- as(extent(-111.4, -110.6, 31.7, 32.6), 'SpatialPolygons')

# phoenix extent
e <- as(extent(-112.8, -111.4, 33.1, 33.9), 'SpatialPolygons')

crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(reference_tend, e)

# vector <- readOGR('/Volumes/BackupPlus/GIS_files/TBC/clipped/Tucson.shp')
# crs(vector) <- "+proj=longlat +datum=WGS84 +no_defs"

# r_penultimate <- mask(reference_tend, vector)
# r_final2 <- crop(r_penultimate, vector)

#Write these environmental datasets out in .asc format

writeRaster(r, filename=paste0("land.asc"), format="ascii", overwrite=TRUE)



# now repeat for all nonreference files

fileNames <- Sys.glob("/Volumes/BackupPlus/GIS_files/December2021/TIF/*.tif")

for (fileName in fileNames) {
# looped over buffered files already prepped for maxent

  print(fileName)

  # process reference file
  assign(paste0("raster_", "raw"), raster(paste0(fileName)))

  raster_projected <- projectRaster(raster_raw, reference_tend, value=NA)

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

  # tucson
  # e <- as(extent(-111.4, -110.6, 31.7, 32.6), 'SpatialPolygons')

  # phoenix
  e <- as(extent(-112.8, -111.4, 33.1, 33.9), 'SpatialPolygons')

  # crs(vector) <- "+proj=longlat +datum=WGS84 +no_defs"

  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r <- crop(raster_tend, e)

  # vector <- readOGR('/Volumes/BackupPlus/GIS_files/TBC/clipped/Tucson.shp')
  # crs(vector) <- "+proj=longlat +datum=WGS84 +no_defs"

  # r_raster_penultimate <- mask(raster_tend, vector)
  # r_raster <- crop(r_raster_penultimate, vector)

  #Now all datasets are identical in extent, resolution, etc.

  #Write these environmental datasets out in .asc format

  print("Writing to file")

  writeRaster(r, filename=paste0(fileName,".asc"), format="ascii", overwrite=TRUE)

  print("Done")
  rm(raster_final_re)
  rm(raster_tend)
  rm(r_raster)
  rm(r)
}
