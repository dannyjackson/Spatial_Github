# Version with no resampling
#!/usr/bin/env Rscript
# Uses this tutorial https://www.azavea.com/blog/2018/10/09/preparing-data-for-maxent-species-distribution-modeling-using-r/


interactive --mem=50G -t 1-0

module load r/4.1.0
module load gdal/3.0.4
module load sqlite/3.25
module load proj/6.3.1
module load proj4/4.9.3

checking PROJ: epsg found and readable... no
Error: proj/epsg not found
Either install missing proj support files, for example
the proj-nad and proj-epsg RPMs on systems using RPMs,
or if installed but not autodetected, set PROJ_LIB to the
correct path, and if need be use the --with-proj-share=
configure argument.


install.packages('sf', lib='/home/dnjacks4/R/')
install.packages('sp', lib='/home/dnjacks4/R/')
install.packages('tidyverse', lib='/home/dnjacks4/R/')
install.packages('rgeos', lib='/home/dnjacks4/R/')
install.packages('scales', lib='/home/dnjacks4/R/')
install.packages('fasterize', lib='/home/dnjacks4/R/')
install.packages('maptools', lib='/home/dnjacks4/R/')

library(sf)
library(sp)

library(raster)

library(rgdal)

library(tidyverse)

library(rgeos)

library(scales)

library(fasterize, lib='/home/dnjacks4/R/')

library(maptools)

# for tucson
# setwd("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/")
# setwd("/home/dnjacks4/FilesToAgave/TucsonLayer")

# for phoenix
# setwd("/home/dnjacks4/FilesToAgave/PhoenixLayer")

# for statewide
setwd("/home/dnjacks4/Tifs_FromBackupPlus/Tifs/tifs")

# set up projection parameter for use throughout script

projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#set up extent parameter for use throughout script

# tucson old extent
# ext <- extent(-111.4, -110.6, 31.7, 32.6)
# tucson new extent 2022
# ext <- extent(-111.183682000, -110.720903000, 32.034553000, 32.554540000)

# phoenix old extent
# ext <- extent(-112.8, -111.4, 33.1, 33.9)
# phoenix new extent 2022
# ext <- extent(-112.584727000, -111.425540000, 33.089419000, 33.885028000)
# Arizona
ext <- extent(-114.8, -109.0, 31.3, 37.0)

# process reference file
# assign(paste0("reference_", "raw"), raster('/Volumes/BackupPlus/GIS_files/May2022/FromAgave/Tifs/nlcd_continuous_clipped_r3_12.tif'))

assign(paste0("reference_", "raw"), raster('../nlcd_continuous_clipped_r3_12.tif'))

reference_projected <- projectRaster(reference_raw, crs=projection)

rm(reference_raw)

# create variable equal to final raster

assign(paste0("reference_final"), reference_projected)

rm(reference_projected)




# Resample all datasets across pixels of reference file

# reference_final_re <- resample(reference_final, reference_final)


# Re-extend reference dataset to make sure that their shared extent was not influenced by the resampling

reference_tend <- extend(reference_final, ext, value=NA)

# tucson extent old
# e <- as(extent(-111.4, -110.6, 31.7, 32.6), 'SpatialPolygons')
# tucson extent 2022
# e <- as(extent(-111.183682000, -110.720903000, 32.034553000, 32.554540000), 'SpatialPolygons')

# phoenix extent
# e <- as(extent(-112.584727000, -111.425540000, 33.089419000, 33.885028000), 'SpatialPolygons')

# arizona extent
e <- as(extent(-114.8, -109.0, 31.3, 37.0), 'SpatialPolygons')

# phoenix
# phx <- readOGR("/home/dnjacks4/FilesToAgave/PhoenixLayer/PhoenixLayer.shp")
# phx_w84 <- spTransform(phx, crs(reference_tend))
# r <- crop(reference_tend, extent(phx_w84))

# tucson
# tuc <- readOGR("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/TucsonLayer.shp")
# tuc <- readOGR("/home/dnjacks4/FilesToAgave/TucsonLayer/TucsonLayer.shp")
# tuc_w84 <- spTransform(tuc, crs(reference_tend))
# r <- crop(reference_tend, extent(tuc_w84))

# Arizona
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(reference_tend, e)
r <- crop(reference_tend, extent(e))

# vector <- readOGR('/Volumes/BackupPlus/GIS_files/TBC/clipped/Tucson.shp')
# crs(vector) <- "+proj=longlat +datum=WGS84 +no_defs"

# r_penultimate <- mask(reference_tend, vector)
# r_final2 <- crop(r_penultimate, vector)

r[is.na(r[])] <- 0

#Write these environmental datasets out in .asc format

rgdal::writeGDAL(as(r, "SpatialGridDataFrame"),
 paste("reference_11.asc"),
 drivername = "AAIGrid")

# writeRaster(r, filename=paste0("reference_11.asc"), format="ascii", overwrite=TRUE)



# now repeat for all nonreference files

# fileNames <- Sys.glob("/scratch/dnjacks4/Tifs/*.tif")

fileNames <- Sys.glob("./*.tif")

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

  # tucson extent
  # e <- as(extent(-111.183682000, -110.720903000, 32.034553000, 32.554540000), 'SpatialPolygons')

  # phoenix extent
  # e <- as(extent(-112.584727000, -111.425540000, 33.089419000, 33.885028000), 'SpatialPolygons')

  # arizona extent
  e <- as(extent(-114.8, -109.0, 31.3, 37.0), 'SpatialPolygons')

  # crs(vector) <- "+proj=longlat +datum=WGS84 +no_defs"

  # crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

  # Phoenix
  # phx <- readOGR("/home/dnjacks4/FilesToAgave/PhoenixLayer/PhoenixLayer.shp")
  # phx_w84 <- spTransform(phx, crs(reference_tend))
  # r <- crop(raster_tend, extent(phx_w84))

  # Tucson
  # tuc <- readOGR("/home/dnjacks4/FilesToAgave/TucsonLayer/# TucsonLayer.shp")
  # tuc <- readOGR("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/TucsonLayer.shp")

  # tuc_w84 <- spTransform(tuc, crs(reference_tend))
  # r <- crop(raster_tend, extent(tuc_w84))

  # Arizona
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r <- crop(reference_tend, extent(e))

  # vector <- readOGR('/Volumes/BackupPlus/GIS_files/TBC/clipped/Tucson.shp')
  # crs(vector) <- "+proj=longlat +datum=WGS84 +no_defs"

  # r_raster_penultimate <- mask(raster_tend, vector)
  # r_raster <- crop(r_raster_penultimate, vector)

  #Now all datasets are identical in extent, resolution, etc.

  r[is.na(r[])] <- 0

  #Write these environmental datasets out in .asc format

  print("Writing to file")

  # writeRaster(r, filename=paste0(fileName,".asc"), format="ascii", overwrite=TRUE)

  rgdal::writeGDAL(as(r, "SpatialGridDataFrame"),
   paste0(fileName,".asc"),
   drivername = "AAIGrid")

  print("Done")
  rm(raster_final_re)
  rm(raster_tend)
  rm(r_raster)
  rm(r)
}



interactive --mem=100G -t 1-0

# Tucson
# all birds
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/TucsonLayer/maxentoutput/ebirdTBC samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_eBirdAndTBC_04_05_2017to2021.csv environmentallayers=/home/dnjacks4/FilesToAgave/TucsonLayer/formaxent randomseed replicates=10 writeplotdata appendtoresultsfile threads=4

#ebird
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/TucsonLayer/maxentoutput/ebird samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_eBird_04_05_2017to2021.csv environmentallayers=/home/dnjacks4/FilesToAgave/TucsonLayer/formaxent randomseed replicates=10 writeplotdata appendtoresultsfile threads=4

#tbc
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/TucsonLayer/maxentoutput/TBC samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_TBC_04_05.csv environmentallayers=/home/dnjacks4/FilesToAgave/TucsonLayer/formaxent randomseed replicates=10 writeplotdata appendtoresultsfile threads=4




# Phoenix
# all birds
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/PhoenixLayer/maxentoutput/ebirdCAPLTER samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_eBirdandCAPLTER_04_05_2017to2021.csv environmentallayers=/home/dnjacks4/FilesToAgave/PhoenixLayer/formaxent randomseed replicates=10 writeplotdata appendtoresultsfile threads=4

#ebird
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/PhoenixLayer/maxentoutput/ebird samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_eBird_04_05_2017to2021.csv environmentallayers=/home/dnjacks4/FilesToAgave/PhoenixLayer/formaxent randomseed replicates=10 writeplotdata appendtoresultsfile threads=4

#cap lter
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/PhoenixLayer/maxentoutput/CAPLTER samplesfile=/home/dnjacks4/FilesToAgave/Birds/CAPLTER_NOCA_CoreAndAdditional.csv environmentallayers=/home/dnjacks4/FilesToAgave/PhoenixLayer/formaxent randomseed replicates=10 writeplotdata appendtoresultsfile threads=4




interactive --mem=75G -t 3-0

# Arizona
# all birds
# uncheck nlcd_az_canopy_r3
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/Arizona/maxentoutput/ebirdTBCCAPLTER samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_eBirdandTBCandCAPLTER_04_05_2017to2021.csv environmentallayers=/scratch/dnjacks4/Tifs randomseed replicates=10 writeplotdata appendtoresultsfile threads=4 -a


# ebird
# uncheck nlcd_az_canopy_r3
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/Arizona/maxentoutput/ebird samplesfile=/home/dnjacks4/FilesToAgave/Birds/NOCAandPYRR_eBird_04_05_2017to2021.csv environmentallayers=/home/dnjacks4/FilesToAgave/Tifs randomseed replicates=10 writeplotdata appendtoresultsfile threads=4

# cap lter and tbc
# uncheck nlcd_az_canopy_r3
java -mx10000m -jar maxent.jar nowarnings noprefixes responsecurves jackknife outputdirectory=/home/dnjacks4/FilesToAgave/Arizona/maxentoutput/TBCCAPLTER samplesfile=/home/dnjacks4/FilesToAgave/Birds/CAPLTER_TBC.csv  environmentallayers=/home/dnjacks4/FilesToAgave/Tifs randomseed replicates=10 writeplotdata appendtoresultsfile threads=4




# running commandline without gui
library("raster")
library("dismo")
library("rgeos")

library("rJava")

library("knitr")
knitr::opts_knit$set(root.dir = '/home/dnjacks4/code/')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar",
    destfile = paste0(system.file("java", package = "dismo"),
        "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute


# This searches for all files that are in the path
# 'data/bioclim/' and have a file extension of .bil. You can
# edit this code to reflect the path name and file extension
# for your environmental variables
clim_list <- list.files("../data/bioclim/", pattern = ".bil$",
    full.names = T)  # '..' leads to the path above the folder where the .rmd file is located

# stacking the bioclim variables to process them at one go
clim <- raster::stack(clim_list)


occ_raw <- read.csv("path_to_birds_csv")

occ_clean <- subset(occ_raw, (!is.na(lat)) & (!is.na(lon)))  #  '!' means the opposite logic value
cat(nrow(occ_raw) - nrow(occ_clean), "records are removed")

# remove duplicated data based on latitude and longitude
dups <- duplicated(occ_clean[c("lat", "lon")])
occ_unique <- occ_clean[!dups, ]
cat(nrow(occ_clean) - nrow(occ_unique), "records are removed")

# make occ spatial
coordinates(occ_unique) <- ~lon + lat
## look for erroneous points
plot(clim[[1]])  # to the first layer of the bioclim layers as a reference
plot(occ_unique, add = TRUE)  # plot the oc_unique on the above raster layer

# remove erroneous points (i.e., only keep good records)
# Set to your own boundaries!!!
occ_unique <- occ_unique[which(occ_unique$lon > -110 & occ_unique$lon <
    -40), ]


# thin occ data (keep one occurrence point per cell)
cells <- cellFromXY(clim[[1]], occ_unique)
dups <- duplicated(cells)
occ_final <- occ_unique[!dups, ]
cat(nrow(occ_unique) - nrow(occ_final), "records are removed")



#this is different for mine, since I just want the boundaries of PHX and Tucson and arizona# this creates a 4-decimal-degree buffer around the
# occurrence data
occ_buff <- buffer(occ_final, 4)

# plot the first element ([[1]]) in the raster stack
plot(clim[[1]])

plot(occ_final, add = T, col = "red")  # adds occurrence data to the plot
plot(occ_buff, add = T, col = "blue")  # adds buffer polygon to the plot# crop study area to a manageable extent (rectangle shaped)
studyArea <- crop(clim,extent(occ_buff))

# the 'study area' created by extracting the buffer area from the raster stack
studyArea <- mask(studyArea,occ_buff)
# output will still be a raster stack, just of the study area

# save the new study area rasters as ascii
writeRaster(studyArea,
            # a series of names for output files
            filename=paste0("../data/studyarea/",names(studyArea),".asc"),
            format="ascii", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)
# select background points from this buffered area; when the number provided
# to set.seed() function, the same random sample will be selected in the next line
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
set.seed(1)
bg <- sampleRandom(x=studyArea,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points
                   sp=T) # return spatial points

plot(studyArea[[1]])
# add the background points to the plotted raster
plot(bg,add=T)
# add the occurrence data to the plotted raster
plot(occ_final,add=T,col="red")


# get the same random sample for training and testing
set.seed(1)

# randomly select 50% for training
selected <- sample(1:nrow(occ_final), nrow(occ_final) * 0.5)

occ_train <- occ_final[selected, ]  # this is the selection to be used for model training
occ_test <- occ_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- extract(clim, occ_train)
# env conditions for testing occ
p_test <- extract(clim, occ_test)
# extracting env conditions for background
a <- extract(clim, bg)


# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))

# (rep(1,nrow(p)) creating the number of rows as the p data
# set to have the number '1' as the indicator for presence;
# rep(0,nrow(a)) creating the number of rows as the a data
# set to have the number '0' as the indicator for absence;
# the c combines these ones and zeros into a new vector that
# can be added to the Maxent table data frame with the
# environmental attributes of the presence and absence
# locations
pder <- as.data.frame(rbind(p, a))





# train Maxent with spatial data
# mod <- maxent(x=clim,p=occ_train)

# train Maxent with tabular data
mod <- maxent(x=pder, ## env conditions
              p=pa,   ## 1:presence or 0:absence

              path=paste0("../output/maxent_outputs"), ## folder for maxent output;
              # if we do not specify a folder R will put the results in a temp file,
              # and it gets messy to read those. . .
              args=c("responsecurves") ## parameter specification
              )
# the maxent functions runs a model in the default settings. To change these parameters,
# you have to tell it what you want...i.e. response curves or the type of features

# view the maxent model in a html brower
mod

# view detailed results
mod@results


# example 1, project to study area [raster]
ped1 <- predict(mod, studyArea)  # studyArea is the clipped rasters
plot(ped1)  # plot the continuous prediction

# example 2, project to the world ped2 <- predict(mod,clim)
# plot(ped2)

# example 3, project with training occurrences [dataframes]
ped3 <- predict(mod, p)
head(ped3)

hist(ped3)  # creates a histogram of the prediction
