#!/usr/bin/env Rscript

library("raster")
library("dismo")
library("rgeos")
library("rJava")
library("knitr")
library("ENMTools")

setwd("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull")

assign("empirical", raster('/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentEmpirical/diff.asc'))


null.files <- list.files(path="/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull/asks/diff/", pattern = '(.asc$)', full.names=TRUE)

stat.raster <- empirical * 0

for (file in null.files){

assign("null", raster(paste0(file)))

# if empirical is (greater) than null, add one to stat.raster
# Then filter the raster to only sites above 950 (alpha = 0.05)

# two tailed
# r <- |empirical| - |null|

# one tailed (only if noca > pyrr)

empirical[empirical < 0] <- 0
null[null < 0] <- 0

r <- empirical - null

r[r < 0] <- 0
r[r > 0] <- 1

stat.raster <- stat.raster + r

}

empirical[stat.raster < 950] <- 0



rgdal::writeGDAL(as(empirical, "SpatialGridDataFrame"),
 paste("stat_difference.asc"),
 drivername = "AAIGrid")
