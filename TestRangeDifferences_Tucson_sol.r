
library("raster")
library("dismo")
library("rgeos")
library("rJava")
library("knitr")
library("ENMTools")

setwd("/scratch/dnjacks4/asks/smaller/tucson")

assign("empirical", raster('/scratch/dnjacks4/asks/smaller/tucson/maxent_output/diff_tucson_mar13.asc'))


null.files <- list.files(path="/scratch/dnjacks4/asks/smaller/tucson/null_output/asks/diff/", pattern = '(.asc$)', full.names=TRUE)

stat.raster.noca <- empirical * 0

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

stat.raster.noca <- stat.raster.noca + r

}

empirical_noca <- empirical 
empirical_noca[stat.raster < 950] <- 0



rgdal::writeGDAL(as(empirical, "SpatialGridDataFrame"),
 paste("stat_difference.asc"),
 drivername = "AAIGrid")

assign("empirical", raster('/scratch/dnjacks4/asks/smaller/tucson/maxent_output/diff_tucson.asc'))


stat.raster.pyrr <- empirical * 0


for (file in null.files){

assign("null", raster(paste0(file)))

# if empirical is (greater) than null, add one to stat.raster
# Then filter the raster to only sites above 950 (alpha = 0.05)

# two tailed
# r <- |empirical| - |null|

# one tailed (only if noca > pyrr)

empirical_pyrr <- empirical * -1
null_pyrr <- null * -1 
empirical_pyrr[empirical_pyrr < 0] <- 0
null_pyrr[null_pyrr < 0] <- 0

r <- empirical_pyrr - null_pyrr

r[r < 0] <- 0
r[r > 0] <- 1

stat.raster.pyrr <- stat.raster.pyrr + r

}

empirical_pyrr <- empirical 

empirical_pyrr[stat.raster.pyrr < 950] <- 0


empirical_both <- empirical_noca + empirical_pyrr

rgdal::writeGDAL(as(empirical_both, "SpatialGridDataFrame"),
 paste("stat_difference_both.asc"),
 drivername = "AAIGrid")

 rgdal::writeGDAL(as(empirical_pyrr, "SpatialGridDataFrame"),
 paste("stat_difference_pyrr.asc"),
 drivername = "AAIGrid")


 rgdal::writeGDAL(as(empirical_noca, "SpatialGridDataFrame"),
 paste("stat_difference_noca.asc"),
 drivername = "AAIGrid")