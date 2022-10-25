#!/usr/bin/env Rscript
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md

# running maxent through commandline without gui

library("raster")
library("dismo")
library("rgeos")
library("rJava")
library("knitr")
library("ENMTools")

setwd("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull")

knitr::opts_knit$set(root.dir = '/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar",
    destfile = paste0(system.file("java", package = "dismo"),
        "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute



all.env.files<-list.files(path="/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull/EnvFiles",full.names=TRUE)

all.env<-stack(all.env.files)

names(all.env)<-c("Clim_PC2", "Clim_PC3", "Dev_PC1", "Dev_PC2", "Elev", "MedianHouseholdIncome", "nlcd_11", "nlcd_41", "nlcd_42", "nlcd_52", "nlcd_71", "nlcd_81", "nlcd_82", "nlcd_90")


# DealwithNAseitherbypropogatingthemtoallotherlayers,orbyturningthemallto0

#propogatemethod
#env<-check.env(env)

#0method

numbers<-c(1:14)
for(item in numbers)
{
all.env[[item]][is.na(all.env[[item]])]<-0
}




# make bird species objects that have colnames(spp)  <- c("species", "lat", "lon")

birds <- read.csv("/Volumes/BackupPlus/GIS_files/December2021/Birds/NOCAandPYRR_eBirdandTBCandCAPLTER_04_05_2017to2021.csv")

keep.cols <- c("species", "decimalLatitude", "decimalLongitude")
colnames(birds)  <- c("species", "lat", "lon")

crs(all.env) <- "+proj=longlat +datum=WGS84 +no_defs"


birds_occ_clean <- subset(birds, (!is.na(lat)) & (!is.na(lon)))

# remove duplicated data based on latitude and longitude
dups <- duplicated(birds_occ_clean[c("lat", "lon")])
birds_occ_unique <- birds_occ_clean[!dups, ]
cat(nrow(birds_occ_clean) - nrow(birds_occ_unique), "records are removed")

# make occ spatial
coordinates(birds_occ_unique) <- ~lon + lat
## look for erroneous points


# remove erroneous points (i.e., only keep good records)
# Set to your own boundaries!!!
birds_occ_unique <- birds_occ_unique[which(birds_occ_unique$lon > -111.183682000 & birds_occ_unique$lon <
    -110.720903000), ]
birds_occ_unique <- birds_occ_unique[which(birds_occ_unique$lat > 32.034553000 & birds_occ_unique$lat <
    32.554540000), ]

permute.function <- function()  {

  # Permute the data
  shuffle <- as.data.frame(birds_occ_unique)

  shuffle <- transform(shuffle, species = sample(species))
  coordinates(shuffle) <- ~lon + lat
  # Assign subset to species

  noca_shuffle_unique <- subset(shuffle, species == "Northern Cardinal")


  # thin shuffle data (keep one occurrence point per cell)
  cells <- cellFromXY(all.env[[1]], noca_shuffle_unique)
  dups <- duplicated(cells)
  noca_shuffle_final <- noca_shuffle_unique[!dups, ]
  cat(nrow(noca_shuffle_unique) - nrow(noca_shuffle_final), "records are removed")


  # select background points from this buffered area; when the number provided
  # to set.seed() function, the same random sample will be selected in the next line
  # use this code before the sampleRandom function every time, if you want to get
  # the same "random samples"
  set.seed(1)
  bg <- sampleRandom(x=all.env,
                     size=10000,
                     na.rm=T, #removes the 'Not Applicable' points
                     sp=T) # return spatial points



  # get the same random sample for training and testing
  set.seed(1)

  # randomly select 50% for training
  selected <- sample(1:nrow(noca_shuffle_final), nrow(noca_shuffle_final) * 0.5)

  noca_shuffle_train <- noca_shuffle_final[selected, ]  # this is the selection to be used for model training
  noca_shuffle_test <- noca_shuffle_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

  # extracting env conditions for training shuffle from the raster
  # stack; a data frame is returned (i.e multiple columns)
  p <- extract(all.env, noca_shuffle_train)
  # env conditions for testing shuffle
  p_test <- extract(all.env, noca_shuffle_test)
  # extracting env conditions for background
  a <- extract(all.env, bg)


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
  # mod <- maxent(x=clim,p=shuffle_train)

  # train Maxent with tabular data
  mod <- maxent(x=pder, ## env conditions
                p=pa,   ## 1:presence or 0:absence

                path=paste0("./output/maxent_outputs"), ## folder for maxent output;
                # if we do not specify a folder R will put the results in a temp file,
                # and it gets messy to read those. . .
                args=c("responsecurves") ## parameter specification
                )
  # the maxent functions runs a model in the default settings. To change these parameters,
  # you have to tell it what you want...i.e. response curves or the type of features

  # view the maxent model in a html brower
  mod

  # count number of files in output directory, divide by 2, and store that as a number
  x <- length(list.files("asks/diff/"))/3 + 1


  # save detailed results
  write.csv(mod@results, paste0("stats/noca_", x, ".csv"))


  # example 1, project to study area [raster]
  noca <- predict(mod, all.env)  # studyArea is the clipped rasters


   rgdal::writeGDAL(as(noca, "SpatialGridDataFrame"),
   paste0("asks/noca/noca_", x, ".asc"),
    drivername = "AAIGrid")






  # for pyrrhuloxia
  pyrr_shuffle_unique <- subset(shuffle, species == "Pyrrhuloxia")


  # thin shuffle data (keep one occurrence point per cell)
  cells <- cellFromXY(all.env[[1]], pyrr_shuffle_unique)
  dups <- duplicated(cells)
  pyrr_shuffle_final <- pyrr_shuffle_unique[!dups, ]
  cat(nrow(pyrr_shuffle_unique) - nrow(pyrr_shuffle_final), "records are removed")



  # select background points from this buffered area; when the number provided
  # to set.seed() function, the same random sample will be selected in the next line
  # use this code before the sampleRandom function every time, if you want to get
  # the same "random samples"
  set.seed(1)
  bg <- sampleRandom(x=all.env,
                     size=10000,
                     na.rm=T, #removes the 'Not Applicable' points
                     sp=T) # return spatial points



  # get the same random sample for training and testing
  set.seed(1)

  # randomly select 50% for training
  selected <- sample(1:nrow(pyrr_shuffle_final), nrow(pyrr_shuffle_final) * 0.5)

  pyrr_shuffle_train <- pyrr_shuffle_final[selected, ]  # this is the selection to be used for model training
  pyrr_shuffle_test <- pyrr_shuffle_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

  # extracting env conditions for training shuffle from the raster
  # stack; a data frame is returned (i.e multiple columns)
  p <- extract(all.env, pyrr_shuffle_train)
  # env conditions for testing shuffle
  p_test <- extract(all.env, pyrr_shuffle_test)
  # extracting env conditions for background
  a <- extract(all.env, bg)


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

                path=paste0("./output/maxent_outputs"), ## folder for maxent output;
                # if we do not specify a folder R will put the results in a temp file,
                # and it gets messy to read those. . .
                args=c("responsecurves") ## parameter specification
                )
  # the maxent functions runs a model in the default settings. To change these parameters,
  # you have to tell it what you want...i.e. response curves or the type of features

  # view the maxent model in a html brower
  mod

  # save detailed results
  write.csv(mod@results, paste0("stats/pyrr_", x, ".csv"))



  # example 1, project to study area [raster]
  pyrr <- predict(mod, all.env)  # studyArea is the clipped rasters


  rgdal::writeGDAL(as(pyrr, "SpatialGridDataFrame"),
  paste0("asks/pyrr/pyrr_", x, ".asc"),
   drivername = "AAIGrid")


  diff <- noca - pyrr

  rgdal::writeGDAL(as(diff, "SpatialGridDataFrame"),
  paste0("asks/diff/diff_", x, ".asc"),
   drivername = "AAIGrid")
   }



replicate(1000, permute.function())
