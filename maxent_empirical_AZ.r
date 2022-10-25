#!/usr/bin/env Rscript
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md

# running maxent through commandline without gui

# person who is updating proj: ians@asu.edu

# interactive --mem=50G -t 1-0


module load r/4.2.1-BLAS
module load gdal/3.0.4
module load sqlite/3.25
# module load proj4/4.9.3

module load proj/6.3.1
module load proj/7.2.1

install.packages("rJava", lib='/home/dnjacks4/R/'),


configure.args=c('--with-proj-share=/usr/share/proj/', '--with-proj-include=/packages/7x/proj/6.3.1/include/'))


library("raster"), lib='/home/dnjacks4/R/')
library("dismo", lib='/home/dnjacks4/R/')
library("rgeos"), lib='/home/dnjacks4/R/')
library("rJava", lib='/home/dnjacks4/R/')
library("knitr", lib='/home/dnjacks4/R/')
library("ENMTools")

library(devtools)
install_github("danlwarren/ENMTools")
library(ENMTools)

setwd("/Volumes/GreyDrive/May2022/FromAgave/ArizonaLayer/MaxentEmpirical/")
setwd("~/FilesToAgave/ArizonaLayer")

# ~/Tifs_FromBackupPlus/Tifs

knitr::opts_knit$set(root.dir = '/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentEmpirical/')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar",
    destfile = paste0(system.file("java", package = "dismo"),
        "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute








# clim.env.files<-list.files(path="/Volumes/BackupPlus/GIS_files/May2022/FromAgave/ArizonaLayer/scratch/asks",full.names=TRUE)

clim.env.files<-list.files(path="/scratch/dnjacks4/asks",full.names=TRUE)

clim.env<-stack(clim.env.files)

names(clim.env)<-c("Clim1.tif", "Clim10.tif", "Clim11.tif", "Clim12.tif", "Clim13.tif", "Clim14.tif", "Clim15.tif", "Clim16.tif", "Clim17.tif", "Clim18.tif", "Clim19.tif", "Clim2.tif", "Clim3.tif", "Clim4.tif", "Clim5.tif", "Clim6.tif", "Clim7.tif", "Clim8.tif", "Clim9.tif")


# DealwithNAseitherbypropogatingthemtoallotherlayers,orbyturningthemallto0

#propogatemethod
#env<-check.env(env)

#0method

numbers <- c(1:19)
for(item in numbers)
{
clim.env[[item]][is.na(clim.env[[item]])]<-0
}


#parallelized
numbers <- seq(1, 19)

Scriptr = function(item){
clim.env[[item]][is.na(clim.env[[item]])]<-0;
};

mclapply(numbers, ScriptR, mc.set.seed=FALSE)

correlations<-raster.cor.matrix(clim.env)
write.csv(correlations,"correlations_AZ_clim.csv")
plot<-raster.cor.plot(clim.env)

pdf(file="corr_plot_AZ_clim.pdf",width=10,height=10,useDingbats=FALSE)
plot
dev.off()


clim.pca <- raster.pca(clim.env, 3)

clim.pc1 <- subset(clim.pca$rasters, "PC1")
clim.pc2 <- subset(clim.pca$rasters, "PC2")
clim.pc3 <- subset(clim.pca$rasters, "PC3")

rgdal::writeGDAL(as(clim.pc1, "SpatialGridDataFrame"),
 paste("Clim_PC1.asc"),
 drivername = "AAIGrid")

 rgdal::writeGDAL(as(clim.pc2, "SpatialGridDataFrame"),
  paste("Clim_PC2.asc"),
  drivername = "AAIGrid")

  rgdal::writeGDAL(as(clim.pc3, "SpatialGridDataFrame"),
   paste("Clim_PC3.asc"),
   drivername = "AAIGrid")


dev.env.files<-list.files(path="/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/formaxent/files/DevelopmentFiles",full.names=TRUE)


dev.env<-stack(dev.env.files)

names(dev.env)<-c("nlcd_21.tif", "nlcd_22.tif", "nlcd_23.tif", "nlcd_24.tif")


# DealwithNAseitherbypropogatingthemtoallotherlayers,orbyturningthemallto0

#propogatemethod
#env<-check.env(env)

#0method

numbers<-c(1:4)
for(item in numbers)
{
dev.env[[item]][is.na(dev.env[[item]])]<-0
}

correlations<-raster.cor.matrix(dev.env)
write.csv(correlations,"correlations_tuc_dev.csv")
plot<-raster.cor.plot(dev.env)

pdf(file="corr_plot_tuc_dev.pdf",width=10,height=10,useDingbats=FALSE)
plot
dev.off()


dev.pca <- raster.pca(dev.env, 2)



env.files<-list.files(path="/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/formaxent/files/OtherFiles",full.names=TRUE)

other.env<-stack(env.files)

names(other.env)<-c("Elev.tif", "MedianHouseholdIncome.tif", "nlcd_11.tif", "nlcd_31.tif", "nlcd_41.tif", "nlcd_42.tif", "nlcd_43.tif", "nlcd_52.tif", "nlcd_71.tif", "nlcd_81.tif", "nlcd_82.tif", "nlcd_90.tif")


numbers<-c(1:12)
for(item in numbers)
{
other.env[[item]][is.na(other.env[[item]])]<-0
}




correlations<-raster.cor.matrix(other.env)
write.csv(correlations,"correlations_tuc_other.csv")
plot<-raster.cor.plot(env)

pdf(file="corr_plot_tuc_other.pdf",width=10,height=10,useDingbats=FALSE)
plot
dev.off()



env <- raster::stack(other.env, clim.pca$rasters, dev.pca$rasters)

names(env)<-c("Elev", "MedianHouseholdIncome", "nlcd_11", "nlcd_31", "nlcd_41", "nlcd_42", "nlcd_43", "nlcd_52", "nlcd_71", "nlcd_81", "nlcd_82", "nlcd_90", "Clim_PC1", "Clim_PC2", "Clim_PC3", "Dev_PC1", "Dev_PC2")

# Drop NLCD_43, Clim_PC1, and NLCD_31

env <- dropLayer(env, c("Clim_PC1", "nlcd_43", "nlcd_31"))

Clim_PC2 <- subset(env, "Clim_PC2")
Clim_PC3 <- subset(env, "Clim_PC3")
Dev_PC1 <- subset(env, "Dev_PC1")
Dev_PC2 <- subset(env, "Dev_PC2")

rgdal::writeGDAL(as(Clim_PC2, "SpatialGridDataFrame"),
 paste("Clim_PC2.asc"),
 drivername = "AAIGrid")

rgdal::writeGDAL(as(Clim_PC3, "SpatialGridDataFrame"),
 paste("Clim_PC3.asc"),
 drivername = "AAIGrid")

rgdal::writeGDAL(as(Dev_PC1, "SpatialGridDataFrame"),
 paste("Dev_PC1.asc"),
 drivername = "AAIGrid")

rgdal::writeGDAL(as(Dev_PC2, "SpatialGridDataFrame"),
 paste("Dev_PC2.asc"),
 drivername = "AAIGrid")

correlations<-raster.cor.matrix(env)
write.csv(correlations,"correlations_tuc_all.csv")
plot<-raster.cor.plot(env)

pdf(file="corr_plot_tuc_all.pdf",width=10,height=10,useDingbats=FALSE)
plot
dev.off()




# make bird species objects that have colnames(spp)  <- c("species", "lat", "lon")

birds <- read.csv("/Volumes/BackupPlus/GIS_files/December2021/Birds/NOCAandPYRR_eBirdandTBCandCAPLTER_04_05_2017to2021.csv")

keep.cols <- c("species", "decimalLatitude", "decimalLongitude")
colnames(birds)  <- c("species", "lat", "lon")

noca_occ_raw <- subset(birds, species == "Northern Cardinal")

crs(env) <- "+proj=longlat +datum=WGS84 +no_defs"


noca_occ_clean <- subset(noca_occ_raw, (!is.na(lat)) & (!is.na(lon)))  #  '!' means the opposite logic value
cat(nrow(noca_occ_raw) - nrow(noca_occ_clean), "records are removed")

# remove duplicated data based on latitude and longitude
dups <- duplicated(noca_occ_clean[c("lat", "lon")])
noca_occ_unique <- noca_occ_clean[!dups, ]
cat(nrow(noca_occ_clean) - nrow(noca_occ_unique), "records are removed")

# make occ spatial
coordinates(noca_occ_unique) <- ~lon + lat
## look for erroneous points
plot(env[[1]])  # to the first layer of the bioclim layers as a reference
plot(noca_occ_unique, add = TRUE)  # plot the oc_unique on the above raster layer

# remove erroneous points (i.e., only keep good records)
# Set to your own boundaries!!!
noca_occ_unique <- noca_occ_unique[which(noca_occ_unique$lon > -111.183682000 & noca_occ_unique$lon <
    -110.720903000), ]
noca_occ_unique <- noca_occ_unique[which(noca_occ_unique$lat > 32.034553000 & noca_occ_unique$lat <
    32.554540000), ]



# thin occ data (keep one occurrence point per cell)
cells <- cellFromXY(env[[1]], noca_occ_unique)
dups <- duplicated(cells)
noca_occ_final <- noca_occ_unique[!dups, ]
cat(nrow(noca_occ_unique) - nrow(noca_occ_final), "records are removed")









# select background points from this buffered area; when the number provided
# to set.seed() function, the same random sample will be selected in the next line
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
set.seed(1)
bg <- sampleRandom(x=env,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points
                   sp=T) # return spatial points

plot(env[[1]])
# add the background points to the plotted raster
plot(bg,add=T)
# add the occurrence data to the plotted raster
plot(noca_occ_final,add=T,col="red")


# get the same random sample for training and testing
set.seed(1)

# randomly select 50% for training
selected <- sample(1:nrow(noca_occ_final), nrow(noca_occ_final) * 0.5)

noca_occ_train <- noca_occ_final[selected, ]  # this is the selection to be used for model training
noca_occ_test <- noca_occ_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- extract(env, noca_occ_train)
# env conditions for testing occ
p_test <- extract(env, noca_occ_test)
# extracting env conditions for background
a <- extract(env, bg)


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

# view detailed results
mod@results


# example 1, project to study area [raster]
noca <- predict(mod, env)  # studyArea is the clipped rasters
plot(noca)  # plot the continuous prediction

rgdal::writeGDAL(as(noca, "SpatialGridDataFrame"),
 paste("noca.asc"),
 drivername = "AAIGrid")








# for pyrrhuloxia

pyrr_occ_raw <- subset(birds, species == "Pyrrhuloxia")

pyrr_occ_clean <- subset(pyrr_occ_raw, (!is.na(lat)) & (!is.na(lon)))  #  '!' means the opposite logic value
cat(nrow(pyrr_occ_raw) - nrow(pyrr_occ_clean), "records are removed")

# remove duplicated data based on latitude and longitude
dups <- duplicated(pyrr_occ_clean[c("lat", "lon")])
pyrr_occ_unique <- pyrr_occ_clean[!dups, ]
cat(nrow(pyrr_occ_clean) - nrow(pyrr_occ_unique), "records are removed")

# make occ spatial
coordinates(pyrr_occ_unique) <- ~lon + lat
## look for erroneous points
plot(env[[1]])  # to the first layer of the bioclim layers as a reference
plot(pyrr_occ_unique, add = TRUE)  # plot the oc_unique on the above raster layer

# remove erroneous points (i.e., only keep good records)
# Set to your own boundaries!!!
pyrr_occ_unique <- pyrr_occ_unique[which(pyrr_occ_unique$lon > -111.183682000 & pyrr_occ_unique$lon <
    -110.720903000), ]
pyrr_occ_unique <- pyrr_occ_unique[which(pyrr_occ_unique$lat > 32.034553000 & pyrr_occ_unique$lat <
    32.554540000), ]

    plot(env[[1]])  # to the first layer of the bioclim layers as a reference
    plot(pyrr_occ_unique, add = TRUE)  # plot the oc_unique on the above raster layer


# thin occ data (keep one occurrence point per cell)
cells <- cellFromXY(env[[1]], pyrr_occ_unique)
dups <- duplicated(cells)
pyrr_occ_final <- pyrr_occ_unique[!dups, ]
cat(nrow(pyrr_occ_unique) - nrow(pyrr_occ_final), "records are removed")


# select background points from this buffered area; when the number provided
# to set.seed() function, the same random sample will be selected in the next line
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
set.seed(1)
bg <- sampleRandom(x=env,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points
                   sp=T) # return spatial points

plot(env[[1]])
# add the background points to the plotted raster
plot(bg,add=T)
# add the occurrence data to the plotted raster
plot(pyrr_occ_final,add=T,col="red")


# get the same random sample for training and testing
set.seed(1)

# randomly select 50% for training
selected <- sample(1:nrow(pyrr_occ_final), nrow(pyrr_occ_final) * 0.5)

pyrr_occ_train <- pyrr_occ_final[selected, ]  # this is the selection to be used for model training
pyrr_occ_test <- pyrr_occ_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- extract(env, pyrr_occ_train)
# env conditions for testing occ
p_test <- extract(env, pyrr_occ_test)
# extracting env conditions for background
a <- extract(env, bg)


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

# view detailed results
mod@results


# example 1, project to study area [raster]
pyrr <- predict(mod, env)  # studyArea is the clipped rasters
plot(pyrr)  # plot the continuous prediction

rgdal::writeGDAL(as(pyrr, "SpatialGridDataFrame"),
 paste("pyrr.asc"),
 drivername = "AAIGrid")

diff <- noca - pyrr

rgdal::writeGDAL(as(diff, "SpatialGridDataFrame"),
 paste("diff.asc"),
 drivername = "AAIGrid")
