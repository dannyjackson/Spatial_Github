module purge
module load r-4.2.2-gcc-11.2.0
module load sqlite-3.38.5-gcc-11.2.0
module load proj-8.2.1-gcc-11.2.0
module load gdal-3.4.3-gcc-11.2.0
module load geos-3.9.1-gcc-11.2.0
module load  gcc-12.1.0-gcc-11.2.0
module load libxc-5.1.7-gcc-12.1.0
module load libvterm-0.1.4-gcc-11.2.0

# running maxent through commandline without gui

library("raster")
library("dismo")
library("rgeos")
library("rJava")
library("knitr")
library("ENMTools")


setwd("/scratch/dnjacks4/asks/smaller/tucson/")




knitr::opts_knit$set(root.dir = '/scratch/dnjacks4/asks/smaller/tucson/maxent_output/')
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar",
    destfile = paste0(system.file("java", package = "dismo"),
        "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute


all.env.files<-list.files(path="/scratch/dnjacks4/asks/smaller/tucson/formaxent", pattern = "*.asc$", full.names=TRUE)

env<-stack(all.env.files)


correlations<-raster.cor.matrix(env)

print("writing correlations matrix")
write.csv(correlations,"correlations_Tuc_all.csv")


birds <- read.csv("~/FilesToAgave/Birds/NOCAandPYRR_eBirdandTBCandCAPLTER_04_05_2017to2021.csv")



# make bird species objects that have colnames(spp)  <- c("species", "lat", "lon")

birds <- read.csv("~/FilesToAgave/Birds/NOCAandPYRR_eBirdandTBCandCAPLTER_04_05_2017to2021.csv")


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
# noca_occ_unique <- noca_occ_unique[which(noca_occ_unique$lon > -111.183682000 & noca_occ_unique$lon <
    -110.720903000), ]
# noca_occ_unique <- noca_occ_unique[which(noca_occ_unique$lat > 32.034553000 & noca_occ_unique$lat <
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
                   size=2500,
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





############### pyrrhuloxia





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
# pyrr_occ_unique <- pyrr_occ_unique[which(pyrr_occ_unique$lon > -111.183682000 & pyrr_occ_unique$lon <
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
                   size=2500,
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




pyrr_spp <- enmtools.species()

pyrr_spp$species.name <- "pyrrhuloxia"
pyrr_spp$presence.points <- pyrr_occ_test
crs(pyrr_spp$presence.points) <- crs(env)
pyrr_spp$range <- background.raster.buffer(pyrr_spp$presence.points, 50000, mask = env)
pyrr_spp$background.points <- background.points.buffer(points = pyrr_spp$presence.points,
radius = 20000, n = 1000, mask = env[[1]])

noca_spp <- enmtools.species()
noca_spp$species.name <- "northerncardinal"
noca_spp$presence.points <- noca_occ_test
crs(noca_spp$presence.points) <- crs(env)
noca_spp$range <- background.raster.buffer(noca_spp$presence.points, 50000, mask = env)
noca_spp$background.points <- background.points.buffer(points = noca_spp$presence.points,
radius = 20000, n = 1000, mask = env[[1]])

id.mx <- identity.test(species.1 = noca_spp, species.2 = pyrr_spp, env = env, type = "mx", nreps = 100)

id.mx

write.csv(id.mx$reps.overlap, "tucson_nichesimilarityreps.csv")
Identity test northerncardinal vs. pyrrhuloxia


Identity test northerncardinal vs. pyrrhuloxia

Identity test p-values:
         D          I   rank.cor      env.D      env.I    env.cor
0.01980198 0.02970297 0.04950495 0.06930693 0.07920792 0.19801980


Replicates:


|          |         D|         I|  rank.cor|     env.D|     env.I|   env.cor|
|:---------|---------:|---------:|---------:|---------:|---------:|---------:|
|empirical | 0.7422603| 0.9364460| 0.8052075| 0.4344639| 0.7390696| 0.1424654|
|rep 1     | 0.8124584| 0.9647743| 0.8619987| 0.7341720| 0.9266411| 0.4252979|
|rep 2     | 0.8007795| 0.9590123| 0.8598044| 0.6973884| 0.9152524| 0.4018990|
|rep 3     | 0.7951330| 0.9561415| 0.8353082| 0.5656355| 0.8403632| 0.2255796|
|rep 4     | 0.7755210| 0.9488503| 0.8310783| 0.5391490| 0.8120642| 0.1855800|
|rep 5     | 0.8129983| 0.9638689| 0.8586603| 0.4264993| 0.7312346| 0.2287873|
