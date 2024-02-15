module purge 
module load r-4.2.2-gcc-11.2.0
module load sqlite-3.38.5-gcc-11.2.0
module load proj-8.2.1-gcc-11.2.0
module load gdal-3.4.3-gcc-11.2.0
module load geos-3.9.1-gcc-11.2.0

library("terra")
library("raster")
library("dismo")
library("rgeos")
library("knitr")
library("ENMTools")
library("parallel")
library("vegan")
library("ggvegan")
library("tidyverse")

AZ.env.files<-list.files(path="/scratch/dnjacks4/asks/smaller/arizona/formaxent/", pattern = "*.asc$", full.names=TRUE)
AZ.clim.files<-list.files(path="/scratch/dnjacks4/asks/smaller/clim/", pattern = "*.asc$", full.names=TRUE)
AZ.dev.files<-list.files(path="/scratch/dnjacks4/asks/smaller/dev/", pattern = "*.asc$", full.names=TRUE)

Tuc.env.files<-list.files(path="/scratch/dnjacks4/asks/smaller/tucson/formaxent/", pattern = "*.asc$", full.names=TRUE)
Tuc.clim.files<-list.files(path="/scratch/dnjacks4/asks/smaller/tucson_clim/", pattern = "*.asc$", full.names=TRUE)
Tuc.dev.files<-list.files(path="/scratch/dnjacks4/asks/smaller/tucson_dev/", pattern = "*.asc$", full.names=TRUE)


Phx.env.files<-list.files(path="/scratch/dnjacks4/asks/smaller/phoenix/formaxent/", pattern = "*.asc$", full.names=TRUE)
Phx.clim.files<-list.files(path="/scratch/dnjacks4/asks/smaller/phoenix/clim/", pattern = "*.asc$", full.names=TRUE)
Phx.dev.files<-list.files(path="/scratch/dnjacks4/asks/smaller/phoenix/dev/", pattern = "*.asc$", full.names=TRUE)


# Phx.env.files<-list.files(path="/scratch/dnjacks4/asks/smaller/phoenix/formaxent/", pattern = "*.asc$", full.names=TRUE)
# Phx.clim.files<-list.files(path="/scratch/dnjacks4/asks/smaller/phoenix/clim/", pattern = "*.asc$", full.names=TRUE)
# Phx.dev.files<-list.files(path="/scratch/dnjacks4/asks/smaller/phoenix/dev/", pattern = "*.asc$", full.names=TRUE)

print("stacking all files")

AZ.env<-stack(AZ.env.files)
AZ.clim<-stack(AZ.clim.files)
AZ.dev<-stack(AZ.dev.files)
Tuc.env<-stack(Tuc.env.files)
Tuc.clim<-stack(Tuc.clim.files)
Tuc.dev<-stack(Tuc.dev.files)
Phx.env<-stack(Phx.env.files)
Phx.clim<-stack(Phx.clim.files)
Phx.dev<-stack(Phx.dev.files)
# Phx.env<-stack(Phx.env.files)
# Phx.clim<-stack(Phx.clim.files)
# Phx.dev<-stack(Phx.dev.files)

birds <- read.csv("~/FilesToAgave/Birds/NOCAandPYRR_eBirdandTBCandCAPLTER_04_05_2017to2021.csv")


keep.cols <- c("species", "decimalLatitude", "decimalLongitude")
colnames(birds)  <- c("species", "lat", "lon")

noca_occ_raw <- subset(birds, species == "Northern Cardinal")
pyrr_occ_raw <- subset(birds, species == "Pyrrhuloxia")


noca_occ_clean <- subset(noca_occ_raw, (!is.na(lat)) & (!is.na(lon)))  #  '!' means the opposite logic value
cat(nrow(noca_occ_raw) - nrow(noca_occ_clean), "records are removed")

# remove duplicated data based on latitude and longitude
dups <- duplicated(noca_occ_clean[c("lat", "lon")])
noca_occ_unique <- noca_occ_clean[!dups, ]
cat(nrow(noca_occ_clean) - nrow(noca_occ_unique), "records are removed")


pyrr_occ_clean <- subset(pyrr_occ_raw, (!is.na(lat)) & (!is.na(lon)))  #  '!' means the opposite logic value
cat(nrow(pyrr_occ_raw) - nrow(pyrr_occ_clean), "records are removed")

# remove duplicated data based on latitude and longitude
dups <- duplicated(pyrr_occ_clean[c("lat", "lon")])
pyrr_occ_unique <- pyrr_occ_clean[!dups, ]
cat(nrow(pyrr_occ_clean) - nrow(pyrr_occ_unique), "records are removed")


coordinates(pyrr_occ_unique)= ~ lon + lat
coordinates(noca_occ_unique)= ~ lon + lat

AZ.env.rasValue.pyrr=raster::extract(AZ.env, pyrr_occ_unique)
AZ.env.rasValue.pyrr = cbind(pyrr_occ_unique, AZ.env.rasValue.pyrr)

AZ.env.rasValue.noca=raster::extract(AZ.env, noca_occ_unique)
AZ.env.rasValue.noca = cbind(noca_occ_unique, AZ.env.rasValue.noca)

AZ.clim.rasValue.pyrr=raster::extract(AZ.clim, pyrr_occ_unique)
AZ.clim.rasValue.pyrr = cbind(pyrr_occ_unique, AZ.clim.rasValue.pyrr)

AZ.clim.rasValue.noca=raster::extract(AZ.clim, noca_occ_unique)
AZ.clim.rasValue.noca = cbind(noca_occ_unique, AZ.clim.rasValue.noca)

AZ.dev.rasValue.pyrr=raster::extract(AZ.dev, pyrr_occ_unique)
AZ.dev.rasValue.pyrr = cbind(pyrr_occ_unique, AZ.dev.rasValue.pyrr)

AZ.dev.rasValue.noca=raster::extract(AZ.dev, noca_occ_unique)
AZ.dev.rasValue.noca = cbind(noca_occ_unique, AZ.dev.rasValue.noca)


AZ.env.rasValue.both=rbind(AZ.env.rasValue.pyrr, AZ.env.rasValue.noca)
AZ.clim.rasValue.both=rbind(AZ.clim.rasValue.pyrr, AZ.clim.rasValue.noca)
AZ.dev.rasValue.both=rbind(AZ.dev.rasValue.pyrr, AZ.dev.rasValue.noca)

AZ.all.rasValue.both=cbind(AZ.env.rasValue.both, AZ.clim.rasValue.both, AZ.dev.rasValue.both)

# combinePointValue=cbind(AZ.env.rasValue.pyrr,AZ.env)
write.csv(AZ.all.rasValue.both,file="AZ.all.rasValue.both.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

AZ_df = AZ.all.rasValue.both
names(AZ_df)

rem <- c("name1","name2") # list of column names

AZ_df2 <- as.data.frame(AZ_df)

Species <- AZ_df2[,-c(1)]
AZ_df_slim <- AZ_df2[,-c(1,17,37,38)]
AZ_df_slim <- drop_na(AZ_df_slim)
AZ_PCA <- rda(AZ_df_slim)

AZ_PCA <- princomp(x = AZ_df_slim)
plot(AZ_PCA$scores, pch = 16, col = as.factor(Species))

AZ_df <- as.data.frame(AZ_df)
AZ_df <- drop_na(AZ_df)

t.test(AZ_df$canopy_az_masked.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$canopy_az_masked.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$canopy_az_masked.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$canopy_az_masked.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$ClimPC_smaller_2.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$ClimPC_smaller_2.tif.asc.croppedf[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$ClimPC_smaller_2.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$ClimPC_smaller_2.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$ClimPC_smaller_3.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$ClimPC_smaller_3.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$ClimPC_smaller_3.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$ClimPC_smaller_3.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Elev.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$Elev.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Elev.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Elev.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$imperviousness_az[AZ_df$species=="Northern Cardinal"], AZ_df$imperviousness_az[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$imperviousness_az, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$imperviousness_az, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$MedianHouseholdIncome.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$MedianHouseholdIncome.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$MedianHouseholdIncome.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$MedianHouseholdIncome.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_11.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_11.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_11.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_11.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_31.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_31.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_31.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_31.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_41.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_41.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_41.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_41.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_42.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_42.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_42.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_42.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_52.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_52.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_52.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_52.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_71.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_71.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_71.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_71.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_81.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_81.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_81.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_81.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_82.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_82.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_82.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_82.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$nlcd_continuous_clipped_r3_90.tif.asc.cropped[AZ_df$species=="Northern Cardinal"], AZ_df$nlcd_continuous_clipped_r3_90.tif.asc.cropped[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$nlcd_continuous_clipped_r3_90.tif.asc.cropped, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$nlcd_continuous_clipped_r3_90.tif.asc.cropped, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim1.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim1.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim1.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim1.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim10.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim10.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim10.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim10.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim11.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim11.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim11.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim11.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim12.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim12.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim12.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim12.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim13.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim13.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim13.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim13.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim14.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim14.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim14.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim14.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim15.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim15.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim15.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim15.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim16.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim16.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim16.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim16.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim17.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim17.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim17.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim17.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim18.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim18.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim18.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim18.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim19.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim19.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim19.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim19.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim2.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim2.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim2.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim2.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim3.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim3.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim3.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim3.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim4.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim4.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim4.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim4.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim5.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim5.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim5.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim5.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim6.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim6.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim6.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim6.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim7.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim7.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim7.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim7.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)


t.test(AZ_df$Clim8.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim8.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim8.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim8.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)

t.test(AZ_df$Clim9.tif[AZ_df$species=="Northern Cardinal"], AZ_df$Clim9.tif[AZ_df$species=="Pyrrhuloxia"])
aggregate(AZ_df$Clim9.tif, list(AZ_df$species), FUN=mean, na.rm=TRUE)
aggregate(AZ_df$Clim9.tif, list(AZ_df$species), FUN=sd, na.rm=TRUE)


Tuc.env.rasValue.pyrr=raster::extract(Tuc.env, pyrr_occ_unique)
Tuc.env.rasValue.pyrr = cbind(pyrr_occ_unique, Tuc.env.rasValue.pyrr)

Tuc.env.rasValue.noca=raster::extract(Tuc.env, noca_occ_unique)
Tuc.env.rasValue.noca = cbind(noca_occ_unique, Tuc.env.rasValue.noca)

Tuc.clim.rasValue.pyrr=raster::extract(Tuc.clim, pyrr_occ_unique)
Tuc.clim.rasValue.pyrr = cbind(pyrr_occ_unique, Tuc.clim.rasValue.pyrr)

Tuc.clim.rasValue.noca=raster::extract(Tuc.clim, noca_occ_unique)
Tuc.clim.rasValue.noca = cbind(noca_occ_unique, Tuc.clim.rasValue.noca)

Tuc.dev.rasValue.pyrr=raster::extract(Tuc.dev, pyrr_occ_unique)
Tuc.dev.rasValue.pyrr = cbind(pyrr_occ_unique, Tuc.dev.rasValue.pyrr)

Tuc.dev.rasValue.noca=raster::extract(Tuc.dev, noca_occ_unique)
Tuc.dev.rasValue.noca = cbind(noca_occ_unique, Tuc.dev.rasValue.noca)

Tuc.env.rasValue.both=rbind(Tuc.env.rasValue.pyrr, Tuc.env.rasValue.noca)
Tuc.clim.rasValue.both=rbind(Tuc.clim.rasValue.pyrr, Tuc.clim.rasValue.noca)
Tuc.dev.rasValue.both=rbind(Tuc.dev.rasValue.pyrr, Tuc.dev.rasValue.noca)

Tuc.all.rasValue.both=cbind(Tuc.env.rasValue.both, Tuc.clim.rasValue.both, Tuc.dev.rasValue.both)


Tuc_df = Tuc.all.rasValue.both
names(Tuc_df)


Tuc_df <- as.data.frame(Tuc_df)
Tuc_df <- drop_na(Tuc_df)

t.test(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_az_canopy_r3.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$ClimPC_smaller_2.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_az_canopy_r3.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$ClimPC_smaller_2.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$ClimPC_smaller_2.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$ClimPC_smaller_3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_az_canopy_r3.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$ClimPC_smaller_3.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$ClimPC_smaller_3.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Elev.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Elev.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Elev.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Elev.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$imperviousness_tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Elev.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$imperviousness_tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$imperviousness_tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$MedianHouseholdIncome.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$MedianHouseholdIncome.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$MedianHouseholdIncome.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$MedianHouseholdIncome.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_11.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_31.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_41.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_42.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_52.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_71.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_81.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_82.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_90.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim1.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim1.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim1.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim1.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim10.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim10.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim10.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim10.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim11.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim11.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim11.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim11.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim12.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim12.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim12.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim12.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim13.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim13.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim13.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim13.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim14.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim14.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim14.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim14.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim15.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim15.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim15.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim15.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim16.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim16.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim16.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim16.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim17.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim17.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim17.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim17.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim18.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim18.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim18.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim18.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim19.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim19.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim19.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim19.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim2.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim2.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim2.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim2.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim3.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim3.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim3.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim3.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim4.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim4.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim4.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim4.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim5.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim5.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim5.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim5.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim6.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim6.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim6.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim6.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim7.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim7.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim7.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim7.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)


t.test(Tuc_df$Clim8.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim8.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim8.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim8.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)


t.test(Tuc_df$Clim9.tif[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim9.tif[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim9.tif, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim9.tif, list(Tuc_df$species), FUN=sd, na.rm=TRUE)












Phx.env.rasValue.pyrr=raster::extract(Phx.env, pyrr_occ_unique)
Phx.env.rasValue.pyrr = cbind(pyrr_occ_unique, Phx.env.rasValue.pyrr)

Phx.env.rasValue.noca=raster::extract(Phx.env, noca_occ_unique)
Phx.env.rasValue.noca = cbind(noca_occ_unique, Phx.env.rasValue.noca)

Phx.clim.rasValue.pyrr=raster::extract(Phx.clim, pyrr_occ_unique)
Phx.clim.rasValue.pyrr = cbind(pyrr_occ_unique, Phx.clim.rasValue.pyrr)

Phx.clim.rasValue.noca=raster::extract(Phx.clim, noca_occ_unique)
Phx.clim.rasValue.noca = cbind(noca_occ_unique, Phx.clim.rasValue.noca)

Phx.dev.rasValue.pyrr=raster::extract(Phx.dev, pyrr_occ_unique)
Phx.dev.rasValue.pyrr = cbind(pyrr_occ_unique, Phx.dev.rasValue.pyrr)

Phx.dev.rasValue.noca=raster::extract(Phx.dev, noca_occ_unique)
Phx.dev.rasValue.noca = cbind(noca_occ_unique, Phx.dev.rasValue.noca)

Phx.env.rasValue.both=rbind(Phx.env.rasValue.pyrr, Phx.env.rasValue.noca)
Phx.clim.rasValue.both=rbind(Phx.clim.rasValue.pyrr, Phx.clim.rasValue.noca)
Phx.dev.rasValue.both=rbind(Phx.dev.rasValue.pyrr, Phx.dev.rasValue.noca)

Phx.all.rasValue.both=cbind(Phx.env.rasValue.both, Phx.clim.rasValue.both, Phx.dev.rasValue.both)


Phx_df = Phx.all.rasValue.both
names(Phx_df)


Phx_df <- as.data.frame(Phx_df)
Phx_df <- drop_na(Phx_df)

aggregate(Phx_df$file, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_az_canopy_r3.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_az_canopy_r3.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)


t.test(Tuc_df$Elev.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Elev.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Elev.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)


t.test(Tuc_df$imperviousness_tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$imperviousness_phoenix[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$imperviousness_phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$MedianHouseholdIncome.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$MedianHouseholdIncome.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$MedianHouseholdIncome.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_11.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_11.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_31.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_31.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_41.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_41.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_42.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_42.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_52.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_52.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_71.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_71.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_81.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_81.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_82.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_82.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Phx_df$nlcd_continuous_clipped_r3_90.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_90.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim1.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim1.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim1.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim10.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim10.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim10.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim11.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim11.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim11.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim12.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim12.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim12.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim13.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim13.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim13.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim14.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim14.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim14.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim15.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim15.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim15.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim16.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim16.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim16.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim17.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim17.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim17.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim18.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim18.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim18.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim19.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim19.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim19.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim2.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim2.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim2.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim3.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim3.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim3.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim4.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim4.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim4.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim5.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim5.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim5.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim6.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim6.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim6.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim7.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim7.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim7.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim8.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim8.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim8.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim9.tif[Tuc_df$species=="Northern Cardinal"], Phx_df$Clim9.tif[Phx_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim9.tif, list(Phx_df$species), FUN=sd, na.rm=TRUE)





















t.test(Tuc_df$Elev.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Elev.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Elev.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Elev.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$MedianHouseholdIncome.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$MedianHouseholdIncome.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$MedianHouseholdIncome.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$MedianHouseholdIncome.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)


t.test(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)


t.test(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim1.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim1.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim1.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim1.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim10.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim10.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim10.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim10.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim11.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim11.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim11.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim11.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim12.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim12.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim12.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim12.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim13.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim13.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim13.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim13.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim14.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim14.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim14.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim14.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim15.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim15.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim15.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim15.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim16.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim16.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim16.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim16.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim17.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim17.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim17.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim17.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim18.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim18.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim18.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim18.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim19.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim19.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim19.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim19.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim2.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim2.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim2.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim2.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim3.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim3.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim3.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim4.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim4.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim4.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim4.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim5.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim5.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim5.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim5.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim6.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim6.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim6.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim6.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim7.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim7.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim7.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim7.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim8.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim8.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim8.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim8.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$Clim9.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$Clim9.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$Clim9.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$Clim9.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_21.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_21.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_21.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_21.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_22.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_22.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_22.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_22.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_23.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_23.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_23.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_23.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

t.test(Tuc_df$nlcd_continuous_clipped_r3_24.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_24.tif.asc.tucson[Tuc_df$species=="Pyrrhuloxia"])
aggregate(Tuc_df$nlcd_continuous_clipped_r3_24.tif.asc.tucson, list(Tuc_df$species), FUN=mean, na.rm=TRUE)
aggregate(Tuc_df$nlcd_continuous_clipped_r3_24.tif.asc.tucson, list(Tuc_df$species), FUN=sd, na.rm=TRUE)

# combinePointValue=cbind(Tuc.env.rasValue.pyrr,Tuc.env)
write.csv(Tuc.all.rasValue.both,file="Tuc.all.rasValue.both.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)





Phx.env.rasValue.pyrr=extract(Phx.env, pyrr_occ_unique)
Phx.env.rasValue.pyrr = cbind(pyrr_occ_unique, Phx.env.rasValue.pyrr)

Phx.env.rasValue.noca=extract(Phx.env, noca_occ_unique)
Phx.env.rasValue.noca = cbind(noca_occ_unique, Phx.env.rasValue.noca)

Phx.clim.rasValue.pyrr=extract(Phx.clim, pyrr_occ_unique)
Phx.clim.rasValue.pyrr = cbind(pyrr_occ_unique, Phx.clim.rasValue.pyrr)

Phx.clim.rasValue.noca=extract(Phx.clim, noca_occ_unique)
Phx.clim.rasValue.noca = cbind(noca_occ_unique, Phx.clim.rasValue.noca)

Phx.dev.rasValue.pyrr=extract(Phx.dev, pyrr_occ_unique)
Phx.dev.rasValue.pyrr = cbind(pyrr_occ_unique, Phx.dev.rasValue.pyrr)

Phx.dev.rasValue.noca=extract(Phx.dev, noca_occ_unique)
Phx.dev.rasValue.noca = cbind(noca_occ_unique, Phx.dev.rasValue.noca)

Phx.env.rasValue.both=rbind(Phx.env.rasValue.pyrr, Phx.env.rasValue.noca)
Phx.clim.rasValue.both=rbind(Phx.clim.rasValue.pyrr, Phx.clim.rasValue.noca)
Phx.dev.rasValue.both=rbind(Phx.dev.rasValue.pyrr, Phx.dev.rasValue.noca)

Phx.all.rasValue.both=cbind(Phx.env.rasValue.both, Phx.clim.rasValue.both, Phx.dev.rasValue.both)


Phx_df = Phx.all.rasValue.both
names(Phx_df)


t.test(Phx_df$Elev.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Elev.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Elev.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Elev.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_az_canopy_r3.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_az_canopy_r3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_az_canopy_r3.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_az_canopy_r3.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$MedianHouseholdIncome.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$MedianHouseholdIncome.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$MedianHouseholdIncome.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$MedianHouseholdIncome.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_11.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_11.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_11.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_11.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_31.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_31.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_31.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_31.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_41.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_41.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_41.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_41.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)


t.test(Phx_df$nlcd_continuous_clipped_r3_42.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_42.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_42.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_42.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)


t.test(Phx_df$nlcd_continuous_clipped_r3_52.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_52.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_52.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_52.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_71.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_71.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_71.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_71.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_81.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_81.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_81.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_81.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_82.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_82.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_82.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_82.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_90.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_90.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_90.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_90.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim1.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim1.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim1.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim1.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim10.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim10.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim10.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim10.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim11.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim11.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim11.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim11.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim12.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim12.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim12.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim12.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim13.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim13.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim13.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim13.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim14.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim14.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim14.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim14.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim15.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim15.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim15.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim15.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim16.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim16.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim16.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim16.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim17.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim17.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim17.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim17.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim18.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim18.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim18.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim18.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim19.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim19.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim19.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim19.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim2.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim2.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim2.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim2.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim3.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim3.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim3.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim3.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim4.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim4.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim4.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim4.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim5.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim5.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim5.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim5.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim6.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim6.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim6.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim6.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim7.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim7.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim7.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim7.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim8.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim8.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim8.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim8.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$Clim9.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$Clim9.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$Clim9.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$Clim9.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_21.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_21.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_21.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_21.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_22.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_22.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_22.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_22.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_23.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_23.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_23.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_23.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

t.test(Phx_df$nlcd_continuous_clipped_r3_24.tif.asc.phoenix[Phx_df$species=="Northern Cardinal"], Tuc_df$nlcd_continuous_clipped_r3_24.tif.asc.tucson[Tuc_df$species=="Northern Cardinal"])
aggregate(Phx_df$nlcd_continuous_clipped_r3_24.tif.asc.phoenix, list(Phx_df$species), FUN=mean, na.rm=TRUE)
aggregate(Phx_df$nlcd_continuous_clipped_r3_24.tif.asc.phoenix, list(Phx_df$species), FUN=sd, na.rm=TRUE)

# combinePointValue=cbind(Phx.env.rasValue.pyrr,Phx.env)
write.csv(Phx.all.rasValue.both,file="Phx.all.rasValue.both.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)
