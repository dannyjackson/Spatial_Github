#!/usr/bin/env Rscript
# Script for hypothesis testing with ENMTools
# export TMPDIR=/Volumes/IcterOS/R_temp


# What command arguments do I need to pass?
# Input directories (clim env files, dev env files, other env files), output directory, reference file

args = commandArgs()

project_name = substr(args[grep("project_name_", args)],14,100000)
outputdirectory = substr(args[grep("outputdirectory_", args)],17,100000)

tempfile(tmpdir="/Volumes/IcterOS/R_temp")

interactive --mem=50G -t 1-0

install.packages("ENMTools")
lib='/home/dnjacks4/R/', dependencies = TRUE)
install.packages("dismo")
install.packages("rgbif")

install.packages("rbison", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(rbison, lib='/home/dnjacks4/R/')

install.packages("rebird", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(rebird, lib='/home/dnjacks4/R/')

install.packages("rvertnet", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(rvertnet, lib='/home/dnjacks4/R/')

install.packages("rgdal", type = "source",
  configure.args="--with-proj-share=/usr/local/Cellar/proj/4.8.0/share/proj", lib='/home/dnjacks4/R/',)

install.packages("terra", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(terra, lib='/home/dnjacks4/R/')


install.packages("rstatix", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(rstatix, lib='/home/dnjacks4/R/')

install.packages("recipes", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(recipes, lib='/home/dnjacks4/R/')

install.packages("CalibratR", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(CalibratR, lib='/home/dnjacks4/R/')

install.packages("fields", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(fields, lib='/home/dnjacks4/R/')

install.packages("pdp", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(pdp, lib='/home/dnjacks4/R/')

install.packages("spocc", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(spocc, lib='/home/dnjacks4/R/')

install.packages("spThin", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(spThin, lib='/home/dnjacks4/R/')

install.packages("raster", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(raster, lib='/home/dnjacks4/R/')

install.packages("ggpubr", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(ggpubr, lib='/home/dnjacks4/R/')

install.packages("caret", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(caret,  lib='/home/dnjacks4/R/')

install.packages("rangeModelMetadata", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(rangeModelMetadata, lib='/home/dnjacks4/R/')

install.packages("dismo", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(dismo, lib='/home/dnjacks4/R/')

install.packages("hypervolume", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(hypervolume, lib='/home/dnjacks4/R/')

install.packages("ENMeval", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(ENMeval, lib='/home/dnjacks4/R/')

install.packages("biomod2", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(biomod2, lib='/home/dnjacks4/R/')

install.packages("ecospat", lib='/home/dnjacks4/R/', dependencies = TRUE)
library(ecospat, lib='/home/dnjacks4/R/')


install.packages("devtools", lib='/home/dnjacks4/R/')
library(devtools)

update.packages(ask = FALSE, lib.loc = '/home/dnjacks4/R/')
install.packages("ENMTools", lib='/home/dnjacks4/R/', dependencies = TRUE)
install.packages("dismo", lib='/home/dnjacks4/R/')
install.packages("rgbif", lib='/home/dnjacks4/R/')

library(ENMTools)
library(dismo)
library(rgbif)
install.extras()



# env.files <- list.files(path = "/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/formaxent/files", full.names = TRUE)

# env.files <- list.files(path = "/Volumes/BackupPlus/GIS_files/May2022/FromAgave/PhoenixLayer/formaxent/files", full.names = TRUE)


# env.files <- list.files(path = "/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/formaxent/files", full.names = TRUE)

env <- stack(env.files)

# names(env) <- c("N11", "N21", "N22", "N23", "N24", "N31", "N41", "N42", "N43", "N52", "N71", "N81", "N82", "N90", "N95", "C1", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "elev", "Tmax3", "Tmax4", "Tmax5")



# Deal with NAs either by propogating them to all other layers, or by turning them all to 0

# propogate method
# env <- check.env(env)

# 0 method

numbers <- c(1:35)
for (item in numbers)
{
env[[item]][is.na(env[[item]])] <- 0
}

correlations <- raster.cor.matrix(env)
write.csv("correlations.csv")
plot <- raster.cor.plot(env)

pdf(file = "corr_plot.pdf", width = 10, height = 10, useDingbats=FALSE)
 plot
   dev.off()


pyrr <- read.csv(("/Volumes/BackupPlus/GIS_files/Birds/ebird_pyrr_AprMay_2016_2021_latlong.csv"))
x <- extract(env, pyrr)
y <- which(is.na(x[,1]), arr.ind=TRUE)
pyrr[y,]
newpyrr <- pyrr[-c(y), ]
write.csv(newpyrr, "/Volumes/BackupPlus/GIS_files/Birds/ebird_pyrr_AprMay_2016_2021_latlong_new.csv")
