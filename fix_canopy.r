library(raster)
library(rgdal)
library(FNN)
library(leaflet)
library(R6)
library(sf)
library(terra)



nlcd <- raster("/home/dnjacks4/nlcd_2016_treecanopy_2019_08_31.img")
#nlcd <- raster("FilesToAgave/nlcd_2016_treecanopy_2019_08_31/nlcd_2016_treecanopy_2019_08_31.img")
vector <- readOGR('/home/dnjacks4/FilesToAgave/Arizona/Arizona.shp')

vector_w84 <- spTransform(vector, crs(nlcd))
r2 <- crop(nlcd, extent(vector_w84))
r3 <- mask(r2, vector_w84)
nlcd_az <- mask(nlcd, vector)
r2 <- ratify(r2)

raster::writeRaster(r3, filename="canopy_az_masked", format="GTiff", overwrite=TRUE)
# raster::writeRaster(r3, filename="imperviousness_az", format="GTiff", overwrite=TRUE)

rgdal::writeGDAL(as(r3, "SpatialGridDataFrame"), "canopy_az_masked.asc", drivername = "AAIGrid")

# rgdal::writeGDAL(as(r3, "SpatialGridDataFrame"), "imperviousness_az.asc", drivername = "AAIGrid")



# process reference file

projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#set up extent parameter for use throughout script

# Arizona
ext <- extent(-114.8, -109.0023, 31.30228, 37.0)


assign(paste0("reference_", "raw"), raster('/home/dnjacks4/FilesToAgave/Tifs/tifs/Clim8.tif'))
reference_projected <- projectRaster(reference_raw, crs=projection)
rm(reference_raw)
assign(paste0("reference_final"), reference_projected)
rm(reference_projected)
reference_tend <- extend(reference_final, ext, value=NA)

# assign(paste0("raster_", "raw"), raster("/scratch/dnjacks4/nlcd_az_imperviousness_r3_second.tif"))

assign(paste0("raster_", "raw"), raster("canopy_az_masked.tif"))

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
e <- as(extent(-114.8, -109.0023, 31.30228, 37.0), 'SpatialPolygons')

# Arizona
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(raster_tend, extent(e))

#Now all datasets are identical in extent, resolution, etc.

r[is.na(r[])] <- 0

#Write these environmental datasets out in .asc format

print("Writing to file")

raster::writeRaster(r, filename="canopy_az", format="GTiff", overwrite=TRUE)

rgdal::writeGDAL(as(r, "SpatialGridDataFrame"), paste0("canopy_az.asc"), drivername = "AAIGrid")

#raster::writeRaster(r, filename="imperviousness", format="GTiff", overwrite=TRUE)

# rgdal::writeGDAL(as(r, "SpatialGridDataFrame"), paste0("/smaller/impervious.asc"), drivername = "AAIGrid")


vector <- readOGR('/home/dnjacks4/FilesToAgave/Arizona/Arizona.shp')
vector_w84 <- spTransform(vector, crs(r))
r2 <- crop(r, extent(vector_w84))
r3 <- mask(r2, vector_w84)
nlcd_az <- mask(r, vector)
r2 <- ratify(r2)

raster::writeRaster(r3, filename="canopy_az_masked", format="GTiff", overwrite=TRUE)
# raster::writeRaster(r3, filename="imperviousness_az", format="GTiff", overwrite=TRUE)

rgdal::writeGDAL(as(r3, "SpatialGridDataFrame"), "canopy_az_masked.asc", drivername = "AAIGrid")

# rgdal::writeGDAL(as(r3, "SpatialGridDataFrame"), "imperviousness_az.asc", drivername = "AAIGrid")


# crop tucson

e <- as(extent(-111.183682000, -110.720903000, 32.034553000, 32.554540000), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r_tucson <- crop(r, extent(e))


rgdal::writeGDAL(as(r_tucson, "SpatialGridDataFrame"),
"imperviousness_tucson.asc",
drivername = "AAIGrid")

# crop phoenix
r <- raster("/scratch/dnjacks4/asks/smaller/arizona/imperviousness_az.asc")
e <- as(extent(-112.584727000, -111.425540000, 33.089419000, 33.885028000), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r_phoenix <- crop(r, extent(e))


rgdal::writeGDAL(as(r_phoenix, "SpatialGridDataFrame"),
"imperviousness_phoenix.asc",
drivername = "AAIGrid")
