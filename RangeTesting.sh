#
# algorithm for testing differences in range between NOCA and PYRR in Tucson


# 0. Run maxent with the empirical dataset of bird observations
Rscript /Volumes/BackupPlus/GIS_files/December2021/Spatial_Github/maxent_empirical.r

# 1. Permute (shuffle) the observations between the species
# 2. Reduce the number of observations to one per grid cell
# 3. Run Maxent
# 4. Create a difference between the species file
# 5. Delete all other files
# 6. Repeat 1000 times
Rscript /Volumes/BackupPlus/GIS_files/December2021/Spatial_Github/maxent_null.r


# 8. Create a summary raster file of the null distribution that shows how many permutations featured a difference equal to or greater than the empirical model
# 9. Generate a raster of the empirical difference between the species that displays only cells where the empirical difference was equal to or greater than the null in <5% of the null models

Rscript /Volumes/BackupPlus/GIS_files/December2021/Spatial_Github/TestRangeDifferences.r

# Test the differences in contribution numbers
# Compare differences in sample averages (no test, just simple averages from the observations)

Rscript /Volumes/BackupPlus/GIS_files/December2021/Spatial_Github/TestEnvContributionDifferences.r
