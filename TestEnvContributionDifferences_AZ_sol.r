#!/usr/bin/env Rscript

module purge 
module load r-4.2.2-gcc-11.2.0
module load sqlite-3.38.5-gcc-11.2.0
module load proj-8.2.1-gcc-11.2.0
module load gdal-3.4.3-gcc-11.2.0
module load geos-3.9.1-gcc-11.2.0

library(tidyverse)

setwd("/scratch/dnjacks4/asks/smaller/null_output/")

fileNames <- list.files(path="/scratch/dnjacks4/asks/smaller/null_output/stats/")

df <- (matrix(ncol = 0, nrow = 76))

for (fileName in fileNames) {

file <- read_csv(paste0("/scratch/dnjacks4/asks/smaller/null_output/stats/", fileName))

name <- gsub(".csv", '', fileName)


colnames(file) <- c("type", paste0(name))

x <- column_to_rownames(file, var = "type")

df <- cbind(df, x)

}

df <- df[grep("contribution", row.names(df)),]

df <- t(df)

df_noca <- df[grep("noca", row.names(df)),]
df_pyrr <- df[grep("pyrr", row.names(df)),]


diff_null <- df_noca - df_pyrr


# Now I have to compute if the empirical file is >= 95% of the diffnull values or not

# read in the empirical file and filter it down to just rows ft "contribution"


noca_file <- read_csv("/scratch/dnjacks4/asks/smaller/maxent_output/output/maxent_outputs/noca/species.csv")

pyrr_file <- read_csv("/scratch/dnjacks4/asks/smaller/maxent_output/output/maxent_outputs/pyrr/species.csv")

colnames(noca_file) <- c("type", "statistic")
colnames(pyrr_file) <- c("type", "statistic")

noca_filtered <- noca_file[grep("contribution", noca_file$type),]
pyrr_filtered <- pyrr_file[grep("contribution", pyrr_file$type),]

noca_x <- column_to_rownames(noca_filtered, var = "type")
pyrr_x <- column_to_rownames(pyrr_filtered, var = "type")


diff_empirical <- noca_x - pyrr_x


# for each row in empirical, get a count of the number of rows under the corresponding column of null that are less than the empirical value
# all rows in empirical with more than 950 in the count column are significant

diff_empirical
diff_null


count_list <- c()
stats <- rownames(diff_empirical)



for (stat in stats) {


emp <- diff_empirical[stat, 'statistic']

count <- table(diff_null[,stat] > emp)["TRUE"]


count_list <- append(count_list, count)}

diff_empirical <- cbind(diff_empirical, count_list)


write.csv(diff_empirical, "env_diff_stats.csv")

# Okay but what does this tell us? It shows which ENV variables contributed more to the NOCA model than to the PYRR model compared to the null model. Which maybe shows species differences... but I should also look at which ENV variables contributed more to the PYRR model than to the NOCA model compared to the null. Or maybe I shouldn't be looking at differences at all, but rather NOCA vs NOCA_null. It's that! That's the answer. Rewrite this script. The question is, "???"
