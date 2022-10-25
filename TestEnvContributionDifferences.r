#!/usr/bin/env Rscript


library(tidyverse)

setwd("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull")

fileNames <- list.files(path="/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull/stats/")

df <- (matrix(ncol = 0, nrow = 72))

for (fileName in fileNames) {

file <- read_csv(paste0("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentNull/stats/", fileName))

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


noca_file <- read_csv("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentEmpirical/noca_1.csv")

pyrr_file <- read_csv("/Volumes/BackupPlus/GIS_files/May2022/FromAgave/TucsonLayer/MaxentEmpirical/pyrr_1.csv")

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
