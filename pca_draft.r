
AZ_df <- read.csv("AZ.all.rasValue.both.csv")

AZ_df = AZ.all.rasValue.both
names(AZ_df)

rem <- c("name1","name2") # list of column names

AZ_df2 <- as.data.frame(AZ_df)

Species <- AZ_df2[,-c(1)]
AZ_df_slim <- AZ_df2[,-c(1,2,3,4,5,18,38, 43, 44)]
AZ_df_slim <- drop_na(AZ_df_slim)
AZ_PCA <- rda(AZ_df_slim)

AZ_PCA <- princomp(x = AZ_df_slim)
plot(AZ_PCA$scores, pch = 16, col = as.factor(Species))
