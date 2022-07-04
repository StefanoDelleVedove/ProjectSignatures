library(tidyverse)
library(pheatmap) 




################################################################################
############################ POSTERIOR MATRIX CREATION #########################


## use reduce(merge, list(dataset vari)) to improve code

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Mclust_Categorization_One")


CN_FeatureData <- read.table("Visit1_Code_CN.txt", header = T, )
ArmBreak_FeatureData <- read.table("Visit1_Code_ArmB.txt", header = T, )
Jump_FeatureData <- read.table("Visit1_Code_Jump.txt", header = T, )
Mb10_FeatureData <- read.table("Visit1_Code_Mb10.txt", header = T, )
Oscill_FeatureData <- read.table("Visit1_Code_Oscill005.txt", header = T, )
Width_FeatureData <- read.table("Visit1_Code_Width.txt", header = T, )

########### Arrange data for Posterior matrix creation ###########

CN_Mclust <-as.data.frame.matrix(table(CN_FeatureData$sample , CN_FeatureData$CN_codeM))
colnames(CN_Mclust)<- paste0("CN_", colnames(CN_Mclust))
write.table(CN_Mclust,"CN_Mclust_Posterior.txt",sep=" ",row.names=TRUE)

ArmB_Mclust <-as.data.frame.matrix(table(ArmBreak_FeatureData$sample , ArmBreak_FeatureData$ArmB_codeM))
colnames(ArmB_Mclust)<- paste0("ArmB_", colnames(ArmB_Mclust))
write.table(ArmB_Mclust,"ArmB_Mclust_Posterior.txt",sep=" ",row.names=TRUE)

Jump_Mclust <-as.data.frame.matrix(table(Jump_FeatureData$sample , Jump_FeatureData$Jump_codeM))
colnames(Jump_Mclust)<- paste0("Jump_", colnames(Jump_Mclust))
write.table(Jump_Mclust,"Jump_Mclust_Posterior.txt",sep=" ",row.names=TRUE)

Mb10_Mclust <-as.data.frame.matrix(table(Mb10_FeatureData$sample , Mb10_FeatureData$Mb10_codeM))
colnames(Mb10_Mclust)<- paste0("Mb10_", colnames(Mb10_Mclust))
write.table(Mb10_Mclust,"Mb10_Mclust_Posterior.txt",sep=" ",row.names=TRUE)

Oscill_Mclust <-as.data.frame.matrix(table(Oscill_FeatureData$Sample , Oscill_FeatureData$Oscill_codeM))
colnames(Oscill_Mclust)<- paste0("Oscill_", colnames(Oscill_Mclust))
write.table(Oscill_Mclust,"Oscill_005_Mclust_Posterior.txt",sep=" ",row.names=TRUE)

Width_Mclust <-as.data.frame.matrix(table(Width_FeatureData$sample , Width_FeatureData$Width_codeM))
colnames(Width_Mclust)<- paste0("Width_", colnames(Width_Mclust))
write.table(Width_Mclust,"Width_Mclust_Posterior.txt",sep=" ",row.names=TRUE)

##################### POSTERIOR MATRIX CREATION ########################

CN_Mclust <- read.table("CN_Mclust_Posterior.txt", header = T, )
ArmB_Mclust <- read.table("ArmB_Mclust_Posterior.txt", header = T, )
Jump_Mclust <- read.table("Jump_Mclust_Posterior.txt", header = T, )
Mb10_Mclust <- read.table("Mb10_Mclust_Posterior.txt", header = T, )
Oscill_Mclust <- read.table("Oscill_005_Mclust_Posterior.txt", header = T, )
Width_Mclust <- read.table("Width_Mclust_Posterior.txt", header = T, )


df_list <- list(CN_Mclust, ArmB_Mclust, Jump_Mclust, Width_Mclust, Oscill_Mclust, Mb10_Mclust)

Feature_PosteriorMatrix <- Reduce(function(x, y) merge(x, y), df_list)




Temp1 <- merge(CN_Mclust, ArmB_Mclust, by="row.names")
Temp2 <- merge(Mb10_Mclust, Jump_Mclust, by="row.names")
Temp3 <- merge(Width_Mclust, Oscill_Mclust, by="row.names")

names(Temp1)[names(Temp1) == 'Row.names'] <- 'Sample'
names(Temp2)[names(Temp2) == 'Row.names'] <- 'Sample'
names(Temp3)[names(Temp3) == 'Row.names'] <- 'Sample'

Temp4 <- merge(Temp1,Temp2, by="Sample")
AllFeat <- merge(Temp4,Temp3, by="Sample")

row.names(AllFeat)  <- AllFeat$Sample # put Sample as Row name
AllFeat <- AllFeat[,-1]

write.table(AllFeat,"COMMPASS_visit1_PosteriorMatrix.txt",sep="\t",row.names=TRUE)

pheatmap(t(AllFeat), cluster_cols = T, cluster_rows = T, scale="row", show_colnames = F,treeheight_row = 0, treeheight_col = 0, color = colorRampPalette(c("navy", "white", "firebrick3"))(100))
colorRampPalette(c("navy","white","red"))(100)

