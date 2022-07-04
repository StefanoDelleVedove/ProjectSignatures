library(tidyverse)
library(ggplot2)
library(dbscan)
library(gmodels)

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito")



###############################################################################
####################### CN change between jump ################################
###############################################################################

rm(list=ls()) #start with clean envirnoment
dati_FeatureBuild <- read.table("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/CoMMPass_VisitSampType_1_BM_Clean.txt", header = T, )
dati_FeatureBuild$CN[dati_FeatureBuild$CN>=8] <- 8


dati_FeatureBuild$CNJump <- 0
for (i in 2:nrow(dati_FeatureBuild)) {
  dati_FeatureBuild$CNJump[i] <- abs(dati_FeatureBuild$CN[i]-dati_FeatureBuild$CN[i-1])
}



write.table(dati_FeatureBuild,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/Dati_visit1_Jump_Clean.txt",sep="\t",row.names=FALSE)
