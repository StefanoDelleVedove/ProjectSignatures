library(tidyverse)
library(ggplot2)
library(dbscan)
library(gmodels)

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start")



###############################################################################
####################### CN break per arm ######################################
###############################################################################

dati_FeatureBuild <- read.table("CoMMPass_VisitSampType_1_BM_Clean.txt", header = T, )



sample <- unique(dati_FeatureBuild$sample)
arms <- unique(dati_FeatureBuild$ChrArm)
dati_ChArmBreak  <- data.frame()
for (paz in sample) {
  print(paz)
  temp_patient <- data.frame(rep(paz, length(arms)))
  for (ar in 1:length(arms)) {
    temp_sample <- dati_FeatureBuild[dati_FeatureBuild$sample %in% paz & dati_FeatureBuild$chr==ar,] # extract single chromosome arm segments per patient
    temp_patient$sample[ar] <- paz
    temp_patient$ChArm[ar] <- arms[ar]
    temp_patient$breakcount[ar] <- 0
    conto <- 0
    for (j in 1:(nrow(temp_sample)-1)) { # j = counter for row number in sample, each segments is checked alone
      if(nrow(temp_sample) >= 2) {
        lowbound <- temp_sample$CN[j+1]-0.1
        upbound <- temp_sample$CN[j+1]+0.1
        condition <- between(temp_sample$CN[j], lowbound, upbound)
        if( condition ==FALSE) conto <- conto+1
      }
    }
    temp_patient$breakcount[ar] <- conto
  }
  dati_ChArmBreak  <- rbind(dati_ChArmBreak, temp_patient)
  rm(temp_patient, temp_sample, conto)
}


write.table(dati_ChArmBreak,"Dati_visit1_ArmBreakCount_Clean.txt",sep="\t",row.names=FALSE)


