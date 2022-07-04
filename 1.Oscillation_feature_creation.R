library(tidyverse)
library(ggplot2)
library(dbscan)
library(gmodels)

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start")


dati_FeatureBuild <- read.table("CoMMPass_VisitSampType_1_BM_Clean.txt", header = T, )


###############################################################################
########################   Oscillation in chromosome CN #########################
################################################################################

dati_FeatureBuild$CN[dati_FeatureBuild$CN >= 8] <- 8

sample <- unique(dati_FeatureBuild$sample)
dati_visit1_Oscillation_Clean <- data.frame()
All_Patient_Oscill <- data.frame("Nome", NA, 0, NA, 0, NA)
names(All_Patient_Oscill) <- c("Sample", "Chr", "Oscill", "Diam", "Oscill2", "Diam2" )

for (paz in sample) {
  # print(paz) 
  temp_patient <- data.frame(rep(paz, 22), NA, 0, NA, 0, NA)
  names(temp_patient) <- c("Sample", "Chr", "Oscill", "Diam", "Oscill2", "Diam2" )
  for (chr in 1:22) {
    # print(chr)
    temp_ChrSample <- dati_FeatureBuild[dati_FeatureBuild$chr== chr & dati_FeatureBuild$sample== paz, ] # extract all segment for 1 patient 1 chromosome
    temp_ChrInterest <- temp_ChrSample[temp_ChrSample$CN <1.8 | temp_ChrSample$CN > 2.2, ]             # exclude diploid segments 
    temp_patient$Chr[chr] <- chr
    if(nrow(temp_ChrInterest)!= 0){ # se esistono segmenti non diploidi nel chromosoma del paziente allora..
      temp_Scan <- dbscan(as.matrix(temp_ChrInterest$CN), 0.05, 3)                                       # find cluster of neighbourhood points are found. 
      temp_Cluster_nFound <- max(temp_Scan$cluster)                                                       # number of clusters found
      temp_ChrInterest$cluster <- temp_Scan$cluster
      if(temp_Cluster_nFound != 0){
        temp_allOscill <- data.frame(Oscill=rep(0,temp_Cluster_nFound), Diam=rep(NA,temp_Cluster_nFound))
        for (j_clust in 1:temp_Cluster_nFound) { # da 1 a n cluster
          temp_Cluster_Interest <- temp_ChrInterest[temp_ChrInterest$cluster == j_clust,]
          temp_allOscill$Oscill[j_clust] <- length(temp_Scan$cluster[temp_Scan$cluster==j_clust])
          temp_allOscill$Diam[j_clust] <- max(temp_Cluster_Interest$CN) - min(temp_Cluster_Interest$CN)
        }
        best <- which.min(temp_allOscill$Diam)
        temp_patient$Oscill[chr] <- temp_allOscill$Oscill[best]
        temp_patient$Diam[chr] <- temp_allOscill$Diam[best]
        if(j_clust > 1){ # se esistono piu cluster aggiungo il secondo 
          best2 <- which.min(temp_allOscill$Diam[-best])
          temp_patient$Oscill2[chr] <- temp_allOscill$Oscill[best2]
          temp_patient$Diam2[chr] <- temp_allOscill$Diam[best2]
        }
      }
    } else { # .. altrimenti inserisci oscillazione 0 per quel chromosoma di quel paziente
      temp_patient$Oscill[chr] <- 0
      temp_patient$Diam[chr] <- NA
    }
  }
  All_Patient_Oscill <- rbind(All_Patient_Oscill,temp_patient)
}
All_Patient_Oscill_clean <- All_Patient_Oscill[-1,]
write.table(All_Patient_Oscill_clean,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/CoMMpass_Oscillation_005int_Feature.txt",sep=" ",row.names=FALSE)


###### POSSIBILI MIGLIORAMENTI
#  kNNdistplot(as.matrix(temp_ChrInterest$CN), 3) 
# grafico per rilevare l'ampiezza ottimale da passare a DBScan
# da grafico: utilizzare il primo punto (partendo dall'origine) di massimo relativo per determinare l'ampiezza dell'intervallo
# ESEMPIO 
# temp_ChrSample <- dati_FeatureBuild[dati_FeatureBuild$chr== 1 & dati_FeatureBuild$sample== "MMRF_1016_1_BM", ]
# temp_ChrInterest <- temp_ChrSample[temp_ChrSample$CN <1.8 | temp_ChrSample$CN > 2.2, ]
# kNNdistplot(as.matrix(temp_ChrInterest$CN), 3)               # primo massimo relativo alla coordinata (10, 0.8-1.1), coord di y data in intervallo perchè valutata ad occhio
# temp_scan <- dbscan(as.matrix(temp_ChrInterest$CN), 0.08, 3)
# NB: se si segue questa procedura bisogna aggiungere un riconoscimento di segmenti consecutivi per validare l'oscillazione (aggiungere variabile "continuita")
