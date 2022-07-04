library(tidyverse)
library(ggplot2)
library(dbscan)
library(gmodels)

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start")


################################################################################
##################### BREAK COUNT PER 10MB #####################################
################################################################################

dati_FeatureBuild <- read.table("CoMMPass_VisitSampType_1_BM_Clean.txt", header = T, )


options(scipen = 100, digits= 8)

dati_10mbBreak <- data.frame()
sample <- unique(dati_FeatureBuild$sample)
chrom <- rep_len(1:22, length(sample)*22)
conto <- 0
for (i in sample) {   # i = index of patient
  print(i) #just for checking the loop 
  for (ch in 1:22) {  # ch = chromosome number
    temp_sample <- dati_FeatureBuild[dati_FeatureBuild$sample %in% i & dati_FeatureBuild$chr==ch,] 
    if(nrow(temp_sample)!=0){
      maxend <- max(temp_sample$end)
      startpoint <- seq(1, (maxend-10000000) , 10000000)
      temp_break <- data.frame(rep(ch,length(startpoint)))
      temp_break$start <- startpoint
      temp_break$end <- seq(10000000, maxend, 10000000)
      ultimo <- c(ch,temp_break$end[nrow(temp_break)]+1,maxend)
      temp_break <- rbind(temp_break, ultimo) 
      names(temp_break) <- c("ChrNum", "start", "end")
      for (c in 1:nrow(temp_break)) { # c = counter for row number in 10mb 
        localend <- temp_break$end[c] 
        localstart <- temp_break$start[c]
        for (j in 1:(nrow(temp_sample))) { # j = counter for row number in sample
          condition <- (localstart <= temp_sample$end[j] && temp_sample$end[j] < localend )
          if(condition==TRUE){
            lowbound <- temp_sample$CN[j+1]-0.1
            upbound <- temp_sample$CN[j+1]+0.1
            condition2 <- between(temp_sample$CN[j], lowbound, upbound)
            if( condition2==FALSE) conto <- conto+1
          }
        }
        temp_break$breakcount[c] <- conto
        conto <- 0
      }
      temp_break$sample <- i
      temp_break$ChrNum  <- ch
      dati_10mbBreak <-rbind(dati_10mbBreak, temp_break)
    }
    names(dati_10mbBreak) <- c("ChrNum", "start", "end","breakcount","sample")
    rm(temp_break)
  }
}



write.table(dati_10mbBreak,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/CoMMPass_10mbBreak_feature.txt",sep="\t",row.names=FALSE)

