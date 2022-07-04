library(tidyverse)
library(ggplot2)
library(dbscan)
library(gmodels)

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets")


#load sataset
dati_seg <- read.table("karyoBLOCKS_CoMMpass_all_samples_fc50k.txt", header = T, )


#adding crude CN
#dati_seg$CN <- 2^((dati_seg$seg.mean) +1 ) 

# for (i in 1:nrow(dati_seg)) {
#   if(dati_seg$CN[i] >= 8) dati_seg$CN[i] <- 8
# }


#adding length in megabase
dati_seg$Mbwidth<- dati_seg$width/1000000
#adding variable with log of segments amplitude
dati_seg$LogMbwidth <- log10(dati_seg$Mbwidth)


# Remove IgH, IgK and IgL loci to reduce artefacts due to VDJ-rearrangement / class-switch recombination
dati_seg$code_row<- 1:nrow(dati_seg)
igh_cnv <- subset(dati_seg, dati_seg$seqnames == 14 & dati_seg$start >106032614 &  dati_seg$end< 108288051 | 
                    dati_seg$seqnames == 22 & dati_seg$start >21080474. &  dati_seg$end< 26065085 | 
                    dati_seg$seqnames == 2 & dati_seg$start >87090568 &  dati_seg$end< 93274235)
Dats_no_igh<- dati_seg[!dati_seg$code_row %in% igh_cnv$code_row ,]

# Remove segments < 50b in size  
dati_no5 <- subset(Dats_no_igh, Dats_no_igh$Mbwidth>0.05)
dati_no5 <- Dats_no_igh


############ remove whole chromosome diploid regions ################# migliorabile

maxim <- 2*1.1                         #maximum for diploid CN
minim <- 2*0.9                         #minimum for diploid CN
sample <- unique(dati_no5$sample)  #sample code list
datibig <- data.frame()
dati_no5$code_row<- 1:nrow(dati_no5)
dati_reject <- data.frame()
for (i in sample) {                                                                #loop for each patient and chromosome
  print(i)   #just a check
  for (ch in 1:22) {
    rm(nolist)
    temp_sample <-dati_no5[dati_no5$sample %in% i & dati_no5$chr==ch,]  
    nolist <- temp_sample$code_row[between(temp_sample$CN, minim, maxim)==FALSE] #list of rows who are not diploid
    if( length(nolist)==0 ) {  # check if the list is void or not
      dati_reject <- rbind(dati_reject, temp_sample)
    }else   datibig <- rbind(datibig, temp_sample)                                 # add data if at least one element of the nolist exist
  }
}
write.table(datibig,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/CoMMPass_AllVisit_Clean.txt" ,sep="\t",row.names=FALSE)

write.table(dati_reject,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/LeftOut_CHRAllDiploid_Sample.txt" ,sep="\t",row.names=FALSE)

####### divide for each visit #######

difference <- c("1_BM","2_BM","3_BM","4_BM","PB")
for (i in difference) {
  temp__clean <- grep("1_BM", datibig$sample) 
  temp__clean <- datibig[temp__clean,]
  write.table(temp__clean,paste0("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/CoMMPass_VisitSampType_", i ,"_Clean.txt" ),sep="\t",row.names=FALSE)
}

