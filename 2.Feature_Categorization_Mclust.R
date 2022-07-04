library(mclust)  #mixture modelling with more solid clustering           
library(ggplot2)
library(glue)
library(fpc)
library(tidyverse)



rm(list=ls()) #start with clean environment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets")

# dati
dati_visit1 <- read.table("CoMMPass_visit1_CleanNoDiplo.txt", header = T, )


######## CATEGORIAZTION THROUGH MIXTURE MODELLING, UNIVARIATE NORMAL ###############

##### CN ####


#max CN = 8
dati_visit1$CN[dati_visit1$CN>=8] <- 8

mod_CNlist <- vector("list", 10) #list to contain all models
CN_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_CNlist[[i]] <- densityMclust(dati_visit1$CN, G=1:15)
  CN_modDescr[i,1] <- i
  CN_modDescr[i,2] <- max(mod_CNlist[[i]]$BIC)
  CN_modDescr[i,3] <- max(mod_CNlist[[i]]$icl)
  CN_modDescr[i,4]<- mod_CNlist[[i]]$G
}
colnames(CN_modDescr) <- c("model", "BIC", "ICL", "Components")

lapply(mod_CNlist, summary) #all models summary
CN_modDescr[CN_modDescr$BIC == max(CN_modDescr$BIC),]
CN_modDescr[CN_modDescr$ICL == max(CN_modDescr$ICL),]
sum_mod_CN <- CN_modDescr[CN_modDescr$BIC == max(CN_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_CN$model
bestmod_CN <- mod_CNlist[[bestmod]]

# merge check between close components
# this uses method="demp" to decide which clusters to merge but stops merging 
# according to the value of prediction strength 
# NB quite long to compute
#In real applications m = 100 or higher can be used; higher m improves the stability of the outcome
result_CN <- mergenormals(dati_visit1[,5], bestmod_CN , method="demp", cutoff=0.2)
summary(result_CN)


# classification of the data 
compon <- length(result_CN$mergedtonumbers)
dati_visit1$CN_codeM <- bestmod_CN$classification
for (i in 1:compon) {
  dati_visit1$CN_codeM[dati_visit1$CN_codeM==i] <- result_CN$mergedtonumbers[i]
}
table(dati_visit1$CN_codeM) # check if everything is ok
dati_visit1$CN_codeM <- as.factor(dati_visit1$CN_codeM)

# plot 
ggplot(dati_visit1) +  # plot density of general CN and mxture components CN
  geom_density(aes(x=CN, group=CN_codeM, fill=CN_codeM, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=CN, y=..count..), lwd=1.1) +scale_fill_viridis_d()

#save 
write.table(dati_visit1,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Visit1_Code_CN.txt",sep="\t",row.names=TRUE)


##### WIDTH ####

mod_Widthlist <- vector("list", 10) #list to contain all models
Width_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_Widthlist[[i]] <- densityMclust(dati_visit1$LogMbwidth, G=1:15)
  Width_modDescr[i,1] <- i
  Width_modDescr[i,2] <- mod_Widthlist[[i]]$bic
  Width_modDescr[i,3] <- mod_Widthlist[[i]]$icl
  Width_modDescr[i,4]<- mod_Widthlist[[i]]$G
}
colnames(Width_modDescr) <- c("model", "BIC", "ICL", "Components")

lapply(mod_Widthlist, summary) #all models summary
Width_modDescr[Width_modDescr$BIC == max(Width_modDescr$BIC),]
Width_modDescr[Width_modDescr$ICL == max(Width_modDescr$ICL),]
sum_mod_Width <- Width_modDescr[Width_modDescr$BIC == max(Width_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_Width$model
bestmod_Width <- mod_Widthlist[[bestmod]]

# Adjusting number of component, method = demp 
# direct estimation of misclassification probability between component
result_Width <- mergenormals(dati_visit1$LogMbwidth, bestmod_Width , method="demp", cutoff=0.1)
summary(result_Width)


compon_Width <- length(result_Width$mergedtonumbers)
dati_visit1$Width_codeM <- bestmod_Width$classification
for (i in 1:compon_Width) {
  dati_visit1$Width_codeM[dati_visit1$Width_codeM==i] <- result_Width$mergedtonumbers[i]
}
table(dati_visit1$Width_codeM)
dati_visit1$Width_codeM <- as.factor(dati_visit1$Width_codeM)

#plot
ggplot(dati_visit1) +  # plot density of general Width and mxture components Width
  geom_density (aes(x=LogMbwidth, group=Width_codeM, fill=Width_codeM, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=LogMbwidth, y=..count..), lwd=1.1)


#save
write.table(dati_visit1,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Visit1_Code_Width.txt",sep="\t",row.names=TRUE)



##### JUMP ####


dati_visit1_jump <- read.table("Pulito/Start/Dati_visit1_Jump_Clean.txt", header = T, )


mod_Jumplist <- vector("list", 10) #list to contain all models
Jump_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_Jumplist[[i]] <- densityMclust(dati_visit1_jump$CNJump, G=1:10)
  Jump_modDescr[i,1] <- i
  Jump_modDescr[i,2] <- mod_Jumplist[[i]]$bic
  Jump_modDescr[i,3] <- mod_Jumplist[[i]]$icl
  Jump_modDescr[i,4]<- mod_Jumplist[[i]]$G
}
colnames(Jump_modDescr) <- c("model", "BIC", "ICL", "Components")

lapply(mod_Jumplist, summary) #all models summary
Jump_modDescr[Jump_modDescr$BIC == max(Jump_modDescr$BIC),]
Jump_modDescr[Jump_modDescr$ICL == max(Jump_modDescr$ICL),]
sum_mod_Jump <- Jump_modDescr[Jump_modDescr$BIC == max(Jump_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_Jump$model
bestmod_Jump <- mod_Jumplist[[bestmod]]

# since the data a well divided in two main blocks the mixed model tend to find just 2 components 
# to solve this the data is split according to the two different chunk of data and then passed again on the mixed models
# applying again gaussian mixture model but just on the big cluster (~ 73k/76k of total obs)  
dati_visit1_jump$Jump_codeM <- as.factor(bestmod_Jump$classification)
lower <- min(dati_visit1_jump$CNJump[dati_visit1_jump$Jump_codeM==2])
upper <- max(dati_visit1_jump$CNJump[dati_visit1_jump$Jump_codeM==1])
ggplot(dati_visit1_jump) + geom_histogram(aes(x=CNJump,color=Jump_codeM, fill=Jump_codeM))

#since the graph present 2 "neighboor" modes a gaussian mixture model is runned in the interested zone (CNJump <= 1,483)
focus <- subset(dati_visit1_jump, dati_visit1_jump$CNJump <= upper)
modelBIC_Jump1 <- mclustBIC(focus$CNJump, G=2:9)

mod_Jumplist <- vector("list", 10) #list to contain all models
Jump_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_Jumplist[[i]] <- densityMclust(focus$CNJump, G=1:10)
  Jump_modDescr[i,1] <- i
  Jump_modDescr[i,2] <- mod_Jumplist[[i]]$bic
  Jump_modDescr[i,3] <- mod_Jumplist[[i]]$icl
  Jump_modDescr[i,4]<- mod_Jumplist[[i]]$G
}
colnames(Jump_modDescr) <- c("model", "BIC", "ICL", "Components")
lapply(mod_Jumplist, summary) #all models summary
Jump_modDescr[Jump_modDescr$BIC == max(Jump_modDescr$BIC),]
Jump_modDescr[Jump_modDescr$ICL == max(Jump_modDescr$ICL),]
sum_mod_Jump <- Jump_modDescr[Jump_modDescr$BIC == max(Jump_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_Jump$model
bestmod_Jump2 <- mod_Jumplist[[bestmod]]
summary(bestmod_Jump2)
# Adjusting number of component, method = demp 
# direct estimation of misclassification probability between component
result_Jump2 <- mergenormals(focus$CNJump, bestmod_Jump2 , method="demp")
summary(result_Jump2)


compon_Jump <- length(result_Jump2$mergedtonumbers)
focus$Jump_codeM <- bestmod_Jump2$classification
for (i in 1:compon_Jump) {
  focus$Jump_codeM[focus$Jump_codeM==i] <- result_Jump2$mergedtonumbers[i]
}
table(focus$Jump_codeM)

# adjusting first model classification according to the new components found
dati_visit1_jump$Jump_codeM <- as.numeric(dati_visit1_jump$Jump_codeM)
new_classific <- max(result_Jump2$mergedtonumbers)+1
dati_visit1_jump$Jump_codeM[dati_visit1_jump$Jump_codeM==2] <- new_classific

# merge the two dataset back
dati2 <- rbind(focus,dati_visit1_jump[dati_visit1_jump$Jump_codeM==new_classific,] )
dati2$Jump_codeM <- as.factor(dati2$Jump_codeM)
table(dati2$Jump_codeM)

#plot
ggplot(dati2) +  # plot density of general Width and mxture components Width
  geom_density (aes(x=CNJump, group=Jump_codeM, fill=Jump_codeM, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=CNJump, y=..count..), lwd=1.1)

ggplot(dati2) +
  geom_density (aes(x=CNJump), alpha=0.9, lwd=1.05, adjust=2)


#save
write.table(dati2,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Visit1_Code_Jump.txt",sep="\t",row.names=TRUE)




##### ARMBREAK ####

dati_visit1_ArmBreak <- read.table("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/Dati_visit1_ArmBreakCount_Clean.txt", header = T, )

mod_ArmBlist <- vector("list", 10) #list to contain all models
ArmB_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_ArmBlist[[i]] <- densityMclust(dati_visit1_ArmBreak$breakcount, G=1:8) # above 7 components never merge
  ArmB_modDescr[i,1] <- i
  ArmB_modDescr[i,2] <- mod_ArmBlist[[i]]$bic
  ArmB_modDescr[i,3] <- mod_ArmBlist[[i]]$icl
  ArmB_modDescr[i,4]<- mod_ArmBlist[[i]]$G
}
colnames(ArmB_modDescr) <- c("model", "BIC", "ICL", "Components")

lapply(mod_ArmBlist, summary) #all models summary
ArmB_modDescr[ArmB_modDescr$BIC == max(ArmB_modDescr$BIC),]
ArmB_modDescr[ArmB_modDescr$ICL == max(ArmB_modDescr$ICL),]
sum_mod_ArmB <- ArmB_modDescr[ArmB_modDescr$BIC == max(ArmB_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_ArmB$model
bestmod_ArmB <- mod_ArmBlist[[bestmod]]


# Adjusting number of component, method = demp 
# direct estimation of misclassification probability between component
# result_ArmB <- mergenormals(dati_visit1_ArmBreak$breakcount, bestmod_ArmB , method="predictive")
# summary(result_ArmB)
# 
# 
# compon_ArmB <- length(result_ArmB$mergedtonumbers)
# dati_visit1_ArmBreak$ArmB_codeM <- bestmod_ArmB$classification
# for (i in 1:compon_ArmB) {
#   dati_visit1_ArmBreak$ArmB_codeM[dati_visit1_ArmBreak$ArmB_codeM==i] <- result_ArmB1$mergedtonumbers[i]
# }
# table(dati_visit1_ArmBreak$ArmB_codeM)
# dati_visit1_ArmBreak$ArmB_codeM <- as.factor(dati_visit1_ArmBreak$ArmB_codeM)
# 


dati_visit1_ArmBreak$ArmB_codeM <- bestmod_ArmB$classification
dati_visit1_ArmBreak$ArmB_codeM <- as.numeric(dati_visit1_ArmBreak$ArmB_codeM)
b <- levels(as.factor(dati_visit1_ArmBreak$ArmB_codeM))
counter <- 1
for (i in b) {
  print(i)
  dati_visit1_ArmBreak$ArmB_codeM[dati_visit1_ArmBreak$ArmB_codeM==i] <- counter
  counter <- counter+1
}
dati_visit1_ArmBreak$ArmB_codeM <- as.factor(dati_visit1_ArmBreak$ArmB_codeM)
table(dati_visit1_ArmBreak$ArmB_codeM)



#plot
ggplot(dati_visit1_ArmBreak) +  # plot density of general Width and mxture components Width
  geom_density(aes(x=breakcount, group=ArmB_codeM, fill=ArmB_codeM, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=breakcount, y=..count..), lwd=1.1) 

#save
write.table(dati_visit1_ArmBreak,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Visit1_Code_ArmB.txt",sep="\t",row.names=TRUE)

##### OSCILLATION ####

dati_visit1_Oscillation <- read.table("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/CoMMpass_Oscillation_005int_Feature.txt", header = T)
#changing NA values to 0 (NA was assigned to whole diploid chromosomes)
dati_visit1_Oscillation$oscill[is.na(dati_visit1_Oscillation$oscill)==T] <- 0
table(dati_visit1_Oscillation$Oscill)


Mclust(dati_visit1_Oscillation$Oscill, G= 1:3)


mod_Oscilllist <- vector("list", 10) #list to contain all models
Oscill_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_Oscilllist[[i]] <- densityMclust(dati_visit1_Oscillation$Oscill, G=2:3) # above 7 components never merge
  Oscill_modDescr[i,1] <- i
  Oscill_modDescr[i,2] <- mod_Oscilllist[[i]]$bic
  Oscill_modDescr[i,3] <- mod_Oscilllist[[i]]$icl
  Oscill_modDescr[i,4]<- mod_Oscilllist[[i]]$G
}
colnames(Oscill_modDescr) <- c("model", "BIC", "ICL", "Components")

lapply(mod_Oscilllist, summary) #all models summary
Oscill_modDescr[Oscill_modDescr$BIC == max(Oscill_modDescr$BIC),]
Oscill_modDescr[Oscill_modDescr$ICL == max(Oscill_modDescr$ICL),]
sum_mod_Oscill <- Oscill_modDescr[Oscill_modDescr$BIC == max(Oscill_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_Oscill$model
bestmod_Oscill <- mod_Oscilllist[[bestmod]]


# Adjusting number of component, method = demp 
# direct estimation of misclassification probability between component
result_Oscill <- mergenormals(dati_visit1_Oscillation$Oscill, bestmod_Oscill , method="demp")
summary(result_Oscill)


compon_Oscill <- length(result_Oscill$mergedtonumbers)
dati_visit1_Oscillation$Oscill_codeM <- bestmod_Oscill$classification
for (i in 1:compon_Oscill) {
  dati_visit1_Oscillation$Oscill_codeM[dati_visit1_Oscillation$Oscill_codeM==i] <- result_Oscill1$mergedtonumbers[i]
}
table(dati_visit1_Oscillation$Oscill_codeM)
dati_visit1_Oscillation$Oscill_codeM <- as.factor(dati_visit1_Oscillation$Oscill_codeM)

#plot
ggplot(dati_visit1_Oscillation) +  # plot density of general Width and mxture components Width
  geom_density (aes(x=Oscill, group=Oscill_codeM, fill=Oscill_codeM, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=Oscill, y=..count..), lwd=1.1)

#save
write.table(dati_visit1_Oscillation,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Visit1_Code_Oscill005.txt",sep=" ",row.names=TRUE)






##### 10MB ####

dati_10mbBreak <- read.table("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/CoMMPass_10mbBreak_feature.txt", header = T, )


mod_Mb10list <- vector("list", 10) #list to contain all models
Mb10_modDescr <- data.frame(nrow = 10, ncol=4)
for (i in 1:10) { # 10 run of the same mixture to ensure consistency
  mod_Mb10list[[i]] <- densityMclust(dati_10mbBreak$breakcount, G=2:4) # above 5 components never merge
  Mb10_modDescr[i,1] <- i
  Mb10_modDescr[i,2] <- mod_Mb10list[[i]]$bic
  Mb10_modDescr[i,3] <- mod_Mb10list[[i]]$icl
  Mb10_modDescr[i,4]<- mod_Mb10list[[i]]$G
}
colnames(Mb10_modDescr) <- c("model", "BIC", "ICL", "Components")

lapply(mod_Mb10list, summary) #all models summary
Mb10_modDescr[Mb10_modDescr$BIC == max(Mb10_modDescr$BIC),]
Mb10_modDescr[Mb10_modDescr$ICL == max(Mb10_modDescr$ICL),]
sum_mod_Mb10 <- Mb10_modDescr[Mb10_modDescr$BIC == max(Mb10_modDescr$BIC),] # assign best model to a variable 
bestmod <- sum_mod_Mb10$model
bestmod_10Mb <- mod_Mb10list[[bestmod]]

# Adjusting number of component, method = demp 
# direct estimation of misclassification probability between component
result_Mb10 <- mergenormals(dati_10mbBreak$breakcount, bestmod_10Mb , method="demp")
summary(result_Mb10)


compon_Mb10 <- length(result_Mb10$mergedtonumbers)
dati_10mbBreak$Mb10_codeM <- bestmod_10Mb$classification
for (i in 1:compon_Mb10) {
  dati_10mbBreak$Mb10_codeM[dati_10mbBreak$Mb10_codeM==i] <- result_Mb101$mergedtonumbers[i]
}
table(dati_10mbBreak$Mb10_codeM)
dati_10mbBreak$Mb10_codeM <- as.factor(dati_10mbBreak$Mb10_codeM)

#plot
ggplot(dati_10mbBreak) +  # plot density of general Width and mxture components Width
  geom_density (aes(x=breakcount, group=Mb10_codeM, fill=Mb10_codeM, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=breakcount, y=..count..), lwd=1.1)

#save
write.table(dati_10mbBreak,"C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Visit1_Code_Mb10.txt",sep=" ",row.names=TRUE)

