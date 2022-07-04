library(flexmix)
library(ggplot2)
library(glue)
library(fpc)
library(tidyverse)


rm(list=ls()) #start with clean environment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start")

# dati
dati_visit1 <- read.table("CoMMPass_VisitSampType_1_BM_Clean.txt", header = T, )

str(dati_visit1_jump$CNJump)
dati_visit1_jump <- read.table("Dati_visit1_Jump_Clean.txt", header = T, )
dati_visit1_jump$CNJump <- as.integer(dati_visit1_jump$CNJump*100)

mod_CN_stepflexmix  <- stepFlexmix(CNJump ~ 1 , data=dati_visit1_jump, k=2:15, model = FLXglm(family = "poisson"), control =list(iter.max=200), nrep=10)
flex_ICL500 <- getModel(mod_CN_stepflexmix,  which = "ICL") 
flex_BIC500 <- getModel(mod_CN_stepflexmix,  which = "BIC") 
plot(mod_CN_stepflexmix)
summary(flex_BIC500)
summary(flex_ICL500)

dati_visit1_jump$Jump_code_flex <- flex_BIC500@cluster
dati_visit1_jump$Jump_code_flex <- as.factor(dati_visit1_jump$Jump_code_flex)
dati_visit1_jump$CNJump <- as.numeric(dati_visit1_jump$CNJump)/100

flex_BIC500@concomitant

ggplot(dati_visit1_jump) +  # plot density of general CN and mxture components CN
  geom_density(aes(x=CNJump, group=Jump_code_flex, fill=Jump_code_flex, y=..count..), alpha=0.9, lwd=1.05, adjust=2) + 
  geom_density(aes(x=CNJump, y=..count..), lwd=1.1) +scale_fill_viridis_d()



dati_visit1_ArmBreak <- read.table("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Start/Dati_visit1_ArmBreakCount_Clean.txt", header = T, )
str(dati_visit1_ArmBreak$breakcount)

mod_ArmB_stepflexmix  <- stepFlexmix(breakcount ~ 1 , data=dati_visit1_ArmBreak, k=2:15, model = FLXglm(family = "poisson"), control =list(iter.max=200), nrep=2)
flex_ICL500 <- getModel(mod_ArmB_stepflexmix,  which = "ICL") 
flex_BIC500 <- getModel(mod_ArmB_stepflexmix,  which = "BIC") 
plot(mod_ArmB_stepflexmix)
summary(flex_BIC500)
summary(flex_ICL500)
