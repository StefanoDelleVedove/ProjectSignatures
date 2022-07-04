library(tidyverse)
library(hdp)


################################################################################
############################# HIERARCHICAL DIRICHLET PROCESS ###################

rm(list=ls()) #start with clean envirnoment
setwd("C:/Users/panet/Desktop/Mielloma multiplo/R studio/Datasets/Pulito/Feature/Mclust_Categorization_One")

#load data
AllFeat <- read.table("COMMPASS_PosteriorMatrix_Feature_Visit1.txt", header = T, )


### preparations 
names_pts<- rownames(AllFeat)
channel_names<- colnames(AllFeat)
genomicData<- (AllFeat) 
n<- ncol(genomicData)
shape<- 1
invscale<- 1
hdp<- hdp_init(ppindex=0, #index of the parent DP for initial DP
               cpindex=1, #index of alphaa and alphab for initial DP
               hh=rep(1/n,n), #params for base distn (uniform Dirichlet)
               alphaa=shape,
               alphab=invscale)

hdp<- hdp_adddp(hdp,
                numdp=nrow(genomicData),
                pp=1,
                cp=1)

hdp<- hdp_setdata(hdp= hdp,dpindex=1:nrow(genomicData)+1,data=genomicData)
hdp<- dp_activate(hdp,1:(nrow(genomicData)+1),10)

# run hDP 
chlist <- vector("list", 4)
for (i in 1:4){
  chlist[[i]] <- hdp_posterior(hdp,
                               burnin=20000,
                               n=100,
                               space=50,
                               cpiter=3,
                               seed=i*1e4)
}

mut_example_multi <- hdp_multi_chain(chlist)
saveRDS(mut_example_multi, "/HDP_result/mut_example_multi_4_chain.RDS")


