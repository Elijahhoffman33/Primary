
options(java.parameters = "-Xmx8000m")

library(lars)
library(xlsx)
library(foreign)
library(BMA)
library(stats)
library(MASS)
library(sva)
library(DESeq2)
library(sandwich)
library(lmtest)
# #rm(list = ls())
# 
# 
# 
# ######################################
# ###
# ### COMPARISONS/MODELS TO RUN:
# ### 
# ### T-tests (compare proteins in Rad vs Sham)
# ###  - Sham vs all Fe
# ###  - Sham vs Ti (published only)
# ###  - Sham vs Ti (published and Britten unpublished)
# ###
# ### Individual models (proteins vs REL3)
# ###  - Sham only rats
# ###  - All Fe rats 
# ###  - All Ti rats
# ###  - Fe + sham rats
# ###  - TI + sham rats
# ###  - All rats
# ###
# ### LARS models
# ###  - Same comparisons as individual protein vs REL3 models
# 
# ######################################
# 
# 
# 
# ###### READ IN RAW DATA AND CLEAN FOR ANALYSIS
# 
# ### Rats to exclude:
# # 7656 (Dutta bad)
# # 6152 (Britten unpublished good)
# # B14C (Britten unpublished bad)
# 
# setwd("\\\\campus.berkeley.edu/eei-dfs/SPH/CERCH/Departmental/RESEARCH/NASA/Datasets/Britten_Dutta proteomics/")
# data.rel3.import<-read.csv("Four_Datasets_Data Imputed.csv")
# 
# data.rel3.import$Variable<-as.character(data.rel3.import$Variable)
# data.rel3.import$radiation.batch[data.rel3.import$Publication=="Britten 2017"]<-5
# 
# data.rel3.import$Publication<-as.character(data.rel3.import$Publication)
# data.rel3.import$Publication[data.rel3.import$Publication %in% c("Tidmore 2021","Tidmore 2022","Tidmore 2023",
#                                                                      "Tidmore 2024","Tidmore 2025")]<-"Tidmore"
# 
# data.rel3.import$Rat_ID<-as.character(data.rel3.import$Rat_ID)
# data.rel3.import$REL3<-as.numeric(as.character(data.rel3.import$REL3))
# 
# # create uniform values for radiation flag (for rad vs sham comparisons)
# data.rel3.import$rad[data.rel3.import$Dose %in% c("0","SHAM")==1]<-0
# data.rel3.import$rad[data.rel3.import$Dose %in% c("20F","F15","T15","T20")==1]<-1
# 
# data.rel3.import$ion[data.rel3.import$Dose %in% c("0","SHAM")]<-"Sham"
# data.rel3.import$ion[data.rel3.import$Dose %in% c("20F","F15")]<-"Fe"
# data.rel3.import$ion[data.rel3.import$Dose %in% c("T15","T20")]<-"Ti"
# 
# # create "bad" performer flag for good vs bad comparisons
# data.rel3.import$bad<-0
# data.rel3.import$bad[data.rel3.import$Treatment=="Bad"]<-1
# 
# # set exclusions for 3 rats with bad protein data
# data.rel3.import$exclude<-0
# data.rel3.import$exclude[data.rel3.import$Rat_ID %in% c("`7656","`6152","`B14C")]<-1
# data.rel3.import<-data.rel3.import[data.rel3.import$exclude==0,]
# 
# # reorder dataset so all the identifier variables are on the left and proteins are all on the right
# dim2<-dim(data.rel3.import)[2]
# data.rel3.import<-data.rel3.import[,c(2:13,dim2-3,dim2-2,dim2-1,dim2,14:(dim(data.rel3.import)[2]-4))]
# 
# ### Create a new function to deal with imputed values. Grabs the minimum positive value from each column
# min.pos<-function(x){
#   return(min(x[x>0],na.rm=T))
# }
# 
# 
# ### RUN ComBat program
# combat.batch<-data.rel3.import$radiation.batch # column of radiation batch values
# combat.id<-as.factor(data.rel3.import$Rat_ID) # column of rat IDs
# combat.proteins<-data.rel3.import[,17:dim(data.rel3.import)[2]] # extract protein values
# combat.output<-ComBat(dat=t(as.matrix(combat.proteins)), batch=as.numeric(combat.batch)) # run actual ComBat step
# combat.output<-as.data.frame(t(combat.output)) # convert to data frame in the right format (rats as rows, proteins as columns)
# 
# # PROBLEM: Sometimes ComBat yields negative values. To deal with this, we impute any missing values
# #          with the minimum positive value for that protein across all rats
# 
# combat.min<-apply(combat.output,2,min.pos) # get minimum positive value
# combat.min.row<-data.frame(t(combat.min)) # create vector of values
# combat.min.df<-combat.min.row
# dim.1<-dim(combat.output)[1]-1
# 
# # loop: create matrix of minimum values
# for(i in 1:dim.1){
#   combat.min.df<-rbind(combat.min.df,combat.min.row)
# }
# combat.out<-combat.output
# combat.out[combat.out<0]<-combat.min.df[combat.out<0] # sub in smallest value for negative values
# 
# #combat.out2<-cbind(data.rel3.import$Rat_ID,combat.out)
# #names(combat.out2)[1]<-"Rat_ID"
# #write.csv(combat.out2,"Four_Datasets_combat_adj.csv")
# #combat.out<-combat.out[combat.batch!=5,]
# 
# 
# ### DEseq requires all variables to be integers. There's also a maximum size of integer R can handle,
# ###   so we divide all the values by 100 (we won't need to do this if as use a routine other than DESeq)
# combat.out.t<-t(combat.out)
# combat.out.t.int<-round(combat.out.t/100) # divide by 100 because some values in matrix are too large for R to handle
# colnames(combat.out.t.int)<-data.rel3.import$Rat_ID # rearrange dataset so rats are columns and proteins are rows
# combat.out.t.int[1:10,1:10]
# 
# 
# data.rel3.meta<-as.matrix(data.rel3.import[,c("Rat_ID","radiation.batch","bad","rad")])
# rownames(data.rel3.meta)<-NULL
# 
# 
# dim(data.rel3.meta)
# dim(combat.out.t.int)
# 
# # take out batch from Britten 2017 paper
# data.rel3.meta<-data.rel3.meta[combat.batch!=5,]
# combat.out.t.int<-combat.out.t.int[,combat.batch!=5]
# 
# 
# 
# ###################
# # Run DEseq
# ###################
# 
# # NOTE: We're still playing around with different settings of the program
# # The code below will run different versions of DESeq, and produce diagnostic MA plots
# # for the different attempts. In the end, we'll just choose one of these (I think)
# 
# # DESeq takes 3 inputs:
# # 1. Protein data (rats as columns, protein values as rows
# # 2. Metadata: rats as rows, variables for IDs and batch, rad/sham, and good/bad
# # 3. Design variables: based on what comparison (STILL WORKING ON THIS PART)
# # It's very important that the rows in the metadata and columns in the protein data have equal lengths and match
# # DESeq also seems to be tricky on this point, so you may need to play around with the data a bit.
# #
# # To run the program, we first create a DESeq object from the items above. Then we can extract the normalized values
# 
# setwd("//campus.berkeley.edu/eei-dfs/SPH/CERCH/Departmental/RESEARCH/NASA/lars/Britten_Dutta proteomics/plots/")
# 
# dds.rel3.1<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                        colData=data.rel3.meta, 
#                        design=~bad)
# deseq1<-DESeq(dds.rel3.1)
# 
# jpeg("MA_GvB.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq1,main="Good/bad")
# dev.off()
# 
# 
# dds.rel3.2<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                  colData=data.rel3.meta, 
#                                  design=~rad)
# deseq2<-DESeq(dds.rel3.2)
# 
# jpeg("MA_RvS.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq2,main="Rad/Sham")
# dev.off()
# 
# 
# dds.rel3.3<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                  colData=data.rel3.meta, 
#                                  design=~bad+rad)
# deseq3<-DESeq(dds.rel3.3)
# 
# jpeg("MA_GvB_RvS.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq3,main="Good/Bad + Rad/Sham")
# dev.off()
# 
# 
# dds.rel3.4<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                  colData=data.rel3.meta, 
#                                  design=~rad+bad)
# deseq4<-DESeq(dds.rel3.4)
# 
# 
# jpeg("MA_RvS_GvB.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq4,main="Rad/Sham + Good/Bad")
# dev.off()
# 
# 
# dds.rel3.5<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                  colData=data.rel3.meta, 
#                                  design=~radiation.batch)
# deseq5<-DESeq(dds.rel3.5)
# 
# jpeg("MA_batch.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq5,main="Batch")
# dev.off()
# 
# 
# dds.rel3.6<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                  colData=data.rel3.meta, 
#                                  design=~radiation.batch+bad)
# deseq6<-DESeq(dds.rel3.6)
# 
# jpeg("MA_GvB_batch.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq6,main="Good/bad + batch")
# dev.off()
# 
# 
# dds.rel3.7<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                  colData=data.rel3.meta, 
#                                  design=~radiation.batch+rad)
# deseq7<-DESeq(dds.rel3.7)
# 
# jpeg("MA_RvS_batch.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq7,main="Rad/Sham + batch")
# dev.off()
# 
# 
# dds.rel3.8<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                    colData=data.rel3.meta, 
#                                    design=~radiation.batch+bad+rad)
# deseq8<-DESeq(dds.rel3.8)
# 
# jpeg("MA_GvB_RvS_batch.jpg", width=6, height=6, pointsize=8, family="sans", units="in", res=500) 
# plotMA(deseq8, main="Good/Bad + Rad/Sham + batch")
# dev.off()
# 
# #dds.rel3.9<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
# #                                   colData=data.rel3.meta, 
# #                                   design=~Rat_ID)
# #deseq9<-DESeq(dds.rel3.9)
# #plotMA(deseq9,main="Rat ID")
# 
# 
# 
# # Extract the normalized values (called counts) from the DESeq object
# 
# 
# dds.rel3<-DESeqDataSetFromMatrix(countData=combat.out.t.int,
#                                    colData=data.rel3.meta, 
#                                    design=~radiation.batch)
# dds.rel3.counts<-estimateSizeFactors(dds.rel3)
# dds.rel3.norm<-counts(dds.rel3.counts, normalized=TRUE)
# dds.rel3.norm.t<-data.frame(t(dds.rel3.norm))
# 
# library(stringr)
# 
# rel3.data.norm<-cbind(data.rel3.import[data.rel3.import$radiation.batch!=5,1:16],dds.rel3.norm.t)
# rel3.data.norm$Rat_ID<-str_remove(rel3.data.norm$Rat_ID,"`")
# 
# 
# #rel3.data.norm<-rel3.data.norm[rel3.data.norm$radiation.batch!=5,]
# 
# library(data.table)
# rel3.data.norm<-rel3.data.norm[order(rel3.data.norm$Publication),]
# 
# rel3.data.long<-melt(rel3.data.norm,id.vars=c("Variable","Rat_ID","rad","ion","bad","radiation.batch","Publication"),
#                      measure.vars=names(rel3.data.norm)[17:3346])
# 
# rel3.data.long$log.value<-log10(rel3.data.long$value)
# 
# rel3.data.long<-rel3.data.long[with(rel3.data.long,order(rel3.data.long$Publication,rel3.data.long$Rat_ID,rel3.data.long$variable)),]
# 
# library(ggplot2)
# 
# jpeg("rel3_violin.jpg",width=10, height=8,units="in",res=500)
# ggplot(rel3.data.long,aes(x=Rat_ID, y=log.value, fill=Publication)) +
#   geom_violin() + ylab("log10 (value)") #+ geom_boxplot(width=0.1)
# dev.off()
# 
# 
# 
# 
# 
# jpeg("rel3_box.jpg",width=10, height=8,units="in",res=500)
# ggplot(rel3.data.long,aes(x=Rat_ID, y=log.value, fill=Publication)) +
#   geom_boxplot() + ylab("log10 (value)")
# dev.off()
# 
# 
# prot.med<-aggregate(rel3.data.long$value, list(rel3.data.long$Rat_ID), FUN=median,na.rm=T)
# names(prot.med)<-c("Rat_ID","median")
# prot.med$log.median<-log10(prot.med$median)
# prot.med
# 
# prot.med<-merge(rel3.data.norm[,c("Rat_ID","Publication")],prot.med,by="Rat_ID")
# prot.med<-prot.med[order(prot.med$Publication),]
# rownames(prot.med)<-NULL
# prot.med
# 
# #write.csv(rel3.data.norm,"Four_Datasets_combat_deseq.csv")
# 
# 
# 
# dim(rel3.data.norm)
# names(rel3.data.norm)[1:20]
# 
# # Library
# library(ggplot2)
# library(tidyr)
# library(dplyr)
# rel3.data.norm[,17:3346] %>% 
#   gather(key="Protein", value="Adundance") %>%
#   ggplot(aes(x=Protein, y=Adundance, fill=Protein)) +
#   geom_violin()






###############################################
### Funcions for invidual protein analyses ### Jump
###############################################


# function to get the IRR and p-value for a SINGLE PROTEIN against the REL3 outcome
# note that after running the negative binomial model, we need to run a variance correction
# with the sandwich estimator (analogous to using the "vce(robust)" option in Stata)

# Input: protein dataset, outcome variable, name of protein for model
# Output: IRR, CI, p-value, and convergence status of model (Y/N)
nb.protein<-function(prot.data,outvar,prot.name){
  
  # select the protein name
  prot<-prot.data[,prot.name]
  
  # scale the protein values by the interquartile range
  if(IQR(prot)==0){
    return(NA)
  }
  prot.iqr<-prot/IQR(prot)
  
  
  
  # run the negative binomial GLM at the heart of the function
  nb.var<-glm.nb(prot.data[,outvar]~prot.iqr)
  nb.var.robust<-coeftest(nb.var, vcovHC(nb.var, type="HC0")) # do variance correction
  nb.var.ci<-confint(nb.var.robust)
  
  # extract the IRR and p-values from the result
  nb.irr<-exp(summary(nb.var)[["coefficients"]][2,1])
  nb.ci.low<-exp(nb.var.ci[2,1])
  nb.ci.high<-exp(nb.var.ci[2,2])
  nb.p<-nb.var.robust[2,4]
  nb.conv<-nb.var[["converged"]]
  #nb.p<-summary(nb.var)[["coefficients"]][2,4]
  
  return(c(nb.irr,nb.ci.low,nb.ci.high,nb.p,nb.conv))
}



# function to apply the nb.protein function across the whole set of proteins
# Input: protein dataset, outcome variable, and subset of participants to include in the analyses
# Output: dataset with model results for all proteins

nb.result<-function(prot.data,outvar,subset=1:nrow(prot.data)){
  
  # pull out the list of protein names
  dim2<-dim(prot.data)[2]
  protein.list<-prot.data[,c(12:dim2)]
  
  # create an empty matrix to store the model results
  nb.reg<-matrix(nrow=dim(protein.list)[2],ncol=5)
  colnames(nb.reg)<-c("IRR","ci.low","ci.high","p","converged")
  rownames(nb.reg)<-names(protein.list)
  
  
  prot.data.sub<-prot.data[subset,]
  
  # for each protein in the list, run the nb.protein function and collect the results
  for (i in 1:dim(protein.list)[2]) {
    nb.var<-nb.protein(prot.data=prot.data.sub,outvar=outvar,prot.name=names(protein.list)[i])
    nb.reg[i,]<-nb.var
  }
  
  # process the output data frame
  nb.reg<-data.frame(nb.reg)
  nb.reg<-cbind(rownames(nb.reg),nb.reg)
  names(nb.reg)[1]<-"protein"
  rownames(nb.reg)<-NULL
  nb.reg$protein<-as.character(nb.reg$protein)
  nb.reg<-nb.reg[order(nb.reg$p),]
  return(nb.reg)
}



# function to do a t-test of rad vs sham (or good vs bad) for ONE protein
#   Note that for a fold change (ratio of the means), 
#   the "0" coding is the denominator and the "1" coding is the numerator
# Input: dataset, protein name for the analysis, variable for group comparison (i.e., rad vs sham or good vs bad)
# Output: Ns for each group, t-statistic, p-value, and fold change (mean2/mean1)
# rad.sham.t<-function(dataset,prot.name,group.var){
#   
#   # assemble the text string for the formula and run the model
#   t.str<-paste0("t.test(",prot.name,"~",group.var,",data=",deparse(substitute(dataset)),")")
#   t.prot<-eval(parse(text=t.str))
#   t.n1<-sum(dataset[,group.var]==0)
#   t.n2<-sum(dataset[,group.var]==1)
# 
#   # pull out the t-statistic and p-value
#   t.stat<-t.prot[["statistic"]]
#   t.pval<-t.prot[["p.value"]]
#   t.fold<-t.prot[["estimate"]][2]/t.prot[["estimate"]][1]
#   return(c(n1=t.n1,n2=t.n2,t.stat,p=t.pval,fold=t.fold))
#   
# }
# 
# # try function above
# rad.sham.t(dataset=rel3.data.norm,prot.name="Q99ML5",group.var="bad")
# 
# 
# # Function to do t-tests of rad vs sham for ALL proteins
# # Input: dataset, list of proteins, group variable for comparison, and subset of rats to include
# rad.sham.t.all<-function(dataset,prot.list,group.var,sub){
#   
#   # create empty results matrix to store the results
#   rad.sham.t.result<-matrix(nrow=length(prot.list),ncol=5)
#   colnames(rad.sham.t.result)<-c("n1","n2","t","p","fold")
#   rownames(rad.sham.t.result)<-prot.list
#   
#   dataset<-dataset[sub,]
#   #print(data.sub[,1:10])
#   
#   # run the p-value function for all proteins in the list and collect the results
#   for (i in 1:length(prot.list)) {
#   
#     res<-rad.sham.t(dataset=dataset,prot.name<-prot.list[i],group.var=group.var)
#     rad.sham.t.result[i,]<-res
#     
#   }
#   
#   # process the results for output
#   rad.sham.t.result<-data.frame(rad.sham.t.result)
#   rownames(rad.sham.t.result)<-NULL
#   rad.sham.t.result$protein<-as.character(prot.list)
#   rad.sham.t.result<-rad.sham.t.result[,c("protein","n1","n2","t","p","fold")]
#   rad.sham.t.result<-rad.sham.t.result[order(rad.sham.t.result$p),]
#   return(rad.sham.t.result)
# }







### Create indicators for subsets of rats

### IPA: t-tests
###   Britten 2017
###     - good vs bad (good is baseline), just rad
###
###   Dutta
###     - bad vs good (rad only)
###     - bad vs good (sham only)
###     - rad vs sham (sham is baseline)
###
###   Tidmore
###     - bad vs good (rad only)
###     - bad vs good (sham only) -- NO - SAME AS DUTTA'S SHAMS
###     - rad vs sham (sham is baseline)

### Single-protein modeling - protein vs REL3
###   Fe exposure - Dutta
###     - rad only
###     - sham only
###     - rad + sham
###
###   Ti exposure - Tidmore
###     - rad only
###     - sham and rad
### 
###   Ti exposure - Britten unpublished
###     - rad only
###     - sham and rad
###
###   Ti exposure - combine Dutta and Britten unpublished
###     - rad only
###     - sham and rad
###
###   Both Fe and Ti
###     - all rats

### Note Dutta and Tidmore use same shams

### Jump NB T-test
library(data.table)
library(stringr)
library(dplyr)

pdata = fread('LARS_Proteins/Data/210113 aim4_imputeddata.csv - aim4_imputeddata.csv.csv')[,-1]
mdata = fread('LARS_Proteins/Data/Four_Datasets_Data - Four_Datasets_Data.csv')[,2:13]
mdata = mdata %>% filter(str_detect(Variable,'Imputed')) 
mdata = mdata %>% filter(!(Rat_ID=='`EA5E'&Treatment=='Good'))

pdata = pdata[,1:31]
x = pdata %>% colnames %>% .[5:31]
x %in% mdata$Rat_ID

head(mdata)
head(pdata)

Rep_Table = pdata[,1:4]
tmp = melt(pdata,id.vars = 1:4,variable.name = 'Rat_ID') %>%  dcast(Rat_ID~ Protein ,value.var = 'value') #%>% .[,1:10]
mdata = mdata %>% filter(Rat_ID%in%tmp$Rat_ID)

data = merge(mdata[,!'Variable'],tmp,by = 'Rat_ID')

###rel3.data.norm = metadata, in wide format, 
### Rep_Table holds replicate information for sorting 
### data hold merged meta data and protein data

### Subsetting

# • Create Run Groups
# 	○ Just Sham
# 	○ Just Fe
# 	○ Just Te
# 	○ Ti + Fe
# 	○ Sham + Ti + Fe
# 	○ Limit inclusion to at least two animals in 
# 		§ The individual group
# 		§ In both for the combined irradiation group
# 		§ In at least one of the the three for the all combined trial
# • 
Rep_Table
# all_of(x)/any_of(x, …, vars)
data$Ion = 'Sham'
data$Ion[str_detect(data$Dose,'F')] = 'Fe'
data$Ion[str_detect(data$Dose,'T')] = 'Ti'
data[,.(Dose,Ion)]
setDF(data)
setnames(data,"Day 1 EL",'Day_1')
cols = colnames(data)[1:11]


only_Sham = data %>% filter(Ion == 'Sham') %>% select(all_of(c(cols, Rep_Table[Sham>=2][[1]] )))
only_Ti = data %>% filter(Ion == 'Ti') %>% select(all_of(c(cols, Rep_Table[Ti>=2][[1]] )))
only_Fe = data %>% filter(Ion == 'Fe') %>% select(all_of(c(cols, Rep_Table[Fe>=2][[1]] )))
both_Ti_Fe = data %>% filter(Ion %in% c('Ti','Fe')) %>% select(all_of(c(cols, Rep_Table[Ti>=2&Fe>2][[1]] )))
all_Sham_Ti_Fe = data %>% select(all_of(c(cols, Rep_Table[Ti>=2 | Fe>2 | Sham>2][[1]] )))

Subsets = list('only_Sham'=only_Sham,'only_Ti'=only_Ti,'only_Fe'=only_Fe,'both_Ti_Fe'=both_Ti_Fe,'all_Sham_Ti_Fe'=all_Sham_Ti_Fe)
sapply(Subsets,dim)

# rel3.data.norm = data

### Run stuff

names(Subsets)

library(parallel)
# nb.all<-nb.result(prot.data=Subsets$all_Sham_Ti_Fe,outvar="REL3")

NB_Results_REL3 = mclapply(Subsets,mc.cores = 10,function(x){
  nb.result(prot.data=x,outvar="REL3")
})

NB_Results_Day_1 = mclapply(Subsets,mc.cores = 10,function(x){
  nb.result(prot.data=x,outvar="Day_1")
})

Caught = lapply(NB_Results_Day_1,function(x){
  x[which(is.na(x$converged)),"protein"]
}) 

data.table(Subset=names(Caught),Excluded=sapply(Caught,Dim),Total=sapply(NB_Results,Dim)[1,],
           Percent=round(sapply(Caught,Dim)/sapply(NB_Results,Dim)[1,],3)*100)
### Write to excel 
# Yvec='REL3'
# lapply(1:length(NB_Results_REL3),function(i){
#   x = NB_Results_REL3[[i]]
#   write.xlsx(x,"LARS_Proteins/output/NB/proteomics_result.xlsx",sheetName=paste0(names(NB_Results_REL3)[i],'_',Yvec),
#              append=TRUE, col.names=TRUE, row.names=FALSE)
# })
# Yvec='Day_1'
# lapply(1:length(NB_Results_Day_1),function(i){
#   x = NB_Results_Day_1[[i]]
#   write.xlsx(x,"LARS_Proteins/output/NB/proteomics_result.xlsx",sheetName=paste0(names(NB_Results_Day_1)[i],'_',Yvec),
#              append=TRUE, col.names=TRUE, row.names=FALSE)
# })

# Good vs bad


# no.excl<-rel3.data.norm$exclude!=1
# 
# dutta<-rel3.data.norm$Publication=="Dutta"
# dutta.rad<-rel3.data.norm$Publication=="Dutta" & rel3.data.norm$rad==1 & no.excl
# dutta.sham<-rel3.data.norm$Publication=="Dutta" & rel3.data.norm$rad==0 & no.excl
# 
# tidmore<-(rel3.data.norm$Publication=="Tidmore" | (rel3.data.norm$Publication=="Dutta" & rel3.data.norm$rad==0)) & no.excl
# tidmore.rad<-rel3.data.norm$Publication=="Tidmore" & rel3.data.norm$rad==1 & no.excl
# 
# brit.unpub<-rel3.data.norm$Publication=="Britten, unpublished" & no.excl
# brit.unpub.rad<-rel3.data.norm$Publication=="Britten, unpublished" & rel3.data.norm$rad==1 & no.excl
# 
# ti.combined<-(rel3.data.norm$Publication %in% c("Britten, unpublished","Tidmore") | 
#   (rel3.data.norm$Publication=="Dutta" & rel3.data.norm$rad==0))  & no.excl
# ti.combined.rad<-rel3.data.norm$Publication %in% c("Britten, unpublished","Tidmore") & rel3.data.norm$rad==1 & no.excl
# 
# brit.2017<-rel3.data.norm$Publication=="Britten 2017" & no.excl
# brit.2017.rad<-rel3.data.norm$Publication=="Britten 2017" & rel3.data.norm$rad==1 & no.excl
# 
# all.rad<-rel3.data.norm$rad==1 & rel3.data.norm$Publication!="Britten 2017" & no.excl 
# all.sham<-rel3.data.norm$rad==0 & rel3.data.norm$Publication!="Britten 2017" & no.excl
# all<-rel3.data.norm$Publication!="Britten 2017" & no.excl

# check the # of rats in each group to make sure they're correct
# sum(brit.2017.rad)
# sum(all)
# sum(all.rad)
# sum(all.sham)
# sum(dutta)
# sum(dutta.rad)
# sum(dutta.sham)
# sum(tidmore)
# sum(tidmore.rad)
# sum(ti.combined)
# sum(ti.combined.rad)





### Good vs Bad

# # Dutta (sad only)
# good.bad.dutta.rad<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                               group.var="bad",sub=dutta.rad)
# 
# # Tidmore (rad only)
# good.bad.tidmore.rad<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                                    group.var="bad",sub=tidmore.rad)
# 
# # Britten 2017 (not in other analyses) - good vs bad
# good.bad.brit.2017.rad<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                                      group.var="bad",sub=brit.2017.rad)
# 
# # All rats (excluding Britten 2017)
# good.bad.all<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                                    group.var="bad",sub=all)
# 
# # All irratiated rats
# good.bad.all.rad<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                              group.var="bad",sub=all.rad)




### Rad vs Sham

# Dutta
# rad.sham.dutta<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                               group.var="rad",sub=dutta)
# 
# # Tidmore
# rad.sham.tidmore<-rad.sham.t.all(dataset=rel3.data.norm,prot.list=names(rel3.data.norm)[17:dim(rel3.data.norm)[2]],
#                                group.var="rad",sub=tidmore)


### negative binomial (single-protein) models

# All rats
# nb.all<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=all)
# 
# # Irratiated rats
# nb.all.rad<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=all.rad)
# 
# # Sham rats
# nb.all.sham<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=all.sham)
# 
# # Dutta dataset
# nb.dutta<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=dutta)
# 
# # Dutta (rad only)
# nb.dutta.rad<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=dutta.rad)
# 
# # Dutta (sham only)
# nb.dutta.sham<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=dutta.sham)
# 
# # Tidmore (and shams from Dutta)
# nb.tidmore<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=tidmore)
# 
# # Tidmore (rad only)
# nb.tidmore.rad<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=tidmore.rad)
# 
# # Tidmore and Britten unpublished combined titanium
# nb.ti.combined<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=ti.combined)
# 
# # Tidmore + Britten unpublished (rad only)
# nb.ti.combined.rad<-nb.result(prot.data=rel3.data.norm,outvar="REL3",subset=ti.combined.rad)






#############################################################################################
###  MAIN FUNCTION
###  Steps:
###     1. Run LARS with input dataset and protein list. Note which proteins are chosen
###     2. Run cross-validated LARS to get list of which proteins
###        are chosen most frequently across iterations
###     3. Pick the top proteins from that list
###     4. Try combinations of those proteins with a specified model size
###        (run with sampling and repetition).
###     5. For each model, calculate the median likelihood across all iterations.
###        Find the "best" combination.
###     6. Run a negative binomial model with the "best" predictors in the full population.
###        (extract the AIC)
###     7. Use the "best" model to predict behavior and regress on observed behavior.
###     8. Extract R2 and adjusted R2
###
###   Inputs:
###     1. Label for group and model (pop.label)
###     2. Dataset to input (dataset)
###     3. Outcome variable name (outvar)
###     4. List of proteins to consider (pro.list)
###     5. Number of proteins from LARS list to consider for "best" model (n.pool)
###     6. Number of proteins to choose from the pool for the "best" model (n.choose)
###     7. Number of sampling runs (n.runs) (default=1000)
###     8. Number of rats to leave out of each sample run (leave.out)
###     9. Random seed for repetition (seed) (default=12345)
###
###   For the sampling, we'll aim to take out 20% of the observations each time.
###   For the number of proteins in the final models, the maximum is half the number of remaining rats in the group
#############################################################################################

lars.protein<-function(pop.label,dataset,outvar,pro.list,n.pool,n.choose,n.runs=1000,leave.out,seed=12345){
  
  # record start time
  t.start<-Sys.time()
  
  # set random seed
  set.seed(seed)
  
  # create object to house results
  result<- vector(mode = "list", length = 12) 
  names(result)<-c("group","LARS","LARS.list","LARS.samp.list","protein.pool",
                   "model.table","best.model","nb.best","aic.best","lm.pred","r2","adj.r2")

  # "Simple" LARS
  lars.init<-lars(x=as.matrix(dataset[,pro.list]),y=dataset[,outvar],type="lar")
  lars.coef<-data.frame(coef(lars.init))
  protein.pct<-apply(apply(lars.coef,2,ne0),2,mean)
  lars.reg<-data.frame(cbind(colnames(lars.coef),lars.init[["entry"]],protein.pct*100))
  names(lars.reg)<-c("protein","entry","pct")
  lars.reg$entry<-as.numeric(as.character(lars.reg$entry))
  lars.reg$pct<-as.numeric(as.character(lars.reg$pct))
  lars.reg$pct<-round(lars.reg$pct,1)
  lars.reg<-lars.reg[lars.reg$entry>0,]
  lars.reg<-lars.reg[order(lars.reg$pct,decreasing=TRUE),]
  rownames(lars.reg)<-NULL


  # LARS with sampling
  lars.samp.all<-matrix(nrow=n.runs,ncol=length(pro.list)+1)
  colnames(lars.samp.all)<-c("total",pro.list)
  
  nrow<-dim(dataset)[1]
  
  for (i in 1:n.runs) {
    samp<-sample(1:nrow,nrow-leave.out)
    lars.samp<-lars(x=as.matrix(dataset[samp,pro.list]),y=dataset[samp,outvar],type="lar")
    lars.sum.samp<-apply(apply(coef(lars.samp),2,ne0),2,sum)
    lars.sum.samp<-c(dim(coef(lars.samp))[1],lars.sum.samp)
    lars.samp.all[i,]<-lars.sum.samp
  }
  
  lars.samp.total<-apply(lars.samp.all,2,sum)
  lars.samp.total<-lars.samp.total[order(lars.samp.total,decreasing = TRUE)]
  lars.samp.df=data.frame(lars.samp.total)
  lars.samp.df$pct=lars.samp.df[,1]/lars.samp.df[1,1]*100
  lars.samp.df<-lars.samp.df[lars.samp.df[,1]>0,]
  names(lars.samp.df)<-c("N","pct")
  lars.samp.df<-lars.samp.df[-1,]
  
  # list of the "best" proteins to use for model selection
  protein.best<-rownames(lars.samp.df)[1:n.pool]
  
  combo<-combn(1:n.pool,n.choose)
  choose.num<-choose(n.pool,n.choose) # number of possible models
  
  # create results matrix for model runs
  nb.res<-matrix(nrow=choose.num,ncol=n.runs)
  colnames(nb.res)<-paste("ll_",1:n.runs,sep="")
  nb.pro.names<-matrix(nrow=choose.num,ncol=n.choose)
  colnames(nb.pro.names)<-paste("protein_",1:n.choose,sep="")

  # Run NB models for each combination
  for (i in 1:choose.num) {
    
    nb.pro.names[i,]<-protein.best[combo[,i]]
    form=paste(protein.best[combo[,i]],collapse="+")

    for(j in 1:n.runs) {
      samp<-sample(1:nrow,nrow-leave.out)
      tryCatch({nb.mod<-glm.nb(paste(outvar,"~",form,sep=""),data=dataset[samp,])},
               error=function(e){cat("ERROR:",form,"num=",j,"sample=",samp,"\n")})
      nb.res[i,j]<-nb.mod[["twologlik"]]
    }
  }
  

  # calculate median likelihood and pick the "best" model
  nb.res2<-data.frame(cbind(nb.pro.names,apply(nb.res,1,median),nb.res))
  names(nb.res2)[n.choose+1]<-"ll_median"
  nb.res2$ll_median<-as.numeric(as.character(apply(nb.res,1,median)))
  nb.res2<-nb.res2[order(nb.res2$ll_median,decreasing=TRUE),]

  # take "best" model and run for full population
  form.best<-paste(as.matrix(nb.res2[which.max(nb.res2$ll_median),1:n.choose]),collapse="+")
  glm.nb.str<-paste(outvar,"~",form.best,sep="")
  nb.best<-glm.nb(glm.nb.str,data=dataset)
  #nb.best<-glm.nb(paste(outvar,"~",form.best,sep=""),data=dataset)
  dataset[,"pred"]<-exp(predict(nb.best))
  #dataset[,"pred"]<-predict(nb.best)
  # regress predicted behavior on observed behavior
  #lm.best<-lm(REL3~pred,data=dataset)
  lm.best<-lm(paste(outvar,"~pred",sep=""),data=dataset)
  
  # assemble results object and return
  result[[1]]<-pop.label
  result[[2]]<-lars.init
  result[[3]]<-lars.reg
  result[[4]]<-lars.samp.df
  result[[5]]<-protein.best  
  result[[6]]<-nb.res2
  result[[7]]<-nb.res2[which.max(nb.res2$ll_median),1:(n.choose+1)]
  result[[8]]<-nb.best
  result[[9]]<-nb.best[["aic"]]
  result[[10]]<-lm.best
  result[[11]]<-summary(lm.best)[["r.squared"]]
  result[[12]]<-summary(lm.best)[["adj.r.squared"]]
  
  
  t.end<-Sys.time()
  print(difftime(t.end,t.start,units="mins"))
  
  return(result)
}


### Helper function for the LARS function - count if a protein is in the LARS output
ne0<-function(n){
  res<-ifelse(n==0,0,1)
  return(res)
}


### Functions to summarize and export the models and results

lars.summ<-function(res){
  cat("Run\n",res[["group"]],
      "\nBest Model:\n",as.matrix(res[["best.model"]]),
      "\nAIC=", res[["aic.best"]],
      "\nr2=", res[["r2"]],
      "\nadj-r2=",res[["adj.r2"]],"\n\n")
}

# pull out what we need from the models: name, AIC, r2, adjusted r2, and the proteins in the "best" model
lars.extract<-function(res){
  best<-as.matrix(res[["best.model"]])
  cat(res[["group"]],res[["aic.best"]],res[["r2"]],res[["adj.r2"]],best[,1:ncol(best)-1],"\n",sep=",")
}


 




###Jump Lars

# plot = lapply(1:length(NB_Results),function(x){
#   x1 = NB_Results[[x]] %>% filter(p<.05)
#   x1$group = names(NB_Results)[[x]]
#   return(x1)
# }) %>% rbindlist
# 
# plot


grid = expand.grid(names(Subsets),2:5)
N_Proteins = 100

Yvec = 'REL3'
library(tictoc)
LARS_Results = mclapply(1:nrow(grid),mc.cores = 10,function(i){
  x = grid[i,]
  tic()
  x1 = lars.protein(pop.label=paste0(x[[1]],'_',x[[2]]),dataset=Subsets[[x[[1]]]],pro.list=NB_Results_REL3[[x[[1]]]]$protein[seq_len(N_Proteins)],
                                 outvar=Yvec,n.pool=10,n.choose=x[[2]],n.runs=1000,leave.out=2,seed=333)
  saveRDS(x1,paste0('LARS_Proteins/Data/output/',paste0(x[[1]],'_',x[[2]],'_',Yvec)))
  print(toc())
})

Yvec = 'Day_1'
library(tictoc)
LARS_Results = mclapply(1:nrow(grid),mc.cores = 10,function(i){
  x = grid[i,]
  tic()
  x1 = lars.protein(pop.label=paste0(x[[1]],'_',x[[2]]),dataset=Subsets[[x[[1]]]],pro.list=NB_Results_Day_1[[x[[1]]]]$protein[seq_len(N_Proteins)],
                    outvar=Yvec,n.pool=10,n.choose=x[[2]],n.runs=100,leave.out=2,seed=333)
  saveRDS(x1,paste0('LARS_Proteins/Data/output/',paste0(x[[1]],'_',x[[2]],'_',Yvec)))
  print(toc())
})



### Export the results to a .csv for processing
setwd("//campus.berkeley.edu/eei-dfs/SPH/CERCH/Departmental/RESEARCH/NASA/lars/Britten_Dutta proteomics/")

# group, AIC, r2, adjusted r2, proteins

sink(file="proteomics_4_datasets_lars_results.csv",append=FALSE)

lapply(list(rel3.all.2,rel3.all.3,rel3.all.4,rel3.all.5,rel3.all.rad.2,rel3.all.rad.3,
            rel3.all.rad.4,rel3.all.rad.5,rel3.all.sham.2,rel3.all.sham.3,rel3.dutta.2,rel3.dutta.3,rel3.dutta.4,
            rel3.dutta.rad.2,rel3.dutta.sham.2,rel3.tidmore.2,rel3.tidmore.3,
            rel3.tidmore.4,rel3.tidmore.5,rel3.tidmore.rad.2,rel3.ti.combined.2,rel3.ti.combined.3,
            rel3.ti.combined.4,rel3.ti.combined.rad.2,rel3.ti.combined.rad.3),lars.extract)

lapply(tmp,lars.extract)
sink()


# pull out what we need from the models: name, AIC, r2, adjusted r2, and the proteins in the "best" model
lars.extract<-function(res){
  best<-as.matrix(res[["best.model"]])
  cat(res[["group"]],res[["aic.best"]],res[["r2"]],res[["adj.r2"]],best[,1:ncol(best)-1],"\n",sep=",")
}


