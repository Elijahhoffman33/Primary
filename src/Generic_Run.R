load("/mnt/c/Users/elija/Desktop/Lab_Shit/R_Space/Worksapces/R_Space_1_7_22.RData")

library(iRF)
library(rpart)
library(rpart.plot)
library(stringr)
library(raster)
library(limma)
library(data.table)
library(ggplot2)
library(tictoc)
library(doParallel)
library(plyr)
library(dplyr)
library(DescTools)
library(optimStrat)


# Sys.sleep(3600)
#1:length(Regions)
#c(1:3,5:length(Regions)
lapply(c(1:length(Regions)),function(I){
  print(IDs[[I]])
  data = Data[field_id%in%IDs[[I]]]
  # data = Data[field_id%in%19847]
  print(data$Total_Acres[[1]])
  tmp <- Run_Split(Regions[I],data,3,Vars,Vars_in,run_voxels = T,
                   Iterations = 250,Iter_runs = 10,Threshold = .05,allocation = 'Neyman')
  print('Saving')
  saveRDS(tmp,paste0('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/Verra_Strata_Tmp/Dir_9/',Regions[I]))
  # return(tmp)
})
