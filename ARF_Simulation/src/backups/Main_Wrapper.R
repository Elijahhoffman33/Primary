library(iRF)
library(rpart)
library(stringr)
library(data.table)
library(parallel)
library(R.utils)
library(ggplot2)
library(plyr)
library(dplyr)
library(terra)
library(sf)

# source('src/Functions/SF_Terra.R')
# source('src/Functions/Strata_Sampling_Main.R')
source('../Soil_Stratification_Sampling/src/Functions/SF_Terra.R')
source('../Soil_Stratification_Sampling/src/Functions/Strata_Sampling_Main.R')


# Main Wrapper ----


# input_path='data/input/Island_field.csv.gz'
# training_path = 'data/input/Island_field_training.csv.gz'
# output_path = 'output/Island_field_Output.csv.gz'

### Global Parameter defaults
Config = list(Region='Default',
              Iterations = 100,Iter_Run = 10,batch_limit = 250,
              allocation='Neyman',
              Max_Features = 2,Cores_Global=NULL,max_iRF_samples=2000,
              Force_OM_Source=F,Force_OM_Public=F,Force_iRF=F,
              Output_Summary=F)

Main_Wrapper = function(input_path,training_path,output_path,...){
  ### Unpack Config to enviroment
  list2env(Config, environment())
  ### Unpack arguments
  list2env(list(...), environment())  
  
  training_data = fread(training_path)
  ### Remove blacklisted columns 
  training_data[,':='('topo_elevation'=NULL,'topo_insolation'=NULL)]
  
  
  ### I'll update this to better estimate RAM use later
  if(is.null(Cores_Global)){
    Cores_Global = detectCores() - 2 
    if(Cores_Global < 1){
      Cores_Global = 1
    }
  }
  
  # Select Public Covariates - Run feature selection ----
  
  ### Calcualte weighted averages (layer depth / total depth)
  Get_Weighted_Avg_Depths(training_data)
  
  ###Run iRF, get hierarchy of importance / Select OM Source
  
  # colnames(training_data)[c(1:11,44:48)] %>% dQuote(F) %>% cat(sep=',')
  Metadata = c("lng","lat","voxel_id","crop","cluster_label","cluster_version","harvest_year",
           "predicted_yield","field_id","acres","year",
           "geoid","county_name","cdl_2017_class","cdl_2018_class","cdl_2019_class")
  
  ### Check if soil sample derived om exists 
  ### test if the om column is present 
  if('om' %in% colnames(training_data)){
    ### And if not mostly NA
    if(training_data[, (sum(is.na(om)|om==0)/.N) < .05 ]){
      OM_Source = 'Soil_Samples'
    } else{ OM_Source = 'Public_Predictions' }
  } else{ OM_Source = 'Public_Predictions' }
  
  if(Force_OM_Source!=F){
    OM_Source = Force_OM_Source
  }
  
  ### Check Data
  training_data = check_data(training_data,Metadata)
  
  ### Downsample training data
  if(nrow(training_data)>max_iRF_samples){
    index = 1:nrow(training_data) %>% sample(size = max_iRF_samples,replace=F)
  } else{ index=seq_len(nrow(training_data)) }
  
  ### Run Repeated Kfold iRF
  source('../Soil_Stratification_Sampling/src/Functions/iRF_Wrapper.R')
  
  print('Running iRF')
  
  if(OM_Source=='Soil_Samples'){
    cols_x = which(!colnames(training_data)%in%c(Metadata,c('om'))) %>% 
      colnames(training_data)[.]
    if(Force_iRF==F){
      tmp = Run_Model(training_data[index,],cols_x,cols_y ='om',Model='iRF',
                      K=5,single=F,ncores = Cores_Global,summary = T)
      
      Selected_Features = tmp$mGI[1:Max_Features][[1]]
    } else{ Selected_Features=Force_iRF }
    
    Selected_OM = 'om'
    
  } else if(OM_Source=='Public_Predictions'){
    ###? We need a better way to select public covariates
    ###  For now Soil Grids is the default 
    
    OM_Vars = c('sg_soc','polaris_om','gssurgo_om_r')[[1]] # The [[1]] is setting SG to default
    
    ###  Check if Public covariates are present 
    col_index = OM_Vars %in% colnames(training_data)
    OM_Vars = OM_Vars[col_index]
    if(Force_OM_Public!=F){
     OM_Vars = Force_OM_Public
    }
    if(length(OM_Vars)==0){
      stop('No Public Covariate Present')
    }
    if(Force_iRF==F){
      OM_Runs = lapply(OM_Vars,function(x){
        print(x)
        cols_x = which(!colnames(training_data)%in%c(Metadata,OM_Vars,'om')) %>%
          colnames(training_data)[.]
        
        Run_Model(training_data[index,],cols_x,cols_y = x,Model='iRF',
                  K=5,single=F,ncores = Cores_Global,rep_kfold = NULL,summary=T)
        
      })
    } else{Selected_Features=Force_iRF}
    
    if(Force_OM_Public==F){
      names(OM_Runs) = OM_Vars
      ### Select Public OM
      index = sapply(OM_Runs,'[[',2) %>% which.max
      Selected_OM = names(index)
      Selected_Features = OM_Runs[[index]]$mGI[1:Max_Features][[1]]
      
    } else{ # Force = T
      
      Selected_OM = OM_Vars
      
      if(Force_iRF==F){
        ### Select Public OM
        names(OM_Runs) = OM_Vars
        ### Select Public OM
        index = sapply(OM_Runs,'[[',2) %>% which.max
        
        Selected_Features = OM_Runs[[index]]$mGI[1:Max_Features][[1]]
      }
      
          }
    
    
  }
  
  rm(training_data)
  
  # Data Prep / Run Simulation ----
  
  
  Main_Data = fread(input_path)
  
  Main_Data = check_data(Main_Data,Metadata)
  Main_Data[,':='('topo_elevation'=NULL,'topo_insolation'=NULL)]
  Get_Weighted_Avg_Depths(Main_Data)
  
  if(OM_Source!='Soil_Samples'){
    ### Remove om, and rename selected to om 
    Main_Data[,'om':=NULL]
    setnames(Main_Data,Selected_OM,'om')
  }
  
  cols = c('voxel_id','lng','lat','acres','om',Selected_Features)
  cols_remove = colnames(Main_Data)[!colnames(Main_Data)%in%cols]
  
  Main_Data[,(cols_remove):=NULL]
  
  ### Get interactions to test 
  Features_Interactions = c(Selected_Features,
                            combn(Selected_Features, 2, FUN=paste, collapse='+'))
  
  ### Run Simulation

  print('Running Sampling Simulation')
  Sim_Result <- Sampling_Simulation(Region,Main_Data,3,Features_Interactions,Iterations = Iterations,Iter_runs = Iter_Run,
                                    allocation = allocation,sampler_type = 'dq',Par = 'outer',batch_limit = batch_limit,
                                    exclude=NULL,ncores = Cores_Global)
  
  ### Select Regime / Export ----
  
  Sim_Summary = Sim_Result$Summary
  
  ### PLotting code
  # span=.3
  # gg <- ggplot(Sim_Summary[Acres_Sample<100&str_detect(Strata,'ooga',negate=T)],
  #              aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1)
  #   geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
  # # gg = gg + facet_wrap(~Region) + geom_vline(data=dataLine,aes(xintercept=int),alpha=.4)
  # gg + theme_classic(base_size = 18) + geom_abline(intercept =3,slope=0,alpha=.5)#+ ylim(0,NA) #+ geom_hline(yintercept=1,alpha=.4) +

  
  tmp = Select_Results(Sim_Summary$Region,x=Sim_Summary) 
  
  ### Take less complex option if less complex option is comperable / better
  ### This was written to iterate over differnt regions, the apply function isnt needed
  Selected = lapply(1:nrow(tmp),function(x){
    x=tmp[x,]
    x1 = Sim_Summary[Region==x$Region&n==x$n]
    x2 = x1[E_Prob<x$E_Prob]
    if(nrow(x2)==0){ return(x) }
    
    complex_x2 = sapply(x2$Strata,complexity)
    complex_x1 = complexity(x$Strata)
    
    if(any(complex_x2 < complex_x1)){
      index = which(complex_x2<complex_x1)
      
      if(length(index)==1){
        return(x2[index,])  
      } 
      else if(str_detect(names(index),':0') %>% all){
        index_1 = which(str_detect(names(index),'om:0'))
        x2[index[index_1],]
      } else{
        warning('Multiple Regimes Selected, using first option. This is probably due to a tie in selection')
        x2[index[1],]
      }
      
      
    } else{return(x)}
    
  }) %>% rbindlist(use.names = T)
  
  Region = as.character(Region)
  Strata=Selected$Strata
  N=Selected$n 
  
  Main_Data <<- Main_Data
  x1 = pull_result(Region,'list',1,local=Sim_Result)
  index = x1[which(x1$Strata==Strata),2]  
  
  Output_Samples = pull_samples(Selected,local = Sim_Result)
  
  ### plotting code
  # tm1 = plot_overlay(Region,index,'raster',var='Strata',plot_samples=T,samples=Output_Samples)
  # input_data = pull_result(Region,'data',Strata = index,local=Sim_Result)
  # quick_map(data,samples=Output_Samples,scale = .26)
  # quick_map(input_data,samples=Output_Samples,scale = .26)
  
  
  Output = Output_Samples[,.(lng,lat)]
  Output$Sampling_Algorithm = 'Simulated_Block_Sampling'
  Output$Is_Control = F 

  ### Output Sample

  fwrite(Output,output_path,compress="gzip")
  
  ### Output Summary Data
  if(Output_Summary){
    Sim_Result$Selected = Selected
    Sim_Result$Selected_Features = Selected_Features
    Sim_Result$Selected_OM = Selected_OM
    
    Sim_Result$Run_Data = Main_Data
    
    Sim_Result$Config = list(Region=Region,
                             Iterations = Iterations,Iter_Run = Iter_Run,batch_limit = batch_limit,
                             allocation=allocation,
                             Max_Features = Max_Features,Cores_Global=Cores_Global,max_iRF_samples=max_iRF_samples,
                             Force_OM_Source=Force_OM_Source,Force_OM_Public=Force_OM_Public,Force_iRF=Force_iRF)
    Sim_Result$Output = Output
    return(Sim_Result)
  }
}

# Functions ----

### Rescale polaris / Soil Grids

Rescale_om = function(tmp_data){
  ## Polaris
  cols <- grep("polaris_om_", colnames(tmp_data), value=T)
  tmp_data[,(cols):=10^(.SD/10),.SDcols=cols]
  
  ## Soil Grids
  cols <- grep("sg_soc_", colnames(tmp_data), value=T)
  tmp_data[,(cols):=(.SD/100*1.72),.SDcols=cols]
} 

### Calcualte weighted averages (layer depth / total depth)
# tmp_data = data.table::copy(data)

Get_Weighted_Avg_Depths = function(tmp_data){
  
  cols = colnames(tmp_data)
  cond_1 =  str_detect(cols,'gssurgo') & str_detect(cols,'_30')
  cond_2 =  str_detect(cols,'.*60')
  index = colnames(tmp_data)[cond_1|cond_2]
  tmp_data[,(index):=NULL]
  
  cols = colnames(tmp_data) %>% str_detect('_0') %>% which %>% colnames(tmp_data)[.] %>% 
    tstrsplit('_0') %>% '[['(1)
  
  ### Set weights
  W = c(5,10,15)/30
  
  lapply(cols,function(x){
    x1 = colnames(tmp_data) %>% str_detect(x) %>% which %>% colnames(tmp_data)[.]
    
    ### Ensure order 0, 5, 15
    order_by = x1 %>% str_extract('_\\d+') %>% str_extract('\\d+') %>% as.integer
    x1 = x1[order(order_by)]
    
    ### Weigthed Average over depths then remove
    tmp_data[,(x):=matrixStats::rowWeightedMeans(as.matrix(.SD),w = W ),.SDcols=x1]
    tmp_data[,(x1):=NULL]
  })
}

### Check Input Data

check_data = function(data_tmp,Meta){
  
  index = which(colnames(data_tmp)%in%Meta)
  seq = seq_len(ncol(data_tmp))[-index]
  
  x1 = lapply(seq,function(i){
    x = data_tmp[,..i][[1]]
    NAs = is.na(x)
    Zeros = x==0
    Is_Finite = is.finite(x)
    return(
      data.table('NA' = sum(NAs),'%NA'=sum(NAs)/length(x),
                 'Zero' = sum(Zeros),'%Zero'=sum(Zeros)/length(x),
                 '!Finite' = sum(!Is_Finite),'%!Finite'=sum(!Is_Finite)/length(x))
    )
    
  }) %>% rbindlist
  
  x1$Column = colnames(data_tmp[,..seq])
  setcolorder(x1,'Column')
  x1
  
  col_remove = x1[which(`%NA`>.9|
                          `%Zero`>.9)][[1]]
  
  col_remove = col_remove[str_detect(col_remove,'polaris_om_|gssurgo_om_|sg_soc',negate=T)]
  if(length(col_remove)>0){
    data_tmp[,(col_remove):=NULL]
  }
  
  ### Check duplicates 
  
  Duplicate_Voxels = duplicated(data_tmp$voxel_id)
  Duplicate_Coordinates = duplicated(data_tmp[,paste(lng,lat)])
  True_Duplicates = duplicated(data_tmp)
  
  row_index = c()
  if(sum(Duplicate_Voxels)>0){
    y = sum(Duplicate_Voxels)>0
    row_index = c(row_index,which(Duplicate_Voxels))
    message(paste0(y,' duplicated voxels found, removing duplicate instances'))    
  }
  if(sum(Duplicate_Coordinates)>0){
    y = sum(Duplicate_Coordinates)>0
    row_index = c(row_index,which(Duplicate_Coordinates))
    message(paste0(y,' duplicated coordinates found, removing duplicate instances'))    
  }
  if(sum(True_Duplicates)>0){
    y = sum(True_Duplicates)
    row_index = c(row_index,which(True_Duplicates))
    message(paste0(y,' true duplicates found, removing duplicate instances'))
  }
  if(length(row_index)>0){
    return(data_tmp[-index])
  } else{
    return(data_tmp)
  }
}

###Decisions
# 5% error (10% margin of error)
# 3% Chance of a given simulation being further than 5% from the true mean
# 
# If Fitting
# First point that is better than 3%
# 
# If all is above 3%
# Whichever is best at 5 acres per sample
# 
# If all below 3%
# Whichever is best at 20 acres per sample
# 
# Or just random simple random sampling if all is flat 
# Strata%in%c('om:2')

### Diagnostic Plots
# gg <- ggplot(tbf[Region==region&Acres_Sample<5000],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
#   geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
# gg
# gg +ylim(0,3.05)

Select_Results = function(region,x,threshold=3.03){
  x = x[Region==region,]
  
  if(nrow(x[!n_samples%in%c(1,2)])==0){ # If only 1 / 2 samples were simulated
    index = x[,n_samples==max(n_samples)&Strata=='om:0']
    return(x[index,])
  }
  
  x = x[!n_samples%in%c(1,2)]
  # check above 
  FUN = function(X,threshold = 3.03){
    x1 =X$E_Prob*100
    y = x1-threshold
    y[y>0] = -1
    selected = which.max(y) %>% X[.,]
    
    if(selected$E_Prob*100 > threshold){
      selected$E_Prob = 100
    }
    return(selected)
  }
  
  x2 = x[,FUN(.SD,threshold = threshold),Strata]
  ### Check ?
  if(any(x2$E_Prob!=0)&!any(x2$n!=1)){
    x2$E_Prob=99
    x = x[n_samples!=1]
  }
  x2 = x2[E_Prob!=100,]
  
  ###If all above %P
  if(nrow(x2)==0){
    x1 = x$Acres_Sample
    ###Get best nearest to 5 acres_sample
    index = which(abs(x1-5)<.05)
    if(length(index)==0){
      min = min(abs(x1-5))
      index = which(abs(x1-5)==min)
      message(paste0('Region: ',region,' nearest 5 acre >.05 A/S away, using ',round(min,2),' A/S instead'))
    }
    
    x2 = x[index,]
    selected = which.min(x2$E_Prob) %>% x2[.]
    
    ###Crossing, or all below
  } else if(nrow(x2)>0){
    diff = (x$E_Prob*100)-threshold
    ###If all are below
    if(!any(diff>0)){
      ###Assign best 
      x1 = x$Acres_Sample
      index = which(abs(x1-20)<.05)
      if(length(index)==0){
        x2 = lapply(unique(x$Strata),function(X){
          
          x1 = x[Strata==X,]$Acres_Sample
          # index = which(abs(x1-20)<.05)
          y = x1-20
          y[y>0] = -1000
          index = which.max(y)
          x[Strata==X,][index,]
          
        }) %>% rbindlist
        
      } else{
        x2 = x[index,]
      }
      
      selected = which.min(x2$E_Prob) %>% x2[.]
      
      ###If all flat
      if(!any(x2$E_Prob*100 > .01)){
        selected = x2[Strata=='om:0']
        
      }
      
    }
    #Standard - any cross threshold
    selected = which.max(x2$Acres_Sample) %>% x2[.]
  }
  
}

complexity = function(x){
  x1 = x %>% tstrsplit(':|\\+') %>% length
  x2 = x %>% tstrsplit(':') %>% '[['(2) %>% as.numeric
  x1 = x1 + x2 
  if(str_detect(x,':0')){return(1)} else{return(x1)}
}

pull_result <- function(Region,Action='list',Strata=NULL,N=5,
                        local=NULL){
  #actions = list, C, simple_plot, data, samples
  if(!is.null(local)){
    tmp <- local
  } else{
    i = files_1[str_detect(files_1,Region) %>% which]
    tmp <-readRDS(i)
  }
  if(Action=='list'){
    return(data.frame(Strata = names(tmp$Run_Index),Index=1:length(tmp$Run_Index)))
  }
  if(Action=='C'){
    out = tmp$Strata_Data[[Strata]]$Partitions[Strata,]
    return(out)
  }
  if(Action=='data'){
    if(!is.null(local)){
      Main_Data = get('Main_Data',envir = parent.frame(max(sys.parents())))
      out = Main_Data[-tmp$Index_Buffer]
    } else{
      out = tmp$Data_in
    }
    out$Strata = tmp$Strata_Data[[Strata]]$Partitions[Strata,]
    return(out)
  }
  if(Action=='summary'){
    out = tmp$Summary
    tmp <-readRDS(i)
    index = names(tmp$Strata_Data)[Strata]
    out = out[Strata%in%index]
    return(out)
  }
}

###Pull samples from Results object

pull_samples = function(x,mode='single',local=local){
  Region= x$Region %>% as.character
  Strata_in=x$Strata
  N=x$n
  
  x1 = pull_result(Region,'list',1,local = local)
  index = x1[which(x1$Strata==Strata_in),2]  
  data_tmp = pull_result(Region,'data',index,local=local)
  
  if(mode=='loop_start'){
    data_tmp <<- data_tmp
  } 
  
  ### Get Strata Allocation 
  proportion = Allocate_Strata(data_tmp,x$Allocation)
  
  Strata_Table = sample(proportion$Strata,size = N,prob = proportion$Proportion,replace=T) %>% table
  
  ### Sample each strata, returns voxel ids
  index = lapply(seq_along(Strata_Table),function(y){
    y1 = Strata_Table[y] %>% names %>% as.numeric
    n = Strata_Table[y][[1]]
    return(data_tmp[Strata==y1,sample(voxel_id,size=n,replace=F)]) 
  }) %>% unlist
  
  return(data_tmp[voxel_id%in%index,])
}




# Command Line Code ----

input_args = commandArgs(trailingOnly=TRUE)

if(length(input_args)>=3){
  input_path=input_args[[1]]
  training_path = input_args[[2]]
  output_path = input_args[[3]]
  
  print('Start')
  Main_Wrapper(input_path,training_path,output_path)
}


