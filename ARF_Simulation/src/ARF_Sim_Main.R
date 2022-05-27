# Main Simulation ---- 
source('src/General_Setup_Edit.R')

source('../Soil_Stratification_Sampling/src/Functions/Strata_Sampling_Main.R')
source('../Soil_Stratification_Sampling/Main_Wrapper.R')

Regions = list.files('ARF_Simulation/data/input/Input_Sets') %>% str_remove('.csv')
# Region = 'field_8248'
Region = 'Full'

data = fread('ARF_Simulation/data/input/Input_Sets/Full.csv')
strata = fread('ARF_Simulation/data/output/strata.gz')
x = merge(data,strata[,.(voxel_id,strata_id)],by='voxel_id')
quick_map(data,'cluster_label',basemap=T)
quick_map(x,'strata_id',basemap=T,mode = 'view')

### Check AGT merging
L = mclapply(unique(data$field_id),mc.cores=15,\(field_sub){
  Main_Data = data %>% filter(field_id==field_sub)
  Main_Data$cluster_label = as.character(Main_Data$cluster_label)
  print(table(Main_Data$cluster_label))
  print(Region)
  n_voxels = 5/.25
  tbl = table(Main_Data$cluster_label)
  tbl = tbl[order(tbl)]
  
  small_AGTs = which(tbl < n_voxels) %>% names(tbl)[.]
  
  # Check for small AGTs, merge if present 
  if(length(small_AGTs)>0){
    
    merge_small_AGTs(small_AGTs,Main_Data,n_voxels)
    
    print(table(Main_Data$cluster_label))
  }
  return(Main_Data)
  
})

x = L %>% rbindlist
quick_map(x,'cluster_label',basemap=T)

### Single Run 
# Main_Wrapper(input_path = '../Soil_Stratification_Sampling/Scratch/temp_dir/temp_dir/inference_data.csv.gz',
#               training_path='../Soil_Stratification_Sampling/Scratch/temp_dir/temp_dir/training_data.csv.gz',
#              field_boundary_path = '../Soil_Stratification_Sampling/Scratch/temp_dir/temp_dir/field_boundaries.gpkg',
#              output_locations_path = 'data/output/Samples.gz','data/output/control_clusters.gz',output_strata_path = 'ARF_Simulation/data/output/strata.gz',
#            max_iRF_samples=100,Force_iRF =c('polaris_ph','polaris_bd'),
#            control_fields=F,Force_OM_Public='polaris_om',
#            Force_OM_Source='Public_Predictions',
#              Allow_Default=F)


Main_Wrapper(input_path = 'ARF_Simulation/data/input/Input_Sets/Full.csv',
             training_path='ARF_Simulation/data/input/Training_Sets/Full.csv',
             field_boundary_path = 'ARF_Simulation/data/input/130_boundaries.gpkg',
             output_locations_path = 'ARF_Simulation/data/output/Samples_test.gz',
             output_control_cluster_path = 'ARF_Simulation/data/output/control_clusters.gz',
             output_strata_path = 'ARF_Simulation/data/output/strata.gz',
             max_iRF_samples=100,Force_iRF =c('cec','ca'),
             control_fields=F,#Force_OM_Public='polaris_om',
             # Force_OM_Source='Public_Predictions',
             Allow_Default=F)
###
library(tictoc)

Max_Iter = 1
which(Regions%in%Region)
# Regions = Regions[Regions %>% str_detect('field',negate=T)]
for(I in 36:100){
  dir.create(paste0('ARF_Simulation/data/output/Sim_',I))
  
  for(Region in Regions[1:length(Regions)]){
    
    # error = try({
    # run_grid = data.table(OM_Source=c('Soil_Samples',rep('Public_Predictions',3)),
    #                       OM_Var=c(F,'sg_soc','polaris_om','gssurgo_om_r'))
    run_grid = data.table(OM_Source=c('Soil_Samples'),
                          OM_Var=c(F))
    for(i in 1:nrow(run_grid)){
      run = run_grid[i,]
      
      input_path = list.files('ARF_Simulation/data/input/Input_Sets',pattern=Region,full.names = T)
      training_path = list.files('ARF_Simulation/data/input/Input_Sets',pattern=Region,full.names = T)
      
      OM_Var = ifelse(run$OM_Var=='FALSE',F,run$OM_Var)
      
      print(Region)
      First_Iter = T
      run_data = NULL
      # L = lapply(1:Max_Iter,function(ii){
      for(ii in 1:Max_Iter){
        print(paste0('Iteration :',ii))
        output_path = paste0('ARF_Simulation/data/output/',Region,'.csv.gz')
        
        
        tic('Run_Time')
        
        if(First_Iter==(T|F)){
          tmp = try(Main_Wrapper(input_path,training_path,output_locations_path = output_path,

                             field_boundary_path = 'ARF_Simulation/data/input/130_boundaries.gpkg',
                             output_strata_path =  'ARF_Simulation/data/output/strata_tmp.gz',
                             control_fields = F,
                             Region=Region,Force_OM_Source=run$OM_Source,Force_OM_Public=OM_Var,
                             Force_iRF=F,Output_Summary=T,Cores_Global=8,
                             Allow_Default=F,aggregate_at='strata',Max_Splits=7,
                             Iterations = 200,Iter_Run = 15,Max_Features=3,
                             max_iRF_samples=1000,
                             Testing_Run=T,Min_Acres_Sample = 1,export_resamples=T))
          
          if(class(tmp)=='try-error'){
            break
          }
          First_Iter <<- F
          Selected_Features = tmp$Selected_Features
          Selected_Features <<- tmp$Selected_Features
          run_data <<- tmp$Run_Data
          run_data <- tmp$Run_Data
        } else{
          tmp = try(Main_Wrapper(input_path,training_path,output_locations_path = output_path,
                                 Region=Region,Force_OM_Source=run$OM_Source,Force_OM_Public=OM_Var,
                                 Force_iRF=Selected_Features,Output_Summary=T,Cores_Global=14,
                                 control_fields=F,Allow_Default=F,aggregate_at='strata'))
          
          if(class(tmp)=='try-error'){
            break
          }
          
          tmp$Run_Data = NULL
        }
        
        toc(log = T)
        
        time =  unlist(lapply(tic.log(format = F), function(x) x$toc - x$tic))
        tic.clearlog()
        
        Max_Runs = tmp$Run_Index %>% sapply(max) %>% max
        iRF_Skipped = ifelse(tmp$Config$Force_iRF==F,F,T)[[1]]
        Run_Stats = data.table(Region=Region,OM_Source=tmp$Selected_OM,
                               Run_Iteration=ii,Acres=tmp$Summary$Total_Acres[[1]],
                               N_Voxels=run_data[,.N],Time_Elapsed=time,iRF_Skipped=iRF_Skipped,
                               Data_Size = object_size(run_data),
                               N_Iter=tmp$Config$Iter_Run*tmp$Config$Iterations,
                               Iter_Run=tmp$Config$Iter_Run,Iterations=tmp$Config$Iterations,
                               batch_limit=tmp$Config$batch_limit,
                               Max_Runs=Max_Runs)
        
        tmp$Run_Stats = Run_Stats
        saveRDS(tmp,paste0('ARF_Simulation/data/output/Sim_',I,'/',Region,':RN_',i,':Iter_',ii))
      }#)
      
    }
    
  }#,Env)
}

# Analysis ----

### Collect Results ----
data = fread('ARF_Simulation/data/input/Input_Sets/Full.csv')

Rescale_om(data)
Get_Weighted_Avg_Depths(data)

get_samples = function(y,files){
  y1 = str_detect(files,y$Region) %>% which %>% files[.] 
  run_data = str_detect(y1,'Iter_1\\b') %>% which %>% y1[.] %>% .[1] %>%   
    readRDS %>% '[['('Run_Data')
  run_data
  return(run_data)
}


Dirs = list.files('ARF_Simulation/data/output/',full.names=F,pattern='Sim_')
Dir = 'Sim_33'

files = list.files(paste0('ARF_Simulation/data/output/',Dir,'/'),full.names=T)
L = files %>% lapply(readRDS)
names(L) =
  files %>% strsplit('//') %>% sapply(.,'[[',2) %>% 
  strsplit(':') %>% sapply(.,'[[',1) 


## 3/23/22 plotting----
span=.3

Sim_Summary = L$Full$Summary

plot = lapply(1:length(L),\(j){
  span=1
  x = L[[j]]
  gg <- ggplot(x$Summary[E_Prob<.03&Acres_Sample<100&str_detect(Strata,'ooga|u|2|3',negate=T)],#&
               # gg <- ggplot(x$Summary[Acres_Sample<100&str_detect(Strata,'ooga|u|2|3',negate=T0)#&
               #                        str_detect(Strata,'AGT',negate=F)],
               
               aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1)
    geom_point(alpha=0,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
  # gg = gg + facet_wrap(~Region) + geom_vline(data=dataLine,aes(xintercept=int),alpha=.4)
  gg = gg + theme_classic(base_size = 18) + geom_abline(intercept =3,slope=0,alpha=.5)#+ ylim(0,NA) #+ geom_hline(yintercept=1,alpha=.4) +
  gg
  x$Summary_Plot = gg
  x$Summary$Region = names(L)[j]
  
  x$Acres = x$Summary$Total_Acres[[1]]
  
  L[[j]] <<- x
  return(x$Summary)
  
}) %>% rbindlist

lapply(L,'[[','Summary_Plot')

gg <- ggplot(Sim_Summary[Acres_Sample<100&str_detect(Strata,'ooga|2|3',negate=T)],
             aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1)
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
# gg = gg + facet_wrap(~Region) + geom_vline(data=dataLine,aes(xintercept=int),alpha=.4)
gg + theme_classic(base_size = 18) + geom_abline(intercept =3,slope=0,alpha=.5)#+ ylim(0,NA) #+ geom_hline(yintercept=1,alpha=.4) +


### split up the vars
# Sim_Summary = L$field_8257$Summary
Sim_Summary = L$Full$Summary

Sim_Summary$n_strata = as.character(Sim_Summary$n_strata)
gg <- ggplot(Sim_Summary[Acres_Sample<100&str_detect(Strata,'ooga',negate=T)],
             aes(x=Acres_Sample,y=E_Prob*100,col=n_strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1)
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
gg = gg + facet_wrap(~Covar) 
gg + theme_classic(base_size = 18) + geom_abline(intercept =3,slope=0,alpha=.5)#+ ylim(0,NA) #+ geom_hline(yintercept=1,alpha=.4) +

### Sumamrizing
x=L %>% lapply('[[','Selected') %>% rbindlist(use.names = T)
setkey(Sim_Summary,'E_Prob')
Sim_Summary[str_detect(Strata,'cec'),rank:=seq(.N)]
Sim_Summary[Covar=='cec',rank:=seq(.N)]

Sim_Summary[str_detect(Strata,'cec'),sum(rank),c('Covar','n_strata')][
  order(V1)][Covar=='cec']

### Tons of carbon shit
# 1.5 %OM * 1.4 cg BD * 30 cm * 2.4710 (hecates to acres) = 63  tonnes / ha
data[,Tons_C := om * polaris_bd/100 * 30 / 2.4710 ]

hist(data$Tons_C)
data[,(Tons_C*.05)/sd(Tons_C)] %>% sd
data[,(om*.05)/sd(om)] %>% sd

### Collect Sim_Dirs ----

Dirs = list.files('ARF_Simulation/data/output/',full.names=F,pattern='Sim_')

Dirs = 'Sim_33'
Dir = 'Sim_33'
files = list.files(paste0('ARF_Simulation/data/output/',Dir,'/'),full.names=T)


Summary = parallel::mclapply(Dirs,mc.cores=1,function(Dir){
  print(Dir)
  files = list.files(paste0('ARF_Simulation/data/output/',Dir,'/'),full.names=T)
  L = files %>% lapply(readRDS)
  # list.files(paste0('ARF_Simulation/data/output/',Dir,'/'),full.names=T)
  ### Run Stats
  # L[[1]]$Run_Stats
  x = lapply(L,'[[','Run_Stats') %>% rbindlist
  x[,Run_Iteration:=paste0(Dir,'_',Run_Iteration)]
  # x[,1:3]

  ### Get Results Stats

  Mean_SD = lapply(L,function(y){
    
    run_data = y$Run_Data

    samples = rbindlist(y$Output_Samples_Resamples,idcol = 'Iter')
    Avgs = samples[,.(True_Mean=mean(run_data$om),True_SD=sd(run_data$om),
    
                      Sim_Mean=mean(om),Sim_SD=sd(om),N_Samples=.N),'Iter']
    return(list(Avgs,list(samples=samples)))
  })

  Summary = lapply(seq_along(Mean_SD),\(y) {
    cbind( x[y,], '[['(Mean_SD[[y]],1) ) 
    }) %>% rbindlist 
  
  Sample_L = lapply(Mean_SD,\(x) x[[2]]$samples)
  
  names(Sample_L) = paste(x$Region,x$OM_Source,x$Run_Iteration,sep = '__')
  return(list(Summary,Sample_L))
})

Results = lapply(Summary,'[[',1) %>% rbindlist
Samples_L = lapply(Summary,'[[',2) %>% do.call(c,.)

names(Samples_L) = names(Samples_L) %>% str_split('__') %>% sapply('[[',1)

### Filter out original polaris / SG runs
# x = Results %>% filter(
#   lapply(Run_Iteration,\(x) strsplit(x,'_')[[1]][2])%in%1:10 & 
#     !OM_Source%in%c('polaris_om','sg_soc'))

# Results = rbind(x,Results %>% filter(
#   !lapply(Run_Iteration,\(x) strsplit(x,'_')[[1]][2])%in%1:10))

### subset for just om and field scale 
Results = Results %>% filter(Region %>% str_detect('field')) %>% 
  filter(OM_Source=='om')

### 

Results[,.((abs(True_Mean-Sim_Mean) < True_Mean*.05)/.N),c('Region','OM_Source')]

Results[,abs(True_Mean-Sim_Mean) < True_Mean*.05] %>% '!'(.) %>% sum
Results[,.((abs(True_Mean-Sim_Mean) < True_Mean*.05)/.N),c('Region','OM_Source')][,hist(V1)]

# General plotting / Scratch ----

### Check indibidual 
# x1 = fread('ARF_Simulation/data/output/field_8248.csv.gz')
# data[lng%in%x1$lng&lat%in%x1$lat,mean(om)]
# data[,mean(om)]
# data[lng%in%x1$lng&lat%in%x1$lat,mean(om)] + data[,mean(om)*.05]
# abs(data[,mean(om)] - data[lng%in%x1$lng&lat%in%x1$lat,mean(om)]) < data[,mean(om)*.05]




# Results plotting, actual simulation ----
Samples_L$Full$om %>% hist
Results[Region=='Full',hist(Sim_Mean,30)]
abline(v = Results[Region=='Full',True_Mean[1]])

plot = copy(Results)
# plot = plot[,.(Avg_Mean=mean(Sim_Mean),SD_Mean=sd(Sim_Mean),
#         Avg_NS=mean(N_Samples),SD_NS=sd(N_Samples),N=.N),'Region']
# plot[order(Avg_Mean),Index:=1:nrow(plot)]

plot[,':='(Avg_Mean=mean(Sim_Mean),SD_Mean=sd(Sim_Mean),
           Avg_NS=mean(N_Samples),SD_NS=sd(N_Samples),N=.N,
           Iter=seq(.N)),'Region']

plot[order(Avg_Mean),Index:=seq(.N),'Iter']
get_confint = \(x,n,level=.95){
  x1 = lm(Sim_Mean ~ 1, x)
  x2 = confint(x1,level = level) %>% as.numeric()
  return(x2[[n]])
}

plot = plot
plot[,c('CI_L', "CI_H"):=list(get_confint(.SD,1),get_confint(.SD,2)),'Index']
plot[,Within_5:=abs(True_Mean-Sim_Mean) < True_Mean*.05]
plot[,Within_5_Percent:=round(sum(Within_5)/.N,2),
     'Index']

labels = plot[,.(Region,Index)][order(Index),unique(Region)]
ggplot(plot[Iter==1,], aes(x=Index, y=Avg_Mean)) + #col=Control_Clusters,
  # group=Control_Clusters)) + 
  geom_violin(data=plot,aes(x=Index %>% as.factor,y=Sim_Mean)) +
  geom_errorbar(aes(ymin=Avg_Mean-SD_Mean, ymax=Avg_Mean+SD_Mean)) +#,linetype=Status),width=.6) +
  geom_line(alpha=.3) + geom_text(label=plot[Iter==1]$Within_5_Percent,size=3,nudge_y = .25,nudge_x = 0) +
  theme_classic(base_size = 22) + #+ geom_vline(aes(xintercept = line),alpha=.5) + 
  labs(y='%OM') + 
  geom_point(data=plot,aes(x=Index,y=Sim_Mean,col=Within_5),size=1) + 
  geom_point(data=plot,aes(x=Index,y=True_Mean)) + 
  scale_x_discrete(breaks = 1:47,

                   labels = labels) +
  theme(axis.text.x = element_text(angle = 90, vjust = +.5, hjust=0))

#+
# geom_smooth(data=plot,aes(x=Index,y=Sim_Mean),se=F) +
# geom_ribbon(data=plot,aes(x=Index,ymin =CI_L,ymax =CI_H) , alpha = 0.1) +
# scale_x_discrete(drop = FALSE) 

###
plot[,.(Mean=mean(N_Samples),SD=sd(N_Samples)),'Index']

ggplot(plot[Iter==1,], aes(x=Index, y=N_Samples)) +
  geom_violin(data=plot,aes(x=Index %>% as.factor,y=N_Samples)) +
  theme_classic(base_size = 22) 


### Investigate those that fall beyond 
plot[,Within_5_Percent %>% mean,'Index'][,hist(V1,20)]
plot[,Within_5_Percent %>% mean,'Index'][,sum(V1<.93)]
Fails = plot[,Within_5_Percent %>% mean,'Index'][,.SD[which(V1<.93)]][[1]]

F_L = mclapply(Fails,mc.cores=10,\(x){
  sel1 = x
  plot[Index==sel1]
  t = quick_map(data[field_id==plot[Index==sel1][,Region][1]],basemap=T,basemap_buffer = 100,
                Lhist=T,style='quantile',n = 6,SB = F) + tm_layout(title=sel1)
  tmap_save(t,paste0('ARF_Simulation/scratch/',x,'.png'))
  t
})
Fails_Region = plot[Iter==1&Index%in%Fails,Region]


### Single selection
sel1 = 35
plot[Index==sel1]
quick_map(data[field_id==plot[Index==sel1][,Region][1]],)
sel = plot[Index==sel1][,Region][1]

Results[Region==sel,hist(Sim_Mean)]
Results[Region==sel,Acres/N_Samples]
### General other, plotting ----


data = fread('ARF_Simulation/data/input/Input_Sets/Full.csv')

Rescale_om(data)
Get_Weighted_Avg_Depths(data)



x = st_read('ARF_Simulation/data/input/locus_samples_20220221.gpkg')

quick_map(data,'polaris_om',mode='plot',style = 'bclust',Lhist = T,n=6,
          legend_size = .4,basemap = F,SB_Width = .3,SB_Pos = 'left') #+


data$cl = data$cl %>% as.factor()


# plot[,value:=value %>% scale(scale = T,center=T),'variable']
# plot$om = scale(plot$om)

Ras %>% scale(scale = T,center = T) %>% hist

# ggplot(plot,aes(x=value,fill=cl))+
#   geom_histogram(bins=50,alpha=.8,position = 'identity')+ facet_wrap(~variable,scales = 'free') +
#   theme_classic(base_size = 22) + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
#   labs(x='%om')

data = fread('ARF_Simulation/data/input/Input_Sets/Full.csv')

Rescale_om(data)
Get_Weighted_Avg_Depths(data)

data$cl = data$cl %>% as.factor()


plot = data %>% melt(measure.vars=c('sg_soc','polaris_om','gssurgo_om_r'))

dir.create('ARF_Simulation/scratch/Public_Vs_Measured_Plots')
gg1 = ggplot(plot,aes(x=value,fill=variable))+
  geom_histogram(bins=150,alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + coord_cartesian(ylim = c(0,2000)) 
gg1
ggsave(gg1,filename = 'ARF_Simulation/scratch/Public_Vs_Measured_Plots/ARF_histograms.png')
# density version 
ggplot(plot,aes(x=value,fill=variable))+
  geom_density(bins=150,alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + coord_cartesian(ylim = c(0,20)) 

### Actual versus predicted 


ggplot(plot,aes(x=value,y=om,color=variable))+
  geom_point(alpha=.7,size=1) + geom_smooth(method="loess",se=F,span=1,aes(group=variable),col='black', linetype="dashed") +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='Public Estimated %OM',y='Sample %OM') + theme_classic(base_size = 22) 

### Residuals plot
ggplot(plot,aes(x=value,y=om-value,color=variable))+
  geom_point(alpha=.7,size=1) + geom_smooth(method="loess",se=F,span=1,aes(group=variable),col='black', linetype="dashed") +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='Public Estimated %OM',y='Residuals') + theme_classic(base_size = 22) 

### Residuals violin 
ggplot(plot,aes(x=variable,y=om-value,fill=variable))+
  geom_violin(alpha=1) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='Public Estimated %OM',y='Residuals') + theme_classic(base_size = 22) +
  geom_boxplot(width=.1,outlier.size=.5,outlier.alpha = 0,alpha=0) + 
  stat_summary(fun=mean, geom="point", shape=20,size=5) 

### Summary Stats
library(Metrics)
cols = c('polaris_om','sg_soc','gssurgo_om_r','om')

data[,.(Variable=cols,'R^2'=lapply(.SD,rsq,actual=om),MSE=lapply(.SD,mse,actual=om),MdAE=lapply(.SD,mdae,actual=om),
        SD=lapply(.SD,sd),Mean=lapply(.SD,mean)),
     .SDcols=cols] 


### Summary over
library(Metrics)
cols = c('polaris_om','sg_soc','gssurgo_om_r','om')
data1 = copy(data)
data1[,(cols):=lapply(.SD,scale),.SDcols=cols]

x = data1[,.(Variable=cols,'R^2'=lapply(.SD,rsq,actual=om),MSE=lapply(.SD,mse,actual=om),MdAE=lapply(.SD,mdae,actual=om),
             SD=lapply(.SD,sd),Mean=lapply(.SD,mean)),
          .SDcols=cols] 
x

x = data[,.(Variable=cols,cor=lapply(.SD,cor,om)),by='field_id',
         .SDcols=cols][Variable!='om']
x$cor = as.numeric(x$cor)
x[,hist(cor)]

x[,.(Avg_Cor=mean(cor)),'cl']

plot[,Cor:=cor(value,om),by=c('variable','field_id')]


### Spatial plotting 
quick_map(data,'sg_soc',mode='view',style='quantile')

data[,.(Variable=cols,'R^2'=lapply(.SD,rsq,actual=om),MSE=lapply(.SD,mse,actual=om),MdAE=lapply(.SD,mdae,actual=om),
        SD=lapply(.SD,sd),Mean=lapply(.SD,mean)),
     .SDcols=cols] 

quick_map(plot,Variable = 'Cor',mode='plot',style = 'quantile',Lhist = T,n=6,
          legend_size = .4,basemap = F,SB_Width = .3,SB_Pos = 'left') #+ #+

### Combined
Ras = c(SF_Raster(data %>% get_SF,'gssurgo_om_r',simple = T,Res = sqrt(Acre_M*.25))[[1]],
        SF_Raster(data %>% get_SF,'polaris_om',simple = T,Res = sqrt(Acre_M*.25))[[1]],
        SF_Raster(data %>% get_SF,'sg_soc',simple = T,Res = sqrt(Acre_M*.25))[[1]],
        SF_Raster(data %>% get_SF,'om',simple = T,Res = sqrt(Acre_M*.25))[[1]],
        SF_Raster(plot[variable=='gssurgo_om_r'] %>% get_SF,'Cor',simple = T,Res = sqrt(Acre_M*.25))[[1]],
        SF_Raster(plot[variable=='polaris_om'] %>% get_SF,'Cor',simple = T,Res = sqrt(Acre_M*.25))[[1]],
        SF_Raster(plot[variable=='sg_soc'] %>% get_SF,'Cor',simple = T,Res = sqrt(Acre_M*.25))[[1]]
)
names(Ras) = c('gssurgo_OM','Polaris_OM','SG_OM','Sample_OM',
               'Cor_gssurgo','Cor_Polaris','Cor_SG')
Cor_Ras = Ras[[c(5:7)]]
Ras=Ras[[c(1:4)]]

tmap_mode('plot')
x = tm_shape(basemap) + tm_rgb() +
  # tm_basemap("Esri.WorldImagery") + 
  tm_shape(Ras) + tm_raster(palette = 'viridis',alpha=.8,legend.hist = T)+#,style='quantile') 
  tm_layout(legend.outside=T,legend.outside.size=.35,legend.hist.height = .5,
            legend.hist.width = .4)
tmap_save(x,filename = '../../../Scratch/tmp/test1.png')
Ras  %>% hist
# data$cl = as.factor(data$cl)
# index = colnames(data) %>% str_detect('_soc|_om|\\bom') %>% which
# ggplot(data %>% melt(measure.vars=index),aes(x=value,fill=cl))+
#   geom_histogram(bins=50,alpha=.8,position = 'identity')+ facet_wrap(~variable,scales = 'free') +
#   theme_classic(base_size = 22) + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2')
# 


quick_map(data,'polaris_om')


#
# Control Plots ----
### Get management practices 

library(jsonlite)
library(stringdist)
library(dendextend)
library(ComplexHeatmap)
library(factoextra)

json = fromJSON("ARF_Simulation/data/input/mgmt_dict.json") %>% fromJSON 
str(json,list.len = 2,max.level = 5)
json %>% sapply(names)
json %>% sapply(Dim)

MGMT = lapply(json,function(x){
  sapply(x,unlist) %>% as.data.table
}) %>% rbindlist
# MGMT$Practices = names(json) %>% sapply(rep,6) %>% as.vector
x = apply(MGMT,2,paste,collapse='')

M = stringdistmatrix(x)
M %>% as.matrix() %>% Heatmap

row_dend = as.dendrogram(hclust(dist(M),method='ward.D2'))
row_dend = color_branches(row_dend, k = 3)
plot(row_dend)

fv = fviz_nbclust(M %>% as.matrix, hcut,k.max = 30,  method = "gap_stat", nboot = 100,
                  maxSE = list(method = "Tibs2001SEmax", SE.factor = 1))+
  labs(subtitle = "Gap statistic method",)

fv

gskmn <- clusGap(M %>% as.matrix, FUN = hcut, K.max = 30, B = 300)
plot(gskmn)
maxSE(f = gskmn$Tab[, "gap"], SE.f = gskmn$Tab[, "SE.sim"])


Heatmap(M %>% as.matrix,cluster_rows = row_dend, cluster_columns = row_dend)

MGMT_cl = data.table(field_id=names(x))
lapply(1:20,\(y){
  cl = hcut(M,y,hc_method = 'ward.D2')$cluster
  col = paste0('cl_',y)
  MGMT_cl[,(col):=cl]
  
})
# cl = hcut(M,3,hc_method = 'ward.D2')$cluster
# MGMT_cl = data.table(field_id=names(x),cl=cl)

### Soil texture ----

library(soiltexture)

data[,tot_text := gssurgo_claytotal_r+gssurgo_silttotal_r+gssurgo_sandtotal_r]
data$tot_text = data$tot_text %>% as.character


data = lapply(data$tot_text %>% unique,function(x){
  x1 = data[tot_text==x]
  x2 = TT.points.in.classes(x1,class.sys = 'USDA.TT',text.tol = as.numeric(x),
                            css.names = c("gssurgo_claytotal_r","gssurgo_silttotal_r","gssurgo_sandtotal_r"))
  
  index = apply(x2,1,function(x){
    which(x==1)
  }) 
  x1$Soil_Texture = colnames(x2)[index]
  x1
}) %>% rbindlist
index = colnames(data) %>% duplicated %>% '!'(.) %>% which 
data = data[,..index]

data = merge(data,data[,.(Avg_Soil_Texture=which.max(table(Soil_Texture)) %>% 
                            names(table(Soil_Texture))[.]),'field_id'],by='field_id')
data$Avg_Soil_Texture = as.factor(data$Avg_Soil_Texture)
quick_map(data,'Avg_Soil_Texture',basemap=T,SB_Pos = 'left')



### Slope Class ----

data$Slope_Class = get_breaks(data$topo_slope,style = 'fixed',breaks=c(0,4,9,17,31,45,100))
quick_map(data,'Slope_Class',basemap=T,SB_Pos = 'left')

data = merge(data,data[,.(Avg_Slope_Class=which.max(table(Slope_Class)) %>%
                            names(table(Slope_Class))[.]),'field_id'],by='field_id')
data$Avg_Soil_Texture = as.factor(data$Avg_Soil_Texture)
quick_map(data,'Slope_Class',basemap=T,SB_Pos = 'left')




## Merge Criteria / OM importance testing ----

MGMT_cl$field_id = as.numeric(MGMT_cl$field_id)
setnames(MGMT_cl,'cl','Practice_Cluster')

data = merge(data,MGMT_cl,by='field_id')

### Manual selection of cluster
data$Practice_Cluster = data$cl_4
quick_map(data,'Practice_Cluster',basemap=T,SB_Pos = 'left')

### Create Control Clusters
data[,Control_Clusters:=paste(Avg_Soil_Texture,Avg_Slope_Class,Practice_Cluster,sep='_')]
data[,length(unique(field_id)),c('Control_Clusters')]
quick_map(data,'Control_Clusters',basemap=T,SB_Pos = 'left')


### Quick Stats
data[,mean(om),c('Practice_Cluster','field_id')]

# ### Averaging

# Results_Avg = Results[,.(Lower_CI_True=mean(Lower_CI_True),Upper_CI_True=mean(Upper_CI_True),
#                          Lower_CI_Sim=mean(Lower_CI_Sim),Upper_CI_Sim=mean(Upper_CI_Sim),
#                          True_Mean=mean(True_Mean),True_SD=mean(True_SD),
#                          Sim_Mean=mean(Sim_Mean),Sim_SD=mean(Sim_SD)),
#                       by=c('Region','OM_Source')]
# 
# Results_Avg[,':='(True_CI_Len=Upper_CI_True-Lower_CI_True,Sim_CI_Len=Upper_CI_Sim-Lower_CI_Sim)]
# 
# Results_Avg[,Region:=str_remove_all(Region,'field_') %>% as.numeric]


### Testing
# Results[OM_Source=='om'&Region %>% str_detect('field'),
#         .(Lower_CI_True=mean(Lower_CI_True),Upper_CI_True=mean(Upper_CI_True),
#           Lower_CI_Sim=mean(Lower_CI_Sim),Upper_CI_Sim_SD=mean(Upper_CI_Sim),
#           True_Mean=mean(True_Mean),True_SD=mean(True_SD),
#           Sim_Mean=mean(Sim_Mean),Sim_SD=mean(Sim_SD)),
#         by=c('Region','OM_Source')]
# 
# Results[OM_Source=='om'&Region %>% str_detect('field'),
#         .(Sim_CI_Len=mean(Upper_CI_Sim-Lower_CI_Sim),Sim_CI_Len_SD=sd(Upper_CI_Sim-Lower_CI_Sim)),
#         by=c('Region','OM_Source')]


### Subset samples 
Results[,Unique_Iter:=seq(.N),c('Region','OM_Source')]
Results[,Region:=str_remove_all(Region,'field_') %>% as.numeric]

max_iter = Results[,Unique_Iter %>% max,c('Region','OM_Source')][[3]] %>% min
Iter = 1



sapply(Ctrl_Iters,\(x) x[[1]]) %>% unlist %>% table %>% sort(T)
tmp = lapply(seq(length(Ctrl_Iters)),\(x) {
  x1 = Ctrl_Iters[[x]][[2]]
  Region = Ctrl_Iters[[x]][[1]]
  x1 = cbind(Region,x1)
  x1$Iter=x
  x1
}) %>% rbindlist

tmp

