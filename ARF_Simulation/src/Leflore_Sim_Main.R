source('src/General_Setup_Edit.R')
source('../Soil_Stratification_Sampling/src/Functions/Strata_Sampling_Main.R')
source('../Soil_Stratification_Sampling/Main_Wrapper.R')



# Region = 'field_8248'
# Region = 'Full'

# Dataset Viewing ----

data = fread('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/Full.csv')
data = Rescale_om(data)
Get_Weighted_Avg_Depths(data)

data = data %>% filter(cl==1)

cols = str_detect(colnames(data),paste(c('sg_soc','polaris_om','gssurgo_om_r','om'),collapse='|')) %>%
  colnames(data)[.]
cols = cols %>% str_detect('60|wdrvi|gssurgo_om_r_30') %>% '!'(.) %>% '['(cols,.)
plot = data %>% melt(measure.vars=cols)
plot$Origin = c('Sample',rep('SG',3),rep('Polaris',3),rep('Gssurgo',3))[plot$variable]

# Simple Case
plot = data %>% melt(measure.vars=c('sg_soc','polaris_om','gssurgo_om_r','om'))


ggplot(plot,aes(x=value,fill=variable))+
  geom_histogram(bins=200,alpha=.6,position = 'identity') +# scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  # geom_density(alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + #coord_cartesian(ylim = c(0,10000)) +
  facet_wrap('Origin')

  
  

# Run Simulations ----
library(tictoc)

Regions_Leflore = list.files('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/') %>% str_remove('.csv') %>% 
  rev
Regions_ARF = list.files('ARF_Simulation/data/input/ARF_Sets/Input_Sets/') %>% str_remove('.csv') %>% 
  rev

Iters = 1
# which(Regions%in%Region)
# Regions = Regions[Regions %>% str_detect('field',negate=T)]
Region_Set = 'Leflore'

for(Region_Set in c('Leflore','ARF')){
  Regions = switch(Region_Set,
         'Leflore'=Regions_Leflore,
         'ARF'=Regions_ARF)
  
  for(I in Iters){
    dir.create(paste0('ARF_Simulation/data/output/Sim_',I))
    
    # for(Region in Regions[1:length(Regions)]){
    for(Region in Regions){
      
      # error = try({
      # run_grid = data.table(OM_Source=c('Soil_Samples',rep('Public_Predictions',3)),
      #                       OM_Var=c(F,'sg_soc','polaris_om','gssurgo_om_r'))
      run_grid = data.table(OM_Source=rep('Soil_Samples',3),
                            OM_Var=rep(F,3),Allocation=c('PPS','Neyman','Wright'))
      # run_grid = data.table(OM_Source=c('Soil_Samples'),
      #                       OM_Var=c(F))
      
      for(i in 1:nrow(run_grid)){
        run = run_grid[i,]
        
        input_path = list.files(paste0('ARF_Simulation/data/input/',Region_Set,'_Sets/Input_Sets'),pattern=Region,full.names = T)
        training_path = list.files(paste0('ARF_Simulation/data/input/',Region_Set,'_Sets/Input_Sets'),pattern=Region,full.names = T)
        
        OM_Var = ifelse(run$OM_Var=='FALSE',F,run$OM_Var)
        Allocation = run$Allocation
        
        print(Region)
        First_Iter = T
        run_data = NULL
        # L = lapply(1:Max_Iter,function(ii){
        for(ii in Iters){
          print(paste0('Iteration :',ii))
          output_path = paste0('ARF_Simulation/data/output/',Region,'.csv.gz')
          
          
          tic('Run_Time')
          
          if(First_Iter==(T|F)){
            tmp = Main_Wrapper(input_path,training_path,output_locations_path = output_path,
                                   
                                   field_boundary_path = paste0('ARF_Simulation/data/input/',Region_Set,'_Inputs/field_boundaries.gpkg'),
                                   output_strata_path =  'ARF_Simulation/data/output/strata_tmp.gz',
                                   control_fields = F,
                                   Region=Region,Force_OM_Source=run$OM_Source,Force_OM_Public=OM_Var,
                                   Force_iRF=F,Output_Summary=T,Cores_Global=8,
                                   Allow_Default=F,aggregate_at='strata',Max_Splits=4,
                                   Iterations = 200,Iter_Run = 15,Max_Features=3,
                                   max_iRF_samples=1000,allocation= Allocation,
                                   Testing_Run=T,Min_Acres_Sample = 1,export_resamples=T)
            
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
                                   Force_iRF=Selected_Features,Output_Summary=T,Cores_Global=8,
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
          
          ### For other targets
          tmp$Summary
          y = str_split(Region,'__')[[1]]
          tmp$Target = y[[length(y)]]
          
          saveRDS(tmp,paste0('ARF_Simulation/data/output/Sim_',I,'/',Region,':RN_',i,'__',
                             Region_Set,'__',':Iter_',ii))
        }#)
        
      }
      
    }#,Env)
  }
}
# Analysis ----

### Collect Results ----
data = fread('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/Full__om.csv')

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
Dir = 'Sim_1'

files = list.files(paste0('ARF_Simulation/data/output/',Dir,'/'),full.names=T)
L = files %>% lapply(readRDS)
names(L) =
  files %>% strsplit('//') %>% sapply(.,'[[',2) %>% 
  strsplit(':') %>% sapply(.,'[[',1) 

### Sampling testing 5/19/22 ----

tmap_options(max.raster = c(plot = 1e8, view = 1e8))

get_strata = function(x,strata,mode=NULL){
  index = which(names(x$Run_Index)==strata)
  x1 = x$Strata_Data[[index]]$Partitions[index,]
  x$Run_Data[,(strata) := x1]
  
  if(!is.null(mode)){
    
    t1 = quick_map(x$Run_Data,basemap = T)
    plot = x$Run_Data %>% get_SF
    tmap_mode('view')
    t1 + SF_Raster(plot,strata,simple = T,Res = sqrt(Acre_M*.26))[[1]] %>% 
      tm_shape(name = 'Strata') + tm_raster(alpha = .4,palette = 'Dark2')
    
  } else{
    quick_map(x$Run_Data,strata,basemap = T)
  }
}

x = L$Sub_3_3__TOC

get_strata(x,'om:3')
get_strata(x,'om:2')
get_strata(x,'om:0')

get_strata(x,'om:3','hek')

regime = x$Selected
regime$n_samples = 35
x$Run_Data$Strata = x$Run_Data$`om:3`
Allocate_Strata(x$Run_Data,regime$Allocation)

y = pull_samples(regime,
             data_tmp=x$Run_Data)
y[,mean(om),'om:3']
y[,.SD[,.N,'om:3'][,N/.N]]
y[,.SD[,.N,'om:3'][,.(Strata=.SD[[1]],
                      Proportion = N/regime$n_samples )]]

z = x$Run_Data
optimum_allocation(data = z, strata = "Strata", 
                   y = "om",
                   nsample = regime$n, method = "Neyman")

x1 = optimStrat::optiallo(regime$n,z$om,stratum=z$Strata) %>% as.data.table %>% distinct
x1[order(stratum)]


## 3/23/22 plotting----
span=.3

Sim_Summary = L$Full$Summary

plot = lapply(1:length(L),\(j){
  span=1
  x = L[[j]]
  gg <- ggplot(x$Summary[E_Prob<100&Acres_Sample<1000&str_detect(Strata,'om')],#&
               # gg <- ggplot(x$Summary[Acres_Sample<100&str_detect(Strata,'ooga|u|2|3',negate=T0)#&
               #                        str_detect(Strata,'AGT',negate=F)],
               
               aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1)
    geom_point(alpha=.4,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
  gg
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

plot = data %>% melt(measure.vars=c('sg_soc','polaris_om','gssurgo_om_r'))

# plot[,value:=value %>% scale(scale = T,center=T),'variable']
# plot$om = scale(plot$om)

Ras %>% scale(scale = T,center = T) %>% hist

# ggplot(plot,aes(x=value,fill=cl))+
#   geom_histogram(bins=50,alpha=.8,position = 'identity')+ facet_wrap(~variable,scales = 'free') +
#   theme_classic(base_size = 22) + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
#   labs(x='%om')

ggplot(plot,aes(x=value,fill=variable))+
  geom_histogram(bins=150,alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + coord_cartesian(ylim = c(0,2000)) 


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


### Metrics Summary ----
library(Metrics)
cols = c('polaris_om','sg_soc','gssurgo_om_r','om')
data = data %>% filter(cl==1)

data1 = copy(data)
data1[,(cols):=lapply(.SD,scale),.SDcols=cols]
data1 = data1[!is.na(data$gssurgo_om_r)]

x = data1[,.(Variable=cols,'R^2'=lapply(.SD,rsq,actual=om),MSE=lapply(.SD,mse,actual=om),MdAE=lapply(.SD,mdae,actual=om),
             SD=lapply(.SD,sd),Mean=lapply(.SD,mean)),
          .SDcols=cols,'cl'] 
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

