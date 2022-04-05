source('src/General_Setup_Edit.R')
#
# Create Testing Data -----------------------------------------------------

# path = 'ARF_Simulation/data/input/arva_om_data_voxels_20210803.parquet'
path1 = 'ARF_Simulation/data/input/130_field_cropforce_pull/130_voxel_data.csv'
path2 = 'ARF_Simulation/data/input/130_field_data_cropforce_me/130_voxel_data.csv'
path3 = 'ARF_Simulation/data/input/130_field_data_cropforce_me_2/130_voxel_data.csv'

paths = c('ARF_Simulation/data/input/2021_full.gz',
          'ARF_Simulation/data/input/soil_sample_gdf_generic.pkl',
          'ARF_Simulation/data/input/soil_samples_raw.pkl')  

source('../Primary/src/Helper_Functions/AWS_Helpers.R')
L = lapply(paths,get_joblib)
samples = L[[3]] %>% st_as_sf(wkt='WKT',crs=4326)
Env = L[[1]]
Interp = L[[2]]
field_data = st_read('ARF_Simulation/data/input/130_boundaries.gpkg')

colnames(Interp)[!colnames(Interp)%in%colnames(Env)]

data = Interp
rm(Interp)

quick_map(data[year!=2021],Variable='ss_om',mode='plot',samples=F) #+ tm_shape(samples) +tm_dots('om')
quick_map(data[year==2018],Variable='ss_om',mode='plot',style = 'bclust',Lhist = T,n=6,
          legend_size = .4,basemap = F,SB_Width = .3,SB_Pos = 'left') +
  tm_layout(
    main.title = "2018", 
    main.title.position = "center")


data1 = get_SF(data)

### fill holes ----
x = data1 %>% filter(year==2018)
y = data1 %>% filter(year==2021)

index = st_nearest_feature(y,x)

### replace
setDF(x)
setDF(y)

x[index,-ncol(x)] = y[,-ncol(y)]
x = st_as_sf(x)
rm(y)

### Interpolate missing
colnames(x) = x %>% colnames %>% str_replace_all('-','_')
cols=colnames(x)[str_detect(colnames(x),'ss_|gssurgo|sg_|polaris_')] 
cols = c(colnames(x)[4:23],cols)

x[,cols] = parallel::mclapply(cols,mc.cores = 10,function(col){
  z = try(idw(formula=as.formula(paste(col, "~ 1")), locations=x[-missing,], newdata=x[missing,],idp=2))
  if(class(z)=='try-error'){
    message(paste0('Error at ',col))
  } else{
    x[missing,col]=z$var1.pred
  }
  return(x[,col] %>% st_drop_geometry)
})

x = st_transform(x,4326)
x1 = x  %>% cbind(st_coordinates(x)) %>% st_drop_geometry %>% as.data.table
setnames(x1,c('X','Y'),c('lng','lat'))

data = x1
rm(data1,x,x1)
setnames(data,'ss_om','om')

### Subseting ----
### Add field data

data$voxel_id = seq(1,nrow(data))+130

data = lapply(seq_along(field_data$field),function(x){
  tmp = field_data[x,]
  x1 = jsonlite::fromJSON(tmp$field)
  index = data %>% get_SF(to_UTM = F) %>% st_intersects(tmp$geom) %>% sapply(sum) %>% '!='(0) %>% which
  meta = data.table(field_id=x1$id,acres=x1$acreage,field_name=x1$name,farm_name=x1$farm$name)
  if(nrow(data[index])>0){
    return(data[index,] %>% cbind(meta,.))
  }
  
}) %>% rbindlist

field_boundaries = field_data$geom

### Field subsetting 

farms = c('JAM','MP','GNF')
data[,is_ARVA:=farm_name%in%farms]
data %>% quick_map(Variable='is_ARVA',basemap = T) + tm_shape(samples)+tm_dots()

### Mapping pieces
quick_map(data,basemap = T,mode='plot',samples=F) +
  field_boundaries %>% tm_shape() + tm_polygons(alpha=.3) + 
  tm_shape(samples)+tm_dots() 


### Subset fields that have soil samples
x = st_contains(field_boundaries,samples) %>% sapply(length) %>% 
  '>'(0) %>% field_boundaries[.]
index = st_filter(data %>% get_SF(to_UTM = F),x) %>% select(voxel_id) %>% st_drop_geometry()
data = data[voxel_id%in%index[[1]]]

quick_map(data,basemap = T,mode='plot',samples=F) +
  tm_shape(samples)+tm_dots() 

quick_map(data,mode='plot',style = 'bclust',Lhist = T,n=6,
          legend_size = .4,basemap = T,SB_Width = .3,SB_Pos = 'left') #+

###Check duplication
# data$voxel_id %>% duplicated %>% which
# data[,coords:=paste(lng,lat)]
# index =data[,paste(lng,lat)] %>% duplicated %>% which
# data = data[-index]

colnames(data) = colnames(data) %>% str_remove('ss_')
fwrite(data,'ARF_Simulation/data/temp/data.csv')
# st_write(field_boundaries,'ARF_Simulation/data/input/Field_Boundaries.gpkg',append=T)

### Create training / subsets ----


DM = dist(data[,.(lng,lat)])

# data$cl <- dbscan::hdbscan(data[,c('lng','lat')], minPts = 25)$cluster %>% as.character()
data$cl = cutree(hclust(DM,method = 'ward.D2'),5) # 10 for the many densities

data$cl = as.factor(as.character(data$cl))
uick_map(data,Variable = 'cl',mode='plot',basemap = T,
         SB_Width = .3,SB_Pos = 'left') #+ tm_shape(samples)+tm_dots() 


### Plot Covariates            # 10:24

ggplot(data %>% melt(measure.vars=390:404),aes(x=value,fill=cl))+
  geom_histogram(bins=50,alpha=.8,position = 'identity')+ facet_wrap(~variable,scales = 'free') +
  theme_classic(base_size = 22) + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2')

### Many / densities
ggplot(data %>% melt(measure.vars=390:404),aes(x=value,fill=cl))+
  # geom_histogram(bins=50,alpha=.8,position = 'identity')+
  geom_density(alpha=.3) +
  facet_wrap(~variable,scales = 'free') +
  theme_classic(base_size = 22)  + scale_fill_brewer(palette = 'PuOr')


### Field boundaries plots
tmp = data %>% get_SF

# ### This one gets concave hull of each field
# x1 = lapply(unique(data$field_id),function(y){
#   get_polygon(tmp %>% filter(field_id==y),buffer = 0,mode = 'polygon')
# }) %>% do.call('rbind',.) %>% st_combine() 
# x2 + tm_shape(x1)+tm_polygons(alpha = .3)

x2 = quick_map(data,mode='view',SB_Pos = 'left',basemap = T)

### Use distance from true boundary 
# x = field_boundaries %>% get_SF %>% st_boundary() %>% st_distance(tmp)
# tmp$index = (x<set_units(44, m)) %>% apply(2,any)

### Use buffer from true boundaries 
buffer = field_boundaries %>% get_SF %>% st_combine %>% st_buffer(-44) 
y = st_filter(tmp,buffer) #%>% plot(add=T)
index = st_intersects(tmp,buffer) %>% '!'(.) %>%  apply(1,any)

### Ploting 
# x3 =
# x2 + tm_shape(x1)+tm_borders(alpha = 1,lwd = 2,col = 'black')
x2 + tm_shape(buffer,name='True_buffer')+tm_polygons(alpha = .5) +
  field_boundaries %>% tm_shape(name='True Boundaries') + tm_polygons(alpha=.5) +
  tm_shape(samples,name='True Samples',)+tm_dots() #+
# tm_shape(tmp,name='is_outside') + tm_dots('index') +
#  tm_shape(x1,name='Concave_boundary') + tm_polygons(alpha = .5) +
# tm_shape(x1_buffer,name='Convave_buffer') + tm_polygons(alpha = .5) 

# x3
# tmap_save(x3,'ARF_Simulation/Farm_Overlay.html')

### Remove points beyond buffer and write full data

data = data[!index,] 

### remove tiny fields
include = data[,table(field_id)>20] %>% which %>%  names %>% as.numeric
data = data[field_id%in%include]

data[,total_acres := acres]
data[,acres := .25]
data[,.(total_acres,summed=sum(acres)),'field_id'][,plot(summed,total_acres)]
# fwrite(data,'ARF_Simulation/data/input/data.csv')

### Create Subsets ----

x2 = quick_map(data,mode='plot',SB_Pos = 'left',basemap = T)

### Select Columns
cols = c('lng','lat','voxel_id','acres','field_id',
         'b','ca','cu','fe','k','mg','mn','na','p',
         's','zn','om','ph','bufferph','cec',
         'bg_red','bg_green','bg_blue','bg_nir','bg_red_edge','bg_red_edge_2',
         'bg_red_edge_3','bg_red_edge_4','bg_swir1','bg_swir2',
         'biom_auc__7_wdrvi','biom_auc__21_wdrvi',
         'cl',
         'topo_elevation','topo_slope','topo_ls','topo_twi',
         'topo_tpi','topo_insolation','topo_plan_curv','topo_prof_curv',
         'cdl_2017_class','cdl_2018_class','cdl_2019_class')
cols = c(cols, colnames(data)[str_detect(colnames(data),'ss_|gssurgo|sg_|polaris_')]) 
# cols_all = cols

dir.create('ARF_Simulation/data/temp/Input_Sets',showWarnings = F)
dir.create('ARF_Simulation/data/temp/Training_Sets',showWarnings = F)

## Full Region
Region = 'Full'
data$Region = Region
fwrite(data[,..cols],paste0('ARF_Simulation/data/input/Training_Sets/',Region,'.csv'))
fwrite(data[,..cols],paste0('ARF_Simulation/data/input/Input_Sets/',Region,'.csv'))


### Sub Regions

lapply(data$cl %>% unique,function(x){
  print(x)
  x1 = data[cl==x,..cols]
  Region = paste0('Sub_',x)
  x1$Region = Region
  x1$cl = NULL
  fwrite(x1,paste0('ARF_Simulation/data/input/Training_Sets/',Region,'.csv'))
  fwrite(x1,paste0('ARF_Simulation/data/input/Input_Sets/',Region,'.csv'))
})

### Individual Fields

d_buffer = 0
lapply(data$field_id %>% unique,function(x){
  print(x)
  x1 = data[,..cols]
  buffer = get_polygon(data %>% get_SF %>% filter(field_id==x),
                       buffer = d_buffer ,mode='polygon')
  
  index = st_intersects(x1 %>% get_SF,buffer) %>%  apply(1,any)
  x1 = x1[index,]
  
  if(d_buffer==0){
    Region = paste0('field_',x)  
  } else{
    Region = paste0('field_',x,'_',d_buffer)
  }
  
  x1$Region = Region
  x1$cl = NULL
  fwrite(x1,paste0('ARF_Simulation/data/input/Training_Sets/',Region,'.csv'))
  x1 = data[field_id==x,..cols]
  x1$cl = NULL
  fwrite(x1,paste0('ARF_Simulation/data/input/Input_Sets/',Region,'.csv'))
})








# Main Simulation ---- 
source('../Soil_Stratification_Sampling/src/Functions/Strata_Sampling_Main.R')
source('../Soil_Stratification_Sampling/Main_Wrapper.R')

Regions = list.files('ARF_Simulation/data/input/Input_Sets') %>% str_remove('.csv')
Region = 'field_8248'
Region = 'Full'

library(tictoc)

Max_Iter = 10
which(Regions%in%Region)
for(I in 10:20){
  dir.create(paste0('ARF_Simulation/data/output/Sim_',I))
  
  for(Region in Regions[1:length(Regions)]){
    
    # error = try({
    run_grid = data.table(OM_Source=c('Soil_Samples',rep('Public_Predictions',3)),
                          OM_Var=c(F,'sg_soc','polaris_om','gssurgo_om_r'))
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
        
        
        if(First_Iter==T){
          tmp = try(Main_Wrapper(input_path,training_path,output_path,
                                 Region=Region,Force_OM_Source=run$OM_Source,Force_OM_Public=OM_Var,
                                 Force_iRF=F,Output_Summary=T,Cores_Global=14))
          
          if(class(tmp)=='try-error'){
            break
          }
          First_Iter <<- F
          Selected_Features = tmp$Selected_Features
          Selected_Features <<- tmp$Selected_Features
          run_data <<- tmp$Run_Data
          run_data <- tmp$Run_Data
        } else{
          tmp = try(Main_Wrapper(input_path,training_path,output_path,
                                 Region=Region,Force_OM_Source=run$OM_Source,Force_OM_Public=OM_Var,
                                 Force_iRF=Selected_Features,Output_Summary=T,Cores_Global=14))
          
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

Summary = parallel::mclapply(Dirs,mc.cores=14,function(Dir){
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
    y1 = y[['Output']]
    run_data = get_samples(y,files)
    y2 = run_data[lng%in%y1$lng&lat%in%y1$lat,.(Sim_Mean=mean(om),Sim_SD=sd(om))]
    
    samples = run_data[lng%in%y1$lng&lat%in%y1$lat,]
    ### Confidence Interval 
    lm <- lm(om ~ 1, samples)
    CI = confint(lm, level=0.9)
    y2[,':='(Lower_CI_Sim=CI[[1]],Upper_CI_Sim=CI[[2]])]
    
    lm <- lm(om ~ 1, run_data)
    CI = confint(lm, level=0.9)
    y2[,':='(Lower_CI_True=CI[[1]],Upper_CI_True=CI[[2]])]
    
    
    return(list(cbind(y2,run_data[,.(True_Mean=mean(om),True_SD=sd(om),N_Samples=nrow(samples))]),
                list(samples=samples)))
  }) 
  
  Summary = cbind(x, lapply(Mean_SD,'[[',1) %>% rbindlist )
  Sample_L = lapply(Mean_SD,\(x) x[[2]]$samples) 
  names(Sample_L) = paste(x$Region,x$OM_Source,x$Run_Iteration,sep = '__')
  return(list(Summary,Sample_L))
})

Results = lapply(Summary,'[[',1) %>% rbindlist
Samples_L = lapply(Summary,'[[',2) %>% do.call(c,.)

### Filter out original polaris / SG runs
x = Results %>% filter(
  lapply(Run_Iteration,\(x) strsplit(x,'_')[[1]][2])%in%1:10 & 
    !OM_Source%in%c('polaris_om','sg_soc'))

Results = rbind(x,Results %>% filter(
  !lapply(Run_Iteration,\(x) strsplit(x,'_')[[1]][2])%in%1:10))

### subset for just om and field scale 
Results = Results %>% filter(Region %>% str_detect('field')) %>% 
  filter(OM_Source=='om')

### 

Results[,.((abs(True_Mean-Sim_Mean) < True_Mean*.05)/.N),c('Region','OM_Source')]

Results[,abs(True_Mean-Sim_Mean) < True_Mean*.05] %>% '!'(.) %>% sum
Results[,.((abs(True_Mean-Sim_Mean) < True_Mean*.05)/.N),c('Region','OM_Source')][,hist(V1)]

# General plotting / Scratch ----


Config = list(Region='Default',
              Iterations = 100,Iter_Run = 10,batch_limit = 250,
              allocation='Neyman',
              Max_Features = 2,Cores_Global=NULL,max_iRF_samples=2000,
              Force_OM_Source=F,Force_OM_Public=F,Force_iRF=F,
              Output_Summary=F)

# 
# lapply(rev(c(1:length(Regions))),function(I){
#   print(IDs[[I]])
#   print(paste0('Acres: ' ,tmp[field_id==Regions[I],Acres] %>% round(2)))
#   
#   tic('Run_Time')
#   tmp1 <-Main_Wrapper(input_path,training_path,output_path)
#   toc(log = T)
#   
#   time =  unlist(lapply(tic.log(format = F), function(x) x$toc - x$tic))
#   tic.clearlog()
#   
#   Max_Runs = tmp1$Run_Index %>% sapply(max) %>% max
#   Run_Stats = data.table(Region=Regions[I],Acres=tmp[field_id==Regions[I],Acres],
#                          N_Voxels=tmp[field_id==Regions[I],N],Time_Elapsed=time,
#                          Data_Size = object_size(Data[field_id%in%IDs[[I]]]), Out_Size = object_size(tmp1),
#                          N_Iter=Iter_Run*Iterations,Iter_Run=Iter_Run,Iterations=Iterations,batch_limit=batch_limit,
#                          Max_Runs=Max_Runs)
#   tmp1$Run_Stats = Run_Stats
#   
#   print('Saving')
#   saveRDS(tmp1,paste0('~/Main/Projects/Stratification_Sampling/data/output/Dir_1/',Regions[I]))
#   rm(tmp1)
#   gc()
# })

### Check indibidual 
# x1 = fread('ARF_Simulation/data/output/field_8248.csv.gz')
# data[lng%in%x1$lng&lat%in%x1$lat,mean(om)]
# data[,mean(om)]
# data[lng%in%x1$lng&lat%in%x1$lat,mean(om)] + data[,mean(om)*.05]
# abs(data[,mean(om)] - data[lng%in%x1$lng&lat%in%x1$lat,mean(om)]) < data[,mean(om)*.05]





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

### Just for ARF
index = names(MGMT)%in%data$field_id
MGMT[,..index]

M = stringdistmatrix(x)
M %>% as.matrix() %>% Heatmap

row_dend = as.dendrogram(hclust(dist(M),method='ward.D2'))
row_dend = color_branches(row_dend, k = 3)
plot(row_dend)

fv = fviz_nbclust(M %>% as.matrix, hcut,k.max = 30,  method = "gap_stat", nboot = 100,
                  maxSE = list(method = "Tibs2001SEmax", SE.factor = 1))+
  labs(subtitle = "Gap statistic method",)

fv

gskmn <- cluster::clusGap(M %>% as.matrix, FUN = hcut, K.max = 30, B = 300,hc_method='ward.D2')
plot(gskmn)
cluster::maxSE(f = gskmn$Tab[, "gap"], SE.f = gskmn$Tab[, "SE.sim"])


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

Ctrl_Iters = mclapply(1:max_iter,mc.cores=14,function(Iter){
  print(Iter)
  Out = list()
  
  
  Results_tmp = Results[Unique_Iter==Iter,]
  Samples = which(names(Samples_L) %in% 
                    Results[Unique_Iter==Iter,paste(paste0('field_',Region),OM_Source,Run_Iteration,sep = '__')]) %>% 
    Samples_L[.]
  
  
  Matched_Fields = mclapply(4:4,mc.cores = 1,\(cl){
    print(cl)
    data1 = data %>% select(field_id,Control_Clusters,paste0('cl_',cl))
    
    setnames(data1,paste0('cl_',cl),'Practice_Cluster')
    
    tmp_data = merge(Results_tmp,data1[,.(field_id,Control_Clusters,Practice_Cluster)] %>% unique,
                     by.x='Region',by.y='field_id')
    
    ### Iterate over each Control Cluster
    lapply(unique(tmp_data$Control_Clusters),
           \(Ctrl_cl){
             
             Results_Sub = tmp_data[Control_Clusters==Ctrl_cl]
             ### Iterate over Regions 
             lapply(Results_Sub$Region,function(x){
               x1 = Results_Sub[Region==x,]
               x2 = Results_Sub[Region!=x,] 
               ### Catch single element clusters
               if(nrow(x2)==0){
                 x2 = x1 
               }
               
               y1 = Samples[[x1[,paste(paste0('field_',Region),OM_Source,Run_Iteration,sep = '__')] ]]
               
               ### Iterate over all other Regions in cluster
               lapply(x2$Region,\(y){
                 y2 = Samples[[x2[Region==y,
                                  paste(paste0('field_',Region),OM_Source,Run_Iteration,sep = '__')] ]]
                 tt = t.test(y1$om,y2$om,conf.level = .9)
                 data.table(From=x1$Region,Against=y,
                            pvalue=tt$p.value,Within_90=tt$p.value>.1)
               }) %>% rbindlist
               
               
             }) %>% rbindlist 
           }) %>% rbindlist %>% cbind(data.table(MGMT_CL=cl)) %>% 
      merge(tmp_data,.,by.x='Region',by.y='From')
  }) %>% rbindlist
  Matched_Fields[,From:=Region]
  
  # Set Clusters ----
  
  tmp = Matched_Fields[MGMT_CL%in%4][Within_90==T]
  No_Matches = unique(Matched_Fields$Region)[ 
    !unique(Matched_Fields$Region)%in%tmp$Region ]
  
  selected = c()
  tbs = data.table()
  for(i in 1:100){
    exclude = tmp[From%in%selected | Against%in%selected][,From]
    
    x = tmp[!Region%in%exclude]
    
    tb= data.table(N_Controls=i,N_Region=length(unique(x$Region)),
                   nrow=nrow(x))
    if(nrow(tbs)>0){
      tb[,Diff:=N_Region-tbs[nrow(tbs),N_Region]]
    } else{
      tb[,Diff:=NA]
    }
    
    if(tb$Diff%in%c(-1,0)){break} 
    
    tbs = rbind(tbs,tb)
    x1 = x[,table(Against)]
    
    ### Check for ties
    if(sum(x1==max(x1))>1){
      x2 = names(x1)[which(x1==max(x1))]
      tb_tmp = lapply(x2,\(x3){
        selected_tmp = c(selected,x3)
        exclude = tmp[From%in%selected_tmp | Against%in%selected_tmp][,From]
        
        x = tmp[!Region%in%exclude]
        tb = data.table(Which=x3,N_Region=length(unique(x$Region)),
                        nrow=nrow(x))
        tb[,Diff:=N_Region-tbs[nrow(tbs),N_Region]]
      }) %>% rbindlist
      selected = c(selected,x2[ tb_tmp[,which.min(Diff)] ])
      
    } else{
      selected = c(selected, which.max(x1) %>% x1[.] %>%  names )
    }
  }
  Out[['selected']] = selected
  Out[['tbs']] = tbs
  
  ### Subset down based on stopping criteria
  # selected = selected[seq(stop)]
  exclude = tmp[From%in%selected | Against%in%selected][,From]
  x = tmp[!Region%in%exclude]
  
  
  plot = copy(Matched_Fields[MGMT_CL%in%4])
  plot = plot[plot[,!duplicated(paste(Region,OM_Source))]]
  plot[order(Control_Clusters,Sim_Mean),Index:=1:nrow(plot)]
  
  index = data.table(Region=as.integer(selected),index=seq(length(selected)))
  plot = merge(plot,index,by='Region',all.x=T)
  
  Status = ifelse(plot$Region%in%No_Matches,'No_Match',NA)
  
  Status[plot$Region%in%x$Region] = 'Missed' 
  Status[plot$Region%in%exclude] = 'Covered' 
  Status[plot$Region%in%selected] = 'Control_Site' 
  
  # plot$Status = c('Control Site','Covered','Missed','Missed')[Status %>% factor]
  plot$Status = c('Covered','Covered','Missed','Missed')[Status %>% factor]
  
  plot$line = plot$Index
  plot$line[!plot$Region%in%No_Matches] = NA
  
  gg = ggplot(plot, aes(x=Index, y=Sim_Mean,col=Control_Clusters,
                        # ggplot(plot, aes(x=Index, y=True_Mean,col=Sel,
                        group=Control_Clusters)) + 
    geom_errorbar(aes(ymin=Upper_CI_Sim, ymax=Lower_CI_Sim,linetype=Status),width=.6) +
    geom_line(alpha=.3) + geom_text(label=plot$index %>% as.character,size=10,nudge_y = .25,nudge_x = -.15) +
    geom_point() + theme_classic(base_size = 22) + geom_vline(aes(xintercept = line),alpha=.5) + 
    labs(y='%OM')
  
  Out[['plot']] = gg
  return(Out)
})


sapply(Ctrl_Iters,\(x) x[[1]]) %>% unlist %>% table %>% sort(T)
tmp = lapply(seq(length(Ctrl_Iters)),\(x) {
  x1 = Ctrl_Iters[[x]][[2]]
  Region = Ctrl_Iters[[x]][[1]]
  x1 = cbind(Region,x1)
  x1$Iter=x
  x1
}) %>% rbindlist

tmp
### Results plotting, actual simulation

Results[,] 
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

plot = plot[Iter<=69]
plot[,c('CI_L', "CI_H"):=list(get_confintN(.SD,1),get_confint(.SD,2)),'Index']
plot[,Within_5:=abs(True_Mean-Sim_Mean) < True_Mean*.05]
plot[,Within_5_Percent:=round(sum(Within_5)/.N,2),
     'Index']

ggplot(plot[Iter==1,], aes(x=Index, y=Avg_Mean)) + #col=Control_Clusters,
  # group=Control_Clusters)) + 
  geom_violin(data=plot,aes(x=Index %>% as.factor,y=Sim_Mean)) +
  geom_errorbar(aes(ymin=Avg_Mean-SD_Mean, ymax=Avg_Mean+SD_Mean)) +#,linetype=Status),width=.6) +
  geom_line(alpha=.3) + geom_text(label=plot[Iter==1]$Within_5_Percent,size=3,nudge_y = .25,nudge_x = 0) +
  theme_classic(base_size = 22) + #+ geom_vline(aes(xintercept = line),alpha=.5) + 
  labs(y='%OM') + 
  geom_point(data=plot,aes(x=Index,y=Sim_Mean,col=Within_5),size=1) + 
  geom_point(data=plot,aes(x=Index,y=True_Mean)) #+
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


