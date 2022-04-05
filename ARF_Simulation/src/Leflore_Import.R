source('src/General_Setup_Edit.R')
#
# Create Testing Data -----------------------------------------------------

# path = 'ARF_Simulation/data/input/arva_om_data_voxels_20210803.parquet'

paths = c('ARF_Simulation/data/input/Leflore_Inputs/2021_full.gz',
          'ARF_Simulation/data/input/Leflore_Inputs/soil_sample_gdf_generic.pkl',
          'ARF_Simulation/data/input/Leflore_Inputs/soil_samples_raw.pkl')  

source('src/Helper_Functions/AWS_Helpers.R')
L = lapply(paths,get_joblib)
samples = L[[3]] %>% st_as_sf(wkt='WKT',crs=4326)
Env = L[[1]]
Interp = L[[2]]
# field_data = st_read('ARF_Simulation/data/input/130_boundaries.gpkg')

colnames(Interp)[!colnames(Interp)%in%colnames(Env)]

data = Interp
data$voxel_id = seq(nrow(data))
rm(Interp)

# quick_map(data,Variable='ss_om',mode='plot',samples=F) #+ tm_shape(samples) +tm_dots('om')
# quick_map(data[year==2018],Variable='ss_om',mode='plot',style = 'bclust',Lhist = T,n=6,
#           legend_size = .4,basemap = F,SB_Width = .3,SB_Pos = 'left') +
#   tm_layout(
#     main.title = "2018", 
#     main.title.position = "center")

### 3/26/22 Investigation ----

hclusCut <- function(x, k, d.meth = 'ward.D2', ...){
  list(cluster = cutree(hclust(as.dist(x), method=d.meth), k=k))
  # list(cluster = cutree(hcut(as.dist(x), method=d.meth), k=k))
}

data1 = get_SF(data)
D = samples$WKT %>% st_coordinates() %>% dist 
cl = hclusCut(D,3)
samples$cl = cl$cluster

# tm_shape(samples %>% filter(cl!=10)) + tm_dots('cl')

samples$year = samples$sample_date %>% str_split('-') %>% sapply('[[',1)
samples$year %>% table

samples %>% filter(cl==3) %>% .$year %>% table 
# tm_shape(samples %>% filter(cl==1)) + tm_dots('om')
# x %>% filter(WKT==x[1,]$WKT) %>% .$om


### cluster boundary == 0
# lapply(unique(data$boundary_id), function)
# x = data %>% filter(boundary_id==0)
# touching_list = st_is_within_distance(x %>% get_SF,dist=50)
# 
# 
# library(igraph)
# g = graph.adjlist(touching_list)
# c = components(g)
# table(c$membership)
# x$boundary_id = c$membership + 1000
# # tm_shape(x %>% get_SF)+tm_dots('boundary_id')
# 
# data = rbind(data[boundary_id!=0],x)

# field hash -> simpler field ids
data$field_id = seq(length(unique(data$field_hash)))[factor(data$field_hash)]
### get boundaries ----

D = data %>% get_SF %>% st_coordinates %>% dist 
cl = hclusCut(D,3)
data$cl = cl$cluster
tm_shape(data %>% get_SF) + tm_dots('field_id')

### Subset
data[,.N,'field_id'][order(N),hist(N,150)]
index = data[,.N,'field_id'][N>10,field_id]
data = data[field_id%in%index,]

# x = data[cl==1] %>% get_SF
x = data #%>% get_SF(to_UTM = F)
# x1 = get_polygon(x,mode='polygon')

polys = lapply(x$field_id %>% unique,\(y){
  print(y)
  y1 = get_polygon(x %>% filter(field_id==y) %>% get_SF,mode='polygon')
  y1$field_id = y
  y1 %>% st_transform(crs = 4326)
}) %>% rbindlist %>% st_as_sf

# tm_shape(polys,bbox = st_boundary(polys))+tm_polygons() + tm_shape(data %>% get_SF) + tm_dots() + 
#   # tm_shape(samples) + tm_dots()
#   tm_shape(samples) + tm_dots('om')

tm_shape(samples,bbox = st_boundary(polys)) + tm_dots('om')




### fill holes ----
# x = data1 %>% filter(year==2018)
# y = data1 %>% filter(year==2021)
# 
# index = st_nearest_feature(y,x)
# 
# ### replace
# setDF(x)
# setDF(y)
# 
# x[index,-ncol(x)] = y[,-ncol(y)]
# x = st_as_sf(x)
# rm(y)
# 
# ### Interpolate missing
# colnames(x) = x %>% colnames %>% str_replace_all('-','_')
# cols=colnames(x)[str_detect(colnames(x),'ss_|gssurgo|sg_|polaris_')] 
# cols = c(colnames(x)[4:23],cols)
# 
# x[,cols] = parallel::mclapply(cols,mc.cores = 10,function(col){
#   z = try(idw(formula=as.formula(paste(col, "~ 1")), locations=x[-missing,], newdata=x[missing,],idp=2))
#   if(class(z)=='try-error'){
#     message(paste0('Error at ',col))
#   } else{
#     x[missing,col]=z$var1.pred
#   }
#   return(x[,col] %>% st_drop_geometry)
# })
# 
# x = st_transform(x,4326)
# x1 = x  %>% cbind(st_coordinates(x)) %>% st_drop_geometry %>% as.data.table
# setnames(x1,c('X','Y'),c('lng','lat'))
# 
# data = x1
# rm(data1,x,x1)
# 
# setnames(data,'ss_om','om')
# 

### Subseting ----
## Add field data ----

# data$voxel_id = seq(1,nrow(data))+130
field_data = polys

data = lapply(seq_along(field_data$field),function(x){
  tmp = field_data[x,]
  index = data %>% get_SF(to_UTM = F) %>% st_intersects(tmp$polygons) %>% sapply(sum) %>% '!='(0) %>% which
  meta = data.table(field_id=tmp$field_id,acres=.25)
  if(nrow(data[index])>0){
    return(data[index,] %>% cbind(meta,.))
  }
  
}) %>% rbindlist

field_boundaries = field_data$polygons


#### Add AGTs ----
# path_og_data = 'ARF_Simulation/data/input/arva_om_data_voxels_20210803.parquet'
# x = arrow::read_parquet(path_og_data) %>% filter(field_id%in%data$field_id)
# 
# ### Fill clusters (AGTs)
# x1 = data %>% get_SF
# y1 = x %>% get_SF
# index = st_nearest_feature(x1,y1)
# data$cluster_label = y1$cluster_label[index]

### Field subsetting 

# farms = c('JAM','MP','GNF')
# data[,is_ARVA:=farm_name%in%farms]
# data %>% quick_map(Variable='is_ARVA',basemap = T) + tm_shape(samples)+tm_dots()

### Mapping pieces
# quick_map(data,basemap = T,mode='plot',samples=F) +
#   field_boundaries %>% tm_shape() + tm_polygons(alpha=.3) + 
#   tm_shape(samples)+tm_dots() 
# 
# 
### Subset fields that have soil samples
x = st_contains(field_boundaries,samples) %>% sapply(length) %>% 
  '>'(0) %>% field_boundaries[.]
index = st_filter(data %>% get_SF(to_UTM = F),x) %>% select(voxel_id) %>% st_drop_geometry()
data = data[voxel_id%in%index[[1]]]

colnames(data) = colnames(data) %>% str_remove('ss_')

quick_map(data,basemap = T,mode='plot',samples=F) +
  tm_shape(samples)+tm_dots() 

quick_map(data,mode='plot',style = 'bclust',Lhist = T,n=6,
          legend_size = .4,basemap = T,SB_Width = .3,SB_Pos = 'left') #+

###Check duplication
# data$voxel_id %>% duplicated %>% which
# data[,coords:=paste(lng,lat)]
# index =data[,paste(lng,lat)] %>% duplicated %>% which
# data = data[-index]


fwrite(data,'ARF_Simulation/data/temp/data.csv')
# st_write(field_boundaries,'ARF_Simulation/data/input/Field_Boundaries.gpkg',append=T)

### Create training / subsets ----
### Plto eaach region 
quick_map(data[cl==1],basemap = T,mode='plot',samples=F,style = 'quantile',Lhist = T,
          n=10,legend_size = .3,SB_Width = .3,SB_Pos = 'left') 
# field_boundaries %>% tm_shape() + tm_polygons(alpha=.5) + 
# tm_shape(samples %>% st_filter(field_boundaries))+tm_dots(alpha=.3) 
quick_map(data[cl==2],basemap = T,mode='plot',samples=F,style = 'bclust',Lhist = T,
          n=8,legend_size = .3,SB_Width = .3,SB_Pos = 'left') 

quick_map(data[cl==3],basemap = T,mode='plot',samples=F,style = 'bclust',Lhist = T,
          n=8,legend_size = .3,SB_Width = .3,SB_Pos = 'left') 




DM = dist(data[cl==1,.(lng,lat)])
x = data[cl==1]
# data$cl <- dbscan::hdbscan(data[,c('lng','lat')], minPts = 25)$cluster %>% as.character()
x$cl_sub = cutree(hclust(DM,method = 'ward.D2'),5) # 10 for the many densities


data[cl==1,cl_sub:=x$cl_sub]

### Region 2
data[cl==2,cl_sub:=1]

### Region 3
x = data[cl==3]
DM = dist(x[,.(lng,lat)])

# data$cl <- dbscan::hdbscan(data[,c('lng','lat')], minPts = 25)$cluster %>% as.character()
x$cl_sub = cutree(hclust(DM,method = 'ward.D2'),3) # 10 for the many densities
quick_map(x,'cl_sub')

data[cl==3,cl_sub:=x$cl_sub]

data$cl_sub = as.factor(data$cl_sub)
### Plot Covariates            
data$cl=as.factor(data$cl)
ggplot(data %>% melt(measure.vars=329:343),aes(x=value,fill=cl_sub))+
  geom_histogram(bins=50,alpha=.8,position = 'identity')+ 
  # facet_wrap(~variable + cl,scales = 'free') +
  trelliscopejs::facet_trelliscope(~variable + cl,scales = 'free',
                                   path = 'ARF_Simulation/scratch/trellis/.',
                                   name='Covariate_Histograms',
                                   nrow = 3, ncol = 4) +#, width = 300) +
  
  theme_classic(base_size = 22) + scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2')

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

# x2 = quick_map(data,mode='plot',SB_Pos = 'left',basemap = T)

### Select Columns
data[,acres := .25]

colnames(data) = colnames(data) %>% str_replace_all('-','_')
cols = c('lng','lat','voxel_id','acres','field_id',
         'b','ca','cu','fe','k','mg','mn','na','p',
         's','zn','om','ph','bufferph','cec',
         'bg_red','bg_green','bg_blue','bg_nir','bg_red_edge','bg_red_edge_2',
         'bg_red_edge_3','bg_red_edge_4','bg_swir1','bg_swir2',
         'biom_auc__7_wdrvi','biom_auc__21_wdrvi',
         'cl','cl_sub',#'cluster_label',
         'topo_elevation','topo_slope','topo_ls','topo_twi',
         'topo_tpi','topo_insolation','topo_plan_curv','topo_prof_curv',
         'cdl_2017_class','cdl_2018_class','cdl_2019_class')
cols = c(cols, colnames(data)[str_detect(colnames(data),'ss_|gssurgo|sg_|polaris_')]) 
# cols_all = cols

dir.create('ARF_Simulation/data/input/Leflore_Sets',showWarnings = F)
dir.create('ARF_Simulation/data/input/Leflore_Sets/Input_Sets',showWarnings = F)
dir.create('ARF_Simulation/data/input/Leflore_Sets/Training_Sets',showWarnings = F)

## Full Region
Region = 'Full'
data$Region = Region
fwrite(data[,..cols],paste0('ARF_Simulation/data/input/Leflore_Sets/Training_Sets/',Region,'.csv'))
fwrite(data[,..cols],paste0('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/',Region,'.csv'))


### Sub Regions

lapply(data$cl %>% unique,function(x){
  
  print(x)
  x1 = data[cl==x,..cols]
  Region = paste0('Region_',x)
  
  fwrite(x1,paste0('ARF_Simulation/data/input/Leflore_Sets/Training_Sets/',Region,'.csv'))
  fwrite(x1,paste0('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/',Region,'.csv'))
  if(length(unique(x1$cl_sub))>1){ 
    lapply(x1$cl_sub %>% unique,function(y){
      
      x2 = x1[cl_sub==y]
      Region = paste0('Sub_',x,'_',y)
      x2$Region = Region
      
      fwrite(x1,paste0('ARF_Simulation/data/input/Leflore_Sets/Training_Sets/',Region,'.csv'))
      fwrite(x1,paste0('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/',Region,'.csv'))
    })
  }
})

### Individual Fields

mclapply(data$field_id %>% unique,mc.cores=14,function(x){
  print(x)
  x1 = data[field_id==x,..cols]
  
  Region = paste0('field_',x,'__',x1$cl,'_',x1$cl_sub)[1]
  
  x1$Region = Region
  
  fwrite(x1,paste0('ARF_Simulation/data/input/Leflore_Sets/Training_Sets/',Region,'.csv'))
  fwrite(x1,paste0('ARF_Simulation/data/input/Leflore_Sets/Input_Sets/',Region,'.csv'))
})


