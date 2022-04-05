library(iRF)
library(drf)
library(data.table)
library(ggplot2)
library(tictoc)
library(caret)
library(tictoc)
library(Metrics)
library(leaflet)
library(sf)
library(terra)
library(gstat)
library(plyr)
library(dplyr)
library(stringr)
library(sp)
library(tmap)

# library(raster)


files <- list.files('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Data/Nematode_Data/csv/',full.names = T)
Data_L <- lapply(files,fread)
lapply(Data_L,Dim)
cols <- lapply(Data_L,colnames)
tmp <- Data_L[[1]]


###New Data (Jess import)


files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Jess_Import/',pattern = '.csv',full.names = T)
x = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Jess_Import/233_field_data/',pattern = '.csv',full.names = T)
files = c(files,x)


df <- data.frame(longitude =data$Long, 
                 latitude = data$Lat)



coordinates(df) <- ~longitude+latitude
leaflet(df)   %>% addCircleMarkers( 
  radius = 3,
  color = 'black',
  stroke = FALSE, fillOpacity = 1
) %>% addProviderTiles(providers$Esri.WorldImagery) #%>% addTiles()



###3000 acres data import and conforamtion


S_files = c('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Edward Winslow/2020/Soil 2020/Joined files',
            '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Edward Winslow/2021/Soil 2021/Join',
            '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Ferebee Farming/2020/Soil 2020/Join',
            '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Ferebee Farming/2021/Soil 2021/Join',
            '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Beechland Farms/joins/Beechland Farms Soil Lab Data 2020 joined and geo.csv',
            '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Beechland Farms/joins/Beechland Farms Soil Lab 2021 geo join.csv')

S_files = lapply(S_files,list.files,pattern='.csv',full.names=T)
S_files[[5]] = '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Beechland Farms/joins/Beechland Farms Soil Lab Data 2020 joined and geo.csv'
S_files[[6]] = '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Beechland Farms/joins/Beechland Farms Soil Lab 2021 geo join.csv'

N_files = c('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Edward Winslow/2020/Nem 2020/Joined files',
'~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Edward Winslow/2021/Nem 2021/Join',
'~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Ferebee Farming/2020/Nem 2020/Join',
'~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Ferebee Farming/2021/Nem 2021/Join')

N_files = lapply(N_files,list.files,pattern='.csv',full.names=T)
N_files[[5]] = '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Beechland Farms/joins/Beechland Farms Nem Lab geo join 2020.csv'
N_files[[6]] = '~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Beechland Farms/joins/Beechland Farms Nematode Lab geo join 2021.csv'

files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Edward Winslow/2020/Soil 2020/Joined files/',pattern = '.csv',recursive = T,full.names = T)

Years = c(2020,2021,2020,2021,2020,2021)
Regions = c('Edward Winslow','Edward Winslow','Ferebee Farming','Ferebee Farming','Beechland Farms','Beechland Farms')

Combined_Regions = list()
for(I in 6:6){
  files = S_files[I][[1]]
  
  
  TL = lapply(files,fread) %>% rbindlist(fill=T)
  TL = st_as_sf(TL,wkt = 'WKT') %>% st_set_crs(OG_CRS)
  
  # tmp = data.frame()
  # for(i in seq_along(TL$FARM)){
  #   tmp = subset(TL,FARM==i) 
  #   get_hull('concave',1.3) 
  # }
  
  tmp = get_SF(TL,c_cols = NULL,to_UTM = T)
  
  tmp1 = tmp %>% st_combine() %>% st_buffer(5) %>% st_buffer(-5) %>% st_sf %>% st_cast('POLYGON') %>% 
    st_buffer(-5)
  
  grdpts <- sf::st_make_grid(tmp1, what = "centers",cellsize = c(Q_Acre,Q_Acre))
  
  tmp2 = lapply(1:nrow(tmp1),function(i){{
    st_intersects(tmp1[i,]$geometry,grdpts) %>% unlist
  }}) %>% unlist
  
  grid = grdpts[tmp2]
  
  Merged = lapply(1:nrow(tmp),function(i){
    x = tmp[i,] %>% st_intersects(grid)
    x = x[1][[1]]
    x1  = grid[x] %>% st_sf
    
    x2 = st_drop_geometry(tmp[i,]) 
    x3 = x2[rep(1:nrow(x2),length(x1))]
    
    x5 = cbind(x3,x1$geometry) %>% st_sf
    
  }) %>% rbindlist 
  
  M_Samples = st_sf(Merged) %>% st_transform(OG_CRS)
  index = apply(M_Samples,2,function(x) sum(!is.na(x))) >0
  M_Samples = M_Samples[,index]
  
  # x1 = subset(M_Samples,FARM=='Jones Lumber')
  # plot(x1[,c('SampleID','pH')])
  
  
  ###Nemos
  
  # files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA with geo joined data/WINSLOW 3000 ALL DATA with geo joined data/Edward Winslow/2020/Nem 2020/Joined files',pattern = '.csv',recursive = T,full.names = T)
  files = N_files[I][[1]]
  TL = lapply(files,fread) %>% rbindlist(fill=T)
  TL = st_as_sf(TL,wkt = 'WKT') %>% st_set_crs(OG_CRS)
  TL = TL[,colnames(TL)[c(1,2,23:50)]]
  
  # index = apply(TL,2,function(x) sum(!is.na(x))) >0
  # TL = TL[,index]
  
  # z = TL[33:38,'SampleID'][[1]]
  # zz = TL1[33:38,]
  Merged_Nemo = lapply(1:nrow(TL),function(i){
    print(i)
    x = TL[i,] %>% st_make_valid() %>% st_intersects(M_Samples)
    x = x[1][[1]]
    x1  = M_Samples[x,] %>% as.data.table()
    
    x2 = st_drop_geometry(TL[i,]) 
    x3 = x2[rep(1:nrow(x2),nrow(x1))]
    
    
    cbind(x3,x1) %>% st_sf
    
  }) #%>% rbindlist 
  
  index = which(sapply(Merged_Nemo,nrow) >0)
  Combined = rbindlist(Merged_Nemo[index])
  Combined$Year=Years[I]
  Combined$Region = Regions[I]
  Combined_Regions[[I]] = Combined
}

tmp = rbindlist(Combined_Regions,fill=T) %>% st_as_sf()
index = which(apply(tmp,2,function(x) sum(!is.na(x))) >0)
tmp = tmp[,index]

apply(tmp,2,function(x) sum(!is.na(x)))
colnames(tmp)[c(1,2,13,14)] = c('SampleID_Nemotode','NemID_Nemotode','SampleID_Soil','NemID_Soil')

file= '/home/elija/Lab_Link/AR1K/Data/Nematode_Data/3000_Acres_Shapefiles/'
RY = paste0(Regions,'_',Years)
lapply(1:6,function(x){
  obj = Combined_Regions[[x]]
  dir.create(paste0(file,RY[[x]]),showWarnings = F)
  st_write(Combined_Regions[[x]],
           paste0(file,RY[[x]],'/',RY[[x]],'.shp'), driver = "ESRI Shapefile")
  
})

###Reload

file= '/home/elija/Lab_Link/AR1K/Data/Nematode_Data/3000_Acres_Shapefiles/'
files = lapply(file,list.files,pattern='.shp',recursive=T,full.names=T)[[1]]

W3 = lapply(files,st_read)

### Passback - Merging OG

W = W3[[2]]
TL

cols = c(colnames(W3[[1]])[c(3:29,41:50,51,53,54,50,63,62,60,59)],"ZnI","SI",'Root_knot','HMA.RESULT','VW.RESULT','CATION.EXC','BASE.SAT_','P','K','Mn','Mn.Avail.C','Cu','S',
         'Na',"Zn","Grower","State", "FarmID","FieldID",'geometry'
)

W = lapply(W3,function(x){
  x1 = st_coordinates(x)
  x = as.data.table(x) 
  x[,c('lng','lat') := .(x1[,1],x1[,2])]
  index = colnames(x) %in% c('lng','lat',cols) %>% colnames(x)[.]
  x = x[,..index]
  return(x)
}) %>% rbindlist(fill=T)


# x1 = sapply(W3,function(x){
#   x1 = colnames(x)
#   which(!x1%in%cols) %>% colnames(x)[.]
# }) 
# x1

# x1[[3]][c(19:26,28:30)] %>% shQuote %>% cat(sep = ',')

setcolorder(W,c('lng','lat','Region','Grower',"FarmID","FieldID"))


### Merge new and old

tmp = fread('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Data/Nematode_Data/Jess_Import/440_field_data/440_voxel_data.csv')
table(tmp$client_name)
tmp1 = get_SF(tmp[client_name=='Beechland Farms'],c_cols = c('lng','lat'),to_UTM = F)

plot(W[Grower=='Beechland Farms',.(lng,lat)])
plot(tmp[client_name=='Beechland Farms',.(lng,lat)])

# x1 = W[Grower=='Beechland Farms',]
# x2 = tmp[client_name=='Beechland Farms',]

x1 = W
x2 = tmp


d  = raster::pointDistance(x2[,.(lng,lat)],x1[,.(lng,lat)],lonlat = T)

diag(d) = 1000000

index = apply(d,1,which.min)
plot(x2[,.(lng,lat)])
points(x1[index,.(lng,lat)],col='blue')

cols = c(1,9,11:12,18,67:81,83:139)
cols_1 = c(-1,-2)
Data_W3 = cbind(x1[index,..cols_1],x2[,..cols])

setcolorder(Data_W3,c('lng','lat'))
# 
# tmp1 = get_SF(Data_W3[Grower=='Beechland Farms'],c_cols = c('lng','lat'),to_UTM = F)
# plot(tmp1[,'Ca'])

# 
# 
# st_set_crs(tmp1,OG_CRS)
# st_set_crs(TL,OG_CRS)
# tm_shape(tmp1) +
#   tm_dots("ca") +
#   tm_facets(by = "client_name")
# 
# plot(TL[,'Ca'])
# plot(W[,'Ca'])
# plot(tmp1[,'ca'])


###Export all datasets

write.csv(Data_W3,"/home/elija/Lab_Link/AR1K/Data/Nematode_Data/Datasets_Export/Winslow_3000_New.csv",row.names = F)

data_AR$lat = NULL
data_AR$lng = NULL
colnames(data_AR)[1:2] = c('lng','lat')
write.csv(data_AR,"/home/elija/Lab_Link/AR1K/Data/Nematode_Data/Datasets_Export/AR_Original.csv",row.names = F)


data_NC$lat = NULL
data_NC$lng = NULL
colnames(data_NC)[1:2] = c('lng','lat')
write.csv(data_NC,"/home/elija/Lab_Link/AR1K/Data/Nematode_Data/Datasets_Export/NC_Original.csv",row.names = F)



write.csv(xcols,"/home/elija/Lab_Link/AR1K/Data/Nematode_Data/Datasets_Export/AR_Original_predictor_columns.csv",row.names = F)
write.csv(xcols,"/home/elija/Lab_Link/AR1K/Data/Nematode_Data/Datasets_Export/NC_Original_predictor_columns.csv",row.names = F)




###For AR data
library(plyr)
library(dplyr)
library(sf)
library(sp)





# ###
# 
# 
# files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/Merged_3000AC/',recursive = T,full.names = T)
# 
# SB= lapply(files,st_read)
# 
# cols = colnames(SB[[2]])[24:27]
# tmp = SB[[2]]
# tmp1 = apply(tmp[,cols],2,function(x)as.numeric(as.character(x)))
# tmp2=cbind(tmp[,2],tmp1)
# plot(tmp2[,cols])
# 
# # plot(Data_EC)
# # Data_EC = rbind(Data_EC[[1]],Data_EC[[2]])
# SB = SB[[1]]
# 
# files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/WINSLOW 3000 ALL DATA (2)/WINSLOW 3000 ALL DATA/Beechland Farms/Sample Procedures/',pattern = '.shp',recursive = T,full.names = T)
# 
# NB= lapply(files,st_read)[[1]]
# 
# 
# Data_EC = cbind(st_coordinates(Data_EC),st_drop_geometry(Data_EC))
# 
# Input =  Data_EC %>% get_SF(c_cols = c('X','Y'),to_UTM = T,CRS = OG_CRS)

data <- Data_L[[2]]

###Add EC data

files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Kyle_Import/',pattern = '.shp',recursive = F,full.names = T)

Data_EC = lapply(files,st_read) 
# plot(Data_EC)
Data_EC = rbind(Data_EC[[1]],Data_EC[[2]])
Data_EC = cbind(st_coordinates(Data_EC),st_drop_geometry(Data_EC))

Input =  Data_EC %>% get_SF(c_cols = c('X','Y'),to_UTM = T,CRS = OG_CRS)
index = !duplicated(st_coordinates(Input))
Input = Input[index,]

grid = get_SF(as.data.frame(data),c_cols = c('X','Y'),to_UTM = T,CRS = OG_CRS) %>% st_coordinates

# x = dist(st_bbox(Input)) %>% as.matrix
# diagonal = x[1,3]
# FB = c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 100) * diagonal * 0.35/100
FB = c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 100)*.8
FB
Variable = 'EC_0_2'
tmp3 = SF_Krig(Variable,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
               cutoff=50,len=5,cv=NA,force_bounds = FB)[[1]]

cols = colnames(Input)[1:4]
krig_out = mclapply(cols,mc.cores = 10,SF_Krig,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
                    cutoff=230,len=10,cv=NA,force_bounds=FB)


Out = krig_out %>% lapply(function(x){
  st_drop_geometry('[['(x,1))}) %>% do.call(cbind,.) %>% 
  cbind(.,st_coordinates(krig_out[[1]][[1]])) %>% 
  st_as_sf(.,coords = c("X", "Y"),
           crs = st_crs(krig_out[[1]][[1]])) 


tmp = cbind(st_coordinates(Out),st_drop_geometry(Out))
tmp=tmp[,cols]
colnames(tmp) = paste0(colnames(tmp),'_VERIS')
data= cbind(data,tmp)


###Add ArvaPy data

tmp = Data_N[[3]]

d  = raster::pointDistance(data[,.(X,Y)],tmp[,.(lng,lat)],lonlat = T)
diag(d) = 1000000
index = apply(d,1,which.min)
plot(tmp[index,.(lng,lat)])
points(data[,.(X,Y)],col='blue')
cols = c(4,11:12,16:19,73:131)
data = cbind(data,tmp[index,..cols])


data_v = rbind.fill(data,tmp[-index,..cols])
setDT(data_v)


index = apply(data,2,function(x) sum(is.na(x))>1) %>% as.matrix %>% which %>% '*'(-1)



data=data[,..index]
data_v=data_v[,..index]

index = data[,lapply(.SD,function(x) sum(x!=0)>5),.SDcols=Dagger:RKN] %>% 
  as.matrix %>% which


cols_y <- colnames(data[,Dagger:RKN])[index]

cols_x <- c(colnames(data[,pH:`EBS-Na`]),colnames(data[,c(50:53,60:113)]))


cols_x = cols_x[!cols_x%in%c("global_predicted_yield_corn_x","global_predicted_yield_generic_x","global_agt_corn_x",
                          "global_agt_generic_x",'topo_elevation')]



xcols <- cols_x
ycols <- cols_y

cols <- c(cols_x,cols_y,c('X','Y','voxelized_yield'))

data_AR = data

files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Jess_Import/Nematocide_Data/',pattern = '.shp',recursive = T,full.names = T)
 
Data_NT = lapply(files,st_read)
x = Data_NT[[1]]
coords = st_centroid(x$geometry) %>%  st_coordinates
index = coords[,1] < -91.367
x= x[index,]
plot(st_coordinates(x))
x1=x

x = Data_NT[[2]]
coords = st_centroid(x$geometry) %>%  st_coordinates
index = coords[,1] < -91.35
x= x[index,]
plot(st_coordinates(x))

Nematicide_geom = rbind(x1,x)
plot(st_coordinates(Nematicide_geom))



###Krig the nonconformers
#BoyPiv StkPiv
#[Field=='StkPiv'] [Field=='StkPiv']
data_v_AR = data_v
data_v =  data_v_AR

Input = get_SF(as.data.frame(data_v),c_cols = c('lng','lat'),to_UTM = T)


box <- get_hull(Input,'concave',1.3)
st_area(box)/Acre_M


Input %>% get_hull('concave',1.3)

grid = Input %>% get_hull('concave',1.3) %>%
  add_buffer(0) %>% 
  get_grid(c(Q_Acre,Q_Acre))


###Test run
# Variable="CEC"
Variable="Stubby-root"
Variable="biom_auc__7_wdrvi"
Variable='Lesion'
Variable='voxelized_yield'
# Variable = cols[17]
# grid = st_coordinates(Input)

# t = Input$biom_auc__7_wdrvi>65
# tt=which(t)
# Input$biom_auc__7_wdrvi+10
hist(Input[,Variable][[1]])
tmp3 = SF_Krig(Variable,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
              cutoff=150,len=10,cv=NA)#,Exclude = 50)
tmp = tmp3[[1]]
mean(tmp$Lesion_SD)
print(mean(tmp3[[3]]))
# OG_CRS = 4326
tmp2 = st_transform(tmp, OG_CRS)
# tmp2=tmp
# .01
Variable = paste0(Variable,'_SD') 
Variable = Variable %>% str_remove('_SD')
plot3 = SF_Raster(tmp2,Variable,F,q=.001,rev=T)[[1]]

# plot = SF_Raster(tmp2,Variable,F,q=.002,rev=F)[[1]]

ggplot(plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis')# + 
# geom_point(data=data_v[!is.na(Stubby_)],aes(x=X,y=Y,color='red',alpha=1))# + scale_color_continuous(type = 'viridis') + guides(color='none')

###With nemoticide
Nematicide_geom = st_set_crs(Nematicide_geom,OG_CRS)

ggplot() + geom_raster(data=plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis') + #geom_sf(data=Nematicide_geom,fill=NA,color=alpha("grey",0.2)) + 
  geom_point(data=data_v,aes(x=X,y=Y,color=data_v[,..Variable][[1]]),alpha=.5,size=5) + scale_color_continuous(type = 'viridis') + guides(color='none')

###Run interpolation

library(parallel)
cols = colnames(Input)[c(23:40,50,53:107)]
cols = c(xcols,ycols)
ncols = colnames(Input)[which(!colnames(Input)%in%cols)]

krig_out = mclapply(cols,mc.cores = 10,SF_Krig,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
                    cutoff=230,len=10,cv=0)

t = sapply(krig_out,Dim)==1 
tcols=cols[t]


krig_out_t =lapply(tcols,SF_Krig,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
                cutoff=230,len=10,cv=NA)
t_r = which(t)
ind = 1:length(krig_out)
x = lapply(ind[-t_r],function(x1){
  krig_out[[x1]]
})

krig_out = c(x,krig_out_t)


Out = krig_out %>% lapply(function(x){
  st_drop_geometry('[['(x,1))}) %>% do.call(cbind,.) %>% 
  cbind(.,st_coordinates(krig_out[[1]][[1]])) %>% 
  st_as_sf(.,coords = c("X", "Y"),
           crs = st_crs(krig_out[[1]][[1]]))





tmp = cbind(st_coordinates(Out),st_drop_geometry(Out))
colnames(tmp)[1:2] = c('lng','lat')
# tmp11=cbind(st_coordinates(Input[,ncols]),st_drop_geometry(Input[,ncols]))
data_AR_voxel = tmp#cbind(tmp,tmp11)



Variable='Lesion'
plot = plot_raster(data_AR_voxel,Variable,.005,krig = F,rev=F)

ggplot(plot,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis') +geom_point(data=data_v,aes(x=X,y=Y,col=Lesion),alpha=.5)





###Nemoticide Analysis

ggplot() + geom_raster(data=plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis') + geom_sf(data=Nematicide_geom,fill=NA,color=alpha("grey",0.2)) + 
  geom_point(data=data_v,aes(x=X,y=Y,color='red'),alpha=1) #+ scale_color_continuous(type = 'viridis') + guides(color='none')


tmp =((Nematicide_geom$geometry))

tmp1 = get_SF(as.data.frame(data_v[!is.na(X)]),c_cols = c('lng','lat'),to_UTM = T)
tmp = st_set_crs(tmp,OG_CRS) %>% st_transform(st_crs((tmp1))) 

tmp = tmp %>% st_combine() %>% st_buffer(10) %>% st_buffer(-10) %>% st_union %>% st_cast('POLYGON')

  
plot(tmp)
plot(tmp1$geometry)

x = st_polygon(lapply(tmp[1], function(x) x[[1]])) %>% st_sfc(crs = st_crs(tmp1))
plot(x)

x1 = st_difference(x,tmp[1])
plot(x1)
index5 = st_intersects(x1,tmp1$geometry)[[1]] 
plot(tmp) ; plot(tmp1$geometry,add=T)

###Remeber to select!
# ind = identify(st_coordinates(tmp1$geometry))

# tmp1_back = tmp1
tmp1 = tmp1_back
CRS = st_crs(tmp1)
tmp1 = as.data.table(tmp1)
colnames(tmp1)[[16]] = 'Stubby_root'

# tmp1 = tmp1
# ind4 = which(tmp1$voxelized_yield < 150 )
# tmp1=tmp1[-ind4,]
###Trnsformation 
BC_List = lapply(tcols,function(x){
  # bc2 <- boxcoxfit(tmp1[,..x][[1]], lambda2 = TRUE)
  bc2 <- BC_fit(tmp1[,..x][[1]])
  lambda1 <- bc2$lambda1
  lambda2 <- bc2$lambda2
  
  x1 = BCTransform(tmp1[,..x][[1]],lambda1,lambda2)
  # tmp1[,(paste0(x,'_T')):=x1]
  tmp1[,(x):=x1]
  return(list(Col=x,L1=lambda1,L2=lambda2))
}) %>% rbindlist

tmp1 = tmp1 %>% as.data.frame %>% st_sf()

# ###back transform
# 
# lapply(ycols,function(x){
#   tmp = BC_List[Col==x,]
#   x1 = BCTransformInverse(data_in[,..x][[1]],tmp$L1,tmp$L2)
#   data_in[,(x):=x1]
#   
# }) %>% rbindlist

control_points = ind %>% c(index5) %>% tmp1[.,]

library(nngeo)

index = lapply(1:nrow(control_points),function(x){
  x1 = st_nn(control_points[x,],tmp1$geometry,k=9)[[1]][-1]
})



tmp3=as.data.table(tmp1)
tcols = c(ycols,'voxelized_yield')
# tcols=paste0(tcols,'_T')
KS_test = lapply(tcols,function(Variable){

    testtmp = lapply(1:nrow(control_points),function(x){
    x1 = as.data.table(control_points)[x,..Variable]
    x2 = tmp3[index[[x]],..Variable]
    x3 = rep(x1,length(x2))-x2
    
    # x2[[1]]
    test = ks.test(x3[[1]],'pnorm',alternative = 'less')
    
    
    
    # meantmp = x1-mean(x2[[1]])
    if(Variable!='njnjnj'){
      x5 = BC_List[Col==Variable,]
      # 
      x1 = BCTransformInverse(x1,x5$L1,x5$L2)
      x2 = BCTransformInverse(x2,x5$L1,x5$L2)
      
      m1 = x1-mean(x2[[1]],na.rm=T)
      
    } else{
      m1 = x1-mean(x2[[1]],na.rm=T)
    }
    
    
    return(list(test$p.value,m1))
  })
  
  Means = sapply(testtmp,'[[',2)
  testtmp = sapply(testtmp,'[[',1)
  ind3 = testtmp == 0
  testtmp[ind3] = .000000001
  overall=  1-pchisq(sum( -2*log(testtmp)),2*length(testtmp),lower.tail = T)
  
  index1 = which(testtmp<.05)

  
  # index = sapply(1:nrow(control_points),function(x){
  #   x1 = st_nn(control_points[x,],tmp1$geometry,k=9)[[1]][-1]
  # })
  # 
  # test_points = tmp1[index,]
  # setDT(test_points)
  # 
  # plot(tmp); points(st_coordinates(tmp1[index,]),col='red') ; points(st_coordinates(control_points[index1,]),col='blue') 
  # points(st_coordinates(control_points[-index1,]),col='orange') 
  # # print(plot1)
  
  return(data.table(Point=1:nrow(control_points),Variable=Variable,KS_Pval=unlist(testtmp),Combined_P=overall,Average_Effect_Size=mean(unlist(Means)),Effect_Size_SD=sd(unlist(Means)),SDEffect_Size=unlist(Means),Significant=testtmp<.05))
})

ktmp = KS_test %>% rbindlist
# ktmp$KS_Pval = ktmp$KS_Pval %>% formatC(., format = "e", digits = 2)

sel = c(1,2,4,5)
ktmp = ktmp[,..sel]
ktmp = ktmp[which(!duplicated(ktmp$Variable)),]
ktmp$Combined_P = ktmp$Combined_P %>% formatC(., format = "g", digits = 2)

ktmp
dcast(ktmp[,-'Significant'], Point +Combined_P + Effect_Size ~ ..., value.var ='KS_Pval' )

# 
# plot= data.table(group='Control',Nemotode='Lesion')
# 
# x1 = melt(test_points,measure.vars = ycols)[,Group:='Test'][,.(Group,variable,value)]
# x2 = melt(control_points,measure.vars = ycols)[,Group:='Control'][,.(Group,variable,value)]
# plot=rbind(x1,x2)
# 
# ggplot(plot,aes(x=value,fill=Group)) + geom_histogram(position = 'dodge') +theme_classic() + labs(x='Nemotode_Count') +
#   facet_wrap(~variable,scales = 'free_x')
# 

### AGTS


tmp2 = get_SF(as.data.frame(data_v[]),c_cols = c('lng','lat'),to_UTM = T)
tmp2$global_agt_generic_x = as.factor(tmp2$global_agt_generic_x)

plot1 = SF_Raster(tmp2,'global_agt_generic_x',F,q=.004,rev=F)
plot(plot1[[2]])

ggplot() + geom_raster(data=plot1[[1]],aes(x=x,y=y,fill=global_agt_generic_x)) + theme_classic() +
  # ggplot() + geom_raster(data=plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + theme_classic() + labs(fill=Variable) +
  # scale_fill_continuous(type = 'viridis') +
  geom_sf(data=test_points$geometry,fill=NA,color='green') +
 geom_sf(data=control_points[index1],fill=NA,color='black') +geom_sf(data=control_points[-index1,],fill=NA,color='orange') +
  geom_sf(data=nemo_overlay,fill=alpha("grey",0.2)) 
  # geom_point(data=tmp2,aes(x=X,y=Y))
  # + labs(fill=Variable) +

 


ggplot(tmp2,aes(x=lng,y=lat,color=global_agt_generic_x)) + geom_point() + theme_classic()

#+ labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis') #+ 
  
  control_points
  test_points
  tmp1
  
  
  control = rbind(as.data.table(control_points),as.data.table(test_points))
  AGT = as.data.table(tmp1)
  
  loop = unique(AGT[,global_agt_generic_x])
  col='Lesion'
  ks_result = lapply(loop,function(x){
    means=c(control[,mean(.SD[[1]],na.rm=T),.SDcols=col],
    AGT[global_agt_generic_x==x,mean(.SD[[1]],na.rm=T),.SDcols=col])
    wh =which.max(means)
    if(wh==1){
      test = ks.test(control[,..col][[1]],AGT[global_agt_generic_x==x,..col][[1]],alternative = 'less')
    } else{
      test = ks.test(control[,..col][[1]],AGT[global_agt_generic_x==x,..col][[1]],alternative = 'greater')
    }
    
    return(list(means,test))
  })
  
ks_result
  # ###General plotting
# data = data_NC
# 
# plot = data
# cols = c(xcols,ycols[8])
# plot = data[,..cols]
# 
# plot=melt(plot,id.vars = 'Lance')
# 
# plot[,Cor:=cor(Lance,value),by=variable]
# plot[,facet:=paste0(variable,', Cor: ',round(Cor,3))]
# 
# cors = plot[,abs(cor(Lance,value)),by=variable][rev(order(V1))]
# cols = cors[!is.na(V1)][1:12,variable]
# 
# plot(data[,.(`BASE SAT.`,Lance)])
# lm = lm(Lance~`BASE SAT.`,data=data)
# abline(lm)
# 
# ggplot(plot[variable%in%cols],aes(x=value,y=Lance)) + geom_point(size=.7) +
#   geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=1) +#  cut(round(N_Ratio),16+
#   facet_wrap(~facet,scales = 'free_x') + theme_classic(base_size = 20)
# 
# ggplot(data,aes(x=CEC,y=Lesion)) +geom_point() + theme_classic()
# ggplot(data,aes_string(x='X',y='Y',col="B")) +geom_point(size=10) + theme_classic()










###For NC data

###Jump 3

files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Jess_Import/',pattern = '.csv',full.names = T)
x = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Jess_Import/233_field_data/',pattern = '.csv',full.names = T)
files = c(files,x)



Data_N = lapply(files,fread)

data <- Data_L[[4]]
cols_pp <- colnames(data[,Root_knot:Cyst_eggs])
cols_x <- c(colnames(data[,`HMA RESULT`:`Mn Avail Crop1`]),colnames(data[,Zn:S]),'Na')
cols <- c(c('Long','Lat','FARM_NAME',"SAMPLE ID",cols_x,cols_pp))

# setnames(data,'Free living','Free_Living')
L <- lapply(c(1,3,4),function(i){
  data <- Data_L[[i]]
  
  data <- data[,..cols]
  
  changeCols <- names(data)[sapply(data, is.logical)]
  data[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  for (col in c(cols_pp)) data[is.na(get(col)), (col) := 0]
  
  data[,Bad_Nemo:=rowSums(.SD),.SDcols=cols_pp]
})

data <- rbindlist(L)
data = data[!is.na(`SAMPLE ID`)]
data[,`SAMPLE ID`:=paste0(FARM_NAME,'_',`SAMPLE ID`)]

index <- apply(data[,..cols_x],1,function(x){
  any(is.na(x)==T)
})

data <- data[!index]
data = data[-30]
data_tmp = data
###Merge voxels

###voxel -> OG
tmp = Data_N[[1]][lat>36]
Voxel_Data = tmp 

d  = raster::pointDistance(data[,.(Long,Lat)],tmp[,.(lng,lat)],lonlat = T)
diag(d) = 100000000000
index = apply(d,1,which.min)

plot(tmp[,.(lng,lat)])
plot(tmp[-index,.(lng,lat)])
points(data[,.(Long,Lat)],col='blue')
# cols = c(16:20,37,38,77:135)
cols = c(4,20,11:12,16:19,37,38,77:135)
data = cbind(data,tmp[index,..cols])
# data_NC = data
# data = data[-30]



data_v = rbind.fill(data,tmp[-index,..cols])
setDT(data_v)


index = apply(data,2,function(x) sum(is.na(x))>1) %>% as.matrix %>% which %>% '*'(-1)
data=data[,..index]
data_v=data_v[,..index]



index = data[,lapply(.SD,function(x) sum(x!=0)>5),.SDcols=cols_pp] %>% 
  as.matrix %>% which

ycols <- colnames(data[,..cols_pp])[index]

xcols <- c(cols_x,
            colnames(data[,59:113]))

xcols = xcols[!xcols%in%c("global_predicted_yield_corn_x","global_predicted_yield_generic_x","global_agt_corn_x",
                         "global_agt_generic_x",'topo_elevation')]
xcols=xcols[xcols%in%colnames(data_v)]


data_NC = data


###Add fall nemo data
files = list.files('~/Lab_Link/AR1K/Data/Nematode_Data/Fall_NC_Data/CSV/',pattern = '.csv',full.names = T)

Data_F = lapply(files,fread)
sapply(Data_F,dim)

Data_F = rbindlist(Data_F)
Data_F[,`SAMPLE_ID`:=paste0(FARM_NAME,'_',`SAMPLE_ID`)]

cols = colnames(Data_F)[20:47]

for (col in c(cols)) Data_F[is.na(get(col)), (col) := 0]
Data_F[,lapply(.SD,function(x) sum(x!=0)),.SDcols=cols]
index = Data_F[,lapply(.SD,function(x) sum(x!=0)>5),.SDcols=cols] %>% as.logical() %>% which

tmp <- colnames(Data_F[,..cols])[index]
ycols_Fall = paste0(tmp,'_Fall')
setnames(Data_F,tmp,ycols_Fall)

cols = c('SAMPLE_ID',ycols_Fall)
Data_F = Data_F[,..cols]

data_NC = merge(data_NC,Data_F,by.x="SAMPLE ID",by.y='SAMPLE_ID',all.x=T)
data_v = merge(data_v,Data_F,by.x="SAMPLE ID",by.y='SAMPLE_ID',all.x=T)



###Krig the nonconformers


grid = lapply(unique(data_v$field_name),function(x){
  Input = get_SF(as.data.frame(data_v[field_name==x]),c_cols = c('lng','lat'),to_UTM = T)
  
  box <- get_hull(Input,'concave',1.3)
  print(st_area(box)/Acre_M)
  
  
  Input %>% get_hull('concave',1.3)
  
  grid = Input %>% get_hull('concave',1.3) %>%
    add_buffer(0) %>% 
    get_grid(c(Q_Acre,Q_Acre))
  return(grid)
  
}) %>% rbindlist %>% as.data.frame() 



x1 = melt(data_v,measure.vars = ycols,id.vars = c('lng','lat'))[,Group:='Summer'][,.(Group,variable,value)]
x2 = melt(data_v,measure.vars = ycols_Fall,id.vars = c('lng','lat'))[,Group:='Fall'][,.(Group,variable,value)]
x2$variable = x2$variable %>% str_remove_all('_Fall')

plot=rbind(x1,x2)

ggplot(plot,aes(x=value,fill=Group)) + geom_histogram(position = 'dodge') +theme_classic() + labs(x='Nemotode_Count') +
  facet_wrap(~variable,scales = 'free_x')

tmp = plot[,mean(value,na.rm = T),.(Group,variable)]
tmp = tmp[1:8,Avg_Change:=tmp[Group=='Fall',V1]-tmp[Group=='Summer',V1]][1:8,c(2,4)]
colnames(tmp)[1]=c("Nemotode")


###Test run
Input = get_SF(as.data.frame(data_v),c_cols = c('lng','lat'),to_UTM = T)

Variable="CATION EXCHANGE"
Variable='voxelized_yield'
Variable='Lesion_Fall'
Variable='Lesion'

# grid = st_coordinates(Input)

tmp3 = SF_Krig(Variable,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
               cutoff=180,len=10,cv=NA)
tmp = tmp3[[1]]
mean(tmp[,Variable][[1]])
print(mean(tmp3[[3]]))
# OG_CRS = 4326
tmp2 = st_transform(tmp, OG_CRS)
# tmp2=tmp
# .01
# Variable = paste0(Variable,'_SD') 
# Variable = Variable %>% str_remove('_SD')
plot3 = SF_Raster(tmp2,Variable,F,q=.003,rev=T)[[1]]


plot_S = plot3
plot_S$Lesion = log10(plot_S$Lesion)
plot_S$Lesion[is.na(plot_S$Lesion)] =0
plot_S$Lesion[plot_S$Lesion<0] =0

plot_F = plot3
plot_F$Lesion_Fall = log10(plot_F$Lesion)
plot_F$Lesion_Fall[is.na(plot_F$Lesion_Fall)] =0
plot_F$Lesion_Fall[plot_F$Lesion_Fall<0] =0

plot_Diff = plot_S
plot_Diff$Lesion = plot_F$Lesion_Fall-plot_S$Lesion
plot_Diff$Lesion = log10(plot_Diff$Lesion)
plot_Diff$Lesion[is.na(plot_Diff$Lesion)] =0
plot_Diff$Lesion[plot_Diff$Lesion<0] =0

colnames(plot_Diff)[3] = 'Lesion'
ggplot(plot_Diff,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis')#,limits=c(0, max(plot_F$Lesion)))

ggplot() + geom_raster(data=plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis') + #geom_sf(data=Nematicide_geom,fill=NA,color=alpha("grey",0.2)) + 
  geom_point(data=data_v,aes(x=Long,y=Lat,color=data_v[,..Variable][[1]]),alpha=1,size=5) + scale_color_continuous(type = 'viridis') + guides(color='none')



###Run interpolation
library(parallel)
# cols = colnames(Input)[c(23:40,50,53:107)]http://192.168.0.118:8787/graphics/e3ce9cf2-3cbd-44b6-a99e-ed559e74be72.png
cols = c(xcols,ycols,'voxelized_yield')
# cols=cols%in%colnames(Input)
ncols = colnames(Input)[which(!colnames(Input)%in%cols)]

krig_out = mclapply(cols,mc.cores = 10,SF_Krig,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
                    cutoff=180,len=10,cv=NA,biom_auc=0)

t = sapply(krig_out,Dim)==1 
tcols=cols[t]
tcols

# krig_out_t =lapply(tcols,SF_Krig,Input,ghttp://192.168.0.118:8787/graphics/e3ce9cf2-3cbd-44b6-a99e-ed559e74be72.pngrid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
#                 cutoff=230,len=10,cv=1)
# 
# 
# krig_out_t = mclapply(tcols,mc.cores = 10,SF_Krig,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
#                       cutoff=230,len=10,cv=1)
# 
# 
# t = sapply(krig_out_t,Dim)==1 
# tcols=tcols[t]




Out = krig_out %>% lapply(function(x){
  st_drop_geometry('[['(x,1))}) %>% do.call(cbind,.) %>% 
  cbind(.,st_coordinates(krig_out[[1]][[1]])) %>% 
  st_as_sf(.,coords = c("X", "Y"),
           crs = st_crs(krig_out[[1]][[1]]))





tmp = cbind(st_coordinates(Out),st_drop_geometry(Out))
colnames(tmp)[1:2] = c('lng','lat')

tmp11=cbind(st_coordinates(Input[,ncols]),st_drop_geometry(Input[,ncols]))
# data_AR_voxel = cbind(tmp,tmp11)
data_NC_voxel = tmp
index = data_NC_voxel %>% colnames %>% str_detect('SD') %>% which 
data_NC_voxel = data_NC_voxel[,-index]


###General plotting
data = data_NC_voxel

data_tmp = data_tmp

# data = data_NC_voxel
    
# plot = data
# cols = c(xcols,ycols[8])
# plot = data[,..cols]
# 
# plot=melt(plot,id.vars = 'Lance')
# 
# plot[,Cor:=cor(Lance,value),by=variable]
# plot[,facet:=paste0(variable,', Cor: ',round(Cor,3))]
# 
# cors = plot[,abs(cor(Lance,value)),by=variable][rev(order(V1))]
# cols = cors[!is.na(V1)][1:12,variable]
# 
# plot(data[,.(`BASE SAT.`,Lance)])
# lm = lm(Lance~`BASE SAT.`,data=data)
# abline(lm)
# 
# ggplot(plot[variable%in%cols],aes(x=value,y=Lance)) + geom_point(size=.7) +
#   geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=1) +#  cut(round(N_Ratio),16+
#   facet_wrap(~facet,scales = 'free_x') + theme_classic(base_size = 20)
# 
# ggplot(data,aes(x=CEC,y=Lesion)) +geom_point() + theme_classic()
# ggplot(data,aes_string(x='X',y='Y',col="B")) +geom_point(size=10) + theme_classic()










###LM and Quant Reg Jump
data_in = data
data_in = data.table::copy(data_AR)

BC_List = lapply(ycols,function(x){
  bc2 <- boxcoxfit(data_in[,..x][[1]], lambda2 = TRUE)
  
  lambda1 <- bc2$lambda[1]
  lambda2 <- bc2$lambda[2]
  
  x1 = BCTransform(data_in[,..x][[1]],lambda1,lambda2)
  data_in[,(x):=x1]
  return(list(Col=x,L1=lambda1,L2=lambda2))
}) %>% rbindlist

###back transform
# 
# lapply(ycols,function(x){
#   tmp = BC_List[Col==x,]
#   x1 = BCTransformInverse(data_in[,..x][[1]],tmp$L1,tmp$L2)
#   data_in[,(x):=x1]
#   
# }) %>% rbindlist


Covar = 'Lesion'
library(parallel)
###Set for AR
colnames(data_in)[16] = 'Stubby_root'
ycols[4] = 'Stubby_root'

LM_Results_AR_BC = mclapply(ycols,mc.cores = 10,FUN = function(Covar){
  setDT(data_in)
  ###Loop across xcols
  tmp = lapply(xcols,function(xcol){
    lapply(1:50,function(j) Run_Model(data_in,xcol,Covar,Model='lm',K=3)) 
    
  }) 

  ##merge xcols
  tmp =lapply(tmp,function(x){
    tmp = collect_results(x,Model = 'lm')
    R2s <- sapply(tmp,'[[',1)
    
    return(list(R2=mean(unlist(R2s))))
  })  
  x_out = data.table(Response=Covar,Covariate=xcols,R2s=unlist(tmp))
  return(x_out)
  }) %>% rbindlist

###Check loss code


library(caret)
library(Metrics)
xcol = colnames(data)[7]
# QR_Results_AR_BC = mclapply(ycols,mc.cores = 10,FUN = function(Covar){
  QR_Results_AR_BC = lapply(ycols,function(Covar){
  setDT(data_in)
  ###Loop across quantiles
  
  # tmp = lapply(seq(.1,1,.1),function(Quantile){
    tmp = mclapply(seq(.1,1,.1),mc.cores=10,FUN=function(Quantile){

        print(Quantile)
    ###Loop across xcols
    tmp = lapply(xcols,function(xcol){
      lapply(1:10,function(j){
        
                  
                  jj = try(Run_Model(data_in,xcol,Covar,Model='Quantile_Regression',K=4,Quantile=Quantile),silent = T)
                  if(class(jj)=='try-error'){
                    return(data.frame())
                  } else{
                    return(jj)
                  }
      # print(xcol)
    }) })
    
    ##merge xcols
    tmp =lapply(tmp,function(x){
      tmp = collect_results(x,Model = 'Quantile_Regression')
      R2s <- sapply(tmp,'[[',1)
      CLs <- sapply(tmp,'[[',3)
      return(list(R2=mean(unlist(R2s)),Check_Loss=mean(unlist(CLs))))
    })  
    
    R2s= sapply(tmp,'[[',1) %>% unlist 
    CLs= sapply(tmp,'[[',2) %>% unlist 
    x_out = data.table(Response=Covar,Covariate=xcols,Quantile=Quantile,R2s=R2s,CLs=CLs)
    return(x_out)
    
  }) %>% rbindlist
  
}) %>% rbindlist


plot = LM_Results_AR_BC[R2s>0]

plot = plot[is.finite(plot$R2s)]
plot[R2s>0,Net_R2:=sum(abs(R2s)),Covariate]

tmp = plot[rev(order(Net_R2)),unique(Covariate)]

plot$Covariate = factor(plot$Covariate,levels=tmp)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(plot,aes(fill=Response,x=Covariate,y=R2s))+ geom_col() +theme_classic(base_size = 22) +
 theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)) + labs(y='R2') +
   scale_fill_manual(values=cbPalette) #+ facet_wrap(~Response)# + ylim(0,NA) ,size=12

# facet_wrap(~Response)-08ewasrtyu890-
# size=NA

ggsave(plot=gg,file='~/Lab_Link/Cool Plots/LM1.png',dpi = 300,units = 'px',width = 1920,height = 1080,limitsize = F)


###QR Plot

plot = QR_Results_AR_BC

plot[,Max_R2:=max(R2s,na.rm = T),Covariate]
# plot = plot[R2s>0.02]

plot = plot[is.finite(plot$R2s)]
plot[R2s>0,Net_R2:=sum(abs(R2s)),Covariate]

tmp = plot[rev(order(Net_R2)),unique(Covariate)]

plot$Covariate = factor(plot$Covariate,levels=tmp)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

span=1
#
Covar = ycols
QR_plots_AR_BC = lapply(ycols,function(Covar){
  gg = ggplot(plot[Max_R2>0.05&Response%in%Covar],aes(color=Covariate,x=Quantile,y=R2s))  + geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=1) +#  cut(round(N_Ratio),16+
    theme_classic(base_size = 22)  +scale_fill_manual(values=cbPalette) + labs(title=Covar,y='Pseudo R2') #+ guides(color=F)
  file = paste0('/mnt/c/Users/elija/Desktop/Plots/Active/',Covar,'.png')
  ggsave(plot=gg,filename = file,width=7.84*2,dpi = 300,device = 'png')
})

QR_plots_NC

theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)) + labs(y='R2') +
  facet_wrap(~Response)# + ylim(0,NA) scale_fill_manual(values=cbPalette)

# facet_wrap(~Response)
# size=NA





###iRF Models 

###Regress Nemotodes Jump

data_in = data.table::copy(data_NC)
setDT(data)
library(geoR)

xcols_in = xcols#xcols_merged[which(xcols_merged!='topo_elevation')]
ycol='Lesion'

apply(data[,..ycols],2,function(x)sum(x!=0))

# setDT(data_in)
# 
# shuffled = data_in[,lapply(.SD,shuffle),.SDcols=xcols_in]
# colnames(shuffled) <- paste(colnames(shuffled),'_shuffled',sep='')
# data_in = cbind(data,shuffled)
# xcols_in = c(xcols_in,colnames(shuffled))
# 
# selected = LM_Results_BC[R2s>.05&Response=='Lesion',Covariate]
# xcols_in = str_detect(xcols_in,paste(selected,collapse = '|')) %>% xcols_in[.]
# xcols_in = str_detect(xcols_in,paste(selected,collapse = '_shuffled'),negate = T) %>% xcols_in[.]

tmp = Run_Model(data_in,xcols_in,ycol,K=4,Model='HAL',interactions = T,single = F,na_rm=T)

Regress_Nemo_iRF = lapply(ycols[c(2)],function(Covar){
  print(Covar)
  tmp = mclapply(1:10,mc.cores = 10,function(j) Run_Model(data_in,xcols_in,Covar,Model='HAL',K=4,interactions = T,na_rm=T,Regression = T)) 
  # data_in$Lesion =
  # tmp = Run_Model(data_in,xcols_in,ycol,K=4,Model='iRF',interactions = T,single = T,na_rm=T)
  
  x3 = sapply(tmp,function(x){
    sapply(x,function(x1){
      x1$Stats$R2
    })
    
  })# %>% as.numeric
  
  x3
  
  ###
  
  interactions = T
  
  x_out=merge_iterations(tmp,F)
  x_out$Nemotode = Covar
  x_out$Full_Object = tmp
  return(x_out)
}) #%>% rbindlist

RN = Regress_Nemo_iRF

# X = tmp1[[2]]
# 
# res = X$A_P
# X$Stats
# Acutal=res$Actual
# Predicted=res$Predicted
# 
# rsq(Actual,Predicted)

x3 = sapply(tmp,function(x){
  sapply(x,function(x1){
    x1$Stats$R2
  })
  
})# %>% as.numeric

x3
index = which(x3==max(x3),arr.ind=T)
Obj = RN[[1]]$Full_Object[[index[[2]]]][[index[[1]]]]

Model = Obj$Model$rf.list

setDF(data_in)
Pred = predict(model,data_in) #%>% BCTransformInverse(lambda1,lambda2)
setDT(data_in)

# Pred = BCTransformInverse(data_in$Lesion,lambda1,lambda2)


plot(Pred,data$Lesion)
rsq(data$Lesion,Pred)

rsq(predict(model,as.data.frame(data)),data$Lesion)
pred = predict(model,as.data.frame(data))

data$Lesion_Predicted = pred


data$Lesion_Predicted = pred



## interpolate and plot results ((IRF Nemo response))


grid = lapply(unique(data$field_name),function(x){
  Input = get_SF(as.data.frame(data_v[field_name==x]),c_cols = c('lng','lat'),to_UTM = T)
  
  box <- get_hull(Input,'concave',1.3)
  st_area(box)/Acre_M
  
  
  Input %>% get_hull('concave',1.3)
  
  grid = Input %>% get_hull('concave',1.3) %>%
    add_buffer(0) %>% 
    get_grid(c(Q_Acre,Q_Acre))
  return(grid)
  
  # 
  # Input = get_SF(as.data.frame(data_v),c_cols = c('lng','lat'),to_UTM = T)
  # 
  # Variable="CATION EXCHANGE"
  # Variable='Lesion'
  # 
  # # grid = st_coordinates(Input)
  # 
  # tmp3 = SF_Krig(Variable,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
  #                cutoff=180,len=10,cv=NA)
  # tmp = tmp3[[1]]
  # mean(tmp[,Variable][[1]])
  # print(mean(tmp3[[3]]))
  # # OG_CRS = 4326
  # tmp2 = st_transform(tmp, OG_CRS)
  # # tmp2=tmp
  # # .01
  # # Variable = paste0(Variable,'_SD') 
  # # Variable = Variable %>% str_remove('_SD')
  # plot3 = SF_Raster(tmp2,Variable,F,q=.003,rev=T)[[1]]
  # 
  # 
  # 
  # gg =ggplot(plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
  #   scale_fill_continuous(type = 'viridis')# + 
  # return(gg)
  # 
}) %>% rbindlist %>% as.data.frame() 



###Test run

Input = get_SF(as.data.frame(data),c_cols = c('lng','lat'),to_UTM = T)

# Variable="CATION EXCHANGE"
# Variable="Root_knot"
Variable='Lesion_Predicted'
Variable='Lesion'
# grid = st_coordinates(Input)

tmp3 = SF_Krig(Variable,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
               cutoff=180,len=10,cv=1,No_Transform = F)
tmp = tmp3[[1]]
mean(tmp[,Variable][[1]])
print(mean(tmp3[[3]]))
# OG_CRS = 4326
tmp2 = st_transform(tmp, OG_CRS)
# tmp2=tmp
# .01
# Variable = paste0(Variable,'_SD') 
# Variable = Variable %>% str_remove('_SD')
plot3 = SF_Raster(tmp2,Variable,F,q=.003,rev=F)[[1]]


code = scales::viridis_pal()(100)[20]
ggplot(plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
  +scale_fill_continuous(type = 'viridis') + theme(panel.background = element_rect(fill = code,
                                    colour = code))
    



df <- data.frame(longitude =Data$Long_Site, 
                 latitude = Data$Lat_Site)


coordinates(df) <- ~longitude+latitude
leaflet(df)  %>% addTiles() %>% addCircleMarkers(
  radius = 3,
  color = 'dodgerblue2',
  stroke = FALSE, fillOpacity = 0.5
)


r = raster::raster(plot4)
pal <- colorNumeric('viridis', values(r),
                    na.color = "transparent")
leaflet() %>% addTiles() %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addRasterImage(r, colors = pal, opacity = .8) %>%
  addLegend(pal = pal, values = values(r),
            title = "Surface temp") 

# + 

ggplot() + geom_raster(data=plot3,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis') + #geom_sf(data=Nematicide_geom,fill=NA,color=alpha("grey",0.2)) + 
  geom_point(data=Input,aes(x=Long,y=Lat,color=Input[,'Lesion'][[1]]),alpha=1,size=5) + scale_color_continuous(type = 'viridis') + guides(color='none')


##

tmp = lapply(Regress_Nemo_iRF,'[[',)
Regress_Nemo_iRF[[1]]
data.table(Nematode=sapply(Regress_Nemo_iRF,'[[',4),R2=sapply(Regress_Nemo_iRF,'[[',1))

sapply(Regress_Nemo_iRF,'[[',1)  


###Yield_Models Jump

library(tictoc)
library(iRF)


shuffle <- function(x){
  tmp1 <- x[sample(length(x))]
  return(tmp1)
}



ycol = 'voxelized_yield'
# ycol = 'Residuals'

###transforms nemos

data_in = data.table::copy(data_AR)
data_in = data.table::copy(data_NC)

###AR
# colnames(data_in)[16] = 'Stubby_root'
# ycols[4] = 'Stubby_root'


xcols_merged = c(xcols,ycols)


###Nemo only
xcols_in = ycols


xcols_in = xcols_merged#xcols_merged[which(xcols_merged!='topo_elevation')]

setDT(data_in)

  setDT(data)
  
  tmp = mclapply(1:10,mc.cores = 10,function(j) Run_Model(data_in,xcols_in,ycol,Model='iRF',K=10,interactions = T,na_rm=T,
                                                          return_folds = T,shuffle=T,CV_Type = 'Stratified')) 
  # tmp1 = Run_Model(data_in,xcols_in,ycol,Model='iRF',K=4,interactions = T,na_rm=T,shuffle = T)
  Yield_NC_All = Run_Model(data_in,xcols_in,ycol,Model='iRF',single=T,interactions = T,na_rm=T,shuffle = T,nshuffle = 10)
  
  folds = lapply(tmp,'[[',2)
  tmp = lapply(tmp,'[[',1)
  
  interactions = T
  
  merge_iterations = function(x,interactions=F){
    x1 = collect_results(x,Model = 'iRF',interactions=interactions)
    R2s <- sapply(x1,'[[',2)
    
    GI <- rbindlist(lapply(x1,'[[',1))
    mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
    
    
    if(interactions==T){
      Iters=lapply(x1,'[[',4) %>%  rbindlist
      x = lapply(x1,'[[',4)
      Iters_out <- Iters[,lapply(.SD,mean,na.rm=T),by=Interaction][rev(order(Prevalence))]
      # out <- list(mGI=mGI,sR2=sR2,m_out=m_out,Iters=Iters_out)
      return(list(R2=mean(unlist(R2s)),mGI=mGI,Iters=Iters_out))
      
    } else{
      # out <- list(mGI=mGI,sR2=sR2,m_out=m_out)
      return(list(R2=mean(unlist(R2s)),mGI=mGI))
      
    }}
  
  x_out=merge_iterations(tmp,F)
  x_out$Full_Object = tmp
  
#   return(x_out)
# } #%>% rbindlist

# iRF_Results_Residuals = x_out
# iRF_Results_AR = x_out
# iRF_Nemo_Only_AR = x_out
  iRF_Nemo_Only_NC_Fall = x_out
# iRF_Results = iRF_Results_AR
iRF_Nemo_Only = x_out

iRF_Results$R2
iRF_Nemo_Only$R2

plot(iRF_Results$mGI$Importance)
index = iRF_Results$mGI$Covar %>% str_detect('shuffled') %>% which %>% min 
abline(v=index)
iRF_Results$mGI[1:(index-1),]

plot(iRF_Nemo_Only$mGI$Importance)
index = iRF_Nemo_Only$mGI$Covar %>% str_detect('shuffled') %>% which %>% min 
abline(v=index)
iRF_Nemo_Only$mGI[1:(index-1),]


x_out=x_out
x3 = sapply(x_out$Full_Object,function(x){
  sapply(x,function(x1){
    x1$Stats$R2
    # x1$Stats$mae
  })
  
})# %>% as.numeric

x3 %>% as.vector %>% sort %>% plot


x3
apply(x3,2,mean)
plot_folds(x_out,ind1=3,K=4,Iters=10,ntop=5,span=1)

plot_folds <- function(x_out,ind1=4,K=4,Iters=10,ntop=3,adds=NULL,span=.5){
  
  grid = expand.grid(1:K,1:Iters)
  tmp = apply(grid,1,function(x){
    index1 = x
    comb = x_out[[4]][[index1[2]]][[index1[1]]]
    R2 = comb$Stats$R2
    AP = comb$A_P
    model = comb$Model
    out = cbind(AP,R2,paste(x,collapse = '_'))
    colnames(out)[4] = 'Model_Number'
    return(list(out,model))
    
    
  })
  tmpAP = lapply(tmp,'[[',1) %>% rbindlist
  AP_plot = tmpAP[str_detect(Model_Number,paste0('_',ind1))]
  tmpm = lapply(tmp,'[[',2)
  
  sort_imp = function(x){
    x2 <-x$importance
    data.table(Covar=rownames(x2),Importance=as.numeric(x2[,1]))[rev(order(Importance))]
  }
  
  
  
  models = lapply(1:K,function(x1){
    gmodel = which(grid[,1]==ind1&grid[,2]==x1) %>% tmpm[[.]]
    imp = gmodel$rf.list %>% sort_imp()
    return(list(Model=gmodel,Imp=gimp))
  }) 
  
  GI = lapply(models,'[[',2) %>% rbindlist
  mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
  
  
  
  
  plot1 = lapply(1:K,function(x1){
    
    x = folds[[ind1]]
    #training only
    # gplot = data_in[-x[[x1]],]
    #test only
    gplot = data_in[x[[x1]],]
    gplot$Group = paste0('Fold_',x1)
    return(gplot)
    
  }) %>% rbindlist
  # nrow(plot1)/4
  
  
  sel = c(ycol,mGI$Covar[1:ntop],adds)
  sel1 = c(mGI$Covar[1:ntop],adds)
  
  plot2 = melt(plot1,measure.vars =sel1)
  plot = melt(plot1,measure.vars =sel)
  
  # index = c(xcols,ycols,ycol,'Group')
  # ggplot(plot[,..index],aes(x=voxelized_yield,fill=Group)) + geom_histogram() + theme_classic()
  
  ###Distribution comparisons
  gg1 = ggplot(plot,aes(x=value,fill=Group)) + geom_histogram()   + facet_wrap(~variable,scales='free_x') + theme_classic() +
    scale_fill_discrete(labels = as.character(round(x3[,ind1],3))) + labs(fill='Fold R^2')# + geom_density(alpha=.3)
  
  ###Top predictors vs response
  gg2 = ggplot(plot2,aes(x=value,y=voxelized_yield,col=Group)) + geom_point(size=1)   + facet_wrap(~variable,scales='free_x') + theme_classic() +
    scale_color_discrete(labels = as.character(round(x3[,ind1],3))) + labs(color='Fold R^2') +
    geom_line(stat='smooth',se = T,method = 'loess',span=span) 
  
  
  ###Residuals vs fitted
  gg3 = ggplot(AP_plot,aes(x=Predicted,y=Actual-Predicted,col=Model_Number)) + geom_point(size=1) +# geom_density_2d_filled() +
    geom_line(stat='smooth',se = T,method = 'loess',span=span) + theme_classic() + labs(color='Fold R^2') +#geom_boxplot()#geom_point()
    scale_color_discrete(labels = as.character(round(x3[,ind1],3)))
  
  return(list(gg1,gg2,gg3))
}

index1 = which(x3==max(x3),arr.ind=T)
comb = x_out[[index]][[2]][[index1[2]]][[index1[1]]]

model = comb$Model

###Iteractions

sort_iters = function(x2){
  data.table(Interaction=x2$int,Prevalence=x2$prevalence,Precision=x2$precision,cpe=x2$cpe,Stability=x2$stability)[rev(order(Prevalence))]
}

iters = sort_iters(Yield_NC_All$Model$interaction)
# iters = iRF_Results_AR$Iters[rev(order(Prevalence))]
index = iters$Interaction %>% str_detect('shuffled') %>% which %>% min 
iters[1:(index-1)]
sel_iters = iters$Interaction[1:(index-1)]


iters = sort_iters(Yield_AR_Nemo$Model$interaction)
# iters = iRF_Nemo_Only$Iters[rev(order(Prevalence))]
index = iters$Interaction %>% str_detect('shuffled') %>% which %>% min 
iters[1:(index-1)]
sel_iters = iters$Interaction[1:(index-1)]

library(reshape2)
library(plotly)

###Validate interactions


HAL_Iters = mclapply(sel_iters,mc.cores = 1,FUN = function(Iters){
  setDT(data)
  
  
  
  tmp1 = sapply(xcols_merged,function(x){
    tmp = str_replace(Iters,x,'ThisSpotonly') %>% tstrsplit('_') 
    if(tmp[[1]]=='ThisSpotonly'){
      return(x)
    }
    if(tmp[[length(tmp)]]=='ThisSpotonly'){
      return(x)
    }
  }) %>% sapply(is.null) %>% '*'(-1)
  iters = which(tmp1==0) %>% names
  
  # ###Individual modesls
  # iter1 = Run_Model(data,iters[[1]],ycol,K = 4,Model='HAL',single=F)
  # iter2 = Run_Model(data,iters[[2]],ycol,K = 4,Model='HAL',single=F)
  
  merge_fun = function(x){
    tmp = collect_results(x,Model = 'lm')
    R2s <- sapply(tmp,'[[',1)
    
    return(list(R2=mean(unlist(R2s))))
  }
  
  ###Combined
  setDT(data_in)
  
  comb = lapply(1:1,function(z) Run_Model(data_in,iters,ycol,K=10,Model='HAL',CV_Type = 'Stratified'))
  tmp =  merge_fun(comb)
  Full_HAL = Run_Model(data_in,iters,ycol,single=T,Model='HAL',CV_Type = 'Stratified')
  #merge iterations
  x_out = data.table(Interaction=Iters,R2s=unlist(tmp))
  return(list(Summary=x_out,Models=comb,Full=Full_HAL))
})# %>% rbindlist



plots_Nemo = plots
Hal_Results_NC = lapply(HAL_Iters,'[[',1) %>% rbindlist
Hal_Results_Full = lapply(HAL_Iters,'[[',3)
HAL_Iters

plots_Nemo = plots
Hal_Results_Nemo = HAL_Iters

# x_out=iRF_Nemo_Only_NC_Fall
x3 = sapply(HAL_Iters[[1]][[2]],function(x){
  sapply(x,function(x1){
    x1$Stats$R2
  })
  
}) # %>% as.numeric

x3
x3 %>% as.vector %>% sort %>% plot
apply(x3,2,mean)

index = which(x3==max(x3),arr.ind=T)
# model = iRF_Nemo_Only_NC_Fall$Full_Object[[index[[2]]]][[index[[1]]]]






###get interaction plots
plots = lapply(sel_iters,function(Iters){
  print(as.character(Iters))
  tmp1 = sapply(xcols_merged,function(x){
    tmp = str_replace(Iters,x,'ThisSpotonly') %>% tstrsplit('_') 
    if(tmp[[1]]=='ThisSpotonly'){
      return(x)
    }
    if(tmp[[length(tmp)]]=='ThisSpotonly'){
      return(x)
    }
  }) %>% sapply(is.null) %>% '*'(-1)
  iters = which(tmp1==0) %>% names
  
  ###
  index=which(sel_iters == Iters)
  sapply(HAL_Iters[[index]][[2]],function(x){
    sapply(x,function(x1){
      x1$Stats$R2
    })
    
  }) # %>% as.numeric
  
  # R2=max(x3)
  
  # index1 = which(x3==max(x3),arr.ind=T)
  # comb = HAL_Iters[[index]][[2]][[index1[2]]][[index1[1]]]
  index1 = which(sel_iters==Iters)
  comb = Hal_Results_Full[[index1]]
  
  model = comb$Model
  setDT(data)
  tmp2=data[,..iters]
  grid = expand.grid(seq(min(tmp2[,1]),max(tmp2[,1]),length.out=100),
                     seq(min(tmp2[,2]),max(tmp2[,2]),length.out=100))
  pred = predict(model,new_data=grid)
  
  surface = cbind(grid,pred)
  surface = t(acast(surface, Var1 ~ Var2, value.var ='pred' ))
  
  axx <- list(
    title = iters[[1]]
  )
  axy <- list(
    title = iters[[2]]
  )
  axz <- list(
    title = ycol
  )
  
  fig <-     plot_ly(x=as.numeric(colnames(surface)),y=as.numeric(as.character(rownames(surface))),z = ~surface) %>% add_surface()
  fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  
  
  ###Indivdual plots
  

  pred_grid = cbind(grid,pred)
  
  # grid = crossing(tmp2)
  # pred_grid = predict(model,new_data=grid) %>% cbind(grid,.)
  # colnames(pred_grid) = c('Var1','Var2','pred')
  setDT(pred_grid)
  
  pd1 = pred_grid[,.(Yhat=mean(pred)),Var1]
  
  pd2 = pred_grid[,.(Yhat=mean(pred)),Var2]
  
  # geom_line(stat='smooth',se = T,method = 'loess',span=.5) + labs(x=iters[[1]],y=ycol)
  gg1 = ggplot(pd1,aes(x=Var1,y=Yhat)) + geom_line() + labs(x=iters[[1]],y=ycol) + theme_classic(base_size = 20)
  gg2 = ggplot(pd2,aes(x=Var2,y=Yhat)) + geom_line() + labs(x=iters[[2]],y=ycol) + theme_classic(base_size = 20)
  
  

  lapply(1:2,function(I){
    if(I==1) gg = gg1
    if(I==2) gg = gg2
    if(I==3){
      orca(fig,paste0('/mnt/c/Users/elija/Desktop/Plots/Active/HAL_Ints_AR_All/',paste(iters,collapse='_'),'.png'))
    } else{
      file = paste0('/mnt/c/Users/elija/Desktop/Plots/Active/HAL_Ints_AR_All/',iters[[I]],'.png')
      ggsave(plot=gg,filename = file,width=7.84*2,dpi = 300,device = 'png')
    }
    
  })
  
  return(list(surface=fig,x=gg1,y=gg2))
  
})


plots_AR_All = plots



###Yield residuals analysis Jump
x_out=iRF_Nemo_Only_NC_Fall
x3 = sapply(x_out$Full_Object,function(x){
  sapply(x,function(x1){
    x1$Stats$R2
  })
  
})# %>% as.numeric
x3
x3 %>% as.vector %>% sort %>% plot


index = which(x3==max(x3),arr.ind=T)
model = iRF_Nemo_Only_NC_Fall$Full_Object[[index[[2]]]][[index[[1]]]]

Pred = predict(model$Model$rf.list,as.data.frame(data_in))
Residuals = data$voxelized_yield-Pred




###Lattice Networks WIP

Lattice_Iters = mclapply(sel_iters,mc.cores = 5,FUN = function(Iters){
  setDT(data)
  
  
  
  tmp1 = sapply(xcols,function(x){
    tmp = str_replace(Iters,x,'ThisSpotonly') %>% tstrsplit('_') 
    if(tmp[[1]]=='ThisSpotonly'){
      return(x)
    }
    if(tmp[[length(tmp)]]=='ThisSpotonly'){
      return(x)
    }
  }) %>% sapply(is.null) %>% '*'(-1)
  iters = which(tmp1==0) %>% names
  
  # ###Individual modesls
  # iter1 = Run_Model(data,iters[[1]],ycol,K = 4,Model='HAL',single=F)
  # iter2 = Run_Model(data,iters[[2]],ycol,K = 4,Model='HAL',single=F)
  
  merge_fun = function(x){
    tmp = collect_results(x,Model = 'lm')
    R2s <- sapply(tmp,'[[',1)
    
    return(list(R2=mean(unlist(R2s))))
  }
  
  ###Combined
  
  comb = lapply(1:10,function(z) Run_Model(data,iters,ycol,K=4,Model='HAL')) %>% merge_fun
  
  #merge iterations
  x_out = data.table(Interaction=Iters,R2s=unlist(comb))
  return(x_out)
}) %>% rbindlist







###Functions
library(hal9001)
###Collect 
collect_results <- function(Results,Model,Regression=T,interactions=F){
  # Results <- Results_HAL
  Collect <- lapply(Results,function(z){
    
    if(Model=='iRF'){
      if(Regression==T){
        sR2 <- sapply(z,function(x){
          x1 <- '[['(x,1)
          x1$R2
        })
      }
    } else{
      
      if(Regression==T){
        sR2 <- sapply(z,function(x){
          x1 <- '[['(x,1)
          x1$R2
        })
      } else{
        sR2 <- sapply(z,function(x){
          x1 <- '[['(x,1)
          x1$Accuracy
        })
        
      }
      
    }
    
    
    #Get Model object
    m_out <- lapply(z,'[[',2)
    
    if(Model=='DRF'){
      GI <- rbindlist(lapply(z,function(x){
        x2 <-x$Importance
        data.frame(Covar=rownames(x2),Importance=as.numeric(x2[,1]))
      }))
      
      mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
      
      Qstats = lapply(z,'[[',5) %>% rbindlist()
      Qstats = Qstats[,.(pR2=mean(pR2,na.rm=T),CL=mean(CL,na.rm=T)),by=Quantile]
      
      out <- list(mGI=mGI,sR2=sR2,Qstats=Qstats,m_out=m_out)
      
      return(out)
    } 
    
    
    if(Model=='iRF'){
      GI <- rbindlist(lapply(z,function(x){
        x1 <- '[['(x,2)
        x2 <- x1$rf.list$importance
        data.frame(Covar=rownames(x2),Importance=as.numeric(x2))
      }))
      
      mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
      
      tmp5 = lapply(z,function(x){
        x1 <- '[['(x,2)
        x2 <- x1$interaction
        data.frame(Interaction=x2$int,Prevalence=x2$prevalence,Precision=x2$precision,cpe=x2$cpe,Stability=x2$stability)
      })
      
      
      if(interactions==T){
        Iters <- lapply(z,function(x){
          x1 <- '[['(x,2)
          x2 <- x1$interaction
          data.table(Interaction=x2$int,Prevalence=x2$prevalence,Precision=x2$precision,cpe=x2$cpe,Stability=x2$stability)
        })
        # index = abs(sR2)-mean(sR2) + sd(sR2) < 0
        index = sR2 < mean(sR2) | sR2 < 0
        Iters = Iters[!index] %>% rbindlist
        
        Iters_out <- Iters[,lapply(.SD,mean,na.rm=T),by=Interaction][rev(order(Prevalence))]
        out <- list(mGI=mGI,sR2=sR2,m_out=m_out,Iters=Iters_out)
        
      } else{
        out <- list(mGI=mGI,sR2=sR2,m_out=m_out)
      }
      return(out)
    } 
    
    if(Model=='HAL'){
      
  
      out <- list(sR2=sR2,m_out=m_out)
      return(out)
    }
    
    if(Model%in%c('lm')){
      out <- list(sR2=sR2,m_out=m_out)
      return(out)
    }
    
    if(Model%in%c('Quantile_Regression')){
      
      sCL <- sapply(z,function(x){
        x1 <- '[['(x,1)
        x1$Check_Loss
      })
      
      out <- list(sR2=sR2,m_out=m_out,sCL=sCL)
      return(out)
    }
    
  })
  return(Collect)
}

###Run_Hal

K <- 5
# Run_HAL <- function(data,cols_x,cols_y,K=5,Regression=T,upsample=F,single=F){
#   if(upsample==T){
#     setDT(data)
#     data$resampled <- F
#     data$ID <- 1:nrow(data)
#     
#     tb <- table(data[,..cols_y])
#     which_up <- as.numeric(names(which.min(tb)))
#     index <- data[,.I[.SD==which_up],.SDcols=cols_y]
#     n <-  tb[-which.min(tb)] - tb[which.min(tb)]
#     x1 <- data[sample(index,size=n,replace = T)]
#     data1 <- rbind(data,x1)
#     
#     folds <- groupKFold(data1$ID,k=K)
#     
#     out <- lapply(1:K,function(k){
#       index <- folds[[k]]
#       index1 <- 1:nrow(data1)
#       fold <- index1[-index]
#       tmp <- HAL_run(data1,folds,cols_x,cols_y,Regression=Regression)
#     })
#     
#     
#   } else if(single==T){
#     out <- HAL_run(data,cols_x,cols_y,Regression=Regression)
#     
#   }else{
#     folds <- createFolds(1:nrow(data),K)
#     out <- lapply(1:K,function(k){
#       tmp <- HAL_run(data,folds[[k]],cols_x,cols_y,Regression=Regression)
#     })
#   }
#   
#   
#   return(out)
# }



HAL_run <- function(data_in,xcols,ycols,fold_index=NA,Regression=T,single=F){
  setDF(data_in)
  if(single==F){
    x=data_in[-fold_index,xcols,drop=F]
    y=data_in[-fold_index,ycols]
    
    xtest=data_in[fold_index,xcols,drop=F]
    ytest=data_in[fold_index,ycols]
  } else{
    x=data_in[,xcols,drop=F]
    y=data_in[,ycols]
  }
  
  
  
  # ml_hal_fit <- try(fit_hal(X = x, Y = y,fit_type='glmnet', yolo = TRUE,n_folds = 5),silent = T)
  if(Regression==T){
    model <- fit_hal(X = x, Y = y,fit_type='glmnet', yolo = F,n_folds = 5)
  } else{
    model <- fit_hal(X = x, Y = y,fit_type='glmnet',family='binomial', yolo = F,n_folds = 5)
    
    
  }
  
  
  
  if(Regression==T){
    if(single==F){
      pred <- predict(model, new_data=xtest)
      result <- data.frame(predicted = pred,
                           actual = ytest)
    } else{
      pred <- predict(model, new_data=x)
      result <- data.frame(predicted = pred,
                           actual = y)
      
      range= seq(min(x),max(x),length.out = 100)
      sim_out = predict(model,new_data=range)
      sim_out = list(x=range,y=sim_out)
    }
    
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    rsq <- function(actual, predicted){
      RSS <- sum((actual-predicted)^2)
      TSS <- sum((actual-mean(actual))^2)
      R2 <- (1-(RSS/TSS))
      return(R2)
      
    } 
    
    mae_iRF <- mae(Actual,Predicted)
    mdae_iRF <- mdae(Actual,Predicted)
    R2_iRF <- rsq(Actual,Predicted)
    R2_iRF
    if(single==F){
      
      collate <- list(Stats=list(mae=mae_iRF,mdae=mdae_iRF,R2=R2_iRF),Model=model,A_P=data.table(Actual,Predicted)) 
    } else{
      
      collate <- list(Stats=list(mae=mae_iRF,mdae=mdae_iRF,R2=R2_iRF),Model=model,
                      A_P=data.table(Actual,Predicted),sim_out=sim_out) 
    }
    
  } else{
    
    preds <- predict(object = model, new_data = xtest,type='classification')
    result <- data.frame(predicted = pred,
                         actual = ytest)
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    Accuracy = sum(Actual==Predicted)/length(Actual)
    
    collate <- list(Stats=list(Accuracy=Accuracy),Model=model,A_P=data.table(Actual,Predicted))
  }
  
  
  return(collate)
}





DLN_run <- function(data_in,xcols,ycols,fold_index=NA,Regression=T,single=F){
  setDF(data_in)
  if(single==F){
    x=data_in[-fold_index,xcols,drop=F]
    y=data_in[-fold_index,ycols]
    
    xtest=data_in[fold_index,xcols,drop=F]
    ytest=data_in[fold_index,ycols]
  } else{
    df=data_in[,c(xcols,ycols),drop=F]
    colnames(df) = c('Var1','Var2','Target')
    
  }
  
  
  
  # ml_hal_fit <- try(fit_hal(X = x, Y = y,fit_type='glmnet', yolo = TRUE,n_folds = 5),silent = T)
  if(Regression==T){
    model <- py$DNL_Function(df,'none',BS = 10,Epochs = 100,xnew=val$Dose)

  } else{
    model <- fit_hal(X = x, Y = y,fit_type='glmnet',family='binomial', yolo = TRUE,n_folds = 5)
    
    
  }
  
  
  
  if(Regression==T){
    if(single==F){
      pred <- predict(model, new_data=xtest)
      result <- data.frame(predicted = pred,
                           actual = ytest)
    } else{
      pred <- predict(model, new_data=x)
      result <- data.frame(predicted = pred,
                           actual = y)
      
      range= seq(min(x),max(x),length.out = 100)
      sim_out = predict(model,new_data=range)
      sim_out = list(x=range,y=sim_out)
    }
    
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    rsq <- function(actual, predicted){
      RSS <- sum((actual-predicted)^2)
      TSS <- sum((actual-mean(actual))^2)
      R2 <- (1-(RSS/TSS))
      return(R2)
      
    } 
    
    mae_iRF <- mae(Actual,Predicted)
    mdae_iRF <- mdae(Actual,Predicted)
    R2_iRF <- rsq(Actual,Predicted)
    R2_iRF
    if(single==F){
      
      collate <- list(Stats=list(mae=mae_iRF,mdae=mdae_iRF,R2=R2_iRF),Model=model,A_P=data.table(Actual,Predicted)) 
    } else{
      
      collate <- list(Stats=list(mae=mae_iRF,mdae=mdae_iRF,R2=R2_iRF),Model=model,
                      A_P=data.table(Actual,Predicted),sim_out=sim_out) 
    }
    
  } else{
    
    preds <- predict(object = model, new_data = xtest,type='classification')
    result <- data.frame(predicted = pred,
                         actual = ytest)
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    Accuracy = sum(Actual==Predicted)/length(Actual)
    
    collate <- list(Stats=list(Accuracy=Accuracy),Model=model,A_P=data.table(Actual,Predicted))
  }
  
  
  return(collate)
}



###Run iRF
# K <- 5
# Run_iRF <- function(data,cols_x,cols_y,K=5,Regression=T,upsample=F,interactions=F){
#   
#   if(upsample==T){
#     setDT(data)
#     data$resampled <- F
#     data$ID <- 1:nrow(data)
#     
#     tb <- table(data[,..cols_y])
#     which_up <- as.numeric(names(which.min(tb)))
#     index <- data[,.I[.SD==which_up],.SDcols=cols_y]
#     n <-  tb[-which.min(tb)] - tb[which.min(tb)]
#     x1 <- data[sample(index,size=n,replace = T)]
#     data1 <- rbind(data,x1)
#       
#     folds <- groupKFold(data1$ID,k=K)
#     
#     out <- lapply(1:K,function(k){
#       index <- folds[[k]]
#       index1 <- 1:nrow(data1)
#       fold <- index1[-index]
#       iRF_run(data1,fold,cols_x,cols_y,ncores = 1,Regression=Regression,interactions=interactions)
#       
#     })
#    
#     
#   }
#   
#   else{
#     folds <- createFolds(1:nrow(data),K)
#     out <- lapply(1:K,function(k){
#       iRF_run(data,folds[[k]],cols_x,cols_y,ncores = 1,Regression=Regression,interactions=interactions)
#     })
#   }
#   
#   
#   return(out)
# }



iRF_run <- function(data_in,fold_index,xcols,ycols,ncores=10,Regression=T,interactions=F,single=F){
  setDF(data_in)
  rit.param <- list(depth=5, nchild=2, ntree=1000)
  tic()
  if(single==T){
    x=data_in[,xcols,drop=F]
    y=data_in[,ycols]
    
    xtest=data_in[,xcols,drop=F]
    ytest=data_in[,ycols]
  } else{
    x=data_in[-fold_index,xcols,drop=F]
    y=data_in[-fold_index,ycols]
    
    xtest=data_in[fold_index,xcols,drop=F]
    ytest=data_in[fold_index,ycols]
  }
  
  
  
  
  
  model <-  iRF(x = x,
                y = y,
                # xtest = xtest,
                # ytest = ytest,
                n.iter = 10,n.core = ncores,
                select.iter = interactions,
                signed=F,
                oob.importance = TRUE,
                type = 'randomForest',
                rit.param=rit.param)
  
  toc()
  
  if(Regression==T){
    pred <- predict(model$rf.list, xtest)
    result <- data.frame(predicted = pred,
                         actual = ytest)
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    rsq <- function(actual, predicted){
      RSS <- sum((actual-predicted)^2)
      TSS <- sum((actual-mean(actual))^2)
      R2 <- (1-(RSS/TSS))
      return(R2)
      
    } 
    
    mae_iRF <- mae(Actual,Predicted)
    mdae_iRF <- mdae(Actual,Predicted)
    R2_iRF <- rsq(Actual,Predicted)
    R2_iRF
    
    collate <- list(Stats=list(mae=mae_iRF,mdae=mdae_iRF,R2=R2_iRF),Model=model,A_P=data.table(Actual,Predicted)) 
  } else{
    pred <- predict(model$rf.list, xtest)
    result <- data.frame(predicted = pred,
                         actual = ytest)
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    Accuracy = sum(Actual==Predicted)/length(Actual)
    
    collate <- list(Stats=list(Accuracy=Accuracy),Model=model,A_P=data.table(Actual,Predicted))
  }
  
  
  return(collate)
}




###DRF 
DRF_run <- function(data_in,fold_index,xcols,ycols,ncores=10,Regression=T,interactions=F){
  library(drf)
  setDF(data_in)

  x=data_in[-fold_index,xcols,drop=F]
  y=data_in[-fold_index,ycols]
  
  xtest=data_in[fold_index,xcols,drop=F]
  ytest=data_in[fold_index,ycols]
  
  model <- drf(X = x, Y = as.matrix(y),num.threads = 1)
  
  drf_importance <- as.data.frame(variable_importance(model))
  rownames(drf_importance) <- colnames(x)
  #names <- colnames(X.train)[rev(order(imp))]
  
  # 
  # Quantile_Predictions <- drf:::predict.drf(model, xtest,transformation=NULL, functional = "quantile",quantiles=seq(0.01,.99,.01))
  # Mean_Predictions <- drf:::predict.drf(model, xtest,transformation=NULL, functional = "mean")
  # 
  # drf_Out <- list(Actual,Quantile_Predictions,Mean_Predictions,drf.forest,drf_importance)#,samples)
  # names(drf_Out) <- c('Actual','Quantile_Predictions','Mean_Predictions','drf.forest','drf_importance')#,'samples')
  # 
  # 
  if(Regression==T){
    
    Quantiles = seq(0.01,.99,.01)
    
    Quantile_Predictions <- drf:::predict.drf(model, xtest,transformation=NULL, functional = "quantile",quantiles=Quantiles)
    Mean_Predictions <- drf:::predict.drf(model, xtest,transformation=NULL, functional = "mean")
    
    Preds = drop(Quantile_Predictions$quantile)
    colnames(Preds) = colnames(Preds) %>% str_remove_all('q=')
    Preds = as.data.table(Preds)
    
      
      rsq <- function(actual, predicted){
        RSS <- sum((actual-predicted)^2)
        TSS <- sum((actual-mean(actual))^2)
        R2 <- (1-(RSS/TSS))
        return(R2)
      } 
      
      
      ###Input full true vector, and the predicted quantile
      
      Check_Loss <- function(actual,prediction,theta){
        t=(actual-prediction)
        p_theta <- function(t,theta){
          if(t>=0){
            p_theta <- t*theta
          } else{
            p_theta = (-(1-theta))*t
            
            return(p_theta)
          }
        }
        collect <- lapply(t,p_theta,theta=theta)
        return(sum(as.numeric(collect)))
      }
      
      
      
      ###loop through quantiles
      
      Q_Out = lapply(Quantiles,function(Quantile){
        library(quantreg)
        library(stringr)
        
        
        model_intercept = rq(as.formula(paste0(ycols,'~','1')),data=data_in[,c(xcols,ycols)],tau=Quantile)
        col = as.character(Quantile)
        pred <- Preds[,..col][[1]]
        pred0 <- predict(model_intercept, xtest)
        
        result <- data.frame(predicted = pred,
                             predicted0=pred0,
                             actual = ytest)
        
        Actual <- result$actual
        Predicted <- result$predicted
        Predicted0 = result$predicted0
        
        
        
        
        Check_Loss <- Check_Loss(Actual,Predicted,Quantile)
        # 
        rho <- function(u,tau=.5)u*(tau - (u < 0))
        Vhat <- sum(rho(Actual-Predicted, Quantile))
        V0 <- sum(rho(Actual-Predicted0, Quantile))
        R2 <- 1-(Vhat/V0)
        
        out = data.table(Quantile=Quantile,pR2=R2,CL=Check_Loss)
        return(out)
      }) %>% rbindlist
      
      
      mae <- NA
      mdae <- NA
      
      
      ###For conditional mean  
      result <- data.frame(predicted = Mean_Predictions$mean[,1],
                           actual = ytest)
      
      Actual <- result$actual
      Predicted <- result$predicted
      
      R2 <- rsq(Actual,Predicted)
      
    
    
    collate <- list(Stats=list(mae=mae,mdae=mdae,R2=R2,Check_Loss=NA),Model=model,A_P=data.table(Actual,Predicted),Q_Preds = Preds,Q_Stats=Q_Out,Importance=drf_importance) 
  } 
  
  
  return(collate)
}



###General modelling function


Run_Model <- function(data,cols_x,cols_y,Model,K=5,Regression=T,upsample=F,Quantile=NA,interactions=F,ncores=1,single=F,na_rm=F,shuffle=F,return_folds=F,nshuffle=length(cols_x)/2,CV_Type='Simple'){
  setDT(data)
  
  if(na_rm==T){
    setDT(data)
    index = which(is.na(data[,..cols_x,drop=F]),arr.ind=T)
    if(nrow(index)>0){
      index=index[,1]
      data = data[-index,]
    }
  }
  if(shuffle==T){
    
    shuffled = data[,lapply(.SD,shuffle),.SDcols=sample(cols_x,nshuffle)]
    colnames(shuffled) <- paste(colnames(shuffled),'_shuffled',sep='')
    data = cbind(data,shuffled)
    cols_x = c(cols_x,colnames(shuffled))
    
  }
  if(upsample==T){
    setDT(data)
    data$resampled <- F
    data$ID <- 1:nrow(data)
    
    tb <- table(data[,..cols_y])
    which_up <- as.numeric(names(which.min(tb)))
    index <- data[,.I[.SD==which_up],.SDcols=cols_y]
    n <-  tb[-which.min(tb)] - tb[which.min(tb)]
    x1 <- data[sample(index,size=n,replace = T)]
    data1 <- rbind(data,x1)
    
    folds <- groupKFold(data1$ID,k=K)
    if(!is.na(k)){
      out <- lapply(1:K,function(k){
        index <- folds[[k]]
        index1 <- 1:nrow(data1)
        fold <- index1[-index]
        tmp <- iRF_run(data1,fold,cols_x,cols_y,ncores = 1,Regression=Regression,interactions=interactions)
      })
    } else{
      
    }
    
    
    
  }
  else if(single==T){
    if(Model=='HAL'){
      return(HAL_run(data,cols_x,cols_y,Regression=Regression,single = T))
    } 
    if(Model=='DLN'){
      return(DNL_run(data,cols_x,cols_y,Regression=Regression,single = T))
    } 
    if(Model=='iRF'){
      return(iRF_run(data,fold_index = NA,cols_x,cols_y,ncores = ncores,Regression=Regression,interactions=interactions,single=T))
    }
    
    
  }
  # else if(S_Kfold==T){
  #   
  #   
  #   # Create grouping based on whether reponse is 0.
  #   bioChemists <- bioChemists %>%
  #     mutate(art_cat = ifelse(art == 0, TRUE, FALSE))
  #   
  #   # Create stratified folds based on these groups
  #   biochem_kfold <- createFolds(bioChemists[["art_cat"]], k = 10, list = TRUE)
  #   
  #   # Use the list of row-indices to create data.frames for each fold
  #   biochem_kdf <- purrr::map_dfr(biochem_kfold, ~bioChemists[.x, ],
  #                                 .id = "Fold") %>%
  #     select(-art_cat)
  #   
  #   # Check proportion of zeros in each fold:
  #   
  #   (check_balance <- biochem_kdf %>%
  #       group_by(Fold) %>%
  #       summarise(proportion_zero = mean(art == 0)))
  # }
  else {
    
    
    folds = switch(CV_Type,
           Simple = createFolds(1:nrow(data),K),
           Stratified = createFolds(data[,..cols_y][[1]],K)) 
    
    # tmp = createFolds(1:nrow(data),K) 
    # tmp1 = createFolds(data$voxelized_yield,K) 
    # 
    # 
    # 
    # plot = lapply(1:K,function(x1){
    #   data.table(Value=data_in$voxelized_yield[tmp[[x1]]],Fold=x1)
    #   # data.table(Value=tmp[[x1]],Fold=x1)
    # }) %>% rbindlist
    # 
    # plot1 = lapply(1:K,function(x1){
    #   data.table(Value=data_in$voxelized_yield[tmp1[[x1]]],Fold=x1)
    #   # data.table(Value=tmp[[x1]],Fold=x1)
    # }) %>% rbindlist
    # plot$Fold = as.factor(plot$Fold)
    # plot1$Fold = as.factor(plot1$Fold)
    # 
    # ggplot(plot1,aes(x=Value,fill=Fold))  + theme_classic() + geom_density(alpha=.3)
    # 
    #    geom_histogram(aes(y = ..density..))+#, colour= "black", fill = "grey") +
    #   geom_density(alpha = .3)
    # facet_wrap(~Fold)

    
    out <- lapply(1:K,function(k){
      if(Model=='iRF'){
        return(iRF_run(data,folds[[k]],cols_x,cols_y,ncores = ncores,Regression=Regression,interactions=interactions))
      }
      if(Model=='DRF'){
        return(DRF_run(data,folds[[k]],cols_x,cols_y,ncores = ncores,Regression=Regression,interactions=F))
      }
      if(Model=='HAL'){
        return(HAL_run(data,fold_index = folds[[k]],cols_x,cols_y,Regression=Regression))
      } 
      if(Model=='lm'){
        return(lm_run(data,folds[[k]],cols_x,cols_y,Regression=Regression))
      }
      if(Model=='Quantile_Regression'){
        return(Quantile_Regression_run(data,folds[[k]],cols_x,cols_y,Regression=Regression,Quantile))
      }
      
      
    })
  }
  
  if(return_folds==T){
    return(list(out=out,folds=folds))
  } else{
    return(out)
  }
  
}



lm_run <- function(data_in,fold_index,xcols,ycols,Regression=T){
  setDF(data_in)
  
  x=data_in[-fold_index,xcols,drop=F]
  y=data_in[-fold_index,ycols]
  
  xtest=data_in[fold_index,xcols,drop=F]
  ytest=data_in[fold_index,ycols]
  
  model = lm(as.formula(paste0(ycols,'~','.')),data=data_in[,c(xcols,ycols)])
  
  if(Regression==T){
    pred <- predict(model, xtest)
    result <- data.frame(predicted = pred,
                         actual = ytest)
    
    Actual <- result$actual
    Predicted <- result$predicted
    
    rsq <- function(actual, predicted){
      RSS <- sum((actual-predicted)^2)
      TSS <- sum((actual-mean(actual))^2)
      R2 <- (1-(RSS/TSS))
      return(R2)
      
    } 
    
    mae_iRF <- mae(Actual,Predicted)
    mdae_iRF <- mdae(Actual,Predicted)
    R2_iRF <- rsq(Actual,Predicted)
    R2_iRF
    
    collate <- list(Stats=list(mae=mae_iRF,mdae=mdae_iRF,R2=R2_iRF),Model=model,A_P=data.table(Actual,Predicted)) 
  } else{
    print('HALT, YOU ARE BREAKING THE LAW')
  }
  
  return(collate)
}






Quantile_Regression_run <- function(data_in,fold_index,xcols,ycols,Regression=T,Quantile){
  library(quantreg)
  
  setDF(data_in)
  
  x=data_in[-fold_index,xcols,drop=F]
  y=data_in[-fold_index,ycols]
  
  xtest=data_in[fold_index,xcols,drop=F]
  ytest=data_in[fold_index,ycols]
  
  model = rq(as.formula(paste0(ycols,'~','.')),data=data_in[,c(xcols,ycols)],tau=Quantile)
  model_intercept = rq(as.formula(paste0(ycols,'~','1')),data=data_in[,c(xcols,ycols)],tau=Quantile)
  
  if(Regression==T){
    pred <- predict(model, xtest)
    pred0 <- predict(model_intercept, xtest)
    
    result <- data.frame(predicted = pred,
                         predicted0=pred0,
                         actual = ytest)
    
    Actual <- result$actual
    Predicted <- result$predicted
    Predicted0 = result$predicted0
    
    rsq <- function(actual, predicted){
      RSS <- sum((actual-predicted)^2)
      TSS <- sum((actual-mean(actual))^2)
      R2 <- (1-(RSS/TSS))
      return(R2)
    } 
    
    
    ###Input full true vector, and the predicted quantile
    
    Check_Loss <- function(actual,prediction,theta){
      t=(actual-prediction)
      p_theta <- function(t,theta){
        if(t>=0){
          p_theta <- t*theta
        } else{
          p_theta = (-(1-theta))*t
          
          return(p_theta)
        }
      }
      collect <- lapply(t,p_theta,theta=theta)
      return(sum(as.numeric(collect)))
    }
    
    
    Check_Loss <- Check_Loss(Actual,Predicted,Quantile)
# 
    rho <- function(u,tau=.5)u*(tau - (u < 0))
    Vhat <- sum(rho(Actual-Predicted, Quantile))
    V0 <- sum(rho(Actual-Predicted0, Quantile))
    R2 <- 1-(Vhat/V0)

    # Vhat <- sum(rho(model$resid, model$tau))
    # V0 <- sum(rho(model_intercept$resid, model_intercept$tau))
    # R2 <- 1-Vhat/V0
    
    
    mae <- mae(Actual,Predicted)
    mdae <- mdae(Actual,Predicted)
    # R2 <- rsq(Actual,Predicted)
    
    
    collate <- list(Stats=list(mae=mae,mdae=mdae,R2=R2,Check_Loss=Check_Loss),Model=model,A_P=data.table(Actual,Predicted)) 
  } else{
    print('HALT, YOU ARE BREAKING THE LAW')
  }
  
  return(collate)
}

BCTransform <- function(y, lambda,shift=0) {
  if (lambda == 0L) { log(y+shift) }
  else { ((y+shift)^lambda - 1) / lambda }
}

BCTransformInverse <- function(yt, lambda,shift) {
  if (lambda == 0L) { exp(yt)-shift }
  else { exp(log(1 + lambda * yt)/lambda)-shift }
}





###inactive


###get interaction plots
# plots = lapply(sel_iters,function(Iters){
#   print(as.character(Iters))
#   tmp1 = sapply(xcols_merged,function(x){
#     tmp = str_replace(Iters,x,'ThisSpotonly') %>% tstrsplit('_') 
#     if(tmp[[1]]=='ThisSpotonly'){
#       return(x)
#     }
#     if(tmp[[length(tmp)]]=='ThisSpotonly'){
#       return(x)
#     }
#   }) %>% sapply(is.null) %>% '*'(-1)
#   iters = which(tmp1==0) %>% names
# 
#   ###Individual models
#   iter1 = Run_Model(data,iters[[1]],ycol,Model='HAL',single=T)   
#   iter2 = Run_Model(data,iters[[2]],ycol,Model='HAL',single=T)   
#   
#   
#   ###Combined
#   
#   comb = Run_Model(data,iters,ycol,Model='HAL',single=T)
#   setDT(data)
#   tmp2=data[,..iters]
#   grid = expand.grid(seq(min(tmp2[,1]),max(tmp2[,1]),length.out=100),
#                      seq(min(tmp2[,2]),max(tmp2[,2]),length.out=100))
#   pred = predict(comb$Model,new_data=grid)
#   
#   surface = cbind(grid,pred)
#   surface = t(acast(surface, Var1 ~ Var2, value.var ='pred' ))
#   
#   axx <- list(
#     title = iters[[1]]
#   )
#   axy <- list(
#     title = iters[[2]]
#   )
#     axz <- list(
#     title = ycol
#   )
#   
#   fig <-     plot_ly(x=as.numeric(colnames(surface)),y=as.numeric(as.character(rownames(surface))),z = ~surface) %>% add_surface()
#   fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
#   
#   
#   ###Indivdual plots
#   
#   gg1 = ggplot(as.data.table(iter1$sim_out),aes(x=x,y=y)) + geom_line() + labs(x=iters[[1]],y=ycol) + theme_classic(base_size = 20)
#   gg2 = ggplot(as.data.table(iter2$sim_out),aes(x=x,y=y)) + geom_line() + labs(x=iters[[2]],y=ycol) + theme_classic(base_size = 20)
#   
#   return(list(surface=fig,x=gg1,y=gg2))
#   
#   })


##merge xcols
# tmp1 =lapply(tmp,function(x){
#   x1 = collect_results(x,Model = 'iRF')
#   R2s <- sapply(x1,'[[',2)
#   
#   GI <- rbindlist(lapply(x1,'[[',1))
#   mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
# 
#   return(list(R2=mean(unlist(R2s)),mGI=mGI))
# })  

# x_out = data.table(Response=Covar,Covariate=xcols,R2s=unlist(tmp))


###DRF

library(geoR)
bc2 <- boxcoxfit(data$Lesion, lambda2 = TRUE)

lambda1 <- bc2$lambda[1]
lambda2 <- bc2$lambda[2]

# data_in$Lesion = BCTransformInverse(data_in$Lesion,lambda1,lambda2)
data_in$Lesion = BCTransform(data_in$Lesion,lambda1,lambda2)

merge_iterations_dRF = function(x){
  x1 = collect_results(x,Model = 'DRF')
  R2s <- sapply(x1,'[[',2)
  
  GI <- rbindlist(lapply(x1,'[[',1))
  mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
  
  Qstats = lapply(x1,'[[',3) %>% rbindlist()
  Qstats = Qstats[,.(pR2=mean(pR2,na.rm=T),CL=mean(CL,na.rm=T)),by=Quantile]
  
  
  
  out <- list(mGI=mGI,sR2=sR2,m_out=m_out)
  return(list(R2=mean(unlist(R2s)),mGI=mGI,Qstats=Qstats))
  
}

#ycols[c(2)]
Regress_Nemo_DRF = lapply('voxelized_yield',function(Covar){
  print(Covar)                                        # xcols_in
  tmp = mclapply(1:10,mc.cores = 10,function(j) Run_Model(data_in,xcols,Covar,K = 4,Model='DRF',na_rm=T)) 
  # data_in$Lesion =
  # tmp = list(Run_Model(data_in,xcols_in,ycol,Model='DRF',K=4,interactions = T,na_rm=T))
  
  
  x_out=merge_iterations_dRF(tmp)
  x_out$Nemotode = Covar
  x_out$Full_Object = tmp
  return(x_out)
}) #%>% rbindlist

RN_DRF = Regress_Nemo_DRF



# X = tmp1[[1]]

x3 = sapply(RN_DRF[[1]]$Full_Object,function(x){
  sapply(x,function(x1){
    x1$Stats$R2
  })
  
})# %>% as.numeric
x3
which(x3==max(x3),arr.ind=T)

model = RN_DRF[[1]]$Full_Object[[7]][[1]]
plot(model$Q_Stats$pR2)

# plot = model$Q_Preds
# setDT(plot)
# 
# x = colnames(plot) %>% as.numeric %>% round(2)
# x1 = which(x%in%round(seq(0,1,.1),2))
# plot$seq = seq_along(plot$`0.01`)
# x1 = c(x1,ncol(plot))
# plot = plot[,..x1] %>% melt(id.vars='seq')
# 
# 
# 
# ggplot(plot,aes(x=seq,y=value,col=variable)) + geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=1) +
#   theme_classic() #+ guides(color='none')
Model = model$Model

QPreds <- predict(Model,data_in[,..xcols],functional = "quantile",quantiles=seq(0.1,.9,.1)) %>% '[['(1) %>% drop %>% as.data.table
QPreds$seq =1:nrow(QPreds)
QPreds = melt(QPreds,id.vars='seq')

MPreds <- predict(Model,data_in[,..xcols],functional = "mean") %>% as.data.table
MPreds$seq =1:nrow(MPreds)
MPreds = melt(MPreds,id.vars='seq')

tmp = data.table(value=data$voxelized_yield,variable='Actual',seq=1:nrow(data))

plot=rbind(MPreds,QPreds,tmp)
plot$seq = factor(plot$seq,levels=order(tmp$value))
plot$seq = seq_along(tmp$value)[plot$seq]


span=.3
#[variable%in%c('mean','Actual')]
ggplot(plot[variable%in%c('mean','Actual','q=0.7','q=0.3','q=0.2','q=0.8')],aes(x=seq,y=value,col=variable)) + geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=1) +
  theme_classic() + geom_point() #+ guides(color='none') +

###Quants
# x3 = lapply(seq_along(RN_DRF[[1]]$Full_Object),function(Iter){
#   X = RN_DRF[[1]]$Full_Object[[Iter]]
#   x1 = lapply(seq_along(X),function(x1){
#     x2 = x[[x1]]$Q_Stats
#     x2$K=x1
#     return(x2)
#   }) %>% rbindlist
#   x1$Iter=Iter
#   return(x1)
# })%>% rbindlist
# 
# index = x3[which.max(pR2),.(Iter,K)]
# 
# x = RN_DRF[[1]]$Full_Object[[index[[1]]]][[index[[2]]]]

Pred = predict(model,data_in) %>% BCTransformInverse(lambda1,lambda2)
setDT(data_in)

# Pred = BCTransformInverse(data_in$Lesion,lambda1,lambda2)


plot(Pred,data$Lesion)
rsq(data$Lesion,Pred)

rsq(predict(model,as.data.frame(data)),data$Lesion)
pred = predict(model,as.data.frame(data))



