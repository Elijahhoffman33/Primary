###SLM -Jump

data = fread('~/Lab_Link/AR1K/Data/SLB/442.csv/inference_data.csv')
Targeted_Fields = st_read('~/Lab_Link/AR1K/Export/Stratification_Sampling_Depo/Greenfield Surface Parcels Targetted/Greenfield_surface_parcels_targetted.shp')
OG_fields = st_read('~/Lab_Link/AR1K/Export/Stratification_Sampling_Depo/Greenfield_surface_parcels/Greenfield_surface_parcels.shp')
OG_fields = st_make_valid(OG_fields)

Input = get_SF(data[sample(1:nrow(data),nrow(data)*.5,replace=F),],to_UTM = F)


tm_shape(Targeted_Fields) + tm_polygons(alpha=.5,fill='ClientName') 

tm_shape(OG_fields) + tm_polygons(alpha=.5,fill='ClientName') + 
  tm_shape(Input[,'geometry']) + tm_symbols(scale = .035,col='black') 




data = data[!duplicated(data[,.(lng,lat)]),]

###Summary plotting
##Assumes Data is created, and Selected is created

save.image("/mnt/c/Users/elija/Desktop/Lab_Shit/R_Space/Worksapces/R_Space_1_10_22_tmap.RData")
load("/mnt/c/Users/elija/Desktop/Lab_Shit/R_Space/Worksapces/R_Space_1_10_22.RData")




###


I = Input$field_id %>% unique
I = I%in%Selected$Region %>% which %>% I[.]
tmp = mclapply(I,mc.cores = 15,function(x){
  print(x)
  x1 = subset(Input,field_id%in%x)
  x2 = x1 %>% get_hull() %>% st_cast('POLYGON')
  x3 = x2[1] %>% st_sf
  x3$Acres_Sample = Selected[Region==x,Acres_Sample]
  x3$N_Samples = Selected[Region==x,n_samples]
  x3$Field_ID = Selected[Region==x,Region]
  return(x3)
  
})

Polys = tmp %>% rbindlist %>% st_as_sf

Acres_Sample = Polys[,'Acres_Sample']
Field_IDs = Polys[,'Field_ID']
Sample_Count = Polys[,'N_Samples']

# x1 = subset(Input,field_id%in%I[[1]])


###Get grid from polygons
# I %>%  get_hull('concave',1.3) %>% get_grid(cellsize =sqrt(Acre_M*10), plot = T)
# tmp = Polys %>% get_hull %>% get_grid(cellsize =sqrt(Acre_M*15), plot = T)

###Rasterize
#CRS = st_crs(Targeted_Fields)
Input = get_SF(Data,to_UTM = T)
Variable = "om"

# Variable = "sg_soc_15-30"
scale = 10
Ras_OM = SF_Raster(Input,Variable,simple = T,Res = sqrt(Acre_M*scale))[[1]]
Ras_CEC = SF_Raster(Input,'sg_cec',simple = T,Res = sqrt(Acre_M*scale))[[1]]
Ras_Nitrogen = SF_Raster(Input,'sg_nitrogen',simple = T,Res = sqrt(Acre_M*scale))[[1]]


library(tmap)

tmap_mode('view')
tmap_mode('plot')


get_breaks(Polys$Acres_Sample,breaks=c(2.5,10,15,20,30,50,100,230),cut=F)
b = get_breaks(Polys$Acres_Sample,breaks=c(2.5,5,10,15,20,30,50,100,230),cut=F)[[2]]
basemap = get_basemap(st_bbox(Ras_OM))
library(basemaps)
Strata = L

lapply(Sample_Points)
''

Targeted_Fields = st_transform(Targeted_Fields,st_crs(Sample_Points))

index  = st_intersects(Sample_Points$geometry,Targeted_Fields) %>% 
  sapply(sum) %>% '!='(0) %>% which #sum

tm_shape(Sample_Points[index,]) + tm_symbols(size=.35) +
  tm_shape(Targeted_Fields) + tm_polygons(fill='OBJECTID')


# t=
tm_basemap("Esri.WorldImagery")  +
  #tm_shape(basemap) + tm_rgb() +
  tm_shape(Ras_OM) + tm_raster(palette = 'viridis',alpha=1,title = 'SOC g/dm^3',style='cont') +
  tm_shape(Ras_CEC) + tm_raster(palette = 'viridis',alpha=1,title = 'CEC mmol(c)/kg',style='cont',style.args=list(n=10)) +
  tm_shape(Ras_Nitrogen) + tm_raster(palette = 'viridis',alpha=1,title = 'Nitrogren cg/Kg',style='cont') +
  # tm_shape(Acres_Sample,bbox = st_bbox(Ras_OM)) + tm_polygons('Acres_Sample',style='fixed',breaks=b,style.args=list(),
  #                                                   alpha = .5,palette ='-YlOrRd') +
  # tm_shape(Sample_Count,bbox = st_bbox(Ras_OM)) + tm_polygons('N_Samples',alpha = .5,palette ='YlOrRd')+
  # tm_shape(Field_IDs,bbox = st_bbox(Ras_OM)) + tm_polygons('Field_ID',alpha = .5,palette ='YlOrRd')+
  
  # tm_shape(Sample_Points) + tm_symbols(scale = .03,col='black') +
  tm_shape(Sample_Points) + tm_symbols(scale = .035,col='black') +
  
  tm_shape(Targeted_Fields) + tm_polygons(alpha=.5,fill='ClientName') +
  # tm_shape(Strata) + tm_raster(style = 'cat',palette ='Dark2',alpha=.7) +
  tm_layout(scale=1.5,outer.margins=c(0,0,0,0))  + tm_scale_bar(position = 'right',text.size = 1)

tmap_save(t, "~/Lab_Link/../Scratch/SLM_Sampling_Stratification.html")

tmap_save(t, "~/Lab_Link/../Scratch/test.png", width=1920, height=1080)



files = list.files(path = "~/Lab_Link/../Scratch/SLM_Stratificattion_Sampling/Field_Data/",pattern = '.shp',recursive = T,full.names = T)


Sample_Points = mclapply(files,mc.cores = 15,function(x){
  x1 = st_read(x)
  return(x1)
}) %>% rbindlist %>% st_as_sf()

sapply(Sample_Points,function(x){Dim(x[[1]])})

x = files %>% str_detect('19665') %>% which %>% files[.]
x1 = st_read(x) 

x=19633
x1 = subset(Polys,Field_ID==x)

y = st_transform(Sample_Points,st_crs(x1))

y1 = st_intersects(x1,y)
y2 = Sample_Points[y1[[1]],] #%>% plot
plot(y2)
y3 = as.data.table(y2)

y2$field_d = as.character(y2$field_d)
plot(y2[,'field_d'])
### duplicates

tmp = data[field_id%in%names(X)] 
data$combined_coords = paste(data$lat,paste(data$lng))
x = table(data$combined_coords)
hist(x)
dup_fields = data[duplicated(data$combined_coords),unique(field_id)]

x1 = data[!duplicated(data$combined_coords),]
keep=dup_fields%in%x1$field_id %>% which %>% dup_fields[.]
toss=which(!dup_fields%in%keep) %>% dup_fields[.]

###move files 
library(filesstrings)

files = list.files(path = "~/Lab_Link/../Scratch/SLM_Stratificattion_Sampling/Field_Data/",full.names = T) 
move = files %>% str_detect(paste(toss,collapse = '|')) %>% which %>% files[.]


file.copy(from = move, to = '~/Lab_Link/../Scratch/SLM_Stratificattion_Sampling/Field_Data_Duplicates/', 
          recursive = T, copy.mode = TRUE)
unlink(move, recursive=TRUE)
###Dedupe samples
Sample_Points_Deduped = subset(Sample_Points,!field_d%in%toss)
Sample_Points = Sample_Points_Deduped

###Strta from rasters

files = list.files(path = "~/Lab_Link/../Scratch/SLM_Stratificattion_Sampling/Field_Data/",pattern = '.TIF',recursive = T,full.names = T) 
files = files %>% str_detect('aux',negate=T) %>% which %>% files[.]


Strata_Rasters = mclapply(files,mc.cores = 1,rast)

L = do.call("merge", Strata_Rasters)          

###
grid=st_make_grid(x,what = 'centers',cellsize = cellsize)

# x = st_intersection(buf,grid)
grid = grid[st_intersects(x,grid)[[1]]]

grid1 = grid %>% st_as_sf() %>%
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry()

gg = ggplot() +
  geom_sf(data = grid, fill = NA, color = "gray") +
  geom_sf(data = x, fill = NA, color = "red") +
  theme_classic()
##

x = subset(Input,field_id==19869)
x = Input
dim(x)
x %>% st_make_grid(n = 1) %>% st_area %>% '/'(Acre_M)

hist(x$gssurgo_om_r_5)
hist(10^x$`polaris_om_15-30`)
hist(x$`sg_cec_15-30`)

Variable='om'
Variable='sg_cec_15-30'
Variable='sg_soc_15-30'
Variable='gssurgo_om_r_15'
Variable='polaris_om_15-30'
plot = SF_Raster(x,Variable,q=.01,rev=F)[[1]]


ggplot(plot,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
  scale_fill_continuous(type = 'viridis')

colnames(x) %>% like('om') %>% which %>% colnames(x)[.]
tm1 = tm_shape(x) + tm_dots('gssurgo_om_r_15')
tm2 = tm_shape(x) + tm_dots('sg_soc_15-30')
x$`polaris_om_15-30_exp` = 10^x$`polaris_om_15-30` 
tm3 = tm_shape(x) + tm_dots('polaris_om_15-30_exp')

tm3 = tm_shape(x) + tm_dots('om')

tmp=as.data.table(x)
SD1 = tmp[,sd(gssurgo_om_r_15),field_id]
SD2 = tmp[,sd(`sg_soc_15_30`),field_id]
SD3 = tmp[,sd(`polaris_om_15-30`),field_id]
SD4 = tmp[,sd(om),field_id]


# mapview(x$)





###Stratafication
data = fread('~/Lab_Link/AR1K/Data/SLB/442.csv/inference_data.csv')
colnames(data) = colnames(data) %>% str_replace_all('-','_')

data$topo_elevation = NULL
data$topo_insolation = NULL

index = colnames(data) %>% str_detect('gssurgo_|polaris_|_60',negate = T) %>% which 
data = data[,..index]

cols = colnames(data) %>% str_detect('_0_5') %>% which %>% colnames(data)[.] %>% 
  tstrsplit('_0_5')
cols = cols[[1]]


lapply(cols,function(x){
  x1 = colnames(data) %>% str_detect(x) %>% which %>% colnames(data)[.]
  data[,(x):=rowMeans(.SD),.SDcols=x1]
})

index = colnames(data) %>% str_detect('_0_5|_5_15|_15_30',negate = T) %>% which 
data = data[,..index]

plot = melt(data,measure.vars = colnames(data)[31:40])#c('sg_soc_0_5','sg_soc_5_15','sg_soc_15_30','om'))
ggplot(plot,aes(fill=variable,x=value)) + theme_classic() + geom_density(alpha=.6) + xlim(0,200)# geom_histogram()


colnames(data)
cols = c(7:23,31:40) #1491
# cols = cols[which(!cols%in%29:32)]
setnames(data,'sg_soc','om')
data$Year = '2020'

###Run iRF, get hierarchy of importance

data$om %>% sample(size = 10000,replace=F) %>% hist
index = 1:nrow(data) %>% sample(size = 10000,replace=F)

x = data[index,..cols][,!'om']
y = data[index,'om'][[1]]

sapply(x,function(x){sum(is.na(x))})
index = is.na(x) %>% apply(1,any) 

x = x[!index,]
y = y[!index]



irf <-iRF(x,y,n.core = 13)
tmp_1 = irf$rf.list$importance
irf_tmp <- data.table(names=names(tmp_1[rev(order(tmp_1)),]),val=tmp_1[rev(order(tmp_1)),])
Vars = irf_tmp[1:2,names]


Data = data.table::copy(data)

tmp = Data[,.(.N,Acres=sum(acres)),field_id]
tmp[order(Acres)]
plot(tmp$Acres)


data <- Data[field_id==19414,]#c('voxel_id','Year','lng','lat','acres','om')]

Vars = c("sg_cec","sg_nitrogen")
Vars = c("sg_cec",'sg_nitrogen','sg_cec+sg_nitrogen','om')
Vars = c('om')

Vars_in = c("sg_cec","sg_nitrogen")


###Run Single

Run_Neyman <- Run_Split('BLM_1',data,3,Vars,Vars_in,run_voxels = T,
                        Iterations = 100,Iter_runs = 10,Threshold = .05,allocation = 'Neyman')


###Loop prep
Data[,Total_Acres:=sum(acres),field_id]
plot(Data[duplicated(field_id),sort(Total_Acres)])

index = Data[Total_Acres>20]$field_id %>% unique %>% sample(replace=F)
tmp = Data[field_id%in%index,sum(acres),field_id]#[order(V1)]

IDs = tmp$field_id
Regions=IDs

###Run loop
Results <- lapply(Regions,function(I){
  tmp <- Run_Split(Regions[I],Saunders,4,Vars,Vars_in,run_voxels = T,
                   Iterations = 100,Iter_runs = 50)
  print('Saving')
  saveRDS(tmp,paste0('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/Verra_Strata_Tmp/Dir_6/',Regions[I]))
  return(tmp)
})


###Batch_Setup Jump Saving / Load
save.image("/mnt/c/Users/elija/Desktop/Lab_Shit/R_Space/Worksapces/R_Space_1_7_22_3.RData")
load("/mnt/c/Users/elija/Desktop/Lab_Shit/R_Space/Worksapces/R_Space_1_7_22_3.RData")


###Collate Results


files_1 <- list.files('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/Verra_Strata_Tmp/Dir_9/',full.names = T)
# files <- files[-((length(files)-4):length(files))]

Results <- data.frame()
for(i in files_1){
  tmp1 <-readRDS(i)$Summary
  Results <- rbind(Results,tmp1)
}
rm(tmp1)

tbf <- Results
rm(Results)
# tbf[,.SD[.N,],by=Region][,1:10]

###All results
# Results <- list()
# for(i in files){
#   tmp1 <-readRDS(i)
#   Results[[tmp1$Region]] <- tmp1
# }
# rm(tmp1)


###Plotting jump

tbf = Results$Summary

tbf = rbind(Run_Neyman$Summary,Run_PPS$Summary)

tb4=tbf

tb4[,Facet:=paste0(Region,'  Acres: ',round(Total_Acres))]
tb4 <- tb4[order(Total_Acres)]
tb4$Facet <- factor(tb4$Facet,levels=unique(tb4$Facet))

levels(factor(tb4[order(Total_Acres),Facet]))

tb3 = tb4
# tb3 <- tb4[between(Total_Acres,200,5000)]

# data = Run_Neyman$Strata_Data$`sg_cec:1`$data_in
# C_om = Run_Neyman$Strata_Data$`om:1`$Partitions[[4]][[1]] %>% as.factor
# C_cec = Run_Neyman$Strata_Data$`om:1`$Partitions[[3]][[1]] %>% as.factor


#[1] "sg_cec:0" "om:0"     "sg_cec:1" "om:1"     "sg_cec:2" "om:2"     "sg_cec:3" "om:3"    


# data$tmp = stratify(data$om,3)
data$tmp = as.factor(data$tmp)
# ggplot(data=data,aes(x=lng,y=lat,col=om))   + geom_tile(size=10,alpha=0.8)+ scale_color_continuous(type = 'viridis') #
# ggplot(data=data,aes(x=lng,y=lat,col=om)) + geom_point(data=data,aes(x=lng,y=lat,col=C_cec))

# gg = ggplot(data=Data,aes(x=sg_cec,y=om)) + theme_classic()  +stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#   scale_fill_continuous(type = "viridis") 
#   
# + geom_point(size=.8,alpha=.01)
#   ggMarginal(gg,type="density") 
#   
#        ggplot(data, aes(x=x, y=y) ) +
#          geom_hex(bins = 70) +
#          scale_fill_continuous(type = "viridis") +
#          theme_bw()
span=.6
#&Strata%in%c('sg_cec:0','sg_cec:1')
tb3[Acres_Sample<50&Total_Acres<300,length(unique(Region))]

###Subset and split
index = tb3[,sample(unique(Region),50,replace=F)]


gg <- ggplot(tb3[Region%in%index&Acres_Sample<50&Total_Acres<300],aes(x=Acres_Sample,y=Prob*100,col=Strata)) +# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.4) + labs(color='Strata') +
  geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=.2) #  cut(round(N_Ratio),16)
# gg +facet_wrap(vars(as.factor(round(Total_Acres))),scales = 'free_x') + ylim(0,NA)+ theme_classic() #+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)
gg +facet_wrap(~Facet,scales = 'free_x') + theme_classic(base_size = 18) + ylim(0,NA)#+ geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)#+ geom_vline(xintercept=10,alpha=.4) 


###Plotting loop

###check presensce 
# Regions_back = Regions
Regions = Regions_back

Regions = files %>% tstrsplit('//') %>% '[['(2) 
missing = Regions%in%x

Regions
nlim <- 20
len = length(Regions)
splits <- round(len/nlim)
if(splits<=1){
  run_groups <- seq_len(len)
}else{
  run_groups <- split(seq_len(len),cut(seq_len(len),splits,labels=F))
}

splits = lapply(run_groups,function(x){
  Regions[x]
}) 
lapply(splits,function(x){
  Data[field_id%in%x,sum(acres),field_id]
}) 

Plots = mclapply(seq_along(splits),mc.cores=10,function(x){
  index = splits[[x]]
  gg <- ggplot(tb3[Region%in%index&Acres_Sample<50&str_detect(Strata,'2asdas',negate=T)],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) +# ,linetype=Year geom_point(alpha=.5,size=1) 
    geom_point(alpha=0,size=.4) + labs(color='Strata') + ylim(0,10) +
    geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=.2) #  cut(round(N_Ratio),16)
  # gg +facet_wrap(vars(as.factor(round(Total_Acres))),scales = 'free_x') + ylim(0,NA)+ theme_classic() #+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)
  gg = gg +facet_wrap(~Facet,scales = 'free_x') + theme_classic(base_size = 18)# + ylim(0,NA)#+ geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)#+ geom_vline(xintercept=10,alpha=.4) 
  
  file = paste0('/mnt/c/Users/elija/Desktop/Plots/Active/Set_',x,'.png')
  print(file)
  ggsave(plot=gg,filename = file,width=7.84*2,dpi = 300,device = 'png')
  
  return(gg)
})


###Regular
tb3 = tbf
tb3 = tbf[between(Acres_Sample,4,100)]
tb3$Prob = format(tb3$Prob*100,scientific = F)
# tb=out[Year=='2018']
span=.1 #[Acres_Sample<150] &Strata%in%c('sg_cec:0','sg_cec:1','om:0','om:1') 19634
gg <- ggplot(tb3[Acres_Sample<50&Region%in%19526&str_detect(Strata,'om',negate=T)],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
gg
gg + facet_wrap(~Strata)
# gg +facet_wrap(vars(as.factor(round(Total_Acres))),scales = 'free_x') + ylim(0,NA)+ theme_classic() #+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)
gg + theme_classic(base_size = 18) #+ ylim(0,NA) + geom_hline(yintercept=1,alpha=.4) + 
#  geom_vline(xintercept=10,alpha=.4)# + coord_flip()# + xlim(2.5,50)#+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)


# sg_nitrogen 
Plot_Raster(Data[field_id==19526],var = 'om',rev=F,q = .008)

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

SRegion = unique(tbf$Region)[261]

x = tbf[Region==SRegion,]
x1 =x$E_Prob*100

# index = which(abs(x1-3.05)<=.005)
threshold = 3.03

Select_Results = function(region,x,threshold=3.03){
  x = x[Region==region,]
  x = x[!n_samples%in%c(1,2)]
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
  if(any(x2$E_Prob!=0)&!any(x2$n!=1)){
    x2$E_Prob=99
    x = x[n_samples!=1]
  }
  x2 = x2[E_Prob!=100,]
  
  
  ###If all above %P
  if(nrow(x2)==0){
    
    # ###Is actually in between
    # if(any(x$E_Prob < threshold)){
    #   
    # }
    x1 = x$Acres_Sample
    # 
    
    ###Get best nearest to 5 acres_sample
    index = which(abs(x1-5)<.05)
    
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
    #Standard
    selected = which.max(x2$Acres_Sample) %>% x2[.]
  }
  
}

tmp = lapply(unique(tbf$Region),Select_Results,x=tbf)
which(sapply(tmp,nrow) != 1 )
tmp = rbindlist(tmp,use.names = T)
which(tmp$n==1)

###Reduce complexity / Check Probability


# y = tbf$Strata %>% unique
complexity = function(x){
  x1 = x %>% tstrsplit(':|\\+') %>% length
  x2 = x %>% tstrsplit(':') %>% '[['(2) %>% as.numeric
  x1 = x1 + x2 
  if(str_detect(x,':0')){return(1)} else{return(x1)}
  
}
# sapply(y,complexity)

Selected = lapply(1:nrow(tmp),function(x){
  x=tmp[x,]
  x1 = tbf[Region==x$Region&n==x$n]
  x2 = x1[E_Prob<x$E_Prob]
  if(nrow(x2)==0){return(x)}
  complex_x2 = sapply(x2$Strata,complexity)
  complex_x = complexity(x$Strata)
  if(any(complex_x<complex_x1)){
    index = which(complex_x<complex_x1)
    return(x2[index,])
  } else{return(x)}
  
  # return(list(paste0(x$Strata,'  Prob: ',x$E_Prob),x1[E_Prob<x$E_Prob]))
}) %>% rbindlist(use.names = T)





#str_detect(Strata,'om|sg_cec+sg_nitrogen:2',negate=F)
gg <- ggplot(x[Acres_Sample<50],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
gg
gg +ylim(0,3.05)



###Export Results Jump




tmp = Selected[23,]
Region= tmp$Region
Region = as.character(Region)
Strata=tmp$Strata
N=tmp$n

# x1 = pull_result(Region,'list',6)
# export_results(Region,Strata,N,.01)

# L = mclapply(sample(1:nrow(Selected),1),mc.cores = 1,function(x){
# L = mclapply(1:nrow(Selected),mc.cores = 10,function(x){
L = mclapply(index,mc.cores = 5,function(x){
  # tmp = Selected[index[2],]
  tmp = Selected[x,]
  Region= tmp$Region
  Region = as.character(Region)
  Strata=tmp$Strata
  N=tmp$n
  
  x1 = pull_result(Region,'list',1)
  
  export_results(Region,Strata,N,tmp_out=tmp)
  
  
})

y = list.files('~/Lab_Link/AR1K/Export/Stratification_Sampling_Depo/Export_2/') %>% str_remove('Field_')
index = which(!Selected$Region%in%y)# %>% Selected$Region[.]


Selected[index,.(Region,Total_Acres)]
x1 = Selected$Strata
x1[index] = 'No_Stratification'
table(x1)

index = x1 %>% str_detect(':0') %>% which 

write.csv(Selected,'~/Lab_Link/../Scratch/SLM_Stratification_Sampling/Overall_Overview/Sampling_Selections.csv',row.names = T)


#Jump SLM 2





export_results = function(Region,Strata,n,tmp_out){
  file= '/home/elija/Lab_Link/AR1K/Export/Stratification_Sampling_Depo/Export_2/'
  
  dir.create(paste0(file,'/Field_',Region),showWarnings = F)
  dir.create(paste0(file,'/Field_',Region,'/','Summary_Plots'),showWarnings = F)
  
  
  index = x1[which(x1$Strata==Strata),2]  
  samples = pull_samples(tmp_out,mode = 'loop_start')
  ###This also creates data_tmp, the source data 
  
  
  if(str_detect(Strata,'cec')){
    tm1 = plot_overlay(Region,index,'raster_var',var='sg_cec',plot_samples=T)
    
    tmap_save(tm1, paste0(file,'Field_',Region,'/Summary_Plots','/','CEC','.png'))

  }
  
  if(str_detect(Strata,'nitrogen')){
    tm1 = plot_overlay(Region,index,'raster_var',var='sg_nitrogen',plot_samples=T)
    
    tmap_save(tm1, paste0(file,'Field_',Region,'/Summary_Plots','/','Nitrogen','.png'))
    }
  
  ###OM default
  tm1 = plot_overlay(Region,index,'raster_var',var='om',plot_samples=T)
  
  tmap_save(tm1, paste0(file,'Field_',Region,'/Summary_Plots','/','OM','.png'))#, width=1920, height=1080)
  
  
  
  ###Strata Map
  
  tm1 = plot_overlay(Region,index,'raster',var='Strata',plot_samples=T)
  
  tmap_save(tm1, paste0(file,'Field_',Region,'/Summary_Plots','/','Strata_Map','.png'))
  
  ###Probability Graph
  
  Re = Region
  gg3 <- ggplot(tbf[Acres_Sample<50&Region%in%Re&str_detect(Strata,'asdasfaf',negate=T)],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
    geom_point(alpha=0,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
  # gg3
  ggsave(plot=gg3,filename = paste0(file,'Field_',Region,'/Summary_Plots','/','Probability_Graph_50','.png'),
         width=7.84*1.5,dpi = 300,device = 'png')
  
  gg3 <- ggplot(tbf[Acres_Sample<150&Region%in%Re&str_detect(Strata,'asdasfaf',negate=T)],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
    geom_point(alpha=0,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
  # gg3
  ggsave(plot=gg3,filename = paste0(file,'Field_',Region,'/Summary_Plots','/','Probability_Graph_150','.png'),
         width=7.84*1.5,dpi = 300,device = 'png')
  
  
  ###Export Sample Points as Shapefile

  shp = get_SF(samples,to_UTM = F)
  shp$field_id = Region
  shp$n_samples = n
  shp$Total_Acres = tmp_out$Total_Acres
  shp$Acres_Sample = tmp_out$Acres_Sample
  
  dir.create(paste0(file,'/Field_',Region,'/','Sample_Locations'),showWarnings = F)
  
  st_write(shp,append=F,
           paste0(file,'Field_',Region,'/','Sample_Locations/',Region,'.shp'), driver = "ESRI Shapefile")
  
  ###Write Strata
  dir.create(paste0(file,'/Field_',Region,'/','Summary_Plots/Strata_Raster'),showWarnings = F)
  
  data_tmp$Strata = as.factor(data_tmp$Strata)
  strata_out = data_tmp %>% get_SF(to_UTM = T) %>% SF_Raster('Strata',simple = T) %>% '[['(1)

  writeRaster(strata_out, paste0(file,'Field_',Region,'/Summary_Plots/Strata_Raster/',Region,'.TIF'), overwrite=TRUE)
  
  
  ###Export Summary table 
  write.csv(tmp_out,paste0(file,'Field_',Region,'/Summary_Plots','/','Summary_Table','.txt'),row.names = F)
  # txt <- paste(paste(dataset,covar,sep=' --- '),splits,sep="\n")
  # write(txt,file=paste('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Export/',dataset,'/Covariate_Splits/',covar,'.txt',sep=''))
  
}


###Pull fresh samples
tmp

pull_samples = function(x,mode='single'){
  Region= x$Region %>% as.character
  Strata_in=x$Strata
  N=x$n
  
  if(mode=='single'){
    x1 = pull_result(Region,'list',1)
    index = x1[which(x1$Strata==Strata_in),2]  
    data_tmp = pull_result(Region,'data',index)
    data_tmp = data_tmp[round(acres,3)==.25,]
  }
  if(mode=='loop_start'){
    x1 = pull_result(Region,'list',1)
    index = x1[which(x1$Strata==Strata_in),2]  
    data_tmp = pull_result(Region,'data',index)
    data_tmp <<- data_tmp
    data_tmp = data_tmp[round(acres,3)==.25,]
    # data_tmp <<- data_tmp
  } 
  
  ###Sampling Proportion
  
  if(x$Allocation == 'PPS'){
    N0 <- data_tmp[,sum(acres)]
    proportion <- data_tmp[order(Strata), lapply(.SD[,'acres'],function(x){
      sum(x)}), by=.(Strata)]
    proportion[,2] = proportion[,2]/N0
    colnames(proportion)[2]='Proportion'
    
  } else if(x$Allocation=='Neyman'){
    
    N_alloc = optimStrat::optiallo(N,data_tmp$om,stratum=data_tmp$Strata) %>% as.data.table()
    N_alloc = N_alloc[!duplicated(stratum),][order(stratum)]
    proportion = N_alloc[,Proportion := nh/N][,-2]
    colnames(proportion)[1] = 'Strata'
  }
  
  Strata_Table = sample(proportion$Strata,size = N,prob = proportion$Proportion,replace=T) %>% table
  
  index = lapply(seq_along(Strata_Table),function(y){
    y1 = Strata_Table[y] %>% names %>% as.numeric
    n = Strata_Table[y][[1]]
    return(data_tmp[Strata==y1,sample(voxel_id,size=n)]) 
  }) %>% unlist
  return(data_tmp[voxel_id%in%index,])
}


###Verification test

tmp
pull_samples(tmp,mode = 'loop_start')

x = mclapply(1:5000,mc.cores = 10,function(x){
  print(x)
  pull_samples(tmp,mode='loop_in')$om
})

x1 = sapply(x,mean)
hist(x1)
Threshold = .05
True_Mean = data_tmp[,mean(om)]
x2 = data.table(Avg=x1)
x2[,abs(Avg-True_Mean)] %>% hist
table(x2[,abs(Avg-True_Mean)]>(Threshold*True_Mean))

Prob = x2[,(sum(abs(Avg-True_Mean)>(Threshold*True_Mean))/.N)]




###Region digging


19634
19526

Region = 19634
Region = 19526
Region = 19483
###Sketchy 
Region = 19623

Region = SRegion

Region = as.character(Region)
pull_result(Region,'list',6)
# tmp = pull_result('19526','data',15)
tmp = pull_result(Region,'summary',12)
plot(tmp$n,tmp$E_Prob)
abline(h=.01)

index = 9
q=.008
q=.02
n=23
Var = 'om'
Var = 'sg_nitrogen'
Var = 'sg_cec'


plot_overlay(Region,index,'raster_var',var=Var,q=q,samples=F,N=n)
plot_overlay(Region,index,'raster',var=Var,q=q,samples=T,N=n)
plot_overlay(Region,index,'raster',var=Var,q=q,samples=F,N=n)

pull_result(Region,'data',index) %>% Plot_Raster(var = 'om',Rev=F,q = .008)


Re = Region
gg <- ggplot(tb3[Acres_Sample<50&Region%in%Re&str_detect(Strata,'asdasfaf',negate=T)],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
gg 




pull_result <- function(Region,Action='list',Strata=NULL,N=5){
  #actions = list, C, simple_plot, data, samples
  i = files_1[str_detect(files_1,Region) %>% which]
  tmp <-readRDS(i)
  if(Action=='list'){
    return(data.frame(Strata = names(tmp$Run_Index),Index=1:length(tmp$Run_Index)))
  }
  if(Action=='C'){
    out = tmp$Strata_Data[[Strata]]$Partitions[[Strata]][[1]]
  }
  if(Action=='simple_plot'){
    tmp$Strata_Data[[Strata]]$Partitions[[Strata]][[2]]
  }
  if(Action=='data'){
    out = tmp$Strata_Data[[Strata]]$data_in
    out$Strata = tmp$Strata_Data[[Strata]]$Partitions[[Strata]][[1]]
    return(out)
  }
  if(Action=='summary'){
    out = tmp$Summary
    tmp <-readRDS(i)
    index = names(tmp$Strata_Data)[Strata]
    out = out[Strata%in%index]
    return(out)
  }
  if(Action=='samples'){
    # index = tmp$Run_Index[[6]]
    
    return(tmp$Voxel_Data[[Strata]][[N]] %>% as.numeric)
  }
  
}




tmp = lapply(1:50,function(x){
  print(x)
  x1 = Selected[x,]$Total_Acres
  gg = tmp_fun(Selected$Region[x])
  return(list(x1,gg))
})

tmp3 = tmp

plot_overlay <- function(Region,Strata,type='raster',var='om',plot_samples=F){
  ##Plot types: raster, raster_var
  
  if(type=='raster'){
    plot = data_tmp %>% get_SF(c_cols= c('lng','lat'))  %>% SF_Raster('Strata',simple=T) %>% '[['(1)
    
    tm = tm_shape(plot) + tm_raster(palette = 'Dark2',title = 'Strata',style='cat') +
      tm_layout(scale=2,legend.outside = T,legend.outside.position = 'right')
    
    if(plot_samples==T){
      Samples = get_SF(samples)
      tm = tm + tm_shape(Samples) + tm_symbols(scale = .3,col='black') 
    }
    
  }
  
  if(type=='raster_var'){
    plot = data_tmp %>% get_SF(c_cols= c('lng','lat'))  %>% SF_Raster(var,simple=T) %>% '[['(1)
    
    # Ras_CEC = SF_Raster(Input,'sg_cec',simple = T,Res = sqrt(Acre_M*scale))[[1]]
    tm = tm_shape(plot) + tm_raster(palette = 'viridis',title = var,style='cont') +
      tm_layout(scale=2,legend.outside = T,legend.outside.position = 'right')
    
    if(plot_samples==T){
      Samples = get_SF(samples)
      tm = tm + tm_shape(Samples) + tm_symbols(scale = .3) 
    }
    
  }
  
  
  return(tm)
}





###defunct
