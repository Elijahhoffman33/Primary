source('src/General_Setup_Edit.R')
source('src/Helper_Functions/AWS_Helpers.R')
source('../Soil_Stratification_Sampling/Main_Wrapper.R')

library(tictoc)
### 13 million acres thing ----

polaris = st_read('scratch/Hilary_Data_Regions/texas_om_bd_polaris.gpkg')
colnames(polaris) = colnames(polaris) %>% str_replace('sg','polaris')
soil_grid = st_read('scratch/Hilary_Data_Regions/texas_soc_bd_sg.gpkg')
data = st_join(polaris,soil_grid)

# 13 million acres texas
field_boundaries = st_read('scratch/Hilary_Data_Regions/Regional_potential_carbon_storage_area.shp')


# soil_grid %>% quick_map('sg_soc_0.5',1200)
# sqrt(Acre_M*1000)

# should be 1km resolution, but practically ends up being 2ish
### CDL Roads

# Pioneer B
data =  get_joblib('scratch/2021_full.gz')

# Sesimic Shoot
data = fread('../Soil_Stratification_Sampling/data/input/442.csv/inference_data.csv')
data = data[!duplicated(data[,.(lng,lat)]),]


# full region
# field_boundaries = st_read('scratch/Pioneer_Data/Greenfield_Ph_1_Outline_NAD27_TxNC/Greenfield_Ph_1_Outline_NAD27_TxNC.shp')
field_boundaries = st_read('scratch/Greenfield_boundaries_OG.gpkg')
# OXY Subset
field_boundaries = st_read('scratch/Pioneer_Data/OXY_NAD27_TxNC/OXY_NAD27_TxNC.shp')

data = data %>% get_SF(to_UTM = F)
field_boundaries = st_transform(field_boundaries,st_crs(data))
data = data %>% st_filter(field_boundaries)


cols = colnames(data) %>% str_detect('cdl') %>% colnames(data)[.]
setDT(data)

# Which cdl values, flagged by chris
exclude =  c(82,124,122,123,121,190,88)

CDL = data %>% select(cols) %>% apply(1,function(x1){
  any(x1%in%exclude)
})



### Create layers
# Pioneer B
x = get_joblib('scratch/2021_full.gz')
field_boundaries = st_read('scratch/528.gpkg') 
#

x = data 
rm(data)
x = remove_geom(x)
x = Rescale_om(x)
x$CDL = CDL

### 13 million acres
## Polaris
x = data 
x = remove_geom(x)
setDT(x)
x = Rescale_om(x)

# tmp_data = copy(x)
# tmp_data[polaris_om_0.5>-10&polaris_om_0.5<10,hist(10^(polaris_om_0.5/10))]
# tmp_data[polaris_bd_0.5>-1 & polaris_bd_0.5 <200,hist(polaris_bd_0.5)]
# cols <- grep("polaris_om_", colnames(tmp_data), value=T)
# tmp_data[,(cols):=.SD/10,.SDcols=cols]

## Soil Grids
# cols <- grep("sg_soc_", colnames(tmp_data), value=T)
# tmp_data[,(cols):=(.SD/100*1.72),.SDcols=cols]
# tmp_data[,hist(sg_soc_0.5)]

# x = copy(tmp_data)

cols = x %>% colnames %>% str_detect('om|bd|sg_soc') %>% colnames(x)[.]
cols = c(cols,c('lng','lat'))
x = x %>% select(cols)

x1 = copy(x)
Get_Weighted_Avg_Depths(x1)
cols = x1 %>% colnames %>% str_detect('om|bd|sg_soc') %>% colnames(x1)[.] 
data = cbind(x,x1[,..cols])
rm(x,x1)

data[polaris_om>10,polaris_om:=1]

data[,hist(sg_bdod)]
data[,hist(polaris_bd)]
data[,hist(sg_soc)]
data[,hist(polaris_om)]

data[polaris_bd<0,polaris_bd:=0]
data[polaris_bd>300,polaris_bd:=130]

### Other

cols = x %>% colnames %>% str_detect('om|bd|sg_soc') %>% colnames(x)[.]
cols = c(cols,c('lng','lat','CDL'))
x = x %>% select(cols)

x1 = copy(x)
Get_Weighted_Avg_Depths(x1)
cols = x1 %>% colnames %>% str_detect('om|bd|sg_soc') %>% colnames(x1)[.] %>% '['(c(-1,-2))
data = cbind(x,x1[,..cols])
rm(x,x1)

### Tmp plot ----

plot = data %>% melt(measure.vars=c('sg_soc','polaris_om'))

ggplot(plot,aes(x=value,fill=variable))+
  geom_histogram(bins=200,alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  geom_density(alpha=.8,position = 'identity') + scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  labs(x='%om') + theme_classic(base_size = 22) + coord_cartesian(ylim = c(0,10000)) 


###


# om = x[1,`polaris_om_5-15`] # %om
# bd = x[1,`polaris_bd_5-15`]/100  # g/cm^3, /100 to get cg to g
# depth = 10

### 13 million acres changes scale to 'hectare', others use acre
get_toc = function(om,bd,depth,scale='hectare'){
  # total carbon in metric tons = fraction of carbon (%om / 100) * BD g/cm * Volume m3
  area = switch(scale,
                'acre'=4046.86,
                'hectare'=10000)
  (om / 100) * bd * (area * depth/100) 
}
### Create layers ----
# OM
# TOC at horizons from soil grids/SSURGO.
# Biomass
# Other
### 13 million acres 
colnames(data) = colnames(data) %>% str_replace('\\.','-')
###

polaris_0_30cm_toc = get_toc(data[,polaris_om],
                             data[,polaris_bd]/100,
                             30)
polaris_0_5cm_toc = get_toc(data[,`polaris_om_0-5`],
                            data[,`polaris_bd_5-15`]/100,
                            5)
polaris_5_15_cm_toc = get_toc(data[,`polaris_om_5-15`],
                              data[,`polaris_bd_5-15`]/100,
                              10)
polaris_15_30cm_toc = get_toc(data[,`polaris_om_15-30`],
                              data[,`polaris_bd_15-30`]/100,
                              15)
### SG TOC
SG_0_30cm_toc = get_toc(data[,sg_soc],
                        data[,sg_bdod]/100,
                        30)
SG_0_5cm_toc = get_toc(data[,`sg_soc_0-5`],
                       data[,`sg_bdod_0-5`]/100,
                       5)
SG_5_15_cm_toc = get_toc(data[,`sg_soc_5-15`],
                         data[,`sg_bdod_5-15`]/100,
                         10)
SG_15_30cm_toc = get_toc(data[,`sg_soc_15-30`],
                         data[,`sg_bdod_15-30`]/100,
                         15)
### %OM
polaris_om_0_30_avg = data$polaris_om
polaris_om_0_5 = data$`polaris_om_0-5`
polaris_om_5_15 = data$`polaris_om_5-15`
polaris_om_15_30 = data$`polaris_om_15-30`

SG_om_0_30_avg = data$sg_soc
SG_om_0_5 = data$`sg_soc_0-5`
SG_om_5_15 = data$`sg_soc_5-15`
SG_om_15_30 = data$`sg_soc_15-30`
###Biomass
biom_auc_7_days = data[,biom_auc__7_wdrvi]
biom_auc_14_21_days = data[,biom_auc__21_wdrvi]
###13 million acres
L = list(polaris_0_30cm_toc,polaris_0_5cm_toc,
         polaris_5_15_cm_toc,polaris_15_30cm_toc,
         SG_0_30cm_toc,SG_0_5cm_toc,
         SG_5_15_cm_toc,SG_15_30cm_toc,
         polaris_om_0_30_avg,polaris_om_0_5,
         polaris_om_5_15,polaris_om_15_30,
         SG_om_0_30_avg,SG_om_0_5,
         SG_om_5_15,SG_om_15_30)
names(L)=c('polaris_0_30cm_toc','polaris_0_5cm_toc',
           'polaris_5_15_cm_toc','polaris_15_30cm_toc',
           'SG_0_30cm_toc','SG_0_5cm_toc',
           'SG_5_15_cm_toc','SG_15_30cm_toc',
           'polaris_om_0_30_avg','polaris_om_0_5',
           'polaris_om_5_15','polaris_om_15_30',
           'SG_om_0_30_avg','SG_om_0_5',
           'SG_om_5_15','SG_om_15_30')

### Other
L = list(CDL,polaris_0_30cm_toc,polaris_0_5cm_toc,
         polaris_5_15_cm_toc,polaris_15_30cm_toc,
         SG_0_30cm_toc,SG_0_5cm_toc,
         SG_5_15_cm_toc,SG_15_30cm_toc,
         polaris_om_0_30_avg,polaris_om_0_5,
         polaris_om_5_15,polaris_om_15_30,
         SG_om_0_30_avg,SG_om_0_5,
         SG_om_5_15,SG_om_15_30,
         biom_auc_7_days,biom_auc_14_21_days)
names(L)=c('CDL','polaris_0_30cm_toc','polaris_0_5cm_toc',
           'polaris_5_15_cm_toc','polaris_15_30cm_toc',
           'SG_0_30cm_toc','SG_0_5cm_toc',
           'SG_5_15_cm_toc','SG_15_30cm_toc',
           'polaris_om_0_30_avg','polaris_om_0_5',
           'polaris_om_5_15','polaris_om_15_30',
           'SG_om_0_30_avg','SG_om_0_5',
           'SG_om_5_15','SG_om_15_30',
           'biom_auc_7_days','biom_auc_14_21_days')

# dir.create('scratch/rasters',showWarnings = F)

tmp = get_SF(data[,.(lng,lat)],to_UTM = F) 
tmp1 = get_SF(data[,.(lng,lat)],to_UTM = T) 

# scale = .3
scale = (16^2)
Ls = do.call(cbind,L) %>% as.data.table
Ls = cbind(tmp,Ls)

CRS_UTM = st_crs(tmp1)
CRS_Target = st_crs(tmp)

v <- vect(Ls)
Ls1 = st_transform(Ls,crs = tmp1 %>% st_crs)
v1 <- vect(Ls1)
Res = sqrt(Acre_M * scale)
# UTM -> WGS 84
r1 = terra::rast(extent=ext(v1),resolution=Res,crs=CRS_UTM$input)
r = terra::rast(extent=ext(v),crs=CRS_Target$input,nrows = nrow(r1),ncols=ncol(r1))
# full
# R = terra::rasterize(v, r1,names(v)[1],background=background)
terraOptions(memfrac=0.8,progress=1)
Rasters = mclapply(seq(nrow(field_boundaries)),mc.cores=10,\(x){
  print(x)
  x1 = field_boundaries[x,]
  v1 = crop(v,x1)
  r1 = crop(r,x1)
  if(length(v1)==0){
    print(NA)
    return(NA)
  }
  
  Ras = lapply(names(v),\(y){
    print(y)
    terra::rasterize(v1, r1,field=y,background=NA)
  }) %>% rast
  names(Ras) = names(v)
  writeRaster(Ras,filename = paste0('scratch/rasters/',x,'.tif'),overwrite=T)
  
  # Ras
})
files = list.files('scratch/rasters/',full.names = T)
Rasters = lapply(files,rast)


###Single

Ras = lapply(names(v),\(y){
  print(y)
  y1 = terra::rasterize(v1, r1,field=y,background=NA)
}) %>% rast
names(Ras) = names(v)
writeRaster(Ras,filename = paste0('scratch/rasters/','tmp','.tif'),overwrite=T)
Rasters = list()
Rasters[[1]] = Ras

### focal on cdl
# lapply(1:length(Rasters),\(x){
#   Rasters[[x]]$CDL <<- Rasters[[x]]$CDL %>% focal(w=3,fun=mean) %>% '>'(0) 
# })

# Rasters = back

# terraOptions(memfrac=0.5,progress=1)
# ### Original method
# Rasters = mclapply(seq(length(L)),mc.cores=3,\(x){
#   print(x)
#   x1 = cbind(tmp,L[[x]])
#   name = names(L)[x]
#   colnames(x1)[1] = name
#   Ras = SF_Raster(x1,name,simple=T,
#                   Res = sqrt(Acre_M * scale))[[1]]
#   
#   names(Ras) = names(L)[x]
#   writeRaster(Ras,filename = paste0('scratch/rasters/',x,'.tif'),overwrite=T)
#   
# })
# 
# files = list.files('scratch/rasters/',full.names = T)
# Raster = rast(files)
# terraOptions(memfrac=0.6,progress=1)
# 
# writeRaster(Raster,filename = 'scratch/Pioneer_Rasters.tif',overwrite=T)
# 
# Raster = rast('scratch/Pioneer_Rasters.tif')

# Ras = lapply(seq(nrow(field_boundaries)),\(x){
#   crop(Raster,field_boundaries[x,])
# })
### Add field data ----

# Pioneer B
field_boundaries = st_read('scratch/528.gpkg') 

collect = lapply(seq(length(field_boundaries$field_id)),\(x){
  tmp = field_boundaries[x,]
  x1 = tmp$field %>% sapply(jsonlite::fromJSON)
  data.table(ID=x1[3,][[1]],'Acreage'=x1[4,][[1]])
}) %>% rbindlist()

field_boundaries$field = NULL
field_boundaries$id = NULL
field_boundaries$field_id = NULL
field_boundaries = cbind(field_boundaries,collect)

# Seismic Shoot
# field_boundaries = st_read('scratch/Pioneer_Data/OXY_NAD27_TxNC/OXY_NAD27_TxNC.shp')

field_boundaries = field_boundaries %>% select(OBJECTID,Area_Acres)
colnames(field_boundaries)[1:2] = c('ID','Acreage')


### Plotting ----

tmap_options(max.raster = c(plot = 1e7, view = 1e8)) #$1e6
tmap_mode('view')


var_breaks = list()
names = Rasters[[1]] %>% names %>% '['(-1)
for(ii in names){
  # for(ii in Rasters[[1]] %>% names){
  x = lapply(1:length(Rasters),\(i){
    Rasters[[i]][[ii]] %>% as.vector() %>% na.omit()
  }) %>% do.call(c,.)
  limits = quantile(x,c(.05,.95))
  x = x[x>limits[1]&x<limits[2]] 
  x1 = get_breaks(x,style = 'equal',n = 7,cut = F) 
  
  var_breaks[[ii]] = x1$brks %>% signif(3) %>% unique
}
var_breaks

var_breaks[['CDL']] = c(0,1)
names = names(Rasters[[1]])[-1]
names[1:8] ='Tons Carbon'
names[9:16] ='%OM'
names[17:18] = 'AUC WDRVI' 
names = c('CDL_Excluded',names)

t = tm_basemap("Esri.WorldImagery") + 
tm_shape(field_boundaries) + tm_polygons(alpha = .6) 


for(i in 1:length(Rasters)){
  print(i)
  # for(i in 1:1){
  for(ii in Rasters[[1]] %>% names){
  # for(ii in 'CDL'){
  # for(ii in Rasters[[1]] %>% names %>% '['(1:3)){
    
      if(i==1){
        title = names[ which( names(Rasters[[1]])==ii )]
        t = t + tm_shape(Rasters[[i]][[ii]]) +
          tm_raster(group = ii,palette = 'viridis',style = 'cont',
                      breaks=var_breaks[[ii]],legend.show = T,title = title)
      } else{
        t = t + tm_shape(Rasters[[i]][[ii]]) +
          tm_raster(group = ii,palette = 'viridis',style = 'cont',
                      breaks=var_breaks[[ii]],legend.show = F)
      }
      
    }
}

t1 = t %>% 
  tmap_leaflet() %>%
  leaflet::hideGroup(names(Rasters[[1]]))

htmlwidgets::saveWidget(t1,'scratch/Full_Greenfield.html')

Raster = rast('scratch/Pioneer_Rasters.tif')[[2:6]]

  t
tmap_save(t,'scratch/Pioneer.html')


###
# t = tm_basemap("Esri.WorldImagery") + 
#   tm_shape(Rasters[[1]][[1:3]]) + tm_raster(palette = 'viridis',style = 'cont') +
#   tm_shape(Rasters[[2]][[1:3]]) + tm_raster(palette = 'viridis',style = 'cont') +
#   tm_shape(Rasters[[3]][[1:3]]) + tm_raster(palette = 'viridis',style = 'cont') +
#   tm_shape(Rasters[[4]][[1:3]]) + tm_raster(palette = 'viridis',style = 'cont') +
#   tm_facets(as.layers=T,free.scales = F) 


### Summarise ----

Ls$CDL = NULL
x = mclapply(seq(nrow(field_boundaries)),mc.cores=5,\(x1){
  print(x1)
  x2 = st_filter(Ls,field_boundaries[x1,])
  if(nrow(x2)==0){return(data.table())}
  x2$field_id = field_boundaries[x1,]$ID
  x2$acreage = field_boundaries[x1,]$Acreage
  x2
}) %>% rbindlist
data = x
rm(x)

colnames(data)

#### Sum TOC ----
data[,':='(polaris_0_30cm_toc_sum = rowSums(cbind(polaris_0_5cm_toc,polaris_5_15_cm_toc,polaris_15_30cm_toc)),
           SG_0_30cm_toc_sum = rowSums(cbind(SG_om_0_5,SG_om_15_30,SG_15_30cm_toc))),]

summary = data[,.(acres=acreage[[1]],
                  Tons_Carbon_polaris_0_30cm = sum(polaris_0_30cm_toc_sum*.25) %>% round,
                  Tons_Carbon_SG_0_30cm = sum(SG_0_30cm_toc_sum*.25) %>% round),'field_id']

summary = summary[rev(order(acres))]
summary %>% setnames(c('Field ID','Acreage',
                       'Tons Carbon Polaris 0-30cm','Tons Carbon Soil Grids 0-30cm'))
x <- summary %>% DT::datatable(options(pageLength=23))
htmlwidgets::saveWidget(x,"scratch/Full_Greenfield_TOC_Table.html")

data1 = data
data1[,1:8] = data[,.SD*.25,.SDcols=1:8]
data1[,22:23] = data[,.SD*.25,.SDcols=22:23]

fwrite(summary,'scratch/Full_Greenfield_TOC.csv')
# fwrite(data1,'Scratch/Pioneer_Public_Data.csv')

x = st_read('../../Documents/Protocol_Referemce/Data/6kcchn7e3u_official_teow/official/wwf_terr_ecos.shp')
Ecoregions = x %>% filter(REALM=='NA') #%>% select(geometry) %>% plot
rm(x)

IPCC_Reference_Regions = st_read('../../Documents/Protocol_Referemce/Data/IPCC-WGI-reference-regions-v4_shapefile/IPCC-WGI-reference-regions-v4.shp') %>% 
  # filter(Continent==filter(Continent=='NORTH-AMERICA')'NORTH-AMERICA')
filter(Continent!='hek')

t = tm_basemap("Esri.WorldImagery")  +
  tm_shape(Ecoregions) + 
  tm_polygons(col = 'ECO_NUM',n=14,alpha=.5,popup.vars=colnames(Ecoregions)[4:6]) +
  tm_shape(IPCC_Reference_Regions) + tm_polygons('Acronym',alpha=.5)
t

t_no_sat = tm_shape(Ecoregions) + 
  tm_polygons(col = 'ECO_NUM',n=14,alpha=.5,popup.vars=colnames(Ecoregions)[4:6]) +
  tm_shape(IPCC_Reference_Regions) + tm_polygons('Acronym',alpha=.5)

t_no_sat

tm_shape(Ecoregions) + 
  tm_polygons(col = 'ECO_NUM',n=14,alpha=.5,popup.vars=colnames(Ecoregions)[4:6]) +
  tm_shape(IPCC_Reference_Regions) + tm_polygons('Acronym',alpha=.5)

### Prior ----

cols <- grep("polaris_om_", colnames(tmp_data), value=T)
tmp_data[,(cols):=10^(.SD/10),.SDcols=cols]

## Soil Grids
cols <- grep("sg_soc_", colnames(tmp_data), value=T)


### Hillary
W = c(5,10,15)/30
Ras = (OM_0_5*W[[1]])+(OM_5_15*W[[2]])+(OM_15_30*W[[3]])

conversion_factor=.04
depth = 30
# hectare_M = 0.0001

HA = 10000
# BD cg/cm3
# 1.5 * 1.4 * 30 * 
# SOC (%) * BD (cg/cm³) * depth (cm) * conversion factor = Mg C / hectare
# 
# for hectares, conversion factor = .01
# for acres, conversion factor = .004

#cm  percent om * BD cg * 4000
30 * 1.5 *1.4 * Acre_M

Ras_CA = Ras_BD * Ras * depth #* Acre_M/10000
hist(Ras_CA)

total_C_HA = mclapply(1:nrow(Poly),mc.cores = 6,function(i){
  # for(i in 1:nrow(Poly)){
  
  x = Poly[i,]
  x1 = crop(Ras_CA,x)
  
  x2 = mask(x1,vect(x))
  # x3 = as.points(x2)
  # tm_shape(x2,name = 'OM: 0-30cm') + tm_raster(palette = 'viridis',alpha=1,title = '%OM',style='cont') +
  #   tm_shape(x,name = 'Fields') + tm_polygons(alpha=.5) 
  
  grid = st_make_grid(get_SF(x),what = 'centers',cellsize = 100)  %>%  st_transform(st_crs(L[[1]]))
  x4 = st_coordinates(grid) %>% as.data.frame
  x5 = terra::extract(x2,x4)
  count = sum(!is.na(x5[,2]))
  tons = x5[,2] %>% sum(na.rm = T)
  
  return(tons)
  
})

total = data.table(Region=Poly$Region,'Tons Carbon'=unlist(total_C_HA))
total$Hectares = st_area(Poly) %>% as.numeric() %>% '/'(10000)
total = rbind(total,data.table(Region='Aggregated Regions',Hectares=sum(total$Hectares),'Tons Carbon'=sum(total[[2]])))

setcolorder(total,c('Region','Hectares'))
total$`Tons Carbon` =formatC(total$`Tons Carbon`, format="d", big.mark=",")
total$Hectares =formatC(total$Hectares, format="d", big.mark=",")

gg = ggtexttable(total,theme = ttheme("light"))
ggsave(gg,filename = 'Hillary_SLM/Summary_Table.png',units = 'px',
       height = 1080,width = 1920)
# files1 = df_ximages$local_file
# Rasts = lapply(files1,rast)
# Ras = mosaic(Rasts[[1]],Rasts[[1]])
# Ras = do.call(terra::mosaic,Rasts)
# Ras = 10^Ras
plot(Ras)


tmap_mode('view')
Poly
setcolorder(Poly,'Region')
t = tm_basemap("Esri.WorldImagery")  +
  #tm_shape(basemap) + tm_rgb() +
  # tm_shape(OM_0_5) + tm_raster(palette = 'viridis',alpha=1,title = 'SOC %',style='cont') +
  # tm_shape(OM_5_15) + tm_raster(palette = 'viridis',alpha=1,title = 'SOC %',style='cont') +
  # tm_shape(OM_15_30) + tm_raster(palette = 'viridis',alpha=1,title = 'SOC %',style='cont') +
  
  tm_shape(Ras_CA,name = 'T') + tm_raster(palette = 'viridis',alpha=1,title = '%OM',style='cont') +
  
  tm_shape(Ras,name = 'OM: 0-30cm') + tm_raster(palette = 'viridis',alpha=1,title = '%OM',style='cont') +
  tm_shape(Poly,name = 'Fields') + tm_polygons(alpha=.5) 


t


tmap_save(t,'Hillary_SLM/Polaris_SOC_0_30_Weighted_Average.html') #, width=1920, height=1080

