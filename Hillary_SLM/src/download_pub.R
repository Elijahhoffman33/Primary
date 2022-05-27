library(data.table)
library(soilDB)
library(aqp)
library(stringr)
library(plyr)
library(dplyr)
library(terra)
library(sf)
source('../../Projects/Stratification_Sampling/src/Functions/SF_Terra.R')
source('../../Projects/Primary/src/Vital_Functions.R')

# files = list.files('Hillary_SLM/data/input/',recursive = T,pattern = '.shp',full.names = T)
# L = lapply(files,st_read)
# 
# x = get_SF(L[[1]],to_UTM = T)
# cellsize=10000
# grid =   grid=st_make_grid(x,what = 'centers',cellsize = cellsize) %>% st_transform(st_crs(L[[1]]))
# 
#   
# 
# 
# x1 = st_coordinates(grid) %>% as.data.frame
# x1$id = 1:nrow(x1)
# colnames(x1)[1:2] = c('lon','lat')
# x1 =x1[,c(3,1,2)]
# your.points <- data.frame(id  = seq_leng(length(x1)), 
#                           lat = c(37.9, 38.1), 
#                           lon = c(-120.3, -121.5), 
#                           stringsAsFactors = FALSE)

# 
# 
# x <- try(fetchSoilGrids(x1))
# 
# 
# 
# 
# 
# library(rgdal)
# library(gdalUtils)
# 
# crs = st_crs(L[[1]])
# 
# bb=st_bbox(L[[1]])  # Example bounding box (homolosine) for Ghana
# bb=as.numeric(bb)
# bb1 = bb[c(3,4,1,2)]
# 
# x2 = mukey.wcs(bb, db = c("gssurgo"), res = 50, quiet = FALSE)
# 
# 
# igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
# igh=crs$input
# igh='EPSG:4326'
# sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"
# 
# 
# 
# # (c(ulx,uly,lrx,lry)
# x2 = gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'),
#                     "./crop_roi_igh_r.tif",
#                     tr=c(250,250),
#                     projwin=bb1,
#                     projwin_srs =igh,
#                     verbose=TRUE)
# 
# ###tesno 
# 
# bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine) for Ghana
# igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
# 
# sg_url="https://files.isric.org/soilgrids/latest/data/"
# 
# 
# x2 = gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'),
#                "./crop_roi_igh_r.tif",
#                tr=c(250,250),
#                projwin=bb,
#                projwin_srs =igh,
#                verbose=TRUE)
# ###WCS
# library(XML)

###Polaris
library(XPolaris)
print(exkansas)


files = list.files('Hillary_SLM/data/input/',recursive = T,pattern = '.shp',full.names = T)
names = files %>% strsplit('//')%>% sapply('[[',2) %>% 
  strsplit('/')%>% sapply('[[',1) %>% str_remove('_wgs84')
# L = lapply(files,st_read)
L = lapply(1:length(files),function(x){
  x1 =files[[x]] %>% st_read %>% st_as_sf
  x1$Region = names[x]
  return(x1)
})

Poly = L %>% rbindlist %>% st_as_sf()
Poly = Poly %>% st_as_sf %>% get_SF(to_UTM = T)
grid = st_make_grid(Poly,what = 'centers',cellsize = 1000)  %>%  st_transform(st_crs(L[[1]]))


x1 = st_coordinates(grid) %>% as.data.frame
x1$id = 1:nrow(x1)

x1 =x1[,c(3,2,1)]

colnames(x1) = c('ID','lat','long')
xplot(locations = x1)
library(parallel)
img_out = mclapply( c('0_5','5_15','15_30'),mc.cores = 3,function(x){
  df_ximages = ximages(locations = x1,
                        statistics = c('mean'),
                        variables = c('bd'),
                        layersdepths = x)
  return(df_ximages)
})

img_out = rbindlist(img_out)

# x1 = xsoil(ximages_output = df_ximages)
# load('Hillary_SLM/data/save_rasters')
file.copy(list.files('/tmp/RtmpYFotnY/POLARISOut/mean/bd/',full.names = T), 'Hillary_SLM/data/bd/',recursive = T,copy.mode = T,)


merge_large_rasters = function(x,breaks,final){
  index = list(split(x,cut(seq_along(x),breaks)))[[1]]
  tmp1 = lapply(seq_along(index),function(x1){
    x2 = index[[x1]] %>% lapply(rast) %>% terra::src(.)
    merge(x2,filename=paste0('Hillary_SLM/data/temp/',x1,'.tif'),memmax=30,progress=1,overwrite=T)
  })
  
  files_tmp = list.files('Hillary_SLM/data/temp/',full.names = T)
  index = list(split(files_tmp,cut(seq_along(files_tmp),2)))[[1]]
  tmp1 = lapply(seq_along(index),function(x1){
    x2 = index[[x1]] %>% lapply(rast) %>% terra::src(.)
    merge(x2,filename=paste0('Hillary_SLM/data/temp/',x1,'_merged.tif'),memmax=30,progress=1,overwrite=T)
  })
  
  files_tmp = list.files('Hillary_SLM/data/temp/',full.names = T,pattern = 'merged') %>% lapply(rast) %>% terra::src(.)
  merge(files_tmp,filename=paste0(final,'.tif'),memmax=30,progress=1,overwrite=T)
  unlink(list.files('Hillary_SLM/data/temp/',full.names = T))
}



merge_large_rasters(BD_0_5,breaks = 4,final = 'Hillary_SLM/data/OM_0_15.tif')
merge_large_rasters(BD_5_15,breaks = 4,final = 'Hillary_SLM/data/OM_5_15.tif')
merge_large_rasters(BD_15_30,breaks = 4,final = 'Hillary_SLM/data/OM_15_30.tif')

Poly = Poly %>%  st_transform(st_crs(OM_5_15))

OM_0_5 = rast('Hillary_SLM/data/OM_0_5.tif') %>% crop(Poly) %>% '^'(10,.) 
OM_5_15 = rast('Hillary_SLM/data/OM_5_15.tif.tif') %>% crop(Poly) %>% '^'(10,.) 
OM_15_30 = rast('Hillary_SLM/data/OM_15_30.tif.tif') %>% crop(Poly) %>% '^'(10,.) 

W = c(5,10,15)/30
Ras = (OM_0_5*W[[1]])+(OM_5_15*W[[2]])+(OM_15_30*W[[3]])

###BD
BD_0_5 = list.files('Hillary_SLM/data/bd/0_5/',full.names = T) 
BD_5_15 = list.files('Hillary_SLM/data/bd/5_15/',full.names = T)
BD_15_30 = list.files('Hillary_SLM/data/bd/15_30/',full.names = T) 


merge_large_rasters(BD_0_5,breaks = 4,final = 'Hillary_SLM/data/BD_0_15.tif')
merge_large_rasters(BD_5_15,breaks = 4,final = 'Hillary_SLM/data/BD_5_15.tif')
merge_large_rasters(BD_15_30,breaks = 4,final = 'Hillary_SLM/data/BD_15_30.tif')


Poly = Poly %>%  st_transform(st_crs(rast('Hillary_SLM/data/BD_5_15.tif.tif')))
BD_0_5 = rast('Hillary_SLM/data/BD_0_15.tif.tif') %>% crop(Poly) 
BD_5_15 = rast('Hillary_SLM/data/BD_5_15.tif.tif') %>% crop(Poly)
BD_15_30 = rast('Hillary_SLM/data/BD_15_30.tif.tif') %>% crop(Poly)



W = c(5,10,15)/30
Ras_BD = (BD_0_5*W[[1]])+(BD_5_15*W[[2]])+(BD_15_30*W[[3]])


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


range(Ras$`lat3132_lon-105-104`)


# tmap_save(t,'SLM_Overview.html')  

### Soil grids WCS


library(rgdal)
library(gdalUtils)

voi = "soc" # variable of interest
depth = "5-15cm"
quantile = "mean"

voi_layer = paste(voi,depth,quantile, sep="_") 
bb = Poly %>% st_transform(crs = 4326) %>% st_bbox %>% as.numeric()
bb = bb[c(3,4,1,2)]
# bb = Poly %>% st_transform(crs = 4269) %>% st_bbox %>% as.numeric()
# bb = bb[c(3,4,1,2)]
# bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine)
#get_SD_bb = function(voi,bb){
#  
#  wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
#  wcs_service = "SERVICE=WCS"
#  wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
#  
#  
#  
#  wcs_request = "DescribeCoverage"
#  
#  wcs = paste(wcs_path, wcs_service, wcs_version, wcs_request, sep="&")
#  
#  
#  
#  l1 <- newXMLNode("WCS_GDAL")
#  l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
#  l1.l <- newXMLNode("CoverageName", voi_layer, parent=l1)
#  
#  # Save to local disk
#  xml.out = "./sg1.xml"
#  saveXML(l1, file = xml.out)
#  
#  
#  
#  gdalinfo("./sg1.xml")
#  
#  
#  
#  # bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine)
#  igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
#  igh='EPSG:4326'
#  # 
#  # bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine)
#  # 
#  ###
#  
#  wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
#  wcs_service = "SERVICE=WCS"
#  wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
#  
#  wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3
#  
#  l1 <- newXMLNode("WCS_GDAL")
#  l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
#  l1.l <- newXMLNode("CoverageName", "soc_5-15cm_mean", parent=l1)
#  
#  # Save to local disk
#  xml.out = "./sg1.xml"
#  saveXML(l1, file = xml.out)
#  
#  # Download raster as GeoTIFF (Warning: it can be large!)
#  file.out <- './test.tif'
#  
#  bb = Poly %>% st_transform(crs = 4326) %>% st_bbox %>% as.numeric()
#  bb[c(4,3,2,1)]
#  bb1=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine)
#  gdal_translate(xml.out, file.out,
#                 tr=c(250,250), projwin=bb[c(1,2,3)],
#                 projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
#                 verbose=TRUE)
#  rast('./test.tif') %>% plot
#  unlink('./test.tif')
#
#)
