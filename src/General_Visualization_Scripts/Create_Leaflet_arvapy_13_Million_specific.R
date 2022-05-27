
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


names[1:7] ='Tons Carbon'
names[8:15] ='%OM'

t = tm_basemap("Esri.WorldImagery") + 
  tm_shape(field_boundaries) + tm_polygons(alpha = .6) 

t = tm_shape(field_boundaries) + tm_polygons(alpha = .6) 

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

htmlwidgets::saveWidget(t1,'scratch/Regional Carbon Map_non_satellite.html')

Raster = rast('scratch/Pioneer_Rasters.tif')[[2:6]]

t
tmap_save(t,'scratch/Pioneer.html')




### Summmarize ----
names(field_boundaries)[2] = 'Acreage'
field_boundaries$ID = 'Regional'
field_boundaries = st_transform(field_boundaries,st_crs(Ls))
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

data[polaris_0_30cm_toc_sum<1000,hist(polaris_0_30cm_toc_sum)]
data[polaris_0_30cm_toc_sum>1000,polaris_0_30cm_toc_sum:=50]
data[,hist(SG_0_30cm_toc_sum)]

summary = data[,.(acres=acreage[[1]],
                  Tons_Carbon_polaris_0_30cm = sum(polaris_0_30cm_toc_sum*10) %>% round,
                  Tons_Carbon_SG_0_30cm = sum(SG_0_30cm_toc_sum*10) %>% round),'field_id']

summary = summary[rev(order(acres))]
summary %>% setnames(c('Field ID','Acreage',
                       'Tons Carbon Polaris 0-30cm','Tons Carbon Soil Grids 0-30cm'))
summary$Acreage = summary$Acreage %>% str_remove(' acres') %>% as.numeric()
summary[,(c('CO2e Polaris 0-30cm','CO2e Soil Grids 0-30cm')) :=
          list(`Tons Carbon Polaris 0-30cm`*3.67,`Tons Carbon Soil Grids 0-30cm`*3.67)]

x <- summary %>% DT::datatable(options(pageLength=23))
htmlwidgets::saveWidget(x,"scratch/Regional_Carbon_Map_TOC_Table.html")


data1 = data
data1[,1:8] = data[,.SD*.25,.SDcols=1:8]
data1[,22:23] = data[,.SD*.25,.SDcols=22:23]

fwrite(summary,'scratch/Regional_Carbon_Map_TOC.csv')
# fwrite(data1,'Scratch/Pioneer_Public_Data.csv')