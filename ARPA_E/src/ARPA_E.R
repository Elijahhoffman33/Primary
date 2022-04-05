library(data.table)

# Creata ARPA-E Data ---- 

to_num = function(x){
  as.numeric(as.character(x))
}

files = list.files('~/Lab_Link/AR1K/Data/ARPA-E/',pattern = '.csv',full.names = T)

DL = lapply(files,fread)
sapply(DL,dim)
setdiff(colnames(DL[[5]]),colnames(DL[[1]]))

sapply(DL,function(x){
  sapply(x,function(x1){
    sum(!is.na(x1))
  })
})


sapply(1:length(DL),function(x){
  x1 = tstrsplit(files[[x]],'//') %>% '[['(2) %>% tstrsplit(' ' ) %>% '[['(1)
  DL[[x]][,Source_File:=x1]
})



Data = rbindlist(DL,fill = T)

update = c(1:3,13:43)
Data[,(update):=lapply(.SD,to_num),.SDcols=update]

index = which(is.na(Data[,.(Lat,Lon)]),arr.ind=T)[,1] %>% unique

Data=Data[-index,]

###correct coordinates
Data[Lon>0,Lon:=Lon*-1]

# tmp = Data[Lon>0]
tmp = Data
# tmp = DL[[1]]
df <- data.frame(longitude =tmp$Lon, 
                 latitude = tmp$Lat)

library(sp)
library(leaflet)
coordinates(df) <- ~longitude+latitude
leaflet(df)   %>% addCircleMarkers( 
  radius = 3,
  color = 'black',
  stroke = FALSE, fillOpacity = 1
) %>% #addProviderTiles(providers$Esri.WorldImagery) #%>% 
  addTiles()


library(shiny)

ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    
    leaflet(df)  %>% addTiles() %>% addCircleMarkers(
      radius = 3,
      color = 'dodgerblue2',
      stroke = FALSE, fillOpacity = 0.5
    ) %>% addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
                         circleMarkerOptions = F, polygonOptions = F)
  })
  
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    print(st_bbox(poly))
    box_tmp =st_bbox(poly)
    saveRDS(box_tmp, file = 'box_tmp')
    
  })
}

shinyApp(ui, server)

#####Subsetting 

x = readRDS('box_tmp')
x = data.frame(Long_Site=c(x[1],x[3]),Lat_Site=c(x[2],x[4]))
tmp_b = get_SF(x,CRS = 4269,c_cols = colnames(x),to_UTM = F)

box <- st_make_grid(tmp_b$geometry, n = 1)
st_area(box)/Acre_M
tmp = get_SF(as.data.frame(Data),c_cols = c('Lon','Lat'),CRS = 4269,to_UTM = F)

plot(tmp$geometry)
plot(box,add=T)

tmp3 = tmp[st_intersects(box,tmp)[[1]],]
coords = st_coordinates(tmp3)

Data = Data[!Lon%in%coords[,1]&!Lat%in%coords[,1]]
ARPA_Data = Data

write.csv(ARPA_Data,'~/Lab_Link/AR1K/Data/ARPA-E/Merged/ARPA_E_Merged.csv')


###With color

tmp = Data
df <- data.frame(longitude =tmp$Lon, 
                 latitude = tmp$Lat,
                 Source_File=tmp$Source_File)

coordinates(df) <- ~longitude+latitude

pal <- colorFactor(
  palette = 'Dark2',
  domain = df$Source_File
)

leaflet(df) %>%
  addCircleMarkers( 
    radius = 3,popup = ~Source_File,
    color = ~pal(Source_File),
    stroke = FALSE, fillOpacity = 1,
  ) %>% #addProviderTiles(providers$Esri.WorldImagery) #%>% 
  addTiles()






### Running everything ----


###Jump 1 

ARPA_Data = fread('~/Lab_Link/AR1K/Data/ARPA-E/Merged/ARPA_E_Merged.csv')
ARPA_Data = ARPA_Data[`Start [cm]`==0]
ARPA_Data = ARPA_Data[Source_File!='MattLacey']
# Data[,Depth_range := paste(`Start [cm]`,`Length [cm]`,sep='_')]

colnames(ARPA_Data)
exclude=-c(1:2,5:10,12:22,30,45,47)
ARPA_Data = ARPA_Data[,..exclude]
###

ARPA_Data[,.N,c("Source_File","Field ID")]
# data = ARPA_Data[Source_File=='TNC']
# 
# data[,.N,'Field ID']
# tmp = get_SF(data,c_cols = c('Lon','Lat'))
# tm_basemap("Esri.WorldImagery") + tm_shape(tmp) + tm_dots('Field ID')

# quick_map(interpolate_fields[[3]],"om",scale = .28,mode = 'view')

scale = .28
Variable='om'
F1 = get_SF(interpolate_fields[[1]]) %>% 
  SF_Raster(Variable,simple = T,Res = sqrt(Acre_M*scale))

F2 = get_SF(interpolate_fields[[2]]) %>% 
  SF_Raster(Variable,simple = T,Res = sqrt(Acre_M*scale))

F3 = get_SF(interpolate_fields[[3]]) %>% 
  SF_Raster(Variable,simple = T,Res = sqrt(Acre_M*scale))

# tm = tm_shape(Ras_OM) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,legend.hist = Lhist,n=n) +
#   tm_scale_bar(position = 'right',width=.36,text.size = .5) + tm_layout(legend.outside=T,legend.outside.size=legend_size,legend.hist.height = .5,
#                                                                         legend.hist.width = 1)
style = 'cont'
n=5

library(basemaps)
F2.5 = merge(F1[[1]],F2[[1]]) 
bbox_new = st_bbox(F2.5)
bbox_new = st_bbox(F3[[1]])

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() %>% st_as_sf() %>%  add_buffer(100) %>% st_bbox()

basemap = basemaps::basemap(bbox_new)



# tmap_mode('view')
tmap_mode('plot')
Lhist = F
legend_size = .3
tm_shape(basemap) + tm_rgb() +
# tm = tm_basemap("Esri.WorldImagery") +
  # tm_shape(F1[[1]]) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,n=n) +
  # tm_shape(F2[[1]]) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,n=n) +
 # tm_shape(F3) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,n=n) +
 #  tm_scale_bar(position = 'right')#,text.size = 1)
  
tm_shape(F3[[1]]) + tm_raster(palette = 'viridis',alpha=.8,title = '%OM 0-15cm',style=style,legend.hist = Lhist,n=n) +
  tm_scale_bar(position = c(.5,.05),width=.35,text.size = .5) + tm_layout(legend.outside=T,legend.outside.size=legend_size,legend.hist.height = .5,
                                                                          legend.hist.width = 1)
                                                                          
# setcolorder(data,'Source_File')
# colnames(data)
# cols = colnames(data)[-c(5:ncol(data))]
# data = data[,lapply(.SD,mean,na.rm=T),by=cols]
# data[is.na(data)] = NA



data1 = data[`Field ID`=='M13']

Fields = c('M13','M4',paste('1st Check','2nd Check','3rd Check',sep='_'))
field = Fields[1]
library(gstat)
library(sp)

interpolate_fields = lapply(Fields,function(field){
  
  data = ARPA_Data[`Field ID`%in%str_split(field,'_')[[1]]]#&`Field ID`!='M4'&`Source_File`!='MattLacey']
  minimum = 30
  counts = apply(data,2,function(x)sum(!is.na(x)))
  counts
  index = which(counts>minimum)
  data = data[,..index]
  
  
  setcolorder(data,'Source_File')
  colnames(data)
  cols = colnames(data)[-c(5:ncol(data))]
  data = data[,lapply(.SD,mean,na.rm=T),by=cols]
  data[is.na(data)] = NA
  
  data1 = data
  
  index =is.na(data1) %>% apply(1,any)
  data1 = data1[!index,]
  
  Input = get_SF(data1,c_cols = c('Lon','Lat'),to_UTM = T)
  # quick_map(data1,"OM [%]",c_cols = c('Lon','Lat'),scale = 1.5)
  Input %>% get_hull('concave',1.3)
  
  
  box <- Input$geometry %>% st_make_grid(cellsize = sqrt(Acre_M/4),what = 'centers')
  box %>% st_make_grid(n = 1) %>%  st_area / Acre_M
  
  grid = Input %>% get_hull('concave',1.3) %>%
    add_buffer(Q_Acre*1) %>% 
    get_grid(c(Q_Acre),plot=T)
  
  ###Test run
  
  Variable=colnames(Input)[7]
  
  source('~/Main/Projects/Primary/src/edited_autofitvariogram.R')
  
  # FB = c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 100)*.8
  # FB
  print('hek')
  # browser()
  tmp3 = SF_Krig(Variable,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
                 cutoff=NA,len=10,cv=NA,force_bounds = NA)[[1]]
  
  ###If errors
  
  tmp2 = tmp3
  
  tmp2 = st_transform(tmp2, 4326)
  
  
  
  ### All cols
  cols=colnames(Input)[3:(ncol(Input)-1)]
  
  krig_out = mclapply(cols,mc.cores = 10,SF_Krig,Input,grid,plot=F,IDW = F,Transform = T,p=2,nmax = NA,
                      cutoff=NA,len=10,cv=NA,force_bounds=NA)
  
  
  Out = krig_out %>% lapply(function(x){
    st_drop_geometry('[['(x,1))}) %>% do.call(cbind,.) %>% 
    cbind(.,st_coordinates(krig_out[[1]][[1]])) %>% 
    st_as_sf(.,coords = c("X", "Y"),
             crs = st_crs(krig_out[[1]][[1]])) 
  
  tmp = st_transform(Out, 4326)
  
  ###Data packaging
  
  
  list_cols = function(x){
    data.table(Column=colnames(x),Index=1:ncol(x))
  }
  
  V_Data = cbind(st_coordinates(tmp),st_drop_geometry(tmp))
  # V_Data = tmp
  index = V_Data %>% colnames %>% str_detect('_SD',negate = T)
  V_Data = V_Data[,index]
  
  list_cols(V_Data)
  colnames(V_Data)[c(1,2,8)] = c('lng','lat','om')
  colnames(V_Data) = colnames(V_Data) %>%  
    str_replace_all(' ','_') %>% str_remove_all(',|\\[|\\]|%')
  setDT(V_Data)
  
  
  
  V_Data = cbind(data1[,c(1,4)],V_Data)
  V_Data[,'voxel_id':=seq_len(.N)+100]
  
  # Arva_M13 = V_Data
  
  ###Set Acres/set boundaries
  
  tmp = get_SF(V_Data,4326,c('lng','lat'))
  
  buffer = tmp %>% get_hull('convex',plot=F) %>% st_buffer(-10) 
  
  plot(tmp$geometry,col='blue')
  plot(buffer,add=T)
  
  index = tmp[st_intersects(buffer,tmp)[[1]],'voxel_id'] %>% st_drop_geometry
  V_Data[voxel_id%in%index[[1]],acres:=.25][!voxel_id%in%index[[1]],acres:=.25/1.5]
  
  # setnames(V_Data,'fiscal_year','Year')
  setcolorder(V_Data,c('voxel_id','lng','lat','acres'))
  V_Data
})
# write.csv(V_Data,'~/Lab_Link/AR1K/Data/Saunders.csv',row.names = F)

interpolate_fields[[3]]$`Field ID` = 'Tendoor'

###Stratafication
runs = lapply(interpolate_fields,function(data){

  # data = interpolate_fields[[1]]
  Iterations = 250 #250
  Iter_Run = 20 #20
  # Iter_Run*Iterations
  batch_limit = 250
  x = data[,7:14]
  x$om = NULL
  x$`TC_` = NULL
  y=data$om
  
  irf <-iRF(x,y,n.core = 5)
  tmp_1 = irf$rf.list$importance
  irf_tmp <- data.table(names=names(tmp_1[rev(order(tmp_1)),]),val=tmp_1[rev(order(tmp_1)),])
  Vars = irf_tmp[1:2,names]
  Vars = c(Vars,
           combn(Vars, 2, FUN=paste, collapse='+'))
  
  
  tmp1 <- Sampling_Simulation(data$`Field ID`[[1]],data,4,Vars,Iterations = Iterations,Iter_runs = Iter_Run,
                              allocation = 'Neyman',sampler_type = 'dq',Par = 'outer',batch_limit = batch_limit,
                              exclude=NULL,ncores = 10,Min_Acres_Sample = .1)  
  tmp1

  })


### Results stuff ----



tbf = lapply(runs,'[[',2) %>% rbindlist

tbf = tmp1$Summary
tbf = rbind(tmp1$Summary[,Facet:=NULL],tmp2$Summary)

tb3 = tbf


# ggplot(ARPA_Data,aes(x=`OM [%]`,fill=Source_File)) + geom_histogram(bins = 50,position = 'identity') +
#   facet_wrap(~Source_File) + theme_classic()

tmp = lapply(unique(tbf$Region),Select_Results,x=tbf)
which(sapply(tmp,nrow) != 1 )
tmp
tmp = rbindlist(tmp,use.names = T)
which(tmp$n==1)

###Take less complex option if less complex option is comperable / better
Selected = lapply(1:nrow(tmp),function(x){
  x=tmp[x,]
  x1 = tbf[Region==x$Region&n==x$n]
  x2 = x1[E_Prob<x$E_Prob]
  if(nrow(x2)==0){ return(x) }
  
  complex_x2 = sapply(x2$Strata,complexity)
  complex_x1 = complexity(x$Strata)
  
  if(any(complex_x2 < complex_x1)){
    
    index = which(complex_x2<complex_x1)
    return(x2[index,])
  } else{return(x)}
  
  # return(list(paste0(x$Strata,'  Prob: ',x$E_Prob),x1[E_Prob<x$E_Prob]))
}) %>% rbindlist(use.names = T)

Selected


### Plotting

# tb3$Prob = format(tb3$Prob*100,scientific = F)
span=.3  # mg|mg:0|om|om:0 Acres_Sample
tb3 = merge(tb3,Selected[,c(2,4,7)],by='Region')
dataLine <- tb3 %>%
  group_by(Region) %>%
  summarize(int = mean(n.y),
            int_AS = mean(Acres_Sample.y))
setnames(tb3,'Acres_Sample.x','Acres_Sample')

gg <- ggplot(tb3[n_samples<100&str_detect(Strata,'om',negate=T)],
             # gg <- ggplot(tb3[Acres_Sample<5&str_detect(Strata,'om',negate=T)],
             aes(x=n_samples,y=E_Prob*100,col=Strata,linetype=Allocation)) + theme_classic()+# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = F,method = 'loess',span=span,alpha=1) #  cut(round(N_Ratio),16)
gg = gg + facet_wrap(~Region) + geom_vline(data=dataLine,aes(xintercept=int),alpha=.4)
gg + theme_classic(base_size = 18) + geom_abline(intercept =3,slope=0,alpha=.5)#+ ylim(0,NA) #+ geom_hline(yintercept=1,alpha=.4) + 

Selected

### Select Results
#   ARPA_Data$NH4 = NULL
#   


