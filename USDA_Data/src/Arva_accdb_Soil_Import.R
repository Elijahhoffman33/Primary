library("readxl")
library(dplyr)
library(stringr)


files <- list.files('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Data/MIR_Data/',full.names = T)

Data <- data.frame()
counter <- 0
for(i in files){
  counter <- counter + 1 
  print(counter)
  file <- list.files(i,full.names = T,include.dirs = F)
  file <- file[!str_detect(file,pattern='Spectra')]
  data <- read_excel(file)
  data <- as.data.frame(data)
  if(nrow(data)==0){
    print(file)
    next
  }
  data <- select(data,smp_id,County,State,fiscal_year,Lat_Site,Long_Site,Lat_County_Centroid,Long_County_Centroid,Top_depth_cm,Bottom_depth_cm,horz_desgn_master,horizon_designation,taxonomic_order,taxonomic_suborder,taxonomic_great_group,Soil_property_name,Value_of_Measured_property)
  data <- data[!duplicated(data$smp_id),]
  colnames(data)[ncol(data)] <- data[1,ncol(data)-1]
  data$Soil_property_name <- NULL
  
  if(nrow(Data)==0){
    Data <- data
  } else{
    Data <- merge(Data,data,all=T,by=colnames(Data)[1:15])
  }
  
  
}

setDT(Data)
# -130.840997,17.087584,-57.017676,51.244127

###Exploration

Data_back = Data

Data = Data_back


Data = Data[Long_Site> -130.840997&Long_Site < -57.017676&
              Lat_Site>20.087584&Lat_Site<51.244127&
              !is.na(`Bulk Density, <2mm Fraction, 1/3 Bar`)]

fwrite(Data,'USDA_Data/data/USDA_Data.csv')


Data[,id:=paste0(Long_Site,'__',Lat_Site)]

# data = Data[`Estimated Organic Carbon, Total C, S prep`<10&`Estimated Organic Carbon, Total C, S prep`>0]
# data = data[County=='Saunders']


###Lacey Export ----

colnames(data)[c(1:15,26:27,37,18)]
index=c(1:15,26:27,37,18)
data = data[,..index][,!'id']

cols = c('Bulk Density, <2mm Fraction, 1/3 Bar',colnames(data)[1:6])
plot = get_SF(data[,..cols],to_UTM = F,c_cols = c('Long_Site','Lat_Site')) 

# t =
  tm_basemap("Esri.WorldImagery")  +
  tm_shape(plot) + tm_symbols(size = .1,col='Bulk Density, <2mm Fraction, 1/3 Bar') + tm_view(symbol.size.fixed=T) +

tmap_save(t, "~/Lab_Link/../Scratch/USDA_BD_Locations.html")
fwrite(data,file = '~/Lab_Link/../Scratch/USDA_Samples_Subset.csv')
fwrite(Data[,!'id'],file = '~/Lab_Link/../Scratch/USDA_Samples_Full.csv')

###


###Prep data ----

data=Data
rm(Data)
colnames(data)
exclude=-c(1,c(7:15))
data = data[Bottom_depth_cm<20,..exclude]
# data = data[top_depth_cm<15,..exclude]

colnames(data)
cols = colnames(data)[-c(6:40)]
data = data[,lapply(.SD,mean,na.rm=T),by=cols]
data[is.na(data)] = NA
# [!duplicated(id)]

data[,.N,by=c('Long_Site','Lat_Site')]

Data=data
rm(data)

library(plotly)
library(ggplot2)
library(leaflet)
library(shiny)
library(leaflet.extras)
library(sf)

Data[,.N,by=.(County)][order(N)]



###Full view

df <- data.frame(longitude =Data$Long_Site, 
                 latitude = Data$Lat_Site)


coordinates(df) <- ~longitude+latitude
leaflet(df)  %>% addTiles() %>% addCircleMarkers(
  radius = 3,
  color = 'dodgerblue2',
  stroke = FALSE, fillOpacity = 0.5
)

###Subset View
# Yamhill
data = Data[County=='Saunders']

minimum = 30
counts = apply(data,2,function(x)sum(!is.na(x)))
index = which(counts>minimum)

data = data[,..index][,!'id']


df <- data.frame(longitude =data$Long_Site, 
                 latitude = data$Lat_Site)

coordinates(df) <- ~longitude+latitude
leaflet(df)  %>% addTiles() %>% addCircleMarkers(
  radius = 3,
  color = 'dodgerblue2',
  stroke = FALSE, fillOpacity = 0.5
)
###Interactive region selection

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
    
# 
#     m <- leaflet() %>%
#       addTiles() %>%
#       addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
#                      circleMarkerOptions = F, polygonOptions = F)
  })
  
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    print(st_bbox(poly))
  })
}

shinyApp(ui, server)





#####Subsetting 

###Capture box
# xmin       ymin       xmax       ymax 
# -123.41455   45.27188 -123.29922   45.31954 

x=data.frame(Long_Site=c(-123.41455,-123.29922),Lat_Site=c(45.27188,45.31954))
tmp_b = get_SF(x)

box <- st_make_grid(tmp_b$geometry, n = 1)
st_area(box)/Acre_M



tmp = get_SF(as.data.frame(data),OG_CRS = 4269)

###Simple bounds
# box <- st_make_grid(tmp$geometry, n = 1)
# st_area(box)/Acre_M

plot(tmp$geometry)
plot(box,add=T)
# tmp3 = st_intersection(box,tmp)
tmp3 = tmp[st_intersects(box,tmp)[[1]],]

plot(box)
plot(tmp3$geometry,add=T)



###Run interpolation

get_SF(as.data.frame(data),OG_CRS = 4269)

Input = tmp
Input = tmp3

Input %>% get_hull('concave',1.3)

grid = Input %>% get_hull('concave',1.3) %>%
  add_buffer(Q_Acre*1.5) %>% 
  get_grid(Q_Acre,plot=T)

###Test run
Variable='Estimated Organic Carbon, Total C, S prep'
tmp = SF_Krig(Variable,Input,grid,plot=T)[[1]]
plot = SF_Raster(tmp,Variable,T)[[1]]

ggplot(plot,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() +
  geom_point() + scale_fill_continuous(type = 'viridis')
  # geom_contour_filled() + theme_classic() + labs(fill=Variable) + geom_point()


###Run interpolation

library(parallel)
krig_out = mclapply(colnames(Input)[4:(ncol(Input)-1)],mc.cores = 10,SF_Krig,Input,grid)
# krig_out = lapply(colnames(Input)[4:(ncol(Input)-1)],SF_Krig,Input,grid) 

Out = krig_out %>% lapply(function(x){
  st_drop_geometry('[['(x,1))}) %>% do.call(cbind,.) %>% 
  cbind(.,st_coordinates(krig_out[[1]][[1]])) %>% 
  st_as_sf(.,coords = c("X", "Y"),
           crs = st_crs(krig_out[[1]][[1]]))


# OG_CRS = 4269
tmp = st_transform(Out, OG_CRS)


V_Data = cbind(st_coordinates(tmp),st_drop_geometry(tmp))
Saunders = V_Data

colnames(Saunders)[c(1,2,4)] = c('lng','lat','om')
colnames(Saunders) = colnames(Saunders) %>%  
  str_replace_all(' ','_') %>% str_remove_all(',')
setDT(Saunders)

Saunders = cbind(data[,1:3],Saunders)
Saunders[,'voxel_id':=seq_len(.N)+100]


###Set Acres/set boundaries
tmp = get_SF(Saunders,OG_CRS,c('lng','lat'))
buffer = tmp %>% get_hull('convex',plot=F) %>% st_buffer(-10) 
plot(tmp$geometry,col='blue')
plot(buffer,add=T)
index = tmp[st_intersects(buffer,tmp)[[1]],'voxel_id'] %>% st_drop_geometry


Saunders[voxel_id%in%index[[1]],acres:=.25][!voxel_id%in%index[[1]],acres:=.25/1.5]

setnames(Saunders,'fiscal_year','Year')
setcolorder(Saunders,c('voxel_id','lng','lat','acres','Year'))
write.csv(Saunders,'~/Lab_Link/AR1K/Data/Saunders.csv',row.names = F)

plot = SF_Raster(tmp,Variable,T)[[1]]
ggplot(plot,aes(x=x,y=y,z=get(Variable))) + 
  # geom_raster(aes(fill=get(Variable))) + scale_fill_continuous(type = 'viridis')
  geom_contour_filled() + theme_classic() + labs(fill=Variable) + geom_point() +
  geom_point() #,color=get(Variable) +labs()


Variable = 'Estimated Organic Carbon, Total C, S prep'
Variable='Carbon, Total NCS'


###Stratafication
colnames(Saunders)
cols = 9:26

x = Saunders[,..cols][,!'om'][,!'Carbon_Total_NCS']
y = Saunders[,'om'][[1]]

irf <-iRF(x,y,n.core = 5)
tmp_1 = irf$rf.list$importance
irf_tmp <- data.table(names=names(tmp_1[rev(order(tmp_1)),]),val=tmp_1[rev(order(tmp_1)),])
Vars = irf_tmp[1:2,names]



###Run Single
Vars_in = Vars

Run <- Run_Split('Saunders',Saunders,4,Vars,Vars_in,run_voxels = T,
                 Iterations = 100,Iter_runs = 50)


###Run loop
Results <- lapply(Regions,function(I){
  tmp <- Run_Split(Regions[I],Saunders,4,Vars,Vars_in,run_voxels = T,
                   Iterations = 100,Iter_runs = 50)
  print('Saving')
  saveRDS(tmp,paste0('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/Verra_Strata_Tmp/Dir_6/',Regions[I]))
  return(tmp)
})


###Batch_Setup 
save.image("/mnt/c/Users/elija/Desktop/Lab_Shit/R_Space/Worksapces/R_Space_11_17_21.RData")
Regions='Saunders'
Data_IN = Saunders





###Collate Results


files <- list.files('/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/Verra_Strata_Tmp/Dir_6/',full.names = T)
# files <- files[-((length(files)-4):length(files))]

Results <- data.frame()
for(i in files){
  tmp1 <-readRDS(i)$Summary
  Results <- rbind(Results,tmp1)
}
rm(tmp1)

tbf <- Results
rm(Results)
# tbf[,.SD[.N,],by=Region][,1:10]

###All results
Results <- list()
for(i in files){
  tmp1 <-readRDS(i)
  Results[[tmp1$Region]] <- tmp1
}
rm(tmp1)


###Plotting




tb4=tbf

tb4[,Facet:=paste0(Region,'  Acres: ',round(Total_Acres))]
tb4 <- tb4[order(Total_Acres)]
tb4$Facet <- factor(tb4$Facet,levels=unique(tb4$Facet))

levels(factor(tb4[order(Total_Acres),Facet]))

tb3 = tb4
tb3 <- tb4[between(Total_Acres,200,5000)]

span=.6

gg <- ggplot(tb3[(E_Prob*100)<20&Year==base_year],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) +# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.4) + labs(color='Strata') +
  geom_line(stat='smooth',se = T,method = 'loess',span=span,alpha=1,size=.2) #  cut(round(N_Ratio),16)
# gg +facet_wrap(vars(as.factor(round(Total_Acres))),scales = 'free_x') + ylim(0,NA)+ theme_classic() #+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)
gg +facet_wrap(~Facet,scales = 'free_x') + theme_classic(base_size = 18) + ylim(0,NA)#+ geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)#+ geom_vline(xintercept=10,alpha=.4) 



###Regular
tb3 = tbf
# tb=out[Year=='2018']
span=.2
gg <- ggplot(t31[Acres_Sample<150],aes(x=Acres_Sample,y=E_Prob*100,col=Strata)) +# ,linetype=Year geom_point(alpha=.5,size=1) 
  geom_point(alpha=1,size=.5) + labs(color='Strata') +geom_line(stat='smooth',se = T,method = 'loess',span=span) #  cut(round(N_Ratio),16)
# gg +facet_wrap(vars(as.factor(round(Total_Acres))),scales = 'free_x') + ylim(0,NA)+ theme_classic() #+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)
gg + theme_classic(base_size = 18) + ylim(0,NA) + geom_hline(yintercept=1,alpha=.4) + 
  geom_vline(xintercept=10,alpha=.4)# + coord_flip()# + xlim(2.5,50)#+ geom_vline(xintercept=10,alpha=.4) + geom_hline(yintercept=1,alpha=.4)# + coord_flip()# + xlim(2.5,50)



###Region digging

data1=rdata$Strata_Data$`cec:1`$data_in

select_strata = 1
year='2018'
samps = 15
plot_type='raster'

get_result('Saunders',5,4,2014,action='list_strata',plot_type='raster',var='om',baked=F)
get_result('Saunders',5,4,2014,action='plot',plot_type='raster',var='om',baked=F)

get_result('Saunders',1,2,2014,action='plot',plot_type='raster_var',var='om',baked=T)

get_result('Field_6160',1,30,2018,action='plot',plot_type='raster_var',var='om',baked=)

get_result('Field_6184',2,4,2018,action='plot',plot_type='contour',var='om',baked=F,ras_q = .01,force_ratio = F)





stop()

















###Rough Density rankin
FUN=function(x,N){
  x1 = as.matrix(dist(x))
  mean(x1[lower.tri(x1,diag = F)])/N
  
}

x = plot1[,.(Dense=FUN(.SD,.N),N=.N),by=c('State','County'),.SDcols=c('Long_Site','Lat_Site')][order(N)]



















ggplot(plot, aes(x=x, y=y) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


# plot$is_na = is.na(plot$`Bulk Density, <2mm Fraction, 1/3 Bar`)

plot1 = data[Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<10&`Estimated Organic Carbon, Total C, S prep`>0]
plot1[,N:=.N,by=c('State','County')][order()]
# plot1[,dist(Lat_Site:Long_Site)]





Data1 = data[`Estimated Organic Carbon, Total C, S prep`<10&`Estimated Organic Carbon, Total C, S prep`>0]
Data1 = data
Data1[,id:=paste0(Long_Site,'__',Lat_Site)]

Data1 = Data1[County=='Yamhill']
hist(Data1[,.N,by=c('State','id')][,N],25)
Data1[,.N,by=c('State','id')][order(N)]

gg = ggplot(Data1,aes(x=Long_Site,y=Lat_Site,color=State))+ theme_classic() +# geom_hex(bins=25)+
  scale_fill_continuous(type = "viridis") + geom_point()

gg 

ggplotly(gg)






###Else


# GeoportailFrance.orthos
# tmp <- Data[which(Data$Top_depth_cm<=18),]
# tmp <- Data
# tmp <- tmp[!duplicated(Data$),]
# tmp <- tmp[!duplicated(paste(Data$Lat_Site,Data$Long_Site)),]
# tmp <- tmp[is.na(tmp$horz_desgn_master),]
# ###Data export
# tmp$Co <- paste(Data$Lat_Site,Data$Long_Site)
# ggplot
# 
# 39.8283° N, 98.5795° W
# 
# p <- ggmap(get_googlemap(center = c(lon = -98.5795, lat =39.8283 ),
#                          zoom = 11, scale = 2,
#                          maptype ='terrain',
#                          color = 'color'))
# 
# Data_Fill <- Data[,1:6]
# 
# write.csv(Data_Fill,file = '/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Export/Site_Lat_Long.csv',row.names = F)
# write.csv(Data,file = '/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Export/Full_Data.csv',row.names = F)

setDF(Data)
collect <- c()
for(i in colnames(Data)){
  tmp <- sum(!is.na(Data[,i]))
  names(tmp) <- i
  collect <- c(collect,tmp)
}
setDT(Data)
collect
# Data_tmp <- Data[,c(1,2,5)]
# Data_tmp[is.na(Data_tmp)] <- 0 
# data <- as.data.frame(melt.data.table(as.data.table(Data_tmp), measure.vars=c("State")),id.vars=c('Lat_Site','Long_Site'))
# data <-  reshape(Data_tmp ,timevar='State',varying = list(names(Data_tmp$State)), idvar = c('Lat_Site','Long_Site'), direction = "long")

data = plot

###regular
gg = ggplot(plot[CS_N>100&Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<2.5&`Estimated Organic Carbon, Total C, S prep`>0],
            aes(color=County,fill=County,x=`Bulk Density, <2mm Fraction, 1/3 Bar`,y=`Estimated Organic Carbon, Total C, S prep`)) +
  geom_point(alpha=1,size=.3)  + geom_line(stat='smooth',se = T,method = 'loess',span=8) + theme_classic()
ggMarginal(gg,type="density",groupFill=T,size = 10,bins=60) 


###S1ubset
plot[CS_N>100&Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<2.5&`Estimated Organic Carbon, Total C, S prep`>0,
     cor:=cor(`Estimated Organic Carbon, Total C, S prep`,`Bulk Density, <2mm Fraction, 1/3 Bar`,method='spearman'),
by=c('State','County')]

gg = ggplot(plot[cor>0&CS_N>100&Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<10&`Estimated Organic Carbon, Total C, S prep`>0],
            aes(color=County,fill=County,x=`Bulk Density, <2mm Fraction, 1/3 Bar`,y=`Estimated Organic Carbon, Total C, S prep`)) +
  geom_point(alpha=1,size=1)  + geom_line(stat='smooth',se = T,method = 'loess',span=8) + theme_classic()
ggMarginal(gg,type="density",groupFill=T,size = 10,bins=60) 



plot[County=='Linn'&CS_N>100&Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<2.5&`Estimated Organic Carbon, Total C, S prep`>0]

###correleation 
###correleation 
plot[CS_N>100&Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<2.5&`Estimated Organic Carbon, Total C, S prep`>0,
     .(cor=cor(`Estimated Organic Carbon, Total C, S prep`,`Bulk Density, <2mm Fraction, 1/3 Bar`,method='spearman'),N=.N),
     by=c('State','County')][order(cor)]

###Counts
plot[,.N,by=c('State','County')][order(N)]





###Investigate correlaton

data1 = data[County=='Saunders']
data2 = data[State=='New Mexico'&County=='Otero']

data3 = rbind(data1,data2)
plot = data1[Top_depth_cm<15&`Estimated Organic Carbon, Total C, S prep`<10&`Estimated Organic Carbon, Total C, S prep`>0]
plot[,CS_N:=.N,by=c('State','County')][order(N)]
plot[,sCut:=cut(Top_depth_cm,c(0,5,10,15),include.lowest=T)]
plot=plot[CS_N>100]

Set = interaction(cut(plot$Top_depth_cm,c(0,5,10,15),include.lowest=T),plot$County)

# plot$Top_depth_cm = as.factor(plot$Top_depth_cm)
gg = ggplot(plot,
            aes(color=Top_depth_cm,x=`Bulk Density, <2mm Fraction, 1/3 Bar`,y=`Estimated Organic Carbon, Total C, S prep`)) +
  geom_point(alpha=1,size=1)  + geom_line(stat='smooth',se = T,method = 'loess',span=8) + theme_classic() + facet_wrap(~State)
# gg
ggMarginal(gg,type="density",groupFill=T,size = 10,bins=60) 

data4 = melt(data3,id.vars = 1:15)
tmp = data4[,.SD[!is.na(value),.N],by=c('County','variable')][V1>100,table(variable)]
index = names(tmp)[tmp>1]
index = index[(length(index)-3):length(index)*-1]
cols = index

data4 = data4[variable%in%index]
ggplot(data = data4, aes(x = value,fill=County)) +
  geom_histogram() + theme_classic(base_size = 22) + theme(strip.text.x = element_text(size = 9)) +
  facet_wrap(~variable, scales = "free")

cols_x=cols[c(-2,-12)]

# Otero
data_in = data3[County=='Otero',..cols]
data_in = data3[County=='Saunders',..cols]
data_in = na.omit(data_in)

x= data_in[,..cols_x]
setDF(x)
y=data_in[,'Estimated Organic Carbon, Total C, S prep'][[1]]
model = iRF(x=x,local_importance=T,
    y=y)

Imp=model$rf.list$importance
Imp = data.table(Var=rownames(Imp),Imp)
Imp = Imp[rev(order(IncNodePurity))]
head(Imp)
plot(predict(model$rf.list),y)













names(collect)[which(collect>10000)]

Data_binary <- Data[sample(1:nrow(Data),6500,replace=F),-4:-5]
Data_binary[!is.na(Data_binary)] <- 1
Data_binary[is.na(Data_binary)] <- 0

Data_storage <- Data

Data[is.na(Data)] <- 0

hist(Data$Top_depth_cm[Data$Top_depth_cm<100],breaks=150)
sum(Data$Top_depth_cm!=0)

library(measurements)
conv_unit(Data$Top_depth_cm,cm,inch)
Data$Top_depth_cm <- Data$Top_depth_cm/2.54
Data$Bottom_depth_cm <- Data$Bottom_depth_cm/2.54
na.omit(Data$Top_depth_cm!=0)



# Data <- apply(Data,c(2),as.numeric)
# Data[is.na(Data)] <- 0

library(ComplexHeatmap)
Heatmap(as.matrix(Data[sample(1:nrow(Data),6000,replace=F),1:5]),show_row_names = F)



Data_binary <- apply(Data_binary,c(2),as.numeric)
Heatmap(as.matrix(Data_binary),show_row_names = F)



tmp <- rev(sort(table(Data$State)))
collect <- c()
for(i in 1:length(tmp)){
  name <- names(tmp)[i]
  tmp2 <- subset(Data,subset=Data$State==name)
  mean <- round(mean(table(tmp2$County)))
  collect <- c(collect,mean)
  median <- median(table(tmp2$County))
  len <- length(unique(tmp2$County))
  tmp[i] <- paste(tmp[i],len,mean,median,sep=':') 
}

##total carbon vs estimated carbon
x <- Data$`Carbon, Total NCS`
y <- Data$`Estimated Organic Carbon, Total C, S prep`
ggplot() + geom_point(aes(x=x,y=y),alpha=.3) + labs(title='Total Carbon vs Estimated Carbon',x='Total Carbon',y='Estimated Carbon') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                            panel.background = element_blank(), axis.line = element_line(colour = "black"))

###Data prep

collect <- c()
for(i in colnames(Data[-1:-5])){
  tmp <- sum(!is.na(Data[,i]))
  names(tmp) <- i
  collect <- c(collect,tmp)
}

selected <- names(collect)[which(collect>10000)]

Data_In <- Data[,c(1:5,which(colnames(Data)%in%selected))]
Data_In <- Data_In[c(-1,-4,-5)]
Data_In <- subset(Data_In,subset=Data_In$State=='California')
z
Data_In$`Carbon, Total NCS` <- NULL
Data_In$State <- NULL
Data_In$County <- NULL
Data_In[is.na(Data_In)] <- 0

tmp <- subset(Data_In,subset=Data_In$County=='Humboldt')
  
###iRF

iRF_run <- function(data,Covar,I){
  
  print(I)
  
  library(caret)
  library(data.table)
  library(dplyr)
  library(tictoc)
  
  data <- Data_In
  
  Mega_Set <- list()
  for(ii in 1:1){
    set.seed(ii)
    data <- data
    # tmp <- createFolds(1:nrow(data),5)
    # Subsets <- list()
    # print(paste('Shuffle:',ii))
    # for(i in 1:1){
    #   Subsets[[i]] <- 0
    #   Subsets[[i]]$training <- data[-tmp[[i]],]
    #   Subsets[[i]]$test <- data[which(seq(1:nrow(data))%in%tmp[[i]]),]
    # }
    # 
    
    
    set.seed(ii)
    data <- data
    tmp <- groupKFold(data$County,5)
    Subsets <- list()
    print(paste('Shuffle:',ii))
    for(i in 1:1){
      Subsets[[i]] <- 0
      Subsets[[i]]$training <- data[tmp[[i]],]
      Subsets[[i]]$test <- data[-which(seq(1:nrow(data))%in%tmp[[i]]),]
    }
    
    error_out <- list()
    for(i in 1:1){
      
      train_set <- Subsets[[i]]$training
      # train_set$voxel_id <- NULL
      train_set$County <- NULL
      # assign(paste('train_set',ii),train_set$pH)
      test_set <- Subsets[[i]]$test
      test_set$County <- NULL
      # samples <- test_set$voxel_id
      # test_set$voxel_id <- NULL
      
      #Create test and training sets
      
      
      
      
      X.test <- test_set
      X.test$`Estimated Organic Carbon, Total C, S prep` <- NULL
      
      X.train <- train_set
      X.train$`Estimated Organic Carbon, Total C, S prep` <- NULL
      
      Y.test <- test_set$`Estimated Organic Carbon, Total C, S prep`
      Y.train <- train_set$`Estimated Organic Carbon, Total C, S prep`
      
      #
      library(iRF)
      library(Metrics)
      rit.param <- list(depth=5, nchild=5, ntree=500, class.id=1)
      ncores <- 5
      
      set.seed(333)
      #print('here')
      tic()
      model_2 <-  iRF(x = X.train,
                      y = Y.train,
                      xtest = X.test,
                      ytest = Y.test,
                      n.iter = 10,   # Number of iterations
                      n.core = ncores,    # Use n cores for parallel training
                      # Return the iteration with highest OOB accuracy
                      select.iter = TRUE,
                      # Number of bootstrap samples to calculate stability scores
                      n.bootstrap = 30,oob.importance = TRUE,
                      
                      # Use ranger or randomForest as the underlying random forest package 
                      ###Note, interaction and importances change between randomfotest implementations
                      type = 'randomForest',
                      # Parameters for RIT
                      rit.param=rit.param
      )
      
      toc()
      # Predict on the test set with the selected random forest
      pred <- predict(model_2$rf.list, X.test)
      result <- data.frame(predicted = pred,
                           actual = Y.test)
      
      Actual <- result$actual
      Predicted <- result$predicted
      
      rsq <- function(actual, predicted){
        RSS <- sum((actual-predicted)^2)
        TSS <- sum((actual-mean(actual))^2)
        R2 <- (1-(RSS/TSS))
        return(R2)
        } 
      
      R2 <- rsq(Actual,Predicted)
      lm = lm(Actual ~ Predicted)
      
      ggplot() + geom_point(aes(x=Predicted,y=Actual),alpha=.3) + xlim(0,10) + ylim(0,10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      
      ggplot() + geom_point(aes(x=Predicted,y=Actual),alpha=.3)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      
      
      # mae_iRF <- mae(Actual,Predicted)
      # mdae_iRF <- mdae(Actual,Predicted)
      # lm = lm(Actual ~ Predicted)
      # R2_iRF <- summary(lm)$r.squared
      # mean_iRF <- mean(c(Actual-Predicted))
      
      # print(R2_iRF)
      
      collate  <- list()
      collate <- list(mae_iRF,mdae_iRF,R2_iRF,mean_iRF,samples,result)
      names(collate) <- c('mae_iRF','mdae_iRF','R2_iRF','mean_iRF','samples','result')
      error_out[[i]] <- list(collate,model_1) 
      names(error_out[[i]]) <- c('Metrics','model')
      gc(full=T)
      
    }
    
    
    
    
    print('adding to mega set')
    # print(error_out[[1]]$Metrics$R2_iRF)
    Mega_Set[[ii]] <- list(error_out)
    print(Mega_Set[[1]][[1]]$Metrics$R2_iRF)
    dir.create(paste("/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/",Covar,sep=''))
    dir.create(paste("/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/",Covar,'/',I,sep=''))
    saveRDS(Mega_Set,file=paste("/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/",Covar,'/',I,'/','Run_',ii,sep=''))
    Mega_Set <- NULL
    error_out <- NULL
    gc(full=T)
  }
  
  
  
  # files <- list.files(path= "/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/tmp_depo_4fold_No_Sulfur/",full.names = TRUE)
  files <- list.files(path=paste("/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/",Covar,'/',I,sep=''),full.names = TRUE)
  # files <- list.f11iles(path= "/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/tmp/Depo_2_4fold_AR//",full.names = TRUE)
  # tmp <- readRDS(file=files[1])
  
  library(dplyr)
  R2 <- c()
  Vector_t <- c()
  Vector_p <- c()
  for(i in files){
    tmp1 <- readRDS(file=i)
    for(ii in (length(tmp1))){
      
      
      for(iii in (1:1)){
        result <- tmp1[[ii]][[1]][[iii]]$Metrics$result
        samples <- tmp1[[ii]][[1]][[iii]]$Metrics$samples
        
        Actual <- result$actual
        Predicted <- result$predicted
        
        lm = lm(Actual ~ Predicted)
        R2_iRF <- summary(lm)$r.squared
        R2 <- c(R2,R2_iRF)
        
        # if(length(R2)==7){
        #   model <- tmp1[[ii]][[1]][[iii]]$model
        #   R2_iRF
        #   model_metrics <- tmp1[[ii]][[1]][[iii]]$Metrics
        #   saveRDS(model,file='/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Models/Leflore_!')
        #   
        #   stop()
        # }
        vector_t <- Actual
        names(vector_t) <- samples
        vector_p <- Predicted
        names(vector_p) <- samples
        
        Vector_t <- c(Vector_t,vector_t)
        Vector_p <- c(Vector_p,vector_p)
      }
      
      
    }
  }
  
  R2
  
  stop_at <- which(R2==max(R2))
  
  
  library(dplyr)
  R2 <- c()
  Vector_t <- c()
  Vector_p <- c()
  for(i in files){
    tmp1 <- readRDS(file=i)
    for(ii in (length(tmp1))){
      
      
      for(iii in (1:1)){
        result <- tmp1[[ii]][[1]][[iii]]$Metrics$result
        samples <- tmp1[[ii]][[1]][[iii]]$Metrics$samples
        
        Actual <- result$actual
        Predicted <- result$predicted
        
        lm = lm(Actual ~ Predicted)
        R2_iRF <- summary(lm)$r.squared
        R2 <- c(R2,R2_iRF)
        
        if(length(R2)==stop_at){
          model <- tmp1[[ii]][[1]][[iii]]$model
          R2_iRF
          model_metrics <- tmp1[[ii]][[1]][[iii]]$Metrics
          dir.create(paste("/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Models/Export/",Covar,sep=''))
          saveRDS(model,file=paste("/mnt/c/Users/elija/Desktop/Lab_Shit/AR1K/Models/Export/",Covar,'/',I,sep=''))
          
          break
        }
        vector_t <- Actual
        names(vector_t) <- samples
        vector_p <- Predicted
        names(vector_p) <- samples
        
        Vector_t <- c(Vector_t,vector_t)
        Vector_p <- c(Vector_p,vector_p)
      }
      
      
    }
  }
  
  
  
}
