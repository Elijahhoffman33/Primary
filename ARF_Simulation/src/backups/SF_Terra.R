library(sf)
library(terra)
# library(basemaps)
# library(tmap)


# NAD83
# EPSG:4269

#Defaukt Values
Acre_M=4046.86
Q_Acre = sqrt(Acre_M/4)

###Functions


get_SF = function(x,CRS=4326,c_cols=c("lng", "lat"),to_UTM=T,set_OG=F){
  # WGS84 = 4326 # Default
  # NAD83 = 4269
  #NAD27 / Texas North Central = 32038
  if(set_OG==T){
    OG_CRS <<- CRS
  }
  
  if(!'sf'%in%class(x)){
    tmp = st_as_sf(x = x,                         
                   coords = c_cols,
                   crs = CRS)  
  } else{
    tmp = st_transform(x,crs = 4326)
  }
  
  
  if(to_UTM==T){
    lonlat2UTM = function(lonlat) {
      utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
      if(lonlat[2] > 0) {
        utm + 32600
      } else{
        utm + 32700
      }
    }
    
    UTM = lonlat2UTM(st_coordinates(tmp))
    
    tmp1 = st_transform(tmp, UTM)
  } else{
    tmp1=tmp
  }
  return(tmp1)
}

###Concave hull


get_hull= function(x,type='convex',concavity=1,plot=T){
  
  if(type=='concave'){
    
    out <- concaveman::concaveman(x, concavity = concavity)
    
    gg= ggplot() +
      # geom_sf(data = conc1, color = "red", fill= NA) +
      geom_sf(data = out, color = "blue", fill = NA) +
      geom_sf(data = x) + theme_classic()
  }
  if(type=='convex'){
    
    pts1_u <- st_union(x)
    
    out <- st_convex_hull(pts1_u)
    
    gg = ggplot() +
      geom_sf(data = out) +
      geom_sf(data = x) + theme_classic()
    
  }
  if(plot==T){
    print(gg)
  }
  return(out)
}

### Buffer

add_buffer = function(x,buffer=10,plot=T){
  buf <- st_buffer(x, dist = buffer)
  gg = ggplot() +
    geom_sf(data = buf) + 
    geom_sf(data = x)
  if(plot==T){
    print(gg)
  }
  return(buf)
}

### Create polygon from points, buffer operations 
### Using concave hull

get_polygon = function(tmp,buffer=0,mode='intersect_index'){
  ### intersect_index to create hull and use buffer to remove boundary points
  ### polygon to export polygon 
  y =   tmp %>% get_hull('concave',concavity = 1.5,plot=F) %>% add_buffer(buffer=buffer,plot = F)
  switch(mode,
         'intersect_index' = tmp %>% st_intersects(y) %>% sapply(sum) %>% '=='(0) %>% which,
         'polygon' = y)
}


### Grid

get_grid = function(x,cellsize=Q_Acre,plot=F){
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
  
  if(plot==T){
    print(gg)
  }
  return(grid1)
}


SF_Raster = function(x,Variable,plot=T,rev=T,q=NA,simple=F,Res=sqrt(Acre_M),background=NA){
  CRS = st_crs(x)
  v <- vect(x)
  
  r = terra::rast(extent=ext(v),resolution=Res,crs=CRS$input)
  R = terra::rasterize(v, r,Variable,background=background)
  
  r_out = as.data.frame(R,xy=T)
  return(list(R,r_out))
}



# x = rast(paste0(file,'Field_',Region,'/',Region,'.TIF'))
# writeRaster(strata_out, paste0(file,'Field_',Region,'/',Region,'.TIF'), overwrite=TRUE)


Plot_Raster = function(data,var='om',Rev=T,q=.001,c_cols=c('lng','lat'),scale='D',discrete=F){
  
  plot = get_SF(data,c_cols=c_cols)  %>% SF_Raster(var,rev=Rev,q=q,plot = F) %>% '[['(1)
  
  Variable = colnames(plot)[3]
  if(discrete==T){
    plot[,3] = factor(plot[,3])
    ggplot(plot,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
      viridis::scale_fill_viridis(discrete = T, option = scale)# scale_fill_continuous(type = 'magma') #+geom_point(data=data_v,aes(x=lng,y=lat),alpha=.5)
  } else{
    ggplot(plot,aes(x=x,y=y,fill=get(Variable),z=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
      viridis::scale_fill_viridis(discrete = F, option = scale)# scale_fill_continuous(type = 'magma') #+geom_point(data=data_v,aes(x=lng,y=lat),alpha=.5)
  }
  
}





###Mapping

quick_map = function(x,Variable='om',scale=.25,mode='plot',c_cols = c('lng','lat'),
                     basemap=F,basemap_buffer=10,samples=F,sample_scale=.1,style='cont',
                     SB=T,SB_Width=.36,SB_Pos='right',legend_size=.3,Lhist=F,n=5){
  
  if(any(class(x)%in%c('data.frame','data.table'))){
    Input = get_SF(x,c_cols=c_cols)
  }else{
    Input=x
  }
  
  Ras = SF_Raster(Input,Variable,simple = T,Res = sqrt(Acre_M*scale))[[1]]
  if(mode=='plot'){
    if(basemap==T){
      tmap_mode('plot')
      bbox_new = st_bbox(Ras)
      
      bbox_new <- bbox_new %>%  # take the bounding box ...
        st_as_sfc() %>% st_as_sf() %>%  add_buffer(basemap_buffer,plot = F) %>% st_bbox()
      
      basemap = basemaps::basemap(bbox_new)
      
      tm = tm_shape(basemap) + tm_rgb() +
        tm_shape(Ras) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,legend.hist = Lhist,n=n) +
         tm_layout(legend.outside=T,legend.outside.size=legend_size,legend.hist.height = .5,
                                                                                   legend.hist.width = 1)
    } else{
      tmap_mode('plot')
      
      tm = tm_shape(Ras) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,legend.hist = Lhist,n=n) +
         tm_layout(legend.outside=T,legend.outside.size=legend_size,legend.hist.height = .5,
                                                                                   legend.hist.width = 1)
      
      
      
    }
    if(class(samples)=='logical'&&samples==T){
      tm = tm + tm_shape(Input) + tm_dots(size=sample_scale,col = 'black') + tm_view(symbol.size.fixed = T)
    } else if(class(samples)[[1]]!='logical'){
      samples_tmp = get_SF(samples,c_cols=c_cols)
      tm = tm + tm_shape(samples_tmp) + tm_dots(size=sample_scale,col = 'black') + tm_view(symbol.size.fixed = T)
    } 
    if(SB==T){
      tm = tm + tm_scale_bar(position = SB_Pos,width=SB_Width,text.size = .5)
    }
  }else{
    tmap_mode('view')
    tm = tm_basemap("Esri.WorldImagery") +
      tm_shape(Ras) + tm_raster(palette = 'viridis',alpha=.8,title = Variable,style=style,n=n) +
      tm_scale_bar()#,text.size = 1)
  }
  if(class(samples)=='logical'&&samples==T){
    tm = tm + tm_shape(Input) + tm_dots(size=sample_scale,col = 'black') + tm_view(symbol.size.fixed = T) 
    
  } else if(class(samples)[[1]]!='logical'){
    samples_tmp = get_SF(samples,c_cols=c_cols)
    tm = tm + tm_shape(samples_tmp) + tm_dots(size=sample_scale,col = 'black') + tm_view(symbol.size.fixed = T)
  } 
  return(tm)
}




