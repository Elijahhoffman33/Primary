Variable='ss_om'
x = data1 %>% filter(year==2021)
grid1 = data1%>% filter(year==2018)

SF_Krig = function(Variable,x,grid1,plot=F,IDW=F,Transform=F,p=2,nmax=NA,cutoff=NA,len=NA,cv=NA,biom_auc=65,No_Transform=T,Exclude=NA,force_bounds=NA){
  # varibale, sf data, coordiantes
  
  library(sp)
  library(gstat)
  source('src/edited_autofitvariogram.R')
  
  
  if(!is.na(Exclude)){
    x1 = x[,Variable][[1]]
    index = x1>Exclude
    x=x[index,]
    print(dim(x))
  }
  
  CRS = st_crs(x)
  print(Variable)
  x= x[,Variable]
  if(any(is.na(x))) x = x[-which(is.na(x)),]
  colnames(x)[1] = 'Variable' 
  
  df1 =  as(x, "Spatial")
  
  
  raster::crs(df1) = NA
  # raster::crs(df1) <- CRS("+init=epsg:32715")
  raster::crs(df1) <- CRS(CRS$input)
  grid2 =grid1
  grid <- SpatialPoints(grid2)
  raster::crs(grid) <- CRS(CRS$input)
  
  if(IDW==T){
    avg_CV = NA
    R2=NA
    
    if(is.na(nmax)){
      ###? Additions
      grid3 = st_coordinates(grid1)
      st_set_crs(grid3,st_crs(x))
      
      tmp3 = idw(formula=Variable ~ 1, locations=x, newdata=grid1,idp=p)
      
      quick_map(data[year==2021],'ss_om',mode='view')
      quick_map(grid1,'ss_om',mode='view')
      tmp3 = idw(formula=Variable ~ 1, locations=df1, newdata=grid,idp=p)
      quick_map(tmp3,'var1.pred',mode='view')
      
      ### 
    } else{
      tmp3 = idw(formula=Variable ~ 1, locations=df1, newdata=grid,idp=p,nmax=nmax)
    }
    
    fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
      formula = NH4 ~ 1,
      data = as(sf_NH4, "Spatial"),
      nmax = 10, nmin = 3,
      set = list(idp = 0.5) # inverse distance power
    )
    
    interp_IDW <- interpolate(grd_template_raster, fit_IDW)
    
    df2 =data.frame(tmp3$var1.pred)
    df2 = cbind(grid1,df2)
    colnames(df2)[3] = Variable
    
    ggplot(df2,aes(x=X,y=Y,fill=Lesion)) + geom_raster() + theme_classic() + labs(fill=Variable) +
      scale_fill_continuous(type = 'viridis') 
    
    df3 = st_as_sf(x = df2,                         
                   coords = c("X", "Y"),
                   crs = CRS)
    
  } else if(Transform==T){
    # back = df1
    # df1=back
    # c=lambda2
    x1 = df1$Variable
    # print(Variable)
    # print(class(x1))
    # print((x1))
    
    # bc <- MASS::boxcox(x1 + ~ 1)
    # lam = (lambda <- bc$x[which.max(bc$y)])
    # lam
    # 
    
    library("geoR")
    
    BC_fit = function(x1){
      
      box_error <- try(bc2 <- boxcoxfit(x1, lambda2 = TRUE),silent = T)
      if(class(box_error)=='try-error'){
        lambda2 <- 0
        # bc2 <- boxcoxfit(x1)
        box_error <- try(boxcoxfit(x1),silent = T)
        if(class(box_error)=='try-error'){
          lambda2 = abs(min(x1)*1.01)
          bc2 <- boxcoxfit(x1+lambda2)
          lambda1 <- bc2$lambda[1]
          # lambda1 = 0L
          # lambda2 = 0
          
        } else {bc2 <- boxcoxfit(x1)}
        
      } else{
        lambda1 <- bc2$lambda[1]
        lambda2 <- bc2$lambda[2]
      }
      out=list()
      out[['lambda1']] = lambda1
      out[['lambda2']] = lambda2
      return(out)
    }
    
    box_error <- try(bc2 <- boxcoxfit(x1, lambda2 = TRUE),silent = T)
    if(class(box_error)=='try-error'){
      lambda2 <- 0
      # bc2 <- boxcoxfit(x1)
      box_error <- try(bc2 <- boxcoxfit(x1),silent = T)
      if(class(box_error)=='try-error'){
        lambda2 = abs(min(x1)*1.01)
        bc2 <- boxcoxfit(x1+lambda2)
        lambda1 <- bc2$lambda[1]
        # lambda1 = 0L
        # lambda2 = 0
        
      } else {
        # bc2 <- boxcoxfit(x1)}
        lambda1 <- bc2$lambda[1]
      }
    } else{
      lambda1 <- bc2$lambda[1]
      lambda2 <- bc2$lambda[2]
    }
    
    if(No_Transform==T){
      lambda1=0
    }
    
    BCTransform <- function(y, lambda,shift=0) {
      if (lambda == 0L) { log(y+shift) }
      else { ((y+shift)^lambda - 1) / lambda }
    }
    
    BCTransformInverse <- function(yt, lambda,shift) {
      if (lambda == 0L) { exp(yt)-shift }
      else { exp(log(1 + lambda * yt)/lambda)-shift }
    }
    # 
    #     yt <- BCTransform(x1, lambda1,lambda2)
    #     yo <- BCTransformInverse(yt,lambda1,lambda2)
    #     unique(round(yo-x1,8))
    #     
    #     df1$Variable = x1
    
    
    # df1$Variable = x1
    # df1$Variable[!is.finite(df1$Variable)] <- 0
    if(lambda1!=0){
      df1$Variable = BCTransform(df1$Variable,lambda1,lambda2)
    }
    
    
    # x3 = BCTransform(df1$Variable,lambda1,lambda2) 
    diagonal = spDists(t(bbox(df1)), longlat = T)[1, 2]
    # diagonal/3
    # cutoff=diagonal * 0.35
    
    # cutoff=150
    # len=10
    # boundaries = c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 100) * diagonal * 0.35/100
    # df1$Variable = log10(df1$Variable+c)
    Bounds=c(2, 4, 6, 9, 12, 15,25, 35, 50, 65, 80, 100)
    
    if(is.na(cutoff)){
      
      vario.fit = edited_autofitVariogram(Variable~1,
                                          df1,
                                          # data.table(1:length(vgm()$short),vgm()$short,vgm()$long)
                                          # show.vgms(models = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],range=30, nugget = 0,max = 100)
                                          # model = c("Exp", "Sph"),
                                          model = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],
                                          kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
                                          fix.values = c(NA,NA,NA),miscFitOptions = list(min.np.bin=2), ##alpha=(0:7) * 45)
                                          verbose = F,boundaries = NULL)#Bounds * cutoff/100)#,boundaries = numeric(0))#numeric(0),miscFitOptions = list(min.np.bin=2))
      
    } else{
      if(!is.na(force_bounds)){
        vario.fit = edited_autofitVariogram(Variable~1,
                                            df1,
                                            # data.table(1:length(vgm()$short),vgm()$short,vgm()$long)
                                            # show.vgms(models = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],range=30, nugget = 0,max = 100)
                                            # model = c("Exp", "Sph"),
                                            model = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],
                                            kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
                                            fix.values = c(NA,NA,NA),miscFitOptions = list(min.np.bin=2), ##alpha=(0:7) * 45)
                                            verbose = F,boundaries = force_bounds)#Bounds * cutoff/100)#,boundaries = numeric(0))#numeric(0),miscFitOptions = list(min.np.bin=2))
        
        
      } else{
        vario.fit = edited_autofitVariogram(Variable~1,
                                            df1,
                                            # data.table(1:length(vgm()$short),vgm()$short,vgm()$long)
                                            # show.vgms(models = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],range=30, nugget = 0,max = 100)
                                            # model = c("Exp", "Sph"),
                                            model = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],
                                            kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
                                            fix.values = c(NA,NA,NA),miscFitOptions = list(min.np.bin=2), ##alpha=(0:7) * 45)
                                            verbose = F,boundaries = seq(0,cutoff,length=len))#Bounds * cutoff/100)#,boundaries = numeric(0))#numeric(0),miscFitOptions = list(min.np.bin=2))
        
      }
      
    }
    
    
    
    plot(vario.fit)
    
    vario_model = vario.fit$var_model
    
    ### If cv 
    if(!is.na(cv)){
      CVs = lapply(1:cv,function(j){
        if(is.na(cutoff)){
          cv.fit = krige.cv(Variable~1,locations=df1,
                            model = vario_model,nfold=nrow(df1))
          
        } else{
          cv.fit = krige.cv(Variable~1,locations=df1,maxdist=cutoff,
                            model = vario_model,nfold=nrow(df1))
          
        }
        
        
        
        x2 = as.data.frame(cv.fit)
        setDT(x2)
        x2 = cbind(df1$Variable,x2)
        # print(x2[,rsq(V1,var1.pred),by=fold])
        # r2 = x2[,rsq(V1,var1.pred),by=fold][,mean(V1,na.rm=T)]
        # r2=Metrics::mae(x2$V1,x2$var1.pred)
        actual = x2$V1 %>% BCTransformInverse(lambda1,lambda2)
        predicted = x2$var1.pred %>% BCTransformInverse(lambda1,lambda2)
        r2 = actual-predicted
        return(r2)
        # r2 = x2[,rsq(BCTransformInverse(V1,lambda1,lambda2),BCTransformInverse(var1.pred,lambda1,lambda2)),by=fold][,mean(V1,na.rm=T)]
        
      })
      # avg_CV = mean(unlist(CVs),na.rm=T)
      avg_CV =CVs[[1]]
      
    } else avg_CV = NA
    
    
    
    ###Prediction Kriging
    if(is.na(cutoff)){
      kriged = krige(Variable~1,locations=df1,
                     model = vario_model,newdata=grid)
      
    } else{
      kriged = krige(Variable~1,locations=df1,maxdist=cutoff,
                     model = vario_model,newdata=grid)
      
    }
    
    
    
    if(lambda1==0){
      df2 =data.frame(kriged$var1.pred,
                      sqrt(kriged$var1.var))
      
    } else{
      df2 =data.frame(BCTransformInverse(kriged$var1.pred,lambda1,lambda2),
                      sqrt(abs(BCTransformInverse(kriged$var1.var,lambda1,lambda2))))
      
    }
    colnames(df2)[1:2] = c(Variable,paste0(Variable,'_SD'))
    df2 = cbind(coordinates(grid),df2)
    # colnames(df2)[3] = Variable
    
    ggplot(df2,aes(x=X,y=Y,fill=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
      scale_fill_continuous(type = 'viridis')
    
    df3 = st_as_sf(x = df2,                         
                   coords = c("X", "Y"),
                   crs = CRS)
    
    
    # lzn.vgm <- variogram(Variable~1,df1) # calculates sample variogram values 
    # lzn.fit <- fit.variogram(lzn.vgm, model=fit.v)#, fit.kappa = TRUE) # fit model
    # plot(lzn.vgm, lzn.fit) # plot the sample values, along with the fit model
    # 
    # x1 = df1$Variable
    
    # robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}
    # Standardization = function(x){(x-mean(x))/sd(x)}
    # Min_max_norm = function(x){(x-min(x))/(max(x)-min(x))}
    # denormalized_d = normalized_d * (max_d - min_d) + min_d
    # 
    # library(MASS)
    # c=1
    # bc <- boxcox(x1+c ~ 1)
    # lam = (lambda <- bc$x[which.max(bc$y)])
    # plot(sort((x1+c)^lam))
    # plot(sort(x1+c))
    # plot(sort(log10(x1+c)))
    # 
    # plot(sort(log10(x1+c)))
    # plot(sort(Min_max_norm(x1+c)))
    # 
    # 
    # plot(x1)
    # plot(log(x1))
    # plot(Standardization(x1))
    # plot(Min_max_norm(x1))
    # plot(sort(robust_scalar(x1)))
    # 
    
    
  } else{
    avg_CV = NA
    R2=NA
    raster::crs(df1) = NA
    # raster::crs(df1) <- CRS("+init=epsg:32715")
    raster::crs(df1) <- CRS(CRS$input)
    grid2 =grid1
    grid <- SpatialPoints(grid2)
    raster::crs(grid) <- CRS(CRS$input)
    
    # grid <- SpatialPoints(grid2)
    # raster::crs(grid) <- CRS("+init=epsg:32715")
    # kriging_result = automap::autoKrige(Variable~1, input_data =df1,nfnew_data = SpatialPoints(grid1),verbose = F)
    # kriging_result = automap::autoKrige(Variable~1, input_data =df1,kappa=c(seq(0.05, 20, 0.1)),new_data = SpatialPoints(grid1),verbose = F)
    kriging_result = automap::autoKrige(Variable~1, input_data =df1,kappa=c(seq(0.05, 20, 0.1)),new_data = grid,verbose = F)
    # kriging_result = automap::autoKrige.cv(Variable~1, input_data =df1,nfold=4,nfnew_data = SpatialPoints(grid1),verbose = c(T,T))
    if(plot==T){
      plot(kriging_result)
    }
    
    df2 = as.data.frame(kriging_result$krige_output)[,c(1:2,4)]
    colnames(df2)[3:4] = c(Variable,paste0(Variable,'_SD'))
    
    df3 = st_as_sf(x = df2,                         
                   coords = c("X", "Y"),
                   crs = CRS)
    
    ggplot(df2,aes(x=X,y=Y,fill=Lesion)) + geom_raster() + theme_classic() + labs(fill=Variable) +
      scale_fill_continuous(type = 'viridis') 
    
  }
  
  
  
  
  # v <- vect(df3)
  
  # r = terra::rast(extent=ext(df3),resolution=c(Q_Acre,Q_Acre),crs=CRS$input)
  # R = rasterize(v, r,Variable)
  # r_out = as.data.frame(R,xy=T)
  # colnames(r_out)[3] = Variable
  
  return(list('SF'=df3,'R_df'=NA,'Avg_CV'=avg_CV))
}



### Backup

# {
#   # x= as.data.frame(x)
#   source('src/edited_autofitvariogram.R')
#   if(Variable=='biom_auc__7_wdrvi'){
#     t = x$biom_auc__7_wdrvi>65
#     tt=which(t)
#     x=x[t,]
#   }
#   
#   if(!is.na(Exclude)){
#     x1 = x[,Variable][[1]]
#     index = x1>Exclude
#     x=x[index,]
#     print(dim(x))
#   }
#   
#   CRS = st_crs(x)
#   print(Variable)
#   x= x[,Variable]
#   if(any(is.na(x))) x = x[-which(is.na(x)),]
#   colnames(x)[1] = 'Variable' 
#   
#   df1 =  as(x, "Spatial")
#   
#   
#   raster::crs(df1) = NA
#   # raster::crs(df1) <- CRS("+init=epsg:32715")
#   raster::crs(df1) <- CRS(CRS$input)
#   grid2 =grid1
#   grid <- SpatialPoints(grid2)
#   raster::crs(grid) <- CRS(CRS$input)
#   
#   if(IDW==T){
#     avg_CV = NA
#     R2=NA
#     
#     if(is.na(nmax)){
#       tmp3 = idw(formula=Variable ~ 1, locations=df1, newdata=grid,idp=p) 
#     } else{
#       tmp3 = idw(formula=Variable ~ 1, locations=df1, newdata=grid,idp=p,nmax=nmax)
#     }
#     
#     
#     df2 =data.frame(tmp3$var1.pred)
#     df2 = cbind(grid1,df2)
#     colnames(df2)[3] = Variable
#     
#     ggplot(df2,aes(x=X,y=Y,fill=Lesion)) + geom_raster() + theme_classic() + labs(fill=Variable) +
#       scale_fill_continuous(type = 'viridis') 
#     
#     df3 = st_as_sf(x = df2,                         
#                    coords = c("X", "Y"),
#                    crs = CRS)
#     
#   } else if(Transform==T){
#     # back = df1
#     # df1=back
#     # c=lambda2
#     x1 = df1$Variable
#     # print(Variable)
#     # print(class(x1))
#     # print((x1))
#     
#     # bc <- MASS::boxcox(x1 + ~ 1)
#     # lam = (lambda <- bc$x[which.max(bc$y)])
#     # lam
#     # 
#     
#     library("geoR")
#     
#     BC_fit = function(x1){
#       
#       box_error <- try(bc2 <- boxcoxfit(x1, lambda2 = TRUE),silent = T)
#       if(class(box_error)=='try-error'){
#         lambda2 <- 0
#         # bc2 <- boxcoxfit(x1)
#         box_error <- try(boxcoxfit(x1),silent = T)
#         if(class(box_error)=='try-error'){
#           lambda2 = abs(min(x1)*1.01)
#           bc2 <- boxcoxfit(x1+lambda2)
#           lambda1 <- bc2$lambda[1]
#           # lambda1 = 0L
#           # lambda2 = 0
#           
#         } else {bc2 <- boxcoxfit(x1)}
#         
#       } else{
#         lambda1 <- bc2$lambda[1]
#         lambda2 <- bc2$lambda[2]
#       }
#       out=list()
#       out[['lambda1']] = lambda1
#       out[['lambda2']] = lambda2
#       return(out)
#     }
#     
#     box_error <- try(bc2 <- boxcoxfit(x1, lambda2 = TRUE),silent = T)
#     if(class(box_error)=='try-error'){
#       lambda2 <- 0
#       # bc2 <- boxcoxfit(x1)
#       box_error <- try(bc2 <- boxcoxfit(x1),silent = T)
#       if(class(box_error)=='try-error'){
#         lambda2 = abs(min(x1)*1.01)
#         bc2 <- boxcoxfit(x1+lambda2)
#         lambda1 <- bc2$lambda[1]
#         # lambda1 = 0L
#         # lambda2 = 0
#         
#       } else {
#         # bc2 <- boxcoxfit(x1)}
#         lambda1 <- bc2$lambda[1]
#       }
#     } else{
#       lambda1 <- bc2$lambda[1]
#       lambda2 <- bc2$lambda[2]
#     }
#     
#     if(No_Transform==T){
#       lambda1=0
#     }
#     
#     BCTransform <- function(y, lambda,shift=0) {
#       if (lambda == 0L) { log(y+shift) }
#       else { ((y+shift)^lambda - 1) / lambda }
#     }
#     
#     BCTransformInverse <- function(yt, lambda,shift) {
#       if (lambda == 0L) { exp(yt)-shift }
#       else { exp(log(1 + lambda * yt)/lambda)-shift }
#     }
#     # 
#     #     yt <- BCTransform(x1, lambda1,lambda2)
#     #     yo <- BCTransformInverse(yt,lambda1,lambda2)
#     #     unique(round(yo-x1,8))
#     #     
#     #     df1$Variable = x1
#     
#     
#     # df1$Variable = x1
#     # df1$Variable[!is.finite(df1$Variable)] <- 0
#     if(lambda1!=0){
#       df1$Variable = BCTransform(df1$Variable,lambda1,lambda2)
#     }
#     
#     
#     # x3 = BCTransform(df1$Variable,lambda1,lambda2) 
#     diagonal = spDists(t(bbox(df1)), longlat = T)[1, 2]
#     # diagonal/3
#     # cutoff=diagonal * 0.35
#     
#     # cutoff=150
#     # len=10
#     # boundaries = c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 100) * diagonal * 0.35/100
#     # df1$Variable = log10(df1$Variable+c)
#     Bounds=c(2, 4, 6, 9, 12, 15,25, 35, 50, 65, 80, 100)
#     
#     if(is.na(cutoff)){
#       
#       vario.fit = edited_autofitVariogram(Variable~1,
#                                           df1,
#                                           # data.table(1:length(vgm()$short),vgm()$short,vgm()$long)
#                                           # show.vgms(models = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],range=30, nugget = 0,max = 100)
#                                           # model = c("Exp", "Sph"),
#                                           model = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],
#                                           kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
#                                           fix.values = c(NA,NA,NA),miscFitOptions = list(min.np.bin=2), ##alpha=(0:7) * 45)
#                                           verbose = F,boundaries = NULL)#Bounds * cutoff/100)#,boundaries = numeric(0))#numeric(0),miscFitOptions = list(min.np.bin=2))
#       
#     } else{
#       if(!is.na(force_bounds)){
#         vario.fit = edited_autofitVariogram(Variable~1,
#                                             df1,
#                                             # data.table(1:length(vgm()$short),vgm()$short,vgm()$long)
#                                             # show.vgms(models = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],range=30, nugget = 0,max = 100)
#                                             # model = c("Exp", "Sph"),
#                                             model = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],
#                                             kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
#                                             fix.values = c(NA,NA,NA),miscFitOptions = list(min.np.bin=2), ##alpha=(0:7) * 45)
#                                             verbose = F,boundaries = force_bounds)#Bounds * cutoff/100)#,boundaries = numeric(0))#numeric(0),miscFitOptions = list(min.np.bin=2))
#         
#         
#       } else{
#         vario.fit = edited_autofitVariogram(Variable~1,
#                                             df1,
#                                             # data.table(1:length(vgm()$short),vgm()$short,vgm()$long)
#                                             # show.vgms(models = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],range=30, nugget = 0,max = 100)
#                                             # model = c("Exp", "Sph"),
#                                             model = vgm()$short[c(-1,-5,-9,-12,-15,-16,-17,-18,-19,-20)],
#                                             kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
#                                             fix.values = c(NA,NA,NA),miscFitOptions = list(min.np.bin=2), ##alpha=(0:7) * 45)
#                                             verbose = F,boundaries = seq(0,cutoff,length=len))#Bounds * cutoff/100)#,boundaries = numeric(0))#numeric(0),miscFitOptions = list(min.np.bin=2))
#         
#       }
#       
#     }
#     
#     
#     
#     plot(vario.fit)
#     
#     # plot(variogram(Variable~1,df1, cloud=T))
#     
#     vario_model = vario.fit$var_model
#     # vg <- variogram(Variable~1,df1) # calculates sample variogram values
#     # plot(vg)
#     #psill,  model,  range,  nugget
#     # vgm = vgm(1, "Sph", 150, .1, kappa = NA,
#     #           add.to = vgm(1, "Gau",150, .1))
#     # 
#     # 
#     # vgm = vgm(.75, "Wav", 70, .05, kappa = 1)
#     
#     # plot(vg,vgm)
#     # vario.fit <- fit.variogram(vg, model=vgm,fit.kappa = T,fit.sills = T,fit.ranges = T)#, fit.kappa = TRUE) # fit model
#     # plot(vg, vario.fit) # plot the sample values, along with the fit model
#     
#     
#     # # plot(vario.fit$exp_var,vario.fit$var_model)
#     # hscat(Variable~1,df1,breaks = vario.fit$exp_var$dist)
#     # 
#     # fit.v = vario.fit$var_model
#     # 
#     
#     
#     if(!is.na(cv)){
#       CVs = lapply(1:cv,function(j){
#         if(is.na(cutoff)){
#           cv.fit = krige.cv(Variable~1,locations=df1,
#                             model = vario_model,nfold=nrow(df1))
#           
#         } else{
#           cv.fit = krige.cv(Variable~1,locations=df1,maxdist=cutoff,
#                             model = vario_model,nfold=nrow(df1))
#           
#         }
#         
#         
#         
#         x2 = as.data.frame(cv.fit)
#         setDT(x2)
#         x2 = cbind(df1$Variable,x2)
#         # print(x2[,rsq(V1,var1.pred),by=fold])
#         # r2 = x2[,rsq(V1,var1.pred),by=fold][,mean(V1,na.rm=T)]
#         # r2=Metrics::mae(x2$V1,x2$var1.pred)
#         actual = x2$V1 %>% BCTransformInverse(lambda1,lambda2)
#         predicted = x2$var1.pred %>% BCTransformInverse(lambda1,lambda2)
#         r2 = actual-predicted
#         return(r2)
#         # r2 = x2[,rsq(BCTransformInverse(V1,lambda1,lambda2),BCTransformInverse(var1.pred,lambda1,lambda2)),by=fold][,mean(V1,na.rm=T)]
#         
#       })
#       # avg_CV = mean(unlist(CVs),na.rm=T)
#       avg_CV =CVs[[1]]
#       
#     } else avg_CV = NA
#     
#     
#     
#     ###Prediction Kriging
#     if(is.na(cutoff)){
#       kriged = krige(Variable~1,locations=df1,
#                      model = vario_model,newdata=grid)
#       
#     } else{
#       kriged = krige(Variable~1,locations=df1,maxdist=cutoff,
#                      model = vario_model,newdata=grid)
#       
#     }
#     
#     
#     
#     if(lambda1==0){
#       df2 =data.frame(kriged$var1.pred,
#                       sqrt(kriged$var1.var))
#       
#     } else{
#       df2 =data.frame(BCTransformInverse(kriged$var1.pred,lambda1,lambda2),
#                       sqrt(abs(BCTransformInverse(kriged$var1.var,lambda1,lambda2))))
#       
#     }
#     colnames(df2)[1:2] = c(Variable,paste0(Variable,'_SD'))
#     df2 = cbind(coordinates(grid),df2)
#     # colnames(df2)[3] = Variable
#     
#     ggplot(df2,aes(x=X,y=Y,fill=get(Variable))) + geom_raster() + theme_classic() + labs(fill=Variable) +
#       scale_fill_continuous(type = 'viridis')
#     
#     df3 = st_as_sf(x = df2,                         
#                    coords = c("X", "Y"),
#                    crs = CRS)
#     
#     
#     # lzn.vgm <- variogram(Variable~1,df1) # calculates sample variogram values 
#     # lzn.fit <- fit.variogram(lzn.vgm, model=fit.v)#, fit.kappa = TRUE) # fit model
#     # plot(lzn.vgm, lzn.fit) # plot the sample values, along with the fit model
#     # 
#     # x1 = df1$Variable
#     
#     # robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}
#     # Standardization = function(x){(x-mean(x))/sd(x)}
#     # Min_max_norm = function(x){(x-min(x))/(max(x)-min(x))}
#     # denormalized_d = normalized_d * (max_d - min_d) + min_d
#     # 
#     # library(MASS)
#     # c=1
#     # bc <- boxcox(x1+c ~ 1)
#     # lam = (lambda <- bc$x[which.max(bc$y)])
#     # plot(sort((x1+c)^lam))
#     # plot(sort(x1+c))
#     # plot(sort(log10(x1+c)))
#     # 
#     # plot(sort(log10(x1+c)))
#     # plot(sort(Min_max_norm(x1+c)))
#     # 
#     # 
#     # plot(x1)
#     # plot(log(x1))
#     # plot(Standardization(x1))
#     # plot(Min_max_norm(x1))
#     # plot(sort(robust_scalar(x1)))
#     # 
#     
#     
#   } else{
#     avg_CV = NA
#     R2=NA
#     raster::crs(df1) = NA
#     # raster::crs(df1) <- CRS("+init=epsg:32715")
#     raster::crs(df1) <- CRS(CRS$input)
#     grid2 =grid1
#     grid <- SpatialPoints(grid2)
#     raster::crs(grid) <- CRS(CRS$input)
#     
#     # grid <- SpatialPoints(grid2)
#     # raster::crs(grid) <- CRS("+init=epsg:32715")
#     # kriging_result = automap::autoKrige(Variable~1, input_data =df1,nfnew_data = SpatialPoints(grid1),verbose = F)
#     # kriging_result = automap::autoKrige(Variable~1, input_data =df1,kappa=c(seq(0.05, 20, 0.1)),new_data = SpatialPoints(grid1),verbose = F)
#     kriging_result = automap::autoKrige(Variable~1, input_data =df1,kappa=c(seq(0.05, 20, 0.1)),new_data = grid,verbose = F)
#     # kriging_result = automap::autoKrige.cv(Variable~1, input_data =df1,nfold=4,nfnew_data = SpatialPoints(grid1),verbose = c(T,T))
#     if(plot==T){
#       plot(kriging_result)
#     }
#     
#     df2 = as.data.frame(kriging_result$krige_output)[,c(1:2,4)]
#     colnames(df2)[3:4] = c(Variable,paste0(Variable,'_SD'))
#     
#     df3 = st_as_sf(x = df2,                         
#                    coords = c("X", "Y"),
#                    crs = CRS)
#     
#     ggplot(df2,aes(x=X,y=Y,fill=Lesion)) + geom_raster() + theme_classic() + labs(fill=Variable) +
#       scale_fill_continuous(type = 'viridis') 
#     
#   }
#   
#   
#   
#   
#   # v <- vect(df3)
#   
#   # r = terra::rast(extent=ext(df3),resolution=c(Q_Acre,Q_Acre),crs=CRS$input)
#   # R = rasterize(v, r,Variable)
#   # r_out = as.data.frame(R,xy=T)
#   # colnames(r_out)[3] = Variable
#   
#   return(list('SF'=df3,'R_df'=NA,'Avg_CV'=avg_CV))
# }