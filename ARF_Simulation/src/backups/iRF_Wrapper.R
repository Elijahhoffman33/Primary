
iRF_run <- function(data_in,fold_index,xcols,ycols,ncores=10,Regression=T,interactions=F,single=F){
  setDF(data_in)
  rit.param <- list(depth=5, nchild=2, ntree=1000) 
  
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
  
  
  
  ### Fit Model
  
  model <-  iRF(x = x,
                y = y,
                n.iter = 10,n.core = ncores,
                select.iter = interactions,
                signed=F,verbose = F,
                oob.importance = TRUE,
                type = 'randomForest',
                rit.param=rit.param)
  
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
    
    # mae_iRF <- mae(Actual,Predicted)
    # mdae_iRF <- mdae(Actual,Predicted)
    R2_iRF <- rsq(Actual,Predicted)
    
    collate <- list(Stats=list(mae=NA,mdae=NA,R2=R2_iRF),Model=model,A_P=data.table(Actual,Predicted)) 
  } 
  
  return(collate)
}


Run_Model <- function(data,cols_x,cols_y,Model,K=5,Regression=T,interactions=F,ncores=1,single=F,return_folds=F,
                      CV_Type='Stratified',rep_kfold=NULL,rm_NAs=T,summary=F){
  setDT(data)
  
  if(!is.null(rep_kfold)){
    
    tmp = lapply(1:rep_kfold,function(j) Run_Model(data,cols_x,cols_y,Model=Model,
              K=K,single=single,ncores = ncores))
    
    return(merge_iterations(tmp,rep_kfold))
    
  }
  
  if(rm_NAs==T){
    index = apply(data,1,function(x) any(is.na(x)))
    if(sum(index)>1){
      data = data[!index,]
    }
  }
  
  if(single==T){
    out = iRF_run(data,fold_index = NA,cols_x,cols_y,ncores = ncores,
            Regression=Regression,interactions=interactions,single=T)
    
  } else { # Cross validation
    
    if(length(table( data[,..cols_y][[1]] ))==1){
      CV_Type = 'Simple' 
    }
    
    folds = switch(CV_Type,
                   Simple = caret::createFolds(1:nrow(data),K),
                   Stratified = caret::createFolds(data[,..cols_y][[1]],K)) 
    
    out <- lapply(1:K,function(k){
      if(Model=='iRF'){
        return(iRF_run(data,folds[[k]],cols_x,cols_y,ncores = ncores,Regression=Regression,interactions=interactions))
      }
      
    })
  }
  
  if(return_folds==T){
    return(list(out=out,folds=folds))
  } else if(summary==T){
    return(merge_iterations(out,nested=F))
  }
  else{
    return(out)
  }
  
}  

merge_iterations = function(x,nested=T){

  if(nested==F){
    x = list(x)
  }
  
  x2 = lapply(x,function(x1){
    
    sR2 <- sapply(x1,function(y){
      y1 <- '[['(y,1)
      y1$R2
    })
    
    GI <- rbindlist(lapply(x1,function(y){
      y1 <- '[['(y,2)
      y2 <- y1$rf.list$importance
      data.frame(Covar=rownames(y2),Importance=as.numeric(y2))
    }))
    
    mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
    
    
    return(list(mGI,sR2))
  })
  
  R2s <- sapply(x2,'[[',2)
  GI <- rbindlist(lapply(x2,'[[',1))
  mGI <- GI[,.(Importance=mean(Importance)),by=Covar][rev(order(Importance))]
  SD_Inner = apply(R2s,2,sd)
  SD_Outer = sd(SD_Inner)
  return(list(mGI=mGI,R2=mean(unlist(R2s)),SD_Inner = SD_Inner,SD_Outer=SD_Outer))
  
}

