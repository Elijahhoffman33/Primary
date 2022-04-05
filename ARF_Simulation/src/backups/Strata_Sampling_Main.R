### Main Function

Sampling_Simulation <- function(Region,data_in,Splits,Vars,Iterations=100,Iter_runs=10,
                                Threshold=.05,allocation='Neyman',sampler_type='dq',
                                Par='outer',batch_limit=200,ncores=10,
                                exclude=NULL,Min_Acres_Sample=2.5,save_data=F){
  Vars = c(Vars,'om')
  grid <- expand.grid(Covar=Vars,Splits=0:Splits,stringsAsFactors = F)
  
  ### Assign buffer to remove boundaries
  
  # ~31m*2^.5  ### Temporary, thinking of swapping to true boundaries 
  # tmp = data_in %>% get_SF
  # y =   tmp %>% get_hull('concave',concavity = 5,plot=F) %>% add_buffer(buffer=-44,plot = F)
  # index = tmp %>% st_intersects(y) %>% sapply(sum) %>% '=='(0) %>% which #sum
  # rm(tmp)
  # index_buffer = index
  index_buffer = 9000000
  
  if(!is.null(exclude)){
    data_in = data_in[-index_buffer,]
    ### Set Total Acres before CDL removal
    Acres <- sum(data_in$acres)
    
    index_cdl = data_in[,which(Exclude)]
    data_in = data_in[-index_cdl,]
    
  } else{
    data_in = data_in[-index_buffer,]
    ### Set Total Acres
    Acres <- sum(data_in$acres)
  }
  
  ### Create Strata
  Partitions <- apply(grid,1,function(x){
    
    var = as.character(x[[1]])
    split = as.numeric(x[[2]])
    Get_Strata(data_in,var,split)  
    
  }) %>% t 
  
  if(###Check that Strata are ordered/nested correctly 
    sapply(1:nrow(Partitions),function(x){
      x1 = Partitions[x,] %>% as.character %>% table %>% names %>% as.numeric()
      cumsum(x1) != cumsum(1:max(x1))
    }) %>% unlist %>% any
  ) stop('Strata incorrectly ordered')

  ### Save strata data
  Strata_Data <- list(Partitions=Partitions)
  
  Partitions <- cbind(grid,Partitions)
  
  ### Get Run List 
  ### values = Acres / N Samples
  Start = Min_Acres_Sample 
  Increment = .1
  
  Runs = round(Acres/seq(Start,Acres,Increment)) %>% unique
  Runs = data.table(n=Runs)
  
  ### Run Sampling Experiment'
  
  cores_outer = switch(Par,
                       'outer'=ncores,
                       'inner'=1)
  
  Results = mclapply(1:nrow(Partitions),mc.cores = cores_outer,function(I){
    
    df = Run_Sampling(I,batch_limit,Par,Partitions,data_in,Runs,Acres,allocation,Iterations,Iter_runs,sampler_type,index_buffer,ncores=ncores)
    
    ### Stats and summary
    
    true_mean <- data_in[,.(mean=mean(.SD[,om]))]
    setnames(true_mean,'mean','True_Mean')
    x = Partitions[I,]
    Splits_tmp = as.numeric(as.character(x[[2]]))
    Covar = as.character(x[[1]])
    df$True_Mean = true_mean[[1]]
    
    out = df[,.(n_samples=n,Total_Acres=Acres,Acres_Sample=Acres/(n),n_strata=Splits_tmp+1,Strata=paste(Covar,Splits_tmp,sep=':'),
                SD=sd(.SD[,Avg]),Mean=mean(.SD[,Avg]),
                E_Prob=(sum(abs(Avg-True_Mean)>(Threshold*True_Mean))/.N)),by=.(n)]
    
    ### Assign Two tailed probabilities 
    TM = true_mean$True_Mean
    out[,Prob:=ttp1(.SD,'SD',target = Threshold*TM)]
    
    out$Region <- Region
    out$Covar=Covar
    
    out$Allocation = allocation
    setcolorder(out,c('Region','Covar','Strata','n'))
    
    return(list('Summary'=out,Strata_Data=Strata_Data,Run_Index=Runs))
  })
  
  Summary_Inter <- lapply(Results,'[[',1) %>% rbindlist
  Strata_Data_Inter <- lapply(Results,'[[',2)
  Run_Index_Inter <- lapply(Results,'[[',3)
  
  names(Strata_Data_Inter) <- paste(Partitions[,1],Partitions[,2],sep=':')
  names(Run_Index_Inter) <- paste(Partitions[,1],Partitions[,2],sep=':')

  # print(paste0(Region,' Finished, Time Elasped:'))
  Output = list('Region'=Region,'Summary'=Summary_Inter,Strata_Data=Strata_Data_Inter,
                Run_Index=Run_Index_Inter,Index_Buffer=index_buffer)
  if(save_data==T){ # False by default
    Output$Data_in = data
    return(Output)
  } else {
    return(Output)
  }
   
  
}


Run_Sampling = function(i,nlim=200,par='inner',
                        Partitions,data_in,Runs,Acres,allocation,iters,
                        Iter_runs,sampler_type,index_buffer,ncores){
  x = Partitions[i,]
  print(paste0('Partition: ',paste(x[[1]],x[[2]],sep=':')))
  Covar = as.character(x[[1]])
  
  data_in$Strata <- as.numeric(x[-1:-2])
  
  Proportion = Allocate_Strata(data_in,allocation)
  
  ### Split Runs into groups, if greater than nlim
  splits <- round(nrow(Runs)/nlim)
  if(splits<=1){
    run_groups <- seq_len(max(Runs$n))
  }else{
    run_groups <- split(seq_len(max(Runs$n)),cut(seq_len(max(Runs$n)),splits,labels=F))
  }
  
  ###Run sampling loop
  
  cores_inner = switch(par,
                       'inner'=  ncores,
                       'outer'=  1)
  
  mclapply(1:Iter_runs,mc.cores=cores_inner,function(jj){
    if(splits<=1){
      Strata_Sampling(data_in[,c('om','Strata')],iterations = iters,
                      run=Runs,proportion = Proportion,type=sampler_type)
    } else{
      lapply(run_groups,function(j){
        Strata_Sampling(data_in[,c('om','Strata')],iterations = iters,
                        run=Runs[n%in%j],proportion = Proportion,type=sampler_type) 
      }) %>% rbindlist
    }
  }) %>% rbindlist
}

Allocate_Strata = function(x,allocation){
  
  if(x[,.(SD=sd(om)),Strata][,round(SD,5)==0] %>% any){
    allocation = 'PPS'
    allocation <<- 'PPS'
  }
  
  if(allocation == 'PPS'){
    N0 <- x[,sum(acres)]
    proportion <- x[order(Strata), lapply(.SD[,'acres'],function(x){
      sum(x)}), by=.(Strata)]
    proportion[,2] = proportion[,2]/N0
    colnames(proportion)[2]='Proportion'
    
  } else if(allocation=='Neyman'){
    
    x1 = optimStrat::optiallo(1,x$om,stratum=x$Strata) %>% as.data.table %>% distinct
    proportion = x1[order(stratum)]
    colnames(proportion)[1] = 'Strata'
    colnames(proportion)[2]='Proportion'
    
  }
  return(proportion)
}


###Assign Two tailed probabilities 

ttp1 <- function(x,col,base=2.5,target=.1){
  df <-x
  prob <- 2*(1-pnorm(target,0,df[,..col][[1]])) 
  return(prob)
}


###Jump 11111

Strata_Sampling <- function(df,iterations,run,proportion,type){
  n_samples <- sum(run$n)
  
  ### Take all Strata Samples using proportion 
  Strata = sample(proportion$Strata,size = n_samples*iterations,prob = proportion$Proportion,replace=T)
  Strata_Table = table(Strata)
  
  Strata_levels <- proportion$Strata
  
  ### Take all within Strata samples, for each strata
  if(type=='base'){ ### Use either base R sampler, or dqsample
    Strata_Samples <- lapply(Strata_levels,function(x){
      sample(subset(df$om,df$Strata==x),size = Strata_Table[x],replace=T)
    })
    
  } else{
    Strata_Samples <- lapply(Strata_levels,function(x){
      dqrng::dqsample(subset(df$om,df$Strata==x),size = Strata_Table[x],replace=T)
    })
    
  }
  
  ###Summary 
  # s1 = sapply(Strata_Samples,sd)
  # s2 = sapply(Strata_Samples,mean)
  # s3 = data_s[,.(SD=sd(om),Mean=mean(om),.N),C][order(C)]
  # s3$Prop = proportion$Proportion
  # s3$Emp_Mean = s2
  # s3$Emp_SD = s1
  # s3
  
  ### Assign Sample labels, add sample order, per strata
  
  Samples_dt <- lapply(seq_along(Strata_levels),function(x){
    tmp <- data.table(Samples=Strata_Samples[[x]],Strata=Strata_levels[x])
    tmp[,count:=seq_len(.N)]
  }) %>% rbindlist
  
  rm(Strata_Samples)
  ### Assign Strata Labels, add sample order, per strata
  
  Strata = as.data.table(Strata)
  Strata[,index:=seq_len(.N)][,count:=seq_len(.N),by=Strata]
  
  ### order both Sample and Strata by strata and sample order
  
  setkey(Strata,Strata,count)
  setkey(Samples_dt,Strata,count)
  
  ###Use keys to join and assign index
  
  setkey(Samples_dt[Strata, Index := index],Index)
  rm(Strata)
  ### Create intervals of length n_sample * iterations 
  
  template <- as.data.table(run)
  template[,count:=n*iterations]
  
  ### Create rowwise chunks corresponding to individual sampling regimes
  
  template = template[,.(iter=rep(seq_len(iterations),each=.SD[,count/iterations])),by=c('n')]
  
  Samples_dt <- cbind(Samples_dt,template)
  
  ### Aggregate iterations
  Samples_dt[,.(SDs=sd(Samples),Avg=mean(Samples)),by=c('n','iter')]
  
  ### Check proportion of strata
  # y = Samples_dt[,.(count=.N),c('Strata','n')]
  # y[,count_1:=count/sum(count),n][order(n,Strata)][,mean(count_1),Strata]
}


Get_Strata <- function(data,var,split){
  if(var=='om'){
    
    Strata = optimStrat::stratify(data$om,split+1)
  } else{ #Tree based
    
    ### Restrict depth if splits = 0 or 1
    if(split%in%c(0,1)){
      fit <- rpart(as.formula(paste0('om ~ ',var[[1]])), data = data, method = "anova",
                   control=rpart.control(maxdepth = 1,minsplit =(11/.25) ,minbucket = 5/.25))
    } else{
      fit <- rpart(as.formula(paste0('om ~ ',var[[1]])), data = data, method = "anova",
                   control=rpart.control(maxdepth = 1,minsplit =(11/.25) ,minbucket = 5/.25))
    }
    
    
    nodes = nrow(fit$cptable)-1
    if(split>nodes){
      split = nodes
    }
    
    ### Extract strata mappings from tree terminal nodes
    
    y = lapply(0:split,function(x){
      closest = abs(fit$cptable[,2] - x) %>% which.min
      cp <- fit$cptable[closest,'CP']
      fit2 <- prune(fit,cp)
      # if(x>0) {
      #   plot(fit2)
      #   text(fit2)
      # }
      where <- as.character(fit2$where)
      where = where %>% as.numeric %>% rownames(fit2$frame)[.]
      return(where)
    })
    
    ### Build nested strata (Strata N will will be a subset of N-1)
    Strata = rep('1',length(y[[1]]))
    for(x in 1:length(y)){
      if(x==1){
        next
      } else{
        current = y[[x]]
        # table(current)
        x1 = setdiff(current,y[[x-1]])
        
        ###If a previous split is skipped, but is not being skipped (length(x1) = 0)
        if(length(x1)!=2&&length(x1)!=0){
          for(x2 in x1){
            Strata[which(current%in%x2)] = as.character(as.numeric(max(Strata))+1)
          }
          
          ###Standard case
        } else{
          x2 = table(current[current%in%x1]) %>% which.min %>% names 
          Strata[which(current%in%x2)] = x
        }
        
      }
    }
  }
  
  x3 = Strata %>% unique %>% as.numeric 
  
  ### Check ordering
  t1 = 1:max(x3)
  if(length(t1)!=length(x3)||any(cumsum(x3) != cumsum(t1))){
    x4 = Strata %>% as.numeric()
    Strata =c(1:length(unique(x4)))[factor(x4)] %>% as.numeric() %>% as.character()
  }
  return(Strata)
}
