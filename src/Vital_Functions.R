###Must have
Dim <- function(x) if(is.null(d <- dim(x))) length(x) else d

### Grab debug env 
depull = function(j=NULL){
  lapply(ls(parent.frame()),function(o)assign(x =o,value =get(o),envir =.GlobalEnv))
}

rsq <- function(actual, predicted){
  RSS <- sum((actual-predicted)^2)
  TSS <- sum((actual-mean(actual))^2)
  R2 <- (1-(RSS/TSS))
  return(R2)
}

###Display object size
library(data.table)

object_size <- function(obj,summary=F,hd=NA){
  if(summary==T){
    print(format(object.size(obj), units = "auto"))
    print(paste0('Total Usage: ',format(object.size(x=lapply(ls(envir = .GlobalEnv), get,1)), units="auto")))
    total <- as.numeric(object.size(x=lapply(ls(envir = .GlobalEnv), get,1)))
    index <- rev(order(sapply(mget(ls(envir = .GlobalEnv),.GlobalEnv),object.size)))
    Size <- sapply(mget(ls(envir = .GlobalEnv),.GlobalEnv),function(x) format(object.size(x),units='auto'))[index]
    bytes <- sapply(mget(ls(envir = .GlobalEnv),.GlobalEnv),function(x) as.numeric(object.size(x)))[index]
    Name <- names(Size)
    x1 <- data.table(Name,Percent_Total=round(bytes/total*100,2),Size)
    
    if(!is.na(hd)){
      print(head(x1,hd))
    } else{
      return(x1)
    }
  } else{
    format(object.size(obj), units = "auto")
  }
}

# object_size('Hek',T,10)
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

push_global = function(j){
  lapply(ls(), function(o) assign(x = o, value = get(o), envir = .GlobalEnv))
}

get_breaks = function(x,n=NA,style='fixed',breaks,cut=T){
  if(style=='fixed'){
    n = length(breaks) + 1
    x1 = classInt::classIntervals(x, style = style, fixedBreaks = breaks)
  } else{
    x1 = classInt::classIntervals(x,n=n,style=style)
  }
  if(cut==T){
    return(cut(x,x1$brks))
  } else(return(x1))
}


