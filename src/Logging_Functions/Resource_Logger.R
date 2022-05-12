library(NCmisc)
library(stringr)
args = commandArgs(trailingOnly=TRUE) #Takes in the commnads supplied by the console as agruments. ie. 1-1 for dataset 1 - iteration 1 

limit <- args[1]
sleep <- args[2]

log <- NULL
for(i in 1:as.numeric(limit)){
  val <- top(CPU = T,RAM=T)
  tmp <- system2('uptime',stderr=T)
  # cpu <- as.numeric(stringr::str_trim(strsplit(strsplit(tmp,':')[[1]][5],',')[[1]][1]))
  cpu = val$CPU$total
  ram_used <- val$RAM$used*10^6
  ram_free <- val$RAM$free*10^6
  ram <- (1-ram_free/(ram_free + ram_used))*100
  
  tmp <- data.frame(i,cpu,ram)
  colnames(tmp)[1] <- 'minute'
  if(is.null(log)==T){
    log <- tmp
  } else{
    log <- rbind(log,tmp)
  }
  write.table(log,file="/mnt/c/Users/elija/Desktop/Main/Projects/Primary/src/Logging_Functions/Log.txt")
  Sys.sleep(as.numeric(sleep))
}


