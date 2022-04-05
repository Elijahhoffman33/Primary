
read_parquet = function(path){
  library(reticulate)
  
  # py_install("pandas")
  # py_install("pyarrow")
  pandas <- import("pandas")
  pyarrow <- import("pyarrow")
  pandas$read_parquet(path)
  
}
source('src/Functions/Read_Parquet.R')

# data = read_parquet(path)
# saveRDS(data,'data/temp/tmp')

