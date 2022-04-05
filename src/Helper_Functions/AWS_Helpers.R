### Function to unpacke AWS storage files

# paths = c('ARF_Simulation/data/input/2021_full.gz',
#           'ARF_Simulation/data/input/soil_sample_gdf_generic.pkl',
#           'ARF_Simulation/data/input/soil_samples_raw.pkl')  

get_joblib = function(x){
  library(reticulate)
  
  py_run_string("
import joblib
import geopandas as gpd
import pandas as pd
")
  if(str_detect(x,'.pkl')){
    ### Wrap joblib
    py_run_string("
def joblib_tmp (x):
  x1 = joblib.load(x)
  x1['WKT']  = gpd.array.to_wkt(x1.geometry.values)
  x1 = pd.DataFrame(x1).drop(['geometry'],axis=1)
  return(x1) 
 ")
    
    x3= py$joblib_tmp(x)
    # x2_1 = x2
    # x2_2 = py$joblib_tmp(x)
    # x2 = x2_1`
    ### Unnest and Reclass 
    # x2[,1:ncol(x2)] = apply(x2,2,as.character) 
    # x2[x2=='NULL'] = NA
    # x3 = hablar::retype(x2) 
    # quick_map(x3,'ss_om')
    # 
    # identical(x2_1$lat,x2_2$lat)
    
  } else if(str_detect(x,'.gz')){
    x3 = py$pd$read_parquet(x)
    
  } else{stop('Wrong file type')}
  
  
  setDT(x3)
  return(x3)
  
}
