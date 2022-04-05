import geopandas as gpd
import pandas as pd
import joblib 


x = joblib.load(f"ARF_Simulation/data/input/soil_samples_raw.pkl")
x.to_file(filename= "ARF_Simulation/data/temp/130_samples_shp/130")

# x4 = x
# x['wkt'] = x.geometry.to_wkt()

x3 = gpd.GeoDataFrame(x,geometry=x.geometry)
x3['wkt'] = x3.geometry.to_wkt()
x3.to_file(filename= "ARF_Simulation/data/temp/130_samples_shp/130")

### Interep from chris

g = joblib.load("ARF_Simulation/data/input/soil_sample_gdf_generic.pkl")

Env = pd.read_parquet("ARF_Simulation/data/input/2021_full.gz")
Interp = joblib.load("ARF_Simulation/data/input/soil_sample_gdf_generic.pkl")

Raw_Soil = joblib.load("ARF_Simulation/data/input/soil_samples_raw.pkl")
Raw_Soil.to_file(filename= "ARF_Simulation/data/temp/Shps/")

### In use function 
def joblib_tmp (x):
  x1 = joblib.load(x)
  x1['WKT']  = gpd.array.to_wkt(x1.geometry.values)
  x1 = pd.DataFrame(x1).drop(['geometry'],axis=1)
  return(x1) 

# 
# g = joblib.load("ARF_Simulation/data/input/soil_sample_gdf_generic.pkl")
# # g1 = gpd.read_file('ARF_Simulation/data/input/Chris_interp/interp.gpkg')
# g2 = gpd.read_file('ARF_Simulation/data/input/Chris_interp/raw.gpkg')
# 
# G = gpd.GeoDataFrame(g,geometry=g.geometry)
# G.to_file(filename= "ARF_Simulation/data/temp/Interpolation.gpkg",driver='GPKG')
# # ities_gdf.to_file("package.gpkg", layer='cities', driver="GPKG")
# 
# G1 = gpd.GeoDataFrame(g1,geometry=g1.geometry)
# G2 = gpd.GeoDataFrame(g2,geometry=g2.geometry)

# import json 
# json_path ="ARF_Simulation/data/input/mgmt_dict.json"
# open(json_path)gzip -d file.gz

# aDict = json.loads(json.load(open(json_path)))
# print(aDict)

# L = list()
# for(i in 0:(length(x$geometry)-1)){
#   print(i)
#   L[[i+1]] = x$geometry[[i]] %>% as.character
# }
# samples = L %>% unlist %>% st_as_sfc %>% st_as_sf(crs=4326) 
# samples$sample_dates = py$x$sample_date %>% as.character


### Read the weird .gz parquet files
# x5 = py$x5
