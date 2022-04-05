library(iRF)
library(rpart)
library(stringr)
library(data.table)
library(parallel)
library(R.utils)
library(ggplot2)
library(plyr)
library(dplyr)
library(terra)
library(sf)
library(tmap)

# source('../Soil_Stratification_Sampling/src/Functions/')
source('src/Vital_Functions.R')
source('../Soil_Stratification_Sampling/src/Functions/SF_Terra.R')
# source('src/Functions/Strata_Sampling_Main.R')
source('src/Helper_Functions/AWS_Helpers.R')


### Basemaps setup
library(basemaps)
set_defaults(map_service ="mapbox", map_type ="satellite",
             map_token ="pk.eyJ1Ijoic2lyaXVzMzMiLCJhIjoiY2t5NmRmdGVjMHVtajJ1cXZ1ZG96b3B4ZyJ9.Q-g4nUhhfsVhdtD2_kCUwA")


### All Cols

# cols = c('lng','lat','voxel_id','acres','field_id',
#          'b','ca','cu','fe','k','mg','mn','na','p',
#          's','zn','om','ph','bufferph','cec',
#          'bg_red','bg_green','bg_blue','bg_nir','bg_red_edge','bg_red_edge_2',
#          'bg_red_edge_3','bg_red_edge_4','bg_swir1','bg_swir2',
#          'biom_auc__7_wdrvi','biom_auc__21_wdrvi',
#          'cl',
#          'topo_elevation','topo_slope','topo_ls','topo_twi',
#          'topo_tpi','topo_insolation','topo_plan_curv','topo_prof_curv',
#          'cdl_2017_class','cdl_2018_class','cdl_2019_class')
# cols = c(cols, colnames(data)[str_detect(colnames(data),'ss_|gssurgo|sg_|polaris_')]) 
