load_libraries <- function(){

  library(sp)
  library(raster)
  library(rgdal)
  library(parallel)
  library(maptools)
 
}

#root_dir = "/gpfs/data1/cmongp/ujjwal/cec/cec_mod"
root_dir ="/gpfs/data1/cmongp/lansong/cec_lan" # This is the directory which your scripts are saved, change to your own path
data_dir ="/gpfs/data1/cmongp/ujjwal/cec/Forest Data/" # This is the directory which your data is located, change to your own path 
setup <- function(num_clusters=10){
  load_libraries()
  rasterOptions(maxmemory = 10e+9, chunksize = 10e+08) # comment this out if the machine is not powerful enough
  setwd(data_dir)# set to your own directory, which your data is located 
}