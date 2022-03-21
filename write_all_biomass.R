#overlay the forest data in each county with the cluster in Whole Sierra
#these are all the library that we need to use in this R script.
library(doMC)
library(sp)
library(raster)
library(rgdal)
library(parallel)
library(maptools)
library(sqldf)
library(dplyr)
registerDoMC(21)

# source here indicates that we can reuse some functions that were written in other scripts stored in the shared folder.
source("shared/cec_utils.R")# some functions will be used in this script
source("shared/config.R")# set up the working directory and expose root_dir and data_dir and results_dir variables
source("shared/forest_type.R")# useless
source("shared/data.R")# useless
source("shared/cluster_results_new.R")#useless

# loads F3 forest, elevation, sit raster,stack all F3 forest data with sit data,and write final_csv to Sorted_Sierra_Nevada.csv") and change the variable name to simple name
load_forest_data <- function(){
  log("loading forest data")
  fl <<- list.files(path=data_dir, pattern=glob2rx("Total*.tif$")) # list all the F3 Data  There are total of 60 files 
  fl_1 <<- raster(fl[1])#read first raster  # All the rasters have fl_l's projection 

  fl.ele <<- raster(file.path(data_dir, "ele_raster.tif")) # "Read our original elevation data, load_preproc_raster" is the function from cec_utils.R
  fl.sit <<- raster(file.path(data_dir, "Fl_sit.tif")) # read our sit data

  log("Making a raster brick")
  fl.stack <<- stack(fl)#stack all the Forest data
  fl.stack <<- stack(fl.stack, fl.sit)#stack with the site raster
  fl.stack <<- shorten_fname(fl.stack)#format our variable name with shorter name
}

# write elevation data for the counties in whole Sierra Nevada and extract the values of biomass from F3 data, write elevation, county name, and biomass values for all the pixels in Sierra Nevada
write_ele_biomass_in_county = function(){
  log("read the forest elevation in whole Sierra Nevada")
  fl.ele.mask <<-raster(file.path(data_dir, "fl_ele_Sierra.tif"))# this is the new forest elevation data in Sierra Nevada without excluding NPS and Wildeness area. the white areas indicate the no data(no biomass)
  df.ele <<- rasterToPoints(fl.ele.mask, spatial=T)#total number70857636
  
  log("read the shapefile of county boundary of Sierra Nevada")
  shp <<- shapefile(file.path(data_dir, "CA_Sierra_TIGER2016.shp"))#this is county boundary shapefile
  shp.curr_county.prj <- spTransform(shp, proj4string(fl.ele))#convert to the same project 
  
  log("Getting county's elevation data for Whole Sierra")
  df.ele.curr_county <- over(df.ele, shp.curr_county.prj)#join the elevation data into the county boundary
  df.ele.curr_county.na.omit <- na.omit(cbind(df.ele@data,df.ele@coords,df.ele.curr_county$NAME))#70857636 
  df.ele.curr_county.na.omit$xi=df.ele.curr_county.na.omit$x 
  df.ele.curr_county.na.omit$yi=df.ele.curr_county.na.omit$y
  
  log("extract the values of different biomass variables for pixel data ")
  coordinates(df.ele.curr_county.na.omit)<- ~x+y
  proj4string(df.ele.curr_county.na.omit)<- proj4string(fl.ele)#convert to the spatial point dataFrame based on XY
  df.data.curr_county <- extract(fl.stack, df.ele.curr_county.na.omit, sp=T) #returns the values of biomass for the cells in which a set of points fall
  df.data.curr_county.latlon <- spTransform(df.data.curr_county, CRS("+proj=longlat +datum=WGS84"))#lat lng will be decmial -121.0098 39.63924
  F3_ele_biomass_df=as.data.frame(df.data.curr_county.latlon)
  
  log("Writing into csv file")
  write.csv(F3_ele_biomass_df, file.path(results_dir, "F3_ele_biomass.csv"))#this table includes the elevation,county name,a bunch of variables realted to biomass.
}

#run line by line
main = function() {
  setup()# This is the function in the Config.R
  load_forest_data()
  write_ele_biomass_in_county()
#  endCluster()
}

main()

print("done processing, exiting")
~                                       