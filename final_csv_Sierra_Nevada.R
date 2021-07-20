library(doMC)
library(sp)
library(raster)
library(rgdal)
library(parallel)
library(maptools)
library(foreign)
library(sqldf)
library(dplyr)

source("/gpfs/data1/cmongp/lansong/cec_lan/cec_utils.R")
source("/gpfs/data1/cmongp/lansong/cec_lan/config.R")
source("/gpfs/data1/cmongp/lansong/cec_lan/forest_type.R")
source("/gpfs/data1/cmongp/lansong/cec_lan/data.R")
source("/gpfs/data1/cmongp/lansong/cec_lan/cluster_results_new.R")

# loads F3 forest, elevation, sit raster,stack all F3 forest data with sit data,and change the variable name to simple name
load_forest_data <- function(){
  # loading forest data rasters
  log("loading forest data")
  fl <<- list.files(pattern=glob2rx("Total*.tif$")) # F3 Data 
  fl_1 <<- raster(fl[1])
  
  # All the rasters have fl_l's projection  
  fl.ele <<- load_preproc_raster("ele_raster.tif") # elevation
  fl.sit <<- load_preproc_raster("Fl_sit.tif") # sit
  
  log("Making a raster brick")
  fl.stack <<- stack(fl)
  fl.stack <<- stack(fl.stack, fl.sit)
  fl.stack <<- shorten_fname(fl.stack)
}

# write elevation data for the counties in whole Sierra Nevada and extract the values of biomass from F3 data
write_ele_biomass_in_county = function(){
  log("read the forest elevation in Sierra Nevada")
  fl.ele.mask <<-raster("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/new_test/fl_ele_Sierra.tif")# this is the new forest elevation data in Sierra Nevada without exclude NPS and Wildeness area, the white areas indicate the no data(no biomass)
  df.ele <<- rasterToPoints(fl.ele.mask, spatial=T)#total number70857636
  log("read boundary of whole Sierra Nevada")
  shp <<- shapefile("/gpfs/data1/cmongp/ujjwal/cec/Forest Data/CA_Counties/CA_Sierra_TIGER2016.shp")
  shp.curr_county.prj <- spTransform(shp, proj4string(fl.ele))

  log("Getting county's elevation data for Whole Sierra")
  df.ele.curr_county <- over(df.ele, shp.curr_county.prj)#spatial join the points(biomass) to the polygons(the boudary of whole Sierra)
  df.ele.curr_county.na.omit <- na.omit(cbind(df.ele@data,df.ele@coords,df.ele.curr_county$NAME))#totalnumber70857636
  df.ele.curr_county.na.omit$xi=df.ele.curr_county.na.omit$x
  df.ele.curr_county.na.omit$yi=df.ele.curr_county.na.omit$y
  
  log("extract the values of biomass variables from F3 data")
  coordinates(df.ele.curr_county.na.omit)<- ~x+y
  proj4string(df.ele.curr_county.na.omit) <- proj4string(fl.ele)
  df.data.curr_county <- extract(fl.stack, df.ele.curr_county.na.omit, sp=T)#returns the values of a Raster* object for the cells in which a set of points fall.
  df.data.curr_county.latlon <- spTransform(df.data.curr_county, CRS("+proj=longlat +datum=WGS84"))#the latitude and longitude shown in Decimal Degrees
  F3_ele_biomass_df=as.data.frame(df.data.curr_county.latlon)
  
  log("Writing into csv file")
  directory <- paste("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs")
  write.csv(F3_ele_biomass_df, paste(directory,"/F3_ele_biomass.csv",sep = ""))#this table includes the elevation,county name,a bunch of variables realted to biomass.
}

#write forest elevation data for cluster in whole Sierra
write_ele_in_cluster = function(){
  df.ele.curr_county.na.omit <-spTransform(df.ele.curr_county.na.omit, CRS("+proj=longlat +datum=WGS84"))
  log("load the shapefile of the cluster in Sierra Nevada")
  #the cluster in Sierra Nevada is produced in ArcGIS
  Sierra_grid_cluster=shapefile("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/new_test/cluster_final_Sierra.shp")# cluster no start with 0
  Sierra_grid_cluster.prj1 <- spTransform(Sierra_grid_cluster, proj4string(df.ele.curr_county.na.omit))#this cluster layer also projects to the same coordinate system(WGS84),
 
  log("overlay the forest elevation with the cluster layer")
  fl_over_cluster1 <- over(df.ele.curr_county.na.omit,Sierra_grid_cluster.prj1)#spatial join the points(forest elevation) to the cluster polygons
  cluster_over_Sierra_csv1 <-na.omit(cbind(df.ele.curr_county.na.omit@data,df.ele.curr_county.na.omit@coords,fl_over_cluster1$cluster_no,fl_over_cluster1$OWN_GROUP,fl_over_cluster1$HAZ_CODE))
  cluster_over_Sierra_df=as.data.frame(cluster_over_Sierra_csv1)
  write.csv(cluster_over_Sierra_df,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/fl_cluster_Sierra.csv")#this table includes the elevation,county name,a bunch of biomass variables,cluster no,land use,fire hazard class(0-3)

}

#combine the all csv files produced above based on X and Y, and then combine another csv file which contains information of forest types based on X and Y
combine_ele_bio_reg_with_cluster= function(){
  # 1: combine the forest biomass table produced in function of write_ele_biomass_in_county with the cluster table produced in function of write_ele_in_cluster
  print("reading the forest biomass table for whole Sierra ")
  fl_sierra=read.csv("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/F3_ele_biomass.csv")#You can directly use the "F3_ele_biomass_df" argument as well
  fl_sierra[is.na(fl_sierra)]=0
  print(nrow(fl_sierra))
  print(colnames(fl_sierra)[1])
  print(colnames(fl_sierra)[2])
  colnames(fl_sierra)[2]="elevation"
  
  print(colnames(fl_sierra)[3]) 
  colnames(fl_sierra)[3]="county_name"
  
  print(colnames(fl_sierra)[66])
  colnames(fl_sierra)[66]="site_class"
  
  print(colnames(fl_sierra)[67])
  colnames(fl_sierra)[67]="lng"
  
  print(colnames(fl_sierra)[68])
  colnames(fl_sierra)[68]="lat"
  
  fl_sierra1=fl_sierra[,-1] #delete the X 
 
  print("reading cluster table for whole Sierra")
  fl_cluster=read.csv("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/fl_cluster_Sierra.csv")#you can directly use "cluster_over_Sierra_df"object as well
  
  print(colnames(fl_cluster)[1])
  fl_cluster1=fl_cluster[,-1]
  
  print(colnames(fl_cluster1))
  colnames(fl_cluster1)=c("ele","name","xi_1","yi_1","x1","y1","cluster_no","land_use","haz_class")
  
  #join two tables based on X y
  fl_sierra1$lng = round(fl_sierra1$lng,4)
  fl_sierra1$lat = round(fl_sierra1$lat,5)
  
  fl_cluster1$x1 = round(fl_cluster1$x1,4)
  fl_cluster1$y1 = round(fl_cluster1$y1,5)
  
  print("left join the cluster table  with biomass table based on the same X, Y value")
  data_join=sqldf('select * from fl_cluster1 LEFT JOIN fl_sierra1 ON fl_cluster1.x1=fl_sierra1.lng and fl_cluster1.y1=fl_sierra1.lat')
  #write.csv(data_join,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/Sierra_biomass_fhz_landuse_cluster.csv")
  
  #2 combine with another table based on the X and Y:Forest type table which includes the information of forest type for each pixel
  print("reading the forest type table for whole Sierra")
  forest_type=read.csv("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/forest_type_Sierra_wgs.csv")
  print(colnames(forest_type)[1])
  forest_type=forest_type[,-1]
  
  forest_type$xi = round(forest_type$xi,4)
  forest_type$yi = round(forest_type$yi,5)
  
  data_join$x1 = round(data_join$x1,4)
  data_join$y1 = round(data_join$y1,5)
  
  print("left join the previous joined table with the forest type table")
  data_join2=sqldf('select * from data_join LEFT JOIN forest_type ON data_join.x1=forest_type.xi and data_join.y1=forest_type.yi')

  print("remove the useleness name")
  cols.dont.want <- c("elevation","county_name","xi_1","yi_1","x1","y1","xi","yi") # if you want to remove multiple columns
  data_clean <- data_join2[, ! names(data_join2) %in% cols.dont.want, drop = F]
  
  print("change the column name")
  print(colnames(data_clean)[1])
  colnames(data_clean)[1]='elevation'
  print(colnames(data_clean)[2])
  colnames(data_clean)[2]="county_name"
  print(colnames(data_clean)[69])
  colnames(data_clean)[69]="reg_d"

  print("add the year to the pixel")
  data_clean$year=rep("2016",nrow(data_clean))

  print("sorted the csv based on cluster no")
  final_csv=arrange(data_clean, desc(cluster_no))
  
  #produce the final csv table which includes all the variables we need in model: elevation, county_name, a bunch of biomass variables, cluster no,land_use,haz_clss,reg_d,lat, lng, and year
  print("write the final csv")
  write.csv(final_csv,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/Sierra_Nevada_sorted.csv")
 
}

#run each function within main() in order
main = function() {
  setup()
  load_forest_data()
  write_ele_biomass_in_county()
  write_ele_in_cluster()# take times
  combine_ele_bio_reg_with_cluster() #time consuming run this function seperately
  endCluster()
}

main()#call the main function