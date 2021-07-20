#overlay the forest data in each county with the cluster in Whole Sierra
#these are all the library that we need to use in this R script.
library(doMC)
library(sp)
library(raster)
library(rgdal)
library(parallel)
library(maptools)
library(foreign)
library(sqldf)
library(doMC)
library(dplyr)
registerDoMC(21)

#source here indicates that we can reuse some functions that were written in other script
source("/gpfs/data1/cmongp/lansong/cec_lan/cec_utils.R")# some functions will be used in this script
source("/gpfs/data1/cmongp/lansong/cec_lan/config.R")#for set up the working directory
source("/gpfs/data1/cmongp/lansong/cec_lan/forest_type.R")# useless
source("/gpfs/data1/cmongp/lansong/cec_lan/data.R")# useless
source("/gpfs/data1/cmongp/lansong/cec_lan/cluster_results_new.R")#useless

# loads F3 forest, elevation, sit raster,stack all F3 forest data with sit data,and chwrite.csv(final_csv,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/county_csv/Sorted_Sierra_Nevada.csv")ange the variable name to simple name
load_forest_data <- function(){
  log("loading forest data")
  fl <<- list.files(pattern=glob2rx("Total*.tif$")) # list all the F3 Data  There are total of 60 files 
  fl_1 <<- raster(fl[1])#read first raster  # All the rasters have fl_l's projection 
  
  fl.ele <<- load_preproc_raster("ele_raster.tif") # "Read our original elevation data, load_preproc_raster" is the function from cec_utils.R
  fl.sit <<- load_preproc_raster("Fl_sit.tif") # read our sit data
  
  log("Making a raster brick")
  fl.stack <<- stack(fl)#stack all the Forest data
  fl.stack <<- stack(fl.stack, fl.sit)#stack with the site raster
  fl.stack <<- shorten_fname(fl.stack)#format our variable name with shorter name
}

# write elevation data for the counties in whole Sierra Nevada and extract the values of biomass from F3 data, write elevation, county name, and biomass values for all the pixels in Sierra Nevada
write_ele_biomass_in_county = function(){
  log("read the forest elevation in whole Sierra Nevada")
  fl.ele.mask <<-raster("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/new_test/fl_ele_Sierra.tif")# this is the new forest elevation data in Sierra Nevada without excluding NPS and Wildeness area. the white areas indicate the no data(no biomass)
  df.ele <<- rasterToPoints(fl.ele.mask, spatial=T)#total number70857636
  
  log("read the shapefile of county boundary of Sierra Nevada")
  shp <<- shapefile("/gpfs/data1/cmongp/ujjwal/cec/Forest Data/CA_Counties/CA_Sierra_TIGER2016.shp")#this is county boundary shapefile
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
  directory <- paste("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs")
  write.csv(F3_ele_biomass_df, paste(directory,"/F3_ele_biomass.csv",sep = ""))#this table includes the elevation,county name,a bunch of variables realted to biomass.
}

#run line by line
main = function() {
  setup()# This is the function in the Config.R
  load_forest_data()
  write_ele_biomass_in_county()
  endCluster()
}

main()

#running the rest of code to join the pixel data with cluster data and forest type data

print("reading the table produced in the function of write_ele_biomass_in_county")
fl_sierra=read.csv("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/F3_ele_biomass.csv")#here I read the csv file we create in the fuction of write_ele_biomass_in_county. if you didn't write csv in the second function, you can directly use the variable name F3_ele_biomass_df
fl_sierra[is.na(fl_sierra)]=0 
#change the column name
colnames(fl_sierra)[2]="elevation"
colnames(fl_sierra)[3]="county_name"
colnames(fl_sierra)[66]="site_class"
colnames(fl_sierra)[67]="lng"
colnames(fl_sierra)[68]="lat"
#read the forest type csv this is another information realted to forst type needed to add to the dataframe
print("reading the forest type table in Whole Sierra")
forest_type=read.csv("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/forest_type_Sierra_wgs.csv")
#read the shapefile of the cluster in whole Sierra
print("reading the cluster in whole Sierra ") #read the cluster results for whole sierra Neveda that we created in ArcMap
Sierra_grid_cluster=shapefile("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/new_test/cluster_final_Sierra.shp")

#list a total of 21 counties in Sierra Nevada
counties=c("Alpine","Yuba","Inyo","Madera","Mariposa","Tehama","Calaveras","Nevada","Shasta","Plumas","Lassen","Sierra","Placer","Butte","Amador","El Dorado","Tulare","Tuolumne","Mono","Fresno","Kern")
df_total=data.frame()
results=foreach(county_name=counties)%dopar%{#parally processing the pixel data in each county using multiple cores.
  print(paste0("start running ",county_name))
  #select pixel data in each county
  select_county1= sqldf(paste0("select * from fl_sierra where county_name =","'",county_name,"'"))
  coordinates(select_county1)<- ~xi+yi #xi &yi are the degree
  proj4string(select_county1) <- proj4string(fl.ele)
  select_county_prj <-spTransform(select_county1, CRS("+proj=longlat +datum=WGS84"))

  # overlay the forest biomass in each county with the cluster in whole Sierra region
  print(paste0("start overlaying the forest biomass in ",county_name," with cluster for whole Sierra"))
  Sierra_grid_cluster.prj1 <- spTransform(Sierra_grid_cluster, proj4string(select_county_prj))
  fl_over_cluster1 <- over(select_county_prj,Sierra_grid_cluster.prj1)#points overlay the cluster polygons
  cluster_over_Sierra_csv1 <-na.omit(cbind(select_county_prj@data,select_county_prj@coords,fl_over_cluster1$cluster_no,fl_over_cluster1$OWN_GROUP,fl_over_cluster1$HAZ_CODE))#some of pixels will not have corresponding cluster no since we deleted the clusters which are not able to be merged to the 
  cluster_over_Sierra_df2=as.data.frame(cluster_over_Sierra_csv1)
  cluster_over_Sierra_df2=cluster_over_Sierra_df2[,-1]
  #change the column name
  colnames(cluster_over_Sierra_df2)[68]="cluster_no"
  colnames(cluster_over_Sierra_df2)[69]="land_use"
  colnames(cluster_over_Sierra_df2)[70]="haz_class"

  #join the forest type table based on X and Y
  print("start joing the forest type table")
  forest_type=forest_type[,-1]
  forest_type$xi = round(forest_type$xi,4)
  forest_type$yi = round(forest_type$yi,5)
  cluster_over_Sierra_df2$lng = round(cluster_over_Sierra_df2$lng,4)
  cluster_over_Sierra_df2$lat = round(cluster_over_Sierra_df2$lat,5)
  data_join1=sqldf('select * from cluster_over_Sierra_df2 LEFT JOIN forest_type ON cluster_over_Sierra_df2.lng=forest_type.xi and cluster_over_Sierra_df2.lat=forest_type.yi')
  
  #remove the useless name
  print("delete useleness name")
  cols.dont.want <- c("xi","yi") # if you want to remove multiple columns
  data_clean1 <- data_join1[, ! names(data_join1) %in% cols.dont.want, drop = F]
  
  #change the column name
  print("change the column name")
  colnames(data_clean1)[69]='reg_d'
  
  #add the year for all pixels
  print("add the year for all pixels")
  data_clean1$year=rep("2016",nrow(data_clean1))
  print("rbind the datasets")
  df_total=rbind(df_total,data_clean1)
  
}
results_final=do.call(rbind,results)#combine the pixel associated with each county
print("write the csv")
final_csv=arrange(results_final, desc(cluster_no))#sort the data based on cluster no
write.csv(final_csv,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/county_csv/Sorted_Sierra_Nevada.csv")
