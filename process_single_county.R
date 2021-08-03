# given an existing `F3_ele_biomass.csv` file, write a single county for testing

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

setup()# This is the function in the Config.R

# load elevation raster data
fl.ele <<- load_preproc_raster(file.path(data_dir, "ele_raster.tif")) # "Read our original elevation data, load_preproc_raster" is the function from cec_utils.R

#running the rest of code to join the pixel data with cluster data and forest type data

print("reading the table produced in the function of write_ele_biomass_in_county")
fl_sierra=read.csv(file.path(results_dir, "F3_ele_biomass.csv"))#here I read the csv file we create in the fuction of write_ele_biomass_in_county. if you didn't write csv in the second function, you can directly use the variable name F3_ele_biomass_df
fl_sierra[is.na(fl_sierra)]=0 
#change the column name
colnames(fl_sierra)[2]="elevation"
colnames(fl_sierra)[3]="county_name"
colnames(fl_sierra)[66]="site_class"
colnames(fl_sierra)[67]="lng"
colnames(fl_sierra)[68]="lat"
#read the forest type csv this is another information realted to forst type needed to add to the dataframe
print("reading the forest type table in Whole Sierra")
forest_type=read.csv(file.path(data_dir, "forest_type_Sierra_wgs.csv"))
#read the shapefile of the cluster in whole Sierra
print("reading the cluster in whole Sierra ") #read the cluster results for whole sierra Neveda that we created in ArcMap
Sierra_grid_cluster=shapefile(file.path(data_dir, "cluster_final_Sierra.shp"))

#list a total of 21 counties in Sierra Nevada (test with just one)
counties=c("Alpine")
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

  # write csv for this county
  write.csv(final_csv,file.path(results_dir, paste(county_name, "_sorted.csv", sep="")))

  print("rbind the datasets")
  df_total=rbind(df_total,data_clean1)
  
}
results_final=do.call(rbind,results)#combine the pixel associated with each county
print("write the csv")`
final_csv=arrange(results_final, desc(as.numeric(as.character(cluster_no))))#I changed this code
write.csv(final_csv,file.path(results_dir, "Sorted_Sierra_Nevada.csv"))

print("done processing, exiting")
