library(sqldf)
library(sf)
library(stars)
library(dplyr)
library(microbenchmark)
library(raster)
# to generate the csv file associated with forest types in whole Sierra Nevada

#1. North Sierra
# mask the F3 forest data in north Sierra
fl.ele.mask <<-raster("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/new_test/fl_ele_Sierra.tif")#this data includes the lat, lng, elevatio
North_new_data=raster("/gpfs/data1/cmongp/ujjwal/cec/NewClusters/NSierra_new_test.tif")#same extent,res,crs as the fl.ele.mask
fl_mask_North=mask(fl.ele.mask,North_new_data)# the boundary of fl.ele.mask will be the same as North_new_data 
#fl_north=writeRaster(fl_mask_North,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/Forest Type/fl_mask_NSierra.tif")

#stack with North_new_data
fl_stack_north=stack(fl_mask_North,North_new_data)
North_df=data.frame(rasterToPoints(fl_stack_north))
#View(North_df)
combine=subset(North_df, (!is.na(North_df[,3])))#North_df[,3] is the the elevation field
#nrow(combine) #21,357,072 
colnames(combine)=c("xi", "yi", "ele", "frst")
#join the table with classfied types in Sierra
north_region=read.csv("/gpfs/data1/cmongp/lansong/test/test_files/NSierra_reg.csv")#this table includes numeric value and corresponding classfied forest types, such as pine, mixed conifer, and other. this table has been created previously by Ujjeal 
#View(north_region)
data=sqldf('select * from combine LEFT JOIN north_region ON combine.frst=north_region.Value')# we can join the combine table and north_region based on the same attribute field
nrow(data)


#2. South Sierra  
south_new_data=raster("/gpfs/data1/cmongp/ujjwal/cec/NewClusters/SouthSierra_new_test.tif")
fl_mask_SSierra=mask(fl.ele.mask,south_new_data)
#fl_south=writeRaster(fl_mask_SSierra,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/Forest Type/fl_mask_SSierra.tif")

#stack with North_new_data
fl_stack_south=stack(fl_mask_SSierra,south_new_data)
south_df=data.frame(rasterToPoints(fl_stack_south))
#View(south_df)
combine_S=subset(south_df, (!is.na(south_df[,3])))
#nrow(combine_S) #31,585,622 
colnames(combine_S)=c("xi", "yi", "ele", "frst")
#join the table with classfied type in Sierra
south_reg = read.csv("/gpfs/data1/cmongp/lansong/test/test_files/SouthSierra_reg.csv")
#View(south_reg)
data_S=sqldf('select * from combine_S LEFT JOIN south_reg ON combine_S.frst=south_reg.Value')


#3. North Interior
northInterior_new_data=raster("/gpfs/data1/cmongp/ujjwal/cec/NewClusters/NorthInterior_new_test1.tif")
fl_mask_NorthInterior=mask(fl.ele.mask,northInterior_new_data)
#fl_NorthInteroir=writeRaster(fl_mask_NorthInterior,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/Forest Type/fl_mask_NSInterior.tif")

#stack with North_new_data
fl_stack_NI=stack(fl_mask_NorthInterior,northInterior_new_data)
NI_df=data.frame(rasterToPoints(fl_stack_NI))
#View(NI_df)
combine_NI=subset(NI_df, (!is.na(NI_df[,3])))
#nrow(combine_NI) #8,240,119  
colnames(combine_NI)=c("xi", "yi", "ele", "frst")
#join the table with classfied type in Sierra
NI_reg = read.csv("/gpfs/data1/cmongp/lansong/test/test_files/NorthInterior_reg.csv")
#View(NI_reg)
data_NI=sqldf('select * from combine_NI LEFT JOIN NI_reg ON combine_NI.frst=NI_reg.Value')


#4. combine all three dataframes produced in step 1,2,and 3 using rbind
new_combine=rbind(data,data_S,data_NI)
nrow(new_combine)

#5. remove the duplicated rows based on X, Y
coordinates(new_combine)<- ~xi+yi
proj4string(new_combine) <- proj4string(fl.ele.mask)
#new_combine@coords
data_rm_duplicated <- new_combine[!duplicated(new_combine@coords),]#remove the duplicated rows if it has the same x and y values
final_data=as.data.frame(data_rm_duplicated)
final_data1=final_data[,c(-3,-4,-5,-6)]
#nrow(final_data)#61131799

#6. process the rest of the pixels
fl_mask_SNI=raster("/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/Forest Type/fl_mask_all3_Sierra.tif")

#use the arcmap math tool-"equal to" the first input :S:\cmongp\lansong\test\test_files\fl_mask_all3_Sierra_na.tif, the second input :/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/Forest Type/fl_mask_all3_Sierra.tif,and final outpu:/gpfs/data1/cmongp/lansong/test/test_files/TEST1.tif
test_raster=raster("/gpfs/data1/cmongp/lansong/test/test_files/TEST1.tif")

result=mask(test_raster,fl.ele.mask)#these are all the rest of pixels that are being to assign "other" as forest type
#writeRaster(result,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/Forest Type/fl_rest.tif")
final=as.data.frame(rasterToPoints(result))
final.na=final[(final$TEST1!=0),]#final$TEST1 has two values including 0 and 1, we need to keep rows that the values are equal to one and delete all the rows which the values are equal to 0
final.na$REGIONAL_D=rep("other",nrow(final.na))#Assign to the "other" to rest of pixels which are not covered by the forest type layer
final.na=final.na[,-3]
colnames(final.na)=c("xi","yi","REGIONAL_D")
#View(final.na)
#nrow(final.na)#9725837

#7. rbind the two tables
final_csv=rbind(final_data1,final.na)
final_csv=na.omit(final_csv)
#nrow(final_csv)
#write.csv(final_csv,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/forest_type_Sierra_Albers.csv")
coordinates(final_csv)<- ~xi+yi
proj4string(final_csv) <- proj4string(fl.ele.mask)
final_csv.latlon <- spTransform(final_csv, CRS("+proj=longlat +datum=WGS84"))#change the projection to the WGS84
final_table=as.data.frame(final_csv.latlon)
#nrow(final_table)
write.csv(final_table,"/gpfs/data1/cmongp/lansong/cec_lan/Sierra_Navada_Clusters/final_csvs/forest_type_Sierra_wgs.csv")

