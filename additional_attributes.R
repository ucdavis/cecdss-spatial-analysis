library(terra)

# Helper function to print current time and message
print_timestamped <- function(message) {
  print(paste(Sys.time(), message))
}

#read in elevation data
#elevTIF = "dem90_hf.tif"
#elevData = rast(elevTIF)

#calculate slope
#slope = terrain(elevCA, v="slope",neighbors=8,unit="degrees")
#plot(slope)

#read in counties
countySHP = "counties/cnty19_1.shp"
countyData = vect(countySHP)
#plot(countyData)

#read in fire hazard severity zones
FHSZ = "FHSZ/FHSZ_SRA_LRA_Combined.shp"
fireData = vect(FHSZ)

#read in land ownership
land_file = "Land_Ownership/ownership24_1.shp"
land_ownership = vect(land_file)
#plot(land_ownership)

#read in forest type
#forest_file = "Forest Type/TreeMap2016_FLDTYPCD.tif"
#forestData = rast(forest_file)
#plot(forestData)

#crop to CA
#extentCA_wsg = ext(c(-125,-114,32,42))
#extentCA_aea = project(extentCA_wsg,"epsg:4326",elevData)
#elevCA = crop(elevData,extentCA_aea)
#plot(elevCA)

#read in csv
GLRBT = read.csv("GLRBT.csv")

GLRBT_len = nrow(GLRBT)
group_size = 1000000
num_groups = ceiling(GLRBT_len/group_size)

completed_GLRBT = NULL

for (i in 1:num_groups) {

  print_timestamped(i)
  g_start = (i-1)*group_size+1
  if ((i*group_size) < GLRBT_len) {
    g_stop = i*group_size
  } else {
    g_stop = GLRBT_len
  } 
  GLRBT_group = GLRBT[g_start:g_stop,]
  
  #print_timestamped("Preparing for and extracting elevation...")
  #project coords to elevation data
  crds = GLRBT_group[,c("Lon","Lat")]
  coords_vect = vect(crds,geom=c("Lon","Lat"))
  crs(coords_vect) <- "+proj=longlat +datum=WGS84"
  #coords_elev = project(coords_vect,elevData)
  
  #extract elev
  #elevExtracted = extract(elevData,coords_elev)
  
  #add elev to table
  #GLRBT_group$Elevation_m = elevExtracted$dem90_hf
  
  print_timestamped("Preparing for and extracting county...")
  #project coords to county data
  coords_county = project(coords_vect,countyData)
  
  #extract counties
  countyExtracted = extract(countyData,coords_county)
  
  #add county to table
  GLRBT_group$County = countyExtracted$COUNTY_NAM

  print_timestamped("Preparing for and extracting FSHZ")  
  #project coords to fire hazard data
  coords_FSHZ = project(coords_vect,fireData)
  
  #extract FSHZ
  hazardExtracted = extract(fireData,coords_FSHZ)
  
  #add FSHZ to table
  GLRBT_group$Hazard_Class = hazardExtracted$FHSZ
  
  #extract slope
  #slopeExtracted = extract(slope,coords_elev)
  
  #add slope to table
  #GLRBT$Slope_degrees = slopeExtracted$slope
  
  print_timestamped("Preparing for and extracting land ownership...")
  #project coords to land ownership
  coords_LO = project(coords_vect,land_ownership)
  
  #extract land ownership
  ownershipExtracted = extract(land_ownership,coords_LO)
  
  #add ownership to table
  GLRBT_group$Land_Ownership = ownershipExtracted$OWN_AGENCY
  
  #print_timestamped("Preparing for and extracting forest data...")
  #project coords to forest data
  #coords_forest = project(coords_vect,forestData)
  
  #extract forest type
  #forestExtracted = extract(forestData,coords_forest)
  
  #add forest type to table
  #GLRBT_group$Forest_type = forestExtracted$Label
  
  #define file name and checkpoint data
  file_name=paste0("GLRBT_checkpoint_",i)
  write.csv(GLRBT_group,file_name)
  
  if (is.null(completed_GLRBT)) {
  completed_GLRBT = GLRBT_group
  } else {
   completed_GLRBT = rbind(completed_GLRBT,GLRBT_group)
  }
  
  completed_length = nrow(completed_GLRBT)
  percent_done=i/num_groups*100
  print(sprintf("Group %d finished. Including the most recent group, there are %d rows... estimated to be %.2fpercent done.",i,completed_length,percent_done))
  print_timestamped("End of loop, next loop")
}

write.csv(completed_GLRBT,"finished_GLRBT.csv")
