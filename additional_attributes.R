library(terra)
library(data.table)

# Helper function to print current time and message
print_timestamped <- function(message) {
  print(paste(Sys.time(), message))
}

#read in elevation data
elevTIF = "Input Data/dem90_hf.tif"
elevData = rast(elevTIF)

#calculate slope
#slope = terrain(elevCA, v="slope",neighbors=8,unit="degrees")
#plot(slope)

#read in counties
countySHP = "Input Data/counties/cnty19_1.shp"
countyData = vect(countySHP)
countyData = project(countyData,elevData)
countyData = rasterize(countyData,elevData,"COUNTY_NAM")
#plot(countyData)

#read in fire hazard severity zones
FHSZ = "Input Data/FHSZ/FHSZ_SRA_LRA_Combined.shp"
fireData = vect(FHSZ)
fireData = project(fireData,elevData)
fireData = rasterize(fireData,elevData,"FHSZ")
#plot(fireData)

#read in land ownership
land_file = "Input Data/Land_Ownership/ownership24_1.shp"
ownData = vect(land_file)
ownData = project(ownData,elevData)
ownData = rasterize(ownData,elevData,"OWN_AGENCY")
#plot(land_ownership)

#read in forest type
forest_file = "Input Data/Forest Type/TreeMap2016_FLDTYPCD.tif"
forestData = rast(forest_file)
#plot(forestData)

#crop to CA
#extentCA_wsg = ext(c(-125,-114,32,42))
#extentCA_aea = project(extentCA_wsg,"epsg:4326",elevData)
#elevCA = crop(elevData,extentCA_aea)
#plot(elevCA)

#read in csv
GLRBT = fread("geolocated_residue_by_treatment.csv")
GLRBT = GLRBT[, -1]
GLRBT_len = nrow(GLRBT)
group_size = 1300000
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
  
  print_timestamped("Preparing for and extracting elevation...")
  #project coords to elevation data
  crds = GLRBT_group[,c("Lon","Lat")]
  unique_crds = unique(crds)
  coords_vect = vect(unique_crds,geom=c("Lon","Lat"))
  crs(coords_vect) <- "+proj=longlat +datum=WGS84"
  coords_elev = project(coords_vect,elevData)
  
  #extract elev
  elevExtracted = extract(elevData,coords_elev,method="simple")
  
  #add elev to table
  elev_add = rep(elevExtracted$dem90_hf, each = 13)
  GLRBT_group$Elevation_m = elev_add
  
  print_timestamped("Preparing for and extracting county...")
  #project coords to county data
  #coords_county = project(coords_vect,countyData)
  coords_county = coords_elev
  #no need for projection
  
  #extract counties
  countyExtracted = extract(countyData,coords_county)
  
  #add county to table
  county_add = rep(countyExtracted$COUNTY_NAM, each=13)
  GLRBT_group$County = county_add

  print_timestamped("Preparing for and extracting FHSZ")  
  #project coords to fire hazard data
  #coords_FHSZ = project(coords_vect,fireData)
  coords_FHSZ = coords_elev
  #no need for projection
  
  #extract FHSZ
  hazardExtracted = extract(fireData,coords_FHSZ)
  
  #add FSHZ to table
  FSHZ_add = rep(hazardExtracted$FHSZ, each=13)
  GLRBT_group$Hazard_Class = FSHZ_add
  
  #extract slope
  #slopeExtracted = extract(slope,coords_elev)
  
  #add slope to table
  #GLRBT$Slope_degrees = slopeExtracted$slope
  
  print_timestamped("Preparing for and extracting land ownership...")
  #project coords to land ownership
  #coords_LO = project(coords_vect,land_ownership)
  coords_LO = coords_elev
  #no need for projection
  
  #extract land ownership
  ownershipExtracted = extract(ownData,coords_LO)
  
  #add ownership to table
  LO_add = rep(ownershipExtracted$OWN_AGENCY, each=13)
  GLRBT_group$Land_Ownership = LO_add
  
  print_timestamped("Preparing for and extracting forest data...")
  #project coords to forest data
  coords_forest = project(coords_vect,forestData)
  
  #extract forest type
  forestExtracted = extract(forestData,coords_forest,method="simple")
  
  #add forest type to table
  forest_add = rep(forestExtracted$Label, each=13)
  GLRBT_group$Forest_type = forest_add
  
  #print_timestamped("checkpointing data...")
  #define file name and checkpoint data
  #file_name=paste0("GLRBT_checkpoint_",i)
  #write.csv(GLRBT_group,file_name)
  
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
