library(terra)

# Setting timezone for consistency
Sys.setenv(TZ='UTC')

# Helper function to print current time and message
print_timestamped <- function(message) {
  print(paste(Sys.time(), message))
}

print_timestamped("Starting the script execution.")

#read in cluster file
clusterFile = "Input Data/Clusters/CaliforniaClusters.shp"
print_timestamped("Reading shapefile...")
clusterData = vect(clusterFile)
print("Number of clusters read: ")
print(nrow(clusterData))
print("Preview of cluster data (head):")
print(head(clusterData))

#read in the raster
rastFile = "Input Data/FCID2018_masked_V6.tif"
print_timestamped("Reading raster file...")
rastFCIDs = rast(rastFile)

#project clusters from WSG84 to AEA
print_timestamped("Projecting cluster data...")
projClusters = project(clusterData, rastFCIDs)

#calculate the coordinates and values of each cluster centroid using the AEA projection
print_timestamped("Calculating centroids...")
centroids = centroids(projClusters)
coords = crds(centroids)
DEMIDs = values(centroids)

#reproject the clusters to WSG84 to work on lat/lon in terms of the WSG84 coordinates
print_timestamped("Re-projecting centroids to WSG84...")
reprojCentroids = project(centroids, "epsg:4326")
reprojCoords = crds(reprojCentroids)

#bind the values and add an additional ID number that is used to merge with the FCIDs later (needed because the extract adds an ID)
print_timestamped("Combining data...")
geolocatedDEMIDs = cbind(DEMIDs,reprojCoords)
geolocatedDEMIDs$ID = c(1:length(geolocatedDEMIDs$DEM360))
print("Head of geolocated DEM IDs:")
print(head(geolocatedDEMIDs))
print("Rows of geolocated DEM IDs:")
print(nrow(geolocatedDEMIDs))

#extract the FCID values contained in each cluster
print_timestamped("Extracting FCID values...")
FCID = extract(rastFCIDs, projClusters)
colnames(FCID)[2] <- "FCID2018"
print("Head of extracted FCIDs:")
print(head(FCID))
print("Rows of extracted FCIDs:")
print(nrow(FCID))

#merge the FCID and clusterID using the ID number assigned in the extract
print_timestamped("Merging FCID and cluster IDs...")
combinedIDs = merge(FCID, geolocatedDEMIDs, all.y=TRUE, by.x="ID")
combinedIDs$FCID2018[is.na(combinedIDs$FCID2018)] <- "-2147483648"
print("Head of Combined IDs:")
print(head(combinedIDs))
print("Rows of Combined IDs:")
print(nrow(combinedIDs))

#read in residue by treatment and add a row for empty cells
print_timestamped("Reading and updating biomass data...")
biomassData = read.csv("residue_by_treatment.csv")
print("Rows of Biomass Data:")
print(nrow(biomassData))

#add a value for raster cells with no FCID
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","20_Proportional_Thin")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","20_Thin_From_Above")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","20_Thin_From_Below")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","40_Proportional_Thin")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","40_Thin_From_Above")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","40_Thin_From_Below")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","60_Proportional_Thin")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","60_Thin_From_Above")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","60_Thin_From_Below")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","80_Proportional_Thin")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","80_Thin_From_Above")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","80_Thin_From_Below")
biomassData[nrow(biomassData) + 1,] = c("-2147483648","0","0","0","0","0","Clearcut")

#convert numbers to numeric
print_timestamped("Converting biomass data to numeric...")
biomassData[,2]=as.numeric(biomassData[,2])
biomassData[,3]=as.numeric(biomassData[,3])
biomassData[,4]=as.numeric(biomassData[,4])
biomassData[,5]=as.numeric(biomassData[,5])
biomassData[,6]=as.numeric(biomassData[,6])

#prepare to group the data and send it through in clusters
clusterIDs = geolocatedDEMIDs$DEM360
num_IDs = length(clusterIDs)
IDs_per_group = 500000
num_groups = ceiling(num_IDs / IDs_per_group)

merge_aggregate_variable = NULL

#merging and aggregating by group
print_timestamped("Merging and aggregating by group...")
for (i in 1:num_groups) {
  print_timestamped("Merging and aggregating IDs number...")
  start_ID = (i-1)*IDs_per_group+1
  if ((i*IDs_per_group) < num_IDs) {
    stop_ID = i*IDs_per_group
  } else {
    stop_ID = num_IDs
  } 
  current_IDs = clusterIDs[start_ID:stop_ID]
  group_i = combinedIDs[combinedIDs$DEM360 %in% current_IDs,]
  print(sprintf("%f through %f",start_ID,stop_ID))
  
  #add the density of biomass indicated
  preAgg_group = merge(group_i, biomassData, by.x="FCID2018", all.x=TRUE)
  
  #aggregate
  columnsToAverage = c(6:10)
  postAgg_group = aggregate(cbind(preAgg_group[,columnsToAverage]),by=list(preAgg_group$DEM360,preAgg_group$x,preAgg_group$y,preAgg_group$treat.name),FUN="mean")
  
  #add aggregated chunk
  if (is.null(merge_aggregate_variable)) {
    merge_aggregate_variable = postAgg_group
  } else {
    merge_aggregate_variable = rbind(merge_aggregate_variable,postAgg_group)
  }
  number_rows=nrow(merge_aggregate_variable)
  percent_done=number_rows/(num_IDs*13)*100
  print(sprintf("Including the most recent group, there are %f rows... estimated to be %fpercent done.",number_rows,percent_done))
}

#reformat data frame
print_timestamped("Reformatting and ordering...")
names(merge_aggregate_variable)[1:4] = c("DEM360","Lon","Lat","Treatment Name")
geolocated_residue_by_treatment=merge_aggregate_variable[order(merge_aggregate_variable$DEM360),]

#export to csv
write.csv(geolocated_residue_by_treatment,"geolocated_residue_by_treatment.csv")
