source("index.R")

library(terra)

rastExtract = function (clusterFile,rastFile) {
clusterData = vect(clusterFile)
print("Number of clusters read: ")
print(nrow(clusterData))
print("Preview of cluster data (head):")
print(head(clusterData))

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

return (combinedIDs)
}