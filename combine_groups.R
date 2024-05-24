library(terra)

# Helper function to print current time and message
print_timestamped <- function(message) {
  print(paste(Sys.time(), message))
}

load("combinedIDs.RData")
load("geolocatedDEMIDs.RData")

clusterIDs = geolocatedDEMIDs$DEM360
num_IDs = length(clusterIDs)
IDs_per_group = 400000
num_groups = ceiling(num_IDs / IDs_per_group)

combination_variable = NULL

#merging and aggregating by group
print_timestamped("Merging and aggregating by group...")
for (i in 1:num_groups) {
  print_timestamped(i)
  start_ID = (i-1)*IDs_per_group+1
  if ((i*IDs_per_group) < num_IDs) {
    stop_ID = i*IDs_per_group
  } else {
    stop_ID = num_IDs
  } 
  
  file_name=paste("IDs_",start_ID,"to_",stop_ID,".csv")
  postAgg_data=read.csv(file_name)
  print(nrow(postAgg_data))
  
  nonZero_data=postAgg_data[!(postAgg_data$Stem6to9_tonsAcre==0&postAgg_data$Stem4to6_tonsAcre==0&postAgg_data$Stem9Plus_tonsAcre==0&postAgg_data$Branch_tonsAcre==0&postAgg_data$Foliage_tonsAcre==0), ]
  print(nrow(nonZero_data))
  
  #add chunks
  if (is.null(combination_variable)) {
    combination_variable = nonZero_data
  } else {
   combination_variable = rbind(combination_variable,nonZero_data)
  }
}

combination_variable = combination_variable[,-1]
names(combination_variable)[1:4] = c("DEM360","Lon","Lat","Treatment Name")
geolocated_residue_by_treatment=combination_variable[order(combination_variable$DEM360),]

#write.csv(combination_variable,"combination_variable.csv")

#export to csv
write.csv(geolocated_residue_by_treatment,"geolocated_residue_by_treatment.csv")
