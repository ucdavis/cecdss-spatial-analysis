source("index.R")

library(terra)

aggClusters = function (biomassData,combinedIDs) {
#prepare to group the data and send it through in clusters
clusterIDs = combinedIDs$DEM360 #I think it is okay for this to be combinedIDs instead of geolocatedIDs. if not, change back
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
  log_memory_usage()
}

#reformat data frame
print_timestamped("Reformatting and ordering...")
names(merge_aggregate_variable)[1:4] = c("DEM360","Lon","Lat","Treatment Name")
geolocated_residue_by_treatment=merge_aggregate_variable[order(merge_aggregate_variable$DEM360),]

return (GLRBT)
}