library(terra)

args = commandArgs(trailingOnly = TRUE)

start_batch = as.numeric(args)

# Check if the required argument is provided
if (length(args) == 0) {
  stop("No starting batch number provided. Please pass a starting batch number as an argument.")
}

# Convert the argument to a numeric value (assuming it is a number)
start_batch <- as.numeric(args[1])

log_memory_usage <- function() {
  mem_info <- gc()
  print(paste("NCells usage (MB):", mem_info[1,2]))
  print(paste("VCells usage (MB):", mem_info[2,2]))
}

# Setting timezone for consistency
Sys.setenv(TZ='UTC')

# Helper function to print current time and message
print_timestamped <- function(message) {
  print(paste(Sys.time(), message))
}

print_timestamped("Starting the script execution.")

#load in combinedIDs and geolocatedDEMIDs
load("combinedIDs.RData")
load("geolocatedDEMIDs.RData")

#read in residue by treatment and add a row for empty cells
print_timestamped("Reading and updating biomass data...")
biomassData = read.csv("residue_by_treatment.csv")
print("Rows of Biomass Data:")
print(nrow(biomassData))

#add a value for raster cells with no FCID
print_timestamped("Adding a value for raster cells with no FCID...")
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
IDs_per_group = 400000
num_groups = ceiling(num_IDs / IDs_per_group)

#merging and aggregating by group
print_timestamped("Merging and aggregating by group...")
for (i in start_batch:num_groups) {
  print_timestamped("Start of loop...")
  start_ID = (i-1)*IDs_per_group+1
  if ((i*IDs_per_group) < num_IDs) {
    stop_ID = i*IDs_per_group
  } else {
    stop_ID = num_IDs
  } 
  current_IDs = clusterIDs[start_ID:stop_ID]
  group_i = combinedIDs[combinedIDs$DEM360 %in% current_IDs,]
  print("Working on the following IDs:")
  print(sprintf("%d through %d",start_ID,stop_ID))
  
  #define file name
  file_name=paste("IDs_",start_ID,"to_",stop_ID,".csv")
  #file_name=paste0("IDs_",start_ID,"_to_",stop_ID,".csv")
  
  #check if that file exists
  file_exists = file.exists(file_name)
  if (file_exists) {
    print_timestamped("Batch has already been processed. End of loop. Checking next batch...")
  } else {
    #add the density of biomass indicated
    print_timestamped("Batch has not yet been processed. Beginning the merge step...")
    preAgg_group = merge(group_i, biomassData, by.x="FCID2018", all.x=TRUE)
    print_timestamped("Merge step completed...")
    
    #aggregate
    print_timestamped("Beginning the aggregate step...")
    columnsToAverage = c(6:10)
    postAgg_group = aggregate(cbind(preAgg_group[,columnsToAverage]),by=list(preAgg_group$DEM360,preAgg_group$x,preAgg_group$y,preAgg_group$treat.name),FUN="mean")
    print_timestamped("Aggregate step completed...")
    
    #write group to .csv
    print_timestamped("Writing group data to csv under the name:")
    print(file_name)
    write.csv(postAgg_group,file=file_name)
  
    number_rows=13*stop_ID
    percent_done=number_rows/(num_IDs*13)*100
    print(sprintf("Group %d finished. Including the most recent group, there are %d rows (computed or saved to .csv already)... estimated to be %.2fpercent done.",i,number_rows,percent_done))
    log_memory_usage()
    print_timestamped("End of loop...")
  }  
}

print_timestamped("Code completed. Csv files must be stacked, reformatted, and written to geolocated_residue_by_treatment.csv")
