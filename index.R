source("rasterExtraction.R")
source("createBiomassData.R")
source("aggregateClusters.R")
source("add_attributes.R")
source("filter.R")

library(terra)
library(data.table)

# Setting timezone for consistency
Sys.setenv(TZ='UTC')

# Helper function to print current time and message
print_timestamped <- function(message) {
  print(paste(Sys.time(), message))
}

print_timestamped("Starting the script execution.")

log_memory_usage <- function() {
  mem_info <- gc()
  print(paste("NCells usage (MB):", mem_info[1,2]))
  print(paste("VCells usage (MB):", mem_info[2,2]))
}

clusterFile = "Input Data/Clusters/CaliforniaClusters.shp"

rastFile = "Input Data/FCID2018_masked_V6.tif"

combinedIDs = rastExtract(clusterFile,rastFile)

biomassFile = "Input Data/residue_by_treatment.csv"

biomassData = createBiomassData(biomassFile)

GLRBT = aggClusters(biomassData,combinedIDs)

#read in elevation data
elevTIF = "Input Data/dem90_hf.tif"
elevData = rast(elevTIF)

#read in counties
countySHP = "Input Data/counties/cnty19_1.shp"
countyData = vect(countySHP)

#read in fire hazard severity zones
FHSZ = "Input Data/FHSZ/FHSZ_SRA_LRA_Combined.shp"
fireData = vect(FHSZ)

#read in land ownership
land_file = "Input Data/Land_Ownership/ownership24_1.shp"
ownData = vect(land_file)

#read in forest type
forest_file = "Input Data/Forest Type/TreeMap2016_FLDTYPCD.tif"
forestData = rast(forest_file)

#add attributes
completed_GLRBT = add_attrbibutes(GLRBT,countyData,fireData,ownData,forestData,elevData)

#filter GLRBT
GLRBT_filtered = filter_GLRBT(completed_GLRBT)

#write to
fwrite(GLRBT_filtered,"GLRBT_filtered.csv")