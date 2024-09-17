source("index.R")

library(terra)

createBiomassData = function (biomassFile) {
#read in residue by treatment
print_timestamped("Reading and updating biomass data...")
biomassData = read.csv(biomassFile)
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

return (biomassData)
}