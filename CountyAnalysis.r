#### Copy of NewScript_ClusterAnalysis_rev.r 3/21/22. Change to single Yuba county and change absolute paths
# don't forget to run `source R/3.6.3` on farm
library(sp)
library(raster)
library(rgdal)
library(parallel)
library(maptools)
library(foreign)
library(sqldf)
library(doMC)
library(dplyr)

# Single county, single processor, so no MC
#registerDoMC(12) # 12 clusters for single-county testing

biomassPath = "/home/postit/ClusterAnalysis/BiomassRasters/"
rootPath = "/home/postit/ClusterAnalysis/"
setwd(rootPath)

projectedDataRasterFiles <<- list.files(rootPath, pattern = glob2rx("Total*.tif$")) # projected rasters in root

if (length(projectedDataRasterFiles) > 0) {
  paste("skipping f3 data projection, already done previously")
} else {
  #convert the projection to projection of california clusters raster
  foreach(l = fl) %do% {
    system(paste0("gdalwarp -tr 30 -30 -t_srs '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0' ", biomassPath, l, " ", l))
  }

  print("conversion complete")
}

#Read counties shape file from working directory (root)
Counties = readOGR(rootPath, "county_siera_proj")
lst = Counties$fips # list of counties
Hzrd = data.frame(haz_class = c(0:3), Haz_name = c("No", "Moderate", "High", "Very High")) # assigning unique ids to hazard zone
Frst = data.frame(Frst_code = c(0:3), reg_d = c("other", "Mixed_Conifer", "Pine", "Other")) # assigning unique ids to forest types
landOwn = data.frame(Lnd_code = c(0:14), land_use = c("Non-federal/non-forest", "Local Government", "Non-Profit Conservancies and Trusts", "CA Dept. of Forestry and Fire Protection",
"CA Dept. of Parks and Recreation", "CA Dept. of Fish and Wildlife", "Other State Lands", "National Park Service", "USDA Forest Service", "Bureau of Reclamation",
"Bureau of Land Management", "US Fish and Wildlife Service", "Bureau of Indian Affairs", "Department of Defense", "Other Federal Lands")) # assigning unique ids to landownship

l = 6115 # single county, Yuba
year = 2016 # TODO: read year of analysis from TIF files
cnt = subset(Counties, Counties$fips == l)
cnt_filename = paste0(cnt$NAME, "_", year, "_clusterData.csv")

# only continue processing this county if an output file doesn't already exist
if (!file.exists(cnt_filename)) {
  print(paste("processing county", cnt$NAME))

  print("stacking rasters for county begin")

  #subsetting rasters, stacking and creating csvs by county parallally
  writeOGR(cnt, "trash/", l, driver = "ESRI Shapefile")
  dir.create(paste0("trash/", l))
  data = list.files(pattern = '.tif')
  for (dat in data) {
    system(paste0("gdalwarp -of GTiff -tr 30 -30 -cutline trash/", l, ".shp -crop_to_cutline ", dat, " trash/", l, "/", dat))
  }
  cal = raster(paste0("trash/", l, "/CaliforniaClusters.tif"))
  values(cal) = 1:length(cal)
  writeRaster(cal, paste0("trash/", l, "/calID.tif"))
  rst = list.files(paste0("trash/", l), pattern = ".tif")
  rst_stck = stack(paste0("trash/", l, "/", rst))

  #function to shorten the names of the stack
  shorten_fname <- function(ras) {

    fnames = names(ras)
    short_names = c()

    for (name in fnames) {
      if (grepl("NoMGT", name, fixed = TRUE)) {
        temp = strsplit(name, "NoMGT")[[1]][1]
        temp = substr(temp, 11, nchar(temp) - 1)
        short_names = c(short_names, tolower(temp))
      } else {
        short_names = c(short_names, tolower(name))
      }
    }

    names(ras) = short_names
    return(ras)

  }

  print("raster stacking complete")

  rst_stck = shorten_fname(rst_stck)
  rst1 = data.frame(rasterToPoints(rst_stck))
  rst1[is.na(rst1)] = 0
  rst2 = subset(rst1, rst1$californiaclusters > 0)

  print("raster -> point conversion complete")

  cal1 = projectRaster(cal, crs = '+proj=longlat +datum=WGS84 +no_defs', method = "ngb") # reprojecting california id raster
  cal2 = data.frame(rasterToPoints(cal1))

  print("reprojection to WGS84 complete")

  rst3 = sqldf('select * from rst2 LEFT JOIN cal2 ON rst2.calid=cal2.calID')
  rst3 = rst3[c(-1, -2, -3)]
  rst3 = sqldf('select * from rst3 LEFT JOIN Hzrd ON rst3.hazard=Hzrd.haz_class')
  rst3 = sqldf('select * from rst3 LEFT JOIN Frst ON rst3.sierrafrst=Frst.Frst_code')
  rst3 = sqldf('select * from rst3 LEFT JOIN landOwn ON rst3.landownship=landOwn.Lnd_code')
  rst3$cnt = cnt$NAME
  rst3$year = 2016
  colnames(rst3)[67] = "lng"
  colnames(rst3)[68] = "lat"
  colnames(rst3)[3] = "site_class"
  colnames(rst3)[1] = "cluster_no"
  rst4 = rst3[c(67, 68, 1:3, 76, 77, 70:75, 7:66)]
  #rst5 <- rapply(object = rst4, f = round, classes = "numeric", how = "replace", digits = 2) 
  colnames(rst4)[9] = "haz_name"
  #colnames(rst5)[6]="county_name"
  rst4 <- rst4[order(rst5$cluster_no),]
  rst4 = subset(rst4, rst4$ba_15 != 0)

  print("dataframe creation complete, writing result")

  write.csv(rst4, paste0(cnt$NAME, "_clusterData.csv"))

  print("result written, exiting")
} else {
  print(paste("skipping county", cnt$NAME))
}

print("all results written, exiting")
