#names=test.kmeans100.lan.song
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names)) 
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)#replace A WITH B
  names
}

load_county_combined_df = function(county_name){
  county_dir <- paste0("counties/",county_name)
  csv_file_path <- paste(county_dir,"/",county_name,"_private_forest2.csv",sep="")
  return (read.csv(csv_file_path))
}



#county_name<-"Amador"


round_lat_long <- function(df){
  df$xi <- round(df$xi,2)
  df$yi <- round(df$yi,2)
  return(df)
}

regularize <- function(a,b){

  vals = !is.na(a[]) # put all value is not null to vals
  a[vals] = b[vals]
  return (a)
}


