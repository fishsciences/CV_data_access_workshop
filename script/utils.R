#--------------------------------------------#
# M. Johnston 
# Utility/convenience functions for analysis 
# Friday 2020-06-19 11:14:28 ----------------# 

#-------------------------------------------------------#
check_and_install_packages = function() {
  
  list.of.packages <- c("tidyverse",
                      "lubridate",
                      "maps", 
                      "rgeos",
                      "maptools",
                      "ggmap",
                      "gganimate", 
                      "sf", 
                      "rgdal", 
                      "av",
                      "gifski")

# make list of packages that are required, but not already downloaded
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

# download packages that are not already present in the library
if(length(new.packages))
  install.packages(new.packages)

# load packages
packages_load <- lapply(list.of.packages, require, character.only = TRUE)

# print warning if there is a problem with installing/loading some of packages
if(any(as.numeric(packages_load)==0)){
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != TRUE], sep = ","), "not loaded!"))
}else{
  print("All packages were successfully loaded.")
}
}

#-------------------------------------------------------#
download_telemetry_data = function() {
 
  # unzip into data folder
  if(
    !file.exists("data/UCD_GS_RESCUE.csv")){
     tempdl <- tempfile()
  
  # download zipped data file from BARD website
  download.file(
    'http://cftc.metro.ucdavis.edu/biotelemetry-autonomous-real-time-database/resources/csvs/UCD_GS_RESCUE.zip',
    tempdl,
    mode = 'wb'
  )
    unzip(tempdl, "UCD_GS_RESCUE.csv", exdir = "data")
    print("Data was successfully added to your /data directory.")}
    else
    print("data already exists")
  
}


# quickly 'vet' a dataframe by previewing rows at the head, middle, and tail:
#-------------------------------------------------------#
vet <- function(d, n = 4L) {
 if(class(d) != 'data.frame') stop('vet() can only vet dataframes')
 left <- as.integer(nrow(d) / 2 - n / 2)
 torso = d[seq_len(n) + left - 1L,]
rbind(head(d, n), torso, tail(d, n))
}


# typing shortcuts
#--------------------------------------------#
len <- function(x){length(unique(x))}
csn <- function(x){colSums(is.na(x))}
rsn <- function(x){rowSums(is.na(x))}
