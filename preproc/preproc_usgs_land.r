
# subset each year's set of 10 reps

cat("Start preproc_usgs_land.r at", date(), "\n")
# this took 2 hours 45 minutes for 2010, without merging the cultivated
# note that f plot_caland processes were running at the same time

library(raster)
library(rgdal)
showTmpFiles() # shows what temp files are there
setwd("/Users/adivi/projects/cnra_carbon/gis_data/")
data_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_land/BAU/State_Class/")
out_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_land/bau_preproc/")

# set some options to make the files smaller
rasterOptions(datatype = "INT4S", tmpdir = "tmp_1")

# read in CALAND bil file 
raster2010.caland <- raster("calandv2_2010_landcat.bil")
#NAvalue(raster.caland) <- -9999
Mode <- function(x) {
  ux <- unique(x)
  ux <-ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}

for (x in 2028:2034) {
	cat("Start year", x , date(), "\n")
	
  rasterlist <- list.files(data_dir, pattern=paste(x, "-SClass.tif", sep=""), full.names=TRUE)
  # create raster stackfor current year
  rstack <- stack(rasterlist)
  # assign USGS CRS to raster stack for current year
  crs(rstack) <- "+init=epsg:5070"
  # combine ag types into one, call it 8
  i <- 1
  for (i in 1:10) {
    rstack[[i]][values(rstack[[i]]) == 8 | values(rstack[[i]]) == 12] <- 8
  }
  ## reduce stacks of monte carlo reps to single layer
  # get mode classifications per pixel for ag, urban, and other of reps for each year (x)
  rasterlayer <- calc(rstack, fun=Mode)
  rm(rstack)
  # Reproject USGS to CALAND's
  rasterlayer.1000m <- projectRaster(rasterlayer,
                               crs="+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", 
                               method="ngb")
  rm(rasterlayer)
  # Step 2 Resample usgs to get 30 x 30 res and same extent
  # takes about 1.75 hrs per year
  rasterlayer.30m <- resample(rasterlayer.1000m, raster2010.caland, method="ngb", 
                                              filename=paste0(out_dir, "USGS_BAU_",x,".tif"), 
                              format="GTiff", overwrite=TRUE)
  rm(rasterlayer.1000m)
  rm(rasterlayer.30m)
  removeTmpFiles(h=0)
  
}
removeTmpFiles(h=0) # to delete all the temporary files
# Warning message:
# sd(<matrix>) is deprecated.
# Use apply(*, 2, sd) instead.

cat("End preproc_usgs_land.r at", date(), "\n")