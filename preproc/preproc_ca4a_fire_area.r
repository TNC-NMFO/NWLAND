# 1/16 degree CA extent, wgs84 lat-lon projection
# using the geotiff files
# output area in sq meters (integer)

# only area for now
# the data are in hectares (integer)
# need to calculate the fractions for reprojection and resampling
# these are the average values from 10 runs

# there are four main models available and 2 RCPs and 3 population
#	currently using the average model with RCP85 and bau pop
#	note that the bau pop is labelled as central on the webpage

cat("Start preproc_ca4a_fire_area.r at", date(), "\n")
# this takes 3 h 20 min per year

library(raster)
library(rasterVis)
library(rgdal)
showTmpFiles() # shows what temp files are there
setwd("/Users/adivi/projects/cnra_carbon/gis_data/")

# RCP8.5
#data_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_fire/canESM2_85_bau/")
#out_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_fire/canESM2_85_bau_preproc/")
#scen_name = "fire_CanESM2_85_AA.all.bau."
#out_name = "fire_area_canESM2_85_bau_"

# RCP4.5
data_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_fire/canESM2_45_bau/")
out_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_fire/canESM2_45_bau_preproc/")
scen_name = "fire_CanESM2_45_AA.all.bau."
out_name = "fire_area_canESM2_45_bau_"

km2ha = 100

# can read in integers because the values are integer hectares
rasterOptions(datatype = "INT4S", chunksize = 2e+9, maxmemory = 1e+12, tmpdir = "tmp_1")

# read in CALAND bil file 
raster2010_caland <- raster("calandv2_2010_landcat.bil")
#NAvalue(raster.caland) <- -9999

for (x in 2098:2100) {
  cat("Start year", x , date(), "\n")

  raster_in <- raster(paste0(data_dir, scen_name, x, ".mu.tif"))

  # get cell area in km^2 and convert burned area to fraction of cell
  cell_area <- area(raster_in, na.rm = TRUE)
  frac_in <- raster_in / (km2ha * cell_area)

  rm(raster_in)
  rm(cell_area)

  # Reproject fire to CALAND's
  frac_in_prj <- projectRaster(frac_in,
                               crs="+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", 
                               method="ngb")
date()
  
  rm(frac_in)
  
  # Step 2 Resample usgs to get 30 x 30 res and same extent
  # takes about 2 hours per year, including write
  # don't write yet
  rasterOptions(datatype = "FLT4S")

  burn_frac_out <- resample(frac_in_prj, raster2010_caland, method="ngb")
  date()
  single_cell_area = res(raster2010_caland)[1]^2
  burn_area_out = round(burn_frac_out * single_cell_area)
  date()
  rm(frac_in_prj)
  rm(burn_frac_out)  

  # write the area file as integer square meters
  rasterOptions(datatype = "INT4S")
  writeRaster(burn_area_out, filename=paste0(out_dir, out_name, x,".tif"), datatype = "INT4S", format="GTiff", overwrite=TRUE, NAflag = -9999)
  date() 
                              
  rm(burn_area_out)
  removeTmpFiles(h=0)
  
}
removeTmpFiles(h=0) # to delete all the temporary files

cat("End preproc_ca4a_fire_area.r at", date(), "\n")