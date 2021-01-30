# new way to get usgs areas
# should take less than 2 hours per year
# write csv file of data frame (3 cols):
#	count: # of 30X30m pixels of usgs land types
#	ro: region-ownership code, region*10 + ownership (each are 1-9)
#	type: 1=developed, 2=cultivated

# this maxes out the memory usage, and didn't write a temp file for one year
# it took 8 min to reach the year loop (without writing the ca raster)
# then 28 min to finish one year

# comparison for 6 years of processing on cropped rasters, 60kmX60km 
# the loop over years took 31 seconds
# the stack of years took 38 seconds
# no temporary files were created, but a stack may need more at once because the years are done at once


cat("Start proc_usgs_land_freq.r at", date(), "\n")
ptm<-proc.time()
library(raster)
setwd("/Users/adivi/projects/cnra_carbon/gis_data/")

data_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_land/bau_preproc/")
gis_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/")

tmp_dir = paste0(data_dir, "tmp_1/")

dir.create(tmp_dir, recursive=TRUE)

# set some options to make this more efficient
rasterOptions(datatype = "INT4S", chunksize = 2e+9, maxmemory = 1e+12, tmpdir = tmp_dir)

raster.caland <- raster("calandv2_2010_landcat.bil")

NAvalue(raster.caland) <- -9999

# preprocessing should be efficient
# first drop the ownership to get region-land tyep
#ca1 <- raster.caland %/% 100
# region (drop the land type)
#ca2 <- ca1 %/% 1000
# ownership (th last two digits)
#ca3 <- raster.caland %% 100
# ro
# ca <- ca2*10 + ca3

# this has been done, so just read it in
#ca <- ((raster.caland %/% 100) %/% 1000)*10 + raster.caland %% 100
#writeRaster(ca, filename = paste0(gis_dir, "calandv2_ro.grd"), datatype = "INT4S", overwrite = TRUE, NAflag = -9999)

ca = raster(paste0(gis_dir, "calandv2_ro.grd"))

years <- 2052:2101

for ( y in years) {
	cat("start year", y, "at", date(), "\n")
	# set up matrix for reclassifying usgs 
	# rcl[,1] = "is", rcl[,2] = "becomes"
	rcl <- cbind(1:11, NA)
	# 2 (developed) becomes 1
	rcl[2,2] <- 1
	# 8 (cultivated) becomes 2 
	rcl[8,2] <- 2
	# all others are NA
	usgs <- raster(paste0(data_dir, "USGS_BAU_", y, ".tif"))
	# reclassify usgs to 1, 2, or NA
	usgs <- reclassify(usgs, rcl)
	cat("reclassify done at", date(), "\n")
	# create new raster with combined ro and landtype (dev or cult)
	# ro (11 to 99) * 10 + (1 or 2) = (110 to 990) + (1 or 2)	
	x <- ca * 10 + usgs 
	cat("band math done at", date(), "\n")
	# create freq table of x. don't use NA values.  the merge is a weird list, not a df
	f <- freq(x, useNA='no', merge=TRUE)
	df_out = data.frame(count = f[,2], ro = trunc(f[,1] / 10), type = f[,1] %% 10, stringsAsFactors = FALSE)
	write.csv(df_out, paste0(data_dir, y, "_USGS_lt_pixels_per_ro.csv"), row.names = FALSE)
	
	# delete the temporary files each year
	file.remove(paste0(tmp_dir, list.files(tmp_dir)))
} # end loop over years

proc.time()-ptm
cat("End proc_usgs_land_freq.r at", date(), "\n")
