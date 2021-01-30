# process the pre-processed fire rasters to get raw tables for write_caland_inputs.r
#  use the csv files of region-ownership indices, rather than locating the raster indices from the raster

# input fire area rasters have been resampled/reprojected to the CALAND 30m grid
#  inputs values are in square meters
#  NA value = -9999

# write csv file of data frame (varying number of columns):
# 	region name
#	ownership name
#	fire burn area by year, hectares, each year is its own column

# all region-ownership combinations will be in the file, even if they do not exist in the data
#	currently there exist 77 out of 81 possible combinations

# region-ownership code for csv files: region*10 + ownership (each are 1-9)
# region and ownership codes are enumerated alphabetically

# the zonal function does 1 year each half hour!!!
# tested the extract function, and it is slightly slower than the brackets
# tested creating a mask, but it doesn't speed up the computation at all

# note: add the "X" to the column lable because they are being added subsequently anyway
#	this makes all columns consistent; otherwise the last column will not have an "X"

cat("Start proc_ca4a_fire_area.r at", date(), "\n")
ptm<-proc.time()
library(raster)

start_year = 2001
end_year = 2100
num_years = end_year - start_year + 1
years <- unique(c(start_year:end_year))

gis_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/")
ro_dir = "/Users/adivi/projects/cnra_carbon/gis_data/region_ownership_v2/"
ro_tag = "_inds.csv"

setwd(gis_dir)

#data_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_fire/canESM2_85_bau_preproc/")
#data_tag = "fire_area_canESM2_85_bau_"
data_dir = paste0("/Users/adivi/projects/cnra_carbon/gis_data/fourth_assess_fire/canESM2_45_bau_preproc/")
data_tag = "fire_area_canESM2_45_bau_"

tmp_dir = paste0(data_dir, "tmp_1/")

dir.create(tmp_dir, recursive=TRUE)

out_name = paste0(data_dir, data_tag, start_year, "_", end_year, ".csv")

# set some options to make this more efficient
rasterOptions(datatype = "INT4S", chunksize = 2e+9, maxmemory = 1e+12, tmpdir = tmp_dir)

# read in the r-o raster file
raster_ro <- raster("calandv2_ro.grd")
NAvalue(raster_ro) <- -9999

# region names
num_reg = 9
reg_names = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")
# ownerhsip names
num_own = 9
own_names = c("BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")
min_ro_id = 11
max_ro_id = 99

ro_area_out = array(dim = c(num_years, max_ro_id))
ro_region_names = array(dim = max_ro_id)
ro_own_names = array(dim = max_ro_id)
ro_region_names[] = NA
ro_own_names[] = NA

# set up the labels
for (ro in min_ro_id:max_ro_id) {
	o_ind = ro %% 10
	if (o_ind != 0) {
		r_ind = trunc(ro / 10)
		ro_region_names[ro] = reg_names[r_ind]
		ro_own_names[ro] = own_names[o_ind]
	}
}

sqm2ha = 1.0/10000.0

for (y_ind in 1:num_years) {
	cat("start year", years[y_ind], "at", date(), "\n")
	fire_area_in = raster(paste0(data_dir, data_tag, years[y_ind], ".tif"))
	
	# get the areas
	out_area = zonal(fire_area_in, raster_ro, fun = "sum", digits = 4)
	
	# store each year
	# loop through and assign the area so that the r-o id matches the index
	# also convert to ha
	for (i in 1:length(out_area[,1])) {
		ro_area_out[y_ind, out_area[i,1]] = sqm2ha * out_area[i,2] 
	}
	
	# delete the temporary files each year
	file.remove(paste0(tmp_dir, list.files(tmp_dir)))
} # end y loop over years

df_out = data.frame(Region = ro_region_names, Ownership = ro_own_names, stringsAsFactors = FALSE)
for (y_ind in 1:num_years) {
	df_out = data.frame(df_out, ro_area_out[y_ind,])
	colnames(df_out)[ncol(df_out)] = paste0("X",years[y_ind],"_ha")
}	
	
# remove the rows with no region-ownership designation; there may still be extra reg-own records
df_out = df_out[complete.cases(df_out[,1:2]),]
	
write.csv(df_out, paste0(out_name), row.names = FALSE)

proc.time()-ptm
cat("End proc_ca4a_fire_area.r at", date(), "\n")
