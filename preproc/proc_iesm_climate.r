# proc_iesm_climate.r

# read in the h1 files from iesm - these files are the 1-d outputs at pft and column level
# use npp and soil carbon to calculate climate scalars for caland, by pft and grid cell
# map pfts to caland land types
# disaggregate values to caland land categories
# output file to be read by write_caland_inputs.r

# use 5-year running averages for npp
#  and 9-year for soil

# one raster processing step takes about 3.5 hours on my desktop, about 2h 20 min on constance
# there are three raster processing steps per year: forest npp, shrub npp, forest soil

# NOTE:
#  this currently does not account for the fact that fresh marsh does not exist in the current input land category file
#  so fresh marsh scalars are output as 1
#  use merge_raw_caland_climate.r to create the actual raw file and subsitute the cultivated scalars for fresh marsh

cat("start proc_iesm_climate.r", date(), "\n")

library(ncdf4)
library(lattice)
library(raster)
library(rasterVis)

CLIMATE = "RCP85"

if(CLIMATE == "RCP85"){ 
	data_dir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp85/"
	out_dir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp85/"
	basename = "b.e11.BRCP85C5BPRP.f09_g16.iESM_uncoupled.001.clm2.h1."
	out_name = paste0(out_dir, "climate_c_scalars_iesm_rcp85_test_soil_10_11.csv")
} else { 
	data_dir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp45/"
	out_dir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp45/"
	basename = "b.e11.BRCP45C5BPRP.f09_g16.iESM_exp2.001"
	out_name = paste0(out_dir, "climate_c_scalars_iesm_rcp45_test.csv")
}

# land category info for output
# the raster file values are: region * 100000 + land type * 100 + ownership
# region ids are 1:9 in alphabetical order above
# land type ids are defined above
# ownership ids are 1:9 in alphabetical order above
# the raster filename for disaggregating the climate data to caland land categories
# and a table with the thematic values and their names
gis_dir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/"
caland_lc_rast_fname = paste0(gis_dir, "calandv2_2010_landcat.bil")
caland_lc_text_fname = paste0(gis_dir, "calandv2_2010_landcat_area_sqm.csv")

# filter outliers based on median absoute deviation
# numstddev denotes the +- limits if the distribution were normal
FILTER = TRUE
numstddev_npp = 2
numstddev_soilc = 1

num_vars = 2
var_names = c("NPP", "TOTSOMC")
comp_names = c("Vegetation", "Soil")
npp_ind = 1
soilc_ind = 2
num_levels = 17 # currently up to 17 pfts available

# process these variables
#vinds = c(npp_ind, soilc_ind)
vinds = c(soilc_ind)

# these correspond to the actual years of data, not the output scalar years
# the output scalar years are determined based on the averaging window (see below) and these data years
start_year = 2010
end_year = 2011

# baseline scalar year
# this scalar always needs to be calculated
# this year needs to be the same for npp and soilc, so make sure it is compatible with the longer avering window below
base_scalar_year = 2010

# averaging windows in years
# these need to be odd numbers to work properly
# i think a value of 1 should work, but it hasn't been tested, and is not recommended

# npp
npp_window = 5
npp_window_half = trunc(npp_window / 2)
start_year_npp_runavg = start_year + npp_window_half
end_year_npp_runavg = end_year - npp_window_half

# soil c
soilc_window = 9
soilc_window_half = trunc(soilc_window / 2)
start_year_soilc_runavg = start_year + soilc_window_half
end_year_soilc_runavg = end_year - soilc_window_half - 1		# can't get the last year because an extra year is needed to calc the difference and the difference is stored in the previous year

# these are the necessary baseline data years
max_window_half = max(npp_window_half, soilc_window_half)
if (soilc_window_half >= npp_window_half) { # need an extra year to calculate the difference
	base_years = c((base_scalar_year - max_window_half):(base_scalar_year + max_window_half + 1))
} else {
	base_years = c((base_scalar_year - max_window_half):(base_scalar_year + max_window_half))
}
num_base_years = length(base_years)

# create the years array to loop over
# also set the appropriate indices for the running average and scalar years
years = c(base_years, start_year:end_year)
base_scalar_year_ind = max_window_half + 1
start_year_npp_runavg_ind = (start_year_npp_runavg - start_year) + 1 + num_base_years
end_year_npp_runavg_ind = (end_year_npp_runavg - start_year) + 1 + num_base_years
start_year_soilc_runavg_ind = (start_year_soilc_runavg - start_year) + 1 + num_base_years
end_year_soilc_runavg_ind = (end_year_soilc_runavg - start_year) + 1 + num_base_years

# these include the baseline scalar data years added to the front of the main data processing years
# these determine the lengths of storage array dimensions
tot_years = length(years)
tot_months = tot_years * 12

# these are the california lat-lon limits to extract from the files (N and E)
max_lat = 43.0
min_lat = 32.0
max_lon = 246.5
min_lon = 235.0

# just need bare to crop pfts
# these are the clm codes
minpft = 0
maxpft = 15

# caland info
proj4_string = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
num_reg = 9
reg = c("Central_Coast", "Central_Valley", "Delta", "Deserts", "Eastside", "Klamath", "North_Coast", "Sierra_Cascades", "South_Coast")
lt = c("Water", "Ice", "Barren", "Sparse", "Desert", "Shrubland", "Grassland", "Savanna", "Woodland", "Forest", "Meadow",
"Coastal_marsh", "Fresh_marsh", "Cultivated", "Developed_all")
lt_codes = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 160)
num_lt = 15
own = c("BLM", "DoD", "Easement", "Local_gov", "NPS", "Other_fed", "Private", "State_gov", "USFS_nonwild")
num_own = 9

# indices for accessing specific land types
water_ind = 1
ice_ind = 2
barren_ind = 3
sparse_ind = 4
desert_ind = 5
shrub_ind = 6
grass_ind = 7
savanna_ind = 8
woodland_ind = 9
forest_ind = 10
meadow_ind = 11
coastmarsh_ind = 12
freshmarsh_ind = 13
cultivated_ind = 14
developed_ind = 15

# negative caland base values for soil c accumulation
negative_caland_soilc_accum_inds  = c(grass_ind, savanna_ind, woodland_ind)

ntag = ".nc"

# seconds per month
secpermonth = 86400 * c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# days per month
dayspermonth = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daysperyear = sum(dayspermonth)

# convert grams to Pg
g2Pg = 1E-15

# convert from km^2 to m^2
kmsq2msq = 1E6

# get some necessary info from the baseline scalar year file 
fname = paste0(data_dir, basename, base_scalar_year, "-01", ntag)	
nc_id = nc_open(fname)
num_lon = nc_id$dim$lon$len
num_lat = nc_id$dim$lat$len
num_cell = nc_id$dim$gridcell$len
num_col = nc_id$dim$column$len
num_pft = nc_id$dim$pft$len
num_time = nc_id$dim$time$len

cols1d_lon = ncvar_get(nc_id, varid = "cols1d_lon", start = c(1), count = c(num_col))
cols1d_lat = ncvar_get(nc_id, varid = "cols1d_lat", start = c(1), count = c(num_col))
cols1d_ixy = ncvar_get(nc_id, varid = "cols1d_ixy", start = c(1), count = c(num_col)) # 2d lon index
cols1d_jxy = ncvar_get(nc_id, varid = "cols1d_jxy", start = c(1), count = c(num_col)) # 2d lat index
cols1d_wtgcell = ncvar_get(nc_id, varid = "cols1d_wtgcell", start = c(1), count = c(num_col)) # column weight relative to grid cell
cols1d_wtlunit = ncvar_get(nc_id, varid = "cols1d_wtlunit", start = c(1), count = c(num_col)) # column weight relative to land unit
# column land unit type - note that the description of types in the file is incorrect
#   1  => (istsoil)    soil (vegetated or bare soil landunit)
#   2  => (istice)     land ice; land ice does not have soil carbon
#   3  => (istdlak)    deep lake; deep lake does not have soil carbon
#   4  => (istslak)    shallow lake (not currently implemented)
#   5  => (istwet)     wetland; wetland does not have soil carbon
#   6  => (isturb)     urban; urban does not have soil carbon
#   7  => (istice_mec) land ice (multiple elevation classes); not included in these sims
#   8  => (istcrop)    crop (only for crop configuration): this is not used! crop is a pft in soil/vegetated
# NPP values for vegetated land unit pfts only
# soil c for columns - use urban and wetland also? (gC/m^2)
cols1d_itype_lunit = ncvar_get(nc_id, varid = "cols1d_itype_lunit", start = c(1), count = c(num_col))

pfts1d_lon = ncvar_get(nc_id, varid = "pfts1d_lon", start = c(1), count = c(num_pft))
pfts1d_lat = ncvar_get(nc_id, varid = "pfts1d_lat", start = c(1), count = c(num_pft))
pfts1d_ixy = ncvar_get(nc_id, varid = "pfts1d_ixy", start = c(1), count = c(num_pft)) # 2d lon index
pfts1d_jxy = ncvar_get(nc_id, varid = "pfts1d_jxy", start = c(1), count = c(num_pft)) # 2d lat index
pfts1d_wtcol = ncvar_get(nc_id, varid = "pfts1d_wtcol", start = c(1), count = c(num_pft)) # pft weight relative to column
# pft vegetation type -  0 to 16 in order:
# 0 = bare
# 1 = needleleaf evergreen temperate tree
# 2 = needleleaf evergreen boreal tree
# 3 = needleleaf deciduous boreal tree
# 4 = broadleaf evergreen tropical tree
# 5 = broadleaf evergreen temperate tree
# 6 = broadleaf deciduous tropical tree
# 7 = broadleaf deciduous temperate tree
# 8 = broadleaf deciduous boreal tree
# 9 = broadleaf evergreen temperate shrub
# 10 = broadleaf deciduous temperate shrub
# 11 = broadleaf deciduous boreal shrub
# 12 = c3 arctic grass
# 13 = c3 non-arctic grass
# 14 = c4 grass
# 15 = crop
# 16 = extra
pfts1d_itype_veg = ncvar_get(nc_id, varid = "pfts1d_itype_veg", start = c(1), count = c(num_pft))
# pft land unit type - note that the description of types in the file is incorrect
#   1  => (istsoil)    soil (vegetated or bare soil landunit)
#   2  => (istice)     land ice; land ice does not have soil carbon
#   3  => (istdlak)    deep lake; deep lake does not have soil carbon
#   4  => (istslak)    shallow lake (not currently implemented)
#   5  => (istwet)     wetland; urban does not have soil carbon
#   6  => (isturb)     urban; urban does not have soil carbon
#   7  => (istice_mec) land ice (multiple elevation classes); not included in these sims
#   8  => (istcrop)    crop (only for crop configuration): this is not used! crop is a pft in soil
# NPP values for vegetated land unit pfts only (gC/m^2/sec)
pfts1d_itype_lunit = ncvar_get(nc_id, varid = "pfts1d_itype_lunit", start = c(1), count = c(num_pft)) 
# 2d lon-lat variables
# landmask and pftmask are identical for these iesm runs
lon = ncvar_get(nc_id, varid = "lon", start = c(1), count = c(num_lon))
lat = ncvar_get(nc_id, varid = "lat", start = c(1), count = c(num_lat))
area = ncvar_get(nc_id, varid = "area", start = c(1,1), count = c(num_lon, num_lat)) # km^2
landfrac = ncvar_get(nc_id, varid = "landfrac", start = c(1,1), count = c(num_lon, num_lat))
landmask = ncvar_get(nc_id, varid = "landmask", start = c(1,1), count = c(num_lon, num_lat))
pftmask = ncvar_get(nc_id, varid = "landmask", start = c(1,1), count = c(num_lon, num_lat))
nc_close(nc_id)

# convert this to m^2
landarea = area * landfrac * landmask * kmsq2msq
landarea_pft = area * landfrac * landmask * pftmask * kmsq2msq

# determine the lat-lons and columns and pft and gridcell elements corresponding to the CA extraction area
# lat-lon values are centers of cells, and cells are about 1 degree
col_reg_inds = intersect(which(cols1d_lon < (max_lon + 0.5) & cols1d_lon > (min_lon - 0.5)), which(cols1d_lat < (max_lat + 0.5) & cols1d_lat > (min_lat - 0.5)))
pft_reg_inds = intersect(which(pfts1d_lon < (max_lon + 0.5) & pfts1d_lon > (min_lon - 0.5)), which(pfts1d_lat < (max_lat + 0.5) & pfts1d_lat > (min_lat - 0.5)))
min_lon_ind = min(cols1d_ixy[col_reg_inds], na.rm=TRUE)
max_lon_ind = max(cols1d_ixy[col_reg_inds], na.rm=TRUE)
min_lat_ind = min(cols1d_jxy[col_reg_inds], na.rm=TRUE)
max_lat_ind = max(cols1d_jxy[col_reg_inds], na.rm=TRUE)
num_lon_reg = max_lon_ind - min_lon_ind + 1
num_lat_reg = max_lat_ind - min_lat_ind + 1
lon_inds = c(min_lon_ind:max_lon_ind)
lat_inds = c(min_lat_ind:max_lat_ind)

min_lon_center = lon[min_lon_ind]
max_lon_center = lon[max_lon_ind]
min_lat_center = lat[min_lat_ind]
max_lat_center = lat[max_lat_ind]

lon_res = (max_lon_center - min_lon_center) / (num_lon_reg - 1)
lat_res = (max_lat_center - min_lat_center) / (num_lat_reg - 1)

# this is the land area matrix for just the region
landarea_reg = landarea[min_lon_ind:max_lon_ind, min_lat_ind:max_lat_ind]
landarea_pft_reg = landarea_pft[min_lon_ind:max_lon_ind, min_lat_ind:max_lat_ind]

temp = array(dim = num_lon_reg)
temp_pftwt = array(dim = num_lon_reg)
temp_count_lt = array(dim = num_lon_reg)
temp_pftwt_sum = array(dim = c(num_lt, num_lon_reg))

# arrays for storing regional time series of (time, lat, lon) variables
grid_vardata = array(dim=c(num_vars, num_levels, num_lon_reg, num_lat_reg, tot_months))
grid_vardata[] = NA
grid_pftwt = array(dim=c(num_vars, num_levels, num_lon_reg, num_lat_reg, tot_months))
grid_pftwt[] = NA
grid_yeardata_lt = array(dim=c(num_vars, num_lt, num_lon_reg, num_lat_reg, tot_years))
grid_yeardata_lt[] = 0.0
grid_yearmonths_lt = array(dim=c(num_vars, num_lt, num_lon_reg, num_lat_reg, tot_years))
grid_yearmonths_lt[] = 0
grid_temp_lt = array(dim=c(num_lt, num_lon_reg))
grid_temp_lt[] = 0.0

# these are all at the caland land type
grid_runavg = array(dim=c(num_vars, num_lt, num_lon_reg, num_lat_reg, tot_years))
grid_runavg[] = 0.0
grid_runavg_count = array(dim=c(num_vars, num_lt, num_lon_reg, num_lat_reg, tot_years))
grid_runavg_count[] = 0
grid_yeardiff = array(dim=c(num_vars, num_lt, num_lon_reg, num_lat_reg, tot_years))
grid_yeardiff[] = 0.0
grid_scalar = array(dim=c(num_vars, num_lt, num_lon_reg, num_lat_reg, tot_years))
grid_scalar[] = NA

# loop over the years to calculate caland land type scalars on the input grid
cat("Start processing land type scalars on input grid", date(), "\n")

totyind = 0
for (y in years) {
	totyind = totyind + 1
	# loop over months
	for (m in 1:12) {
		# read in the file
		if(m > 9) {
			mtag = m
		}else {
			mtag = paste0(0, m)
		}
		totmind = (totyind - 1) * 12 + m
		
		fname = paste0(data_dir, basename, y, "-", mtag, ntag)
		nc_id = nc_open(fname)
		
		# loop over variables
		for (v in vinds) {
			# first read the data and get the indices of the vegetatedland type
			if (var_names[v] == "NPP") { 
				val_count = num_pft
				all_indata = ncvar_get(nc_id, varid = var_names[v], start = c(1,1), count = c(val_count,1))
				all_landtypes = ncvar_get(nc_id, varid = "pfts1d_itype_lunit", start = c(1), count = c(val_count))
				reg_lt_inds = intersect(which(all_landtypes == 1), pft_reg_inds)
			} else if (var_names[v] == "TOTSOMC") { 
				val_count = num_col
				all_indata = ncvar_get(nc_id, varid = var_names[v], start = c(1,1), count = c(val_count,1))
				all_landtypes = ncvar_get(nc_id, varid = "cols1d_itype_lunit", start = c(1), count = c(val_count))
				reg_lt_inds = intersect(which(all_landtypes == 1), col_reg_inds)
			} else {
				cat("Error: variable not supported\n")
				stop()
			}
			
			# loop over the rows and fill the grid array here
			for (j in 1:num_lat_reg) {

				# loop over the pfts
				
				# aggregate the clm pft values to caland land types
				# need to do this here because the pft weights can change monthly
				# for soil, the value is the same for all pfts in a column, so don't need to average; just add the value once
				
				# soil goes to: all except water, ice, barren, sparse because none of these have accumulation values in CALAND
				# tree pfts go to: forest vegetation, woodland vegetation, savanna vegetation, developed vegetation
				# shrub pfts go to: shrubland vegetation
				
				# set the unused npp and soil c values to NA (those not explicitly mapped above, including water and ice)
				
				temp_pftwt_sum[] = 0.0
				temp_count_lt[] = 0
				grid_temp_lt[] = 0.0
				
				for (l in minpft:maxpft) {
					temp[] = NA
					temp_pftwt[] = NA
					if (var_names[v] == "NPP") {
						temp_data_inds = which(pfts1d_jxy[reg_lt_inds] == min_lat_ind + j - 1 & pfts1d_itype_veg[reg_lt_inds] == l)
						
						lon_inds_in = pfts1d_ixy[reg_lt_inds][temp_data_inds]
						for (i in 1:length(lon_inds_in)) {
							set_ind = which(lon_inds == lon_inds_in[i])
							temp[set_ind] = all_indata[reg_lt_inds][temp_data_inds][i] * secpermonth[m]
							temp_pftwt[set_ind]  = pfts1d_wtcol[reg_lt_inds][temp_data_inds][i]
						}
						
						# set this month's pft weight with respect to column; these should add to 1 across all pfts
						grid_pftwt[v, l+1, , j, totmind] = temp_pftwt
						
						# store this month's values for the pfts
						grid_vardata[v, l+1, , j, totmind] = temp	
					
						# keep track of number of months with valid data for each pft
						valid_inds = which(!is.na(temp))
						
						# need to filter out invalid values
						na_inds = which(is.na(temp))
						temp[na_inds] = 0.0
						temp_pftwt[na_inds] = 0.0
						
						### aggregate the npp to caland land type and year
						
						# only set these once
						# ice, water, barren, sparse, desert, cultivated, coastal marsh, fresh marsh, grassland, meadow
						if (l == 0) {
							grid_yeardata_lt[v, c(water_ind, ice_ind, barren_ind, sparse_ind, desert_ind, grass_ind, meadow_ind, cultivated_ind, coastmarsh_ind, freshmarsh_ind), , j, totyind] = NA
							grid_yearmonths_lt[v, c(water_ind, ice_ind, barren_ind, sparse_ind, desert_ind, grass_ind, meadow_ind, cultivated_ind, coastmarsh_ind, freshmarsh_ind), , j, totyind] = 0
						}
						
						# tree pfts (1-8)
						if (l >=1 & l <= 8) {
							# keep track of number of months with valid data for each land type
							if ( l == 1 ) { temp_count_lt[] = 0 }
							temp_count_lt[valid_inds] = 1
							if (l == 8) {
								grid_yearmonths_lt[v, forest_ind, , j, totyind] = grid_yearmonths_lt[v, forest_ind, , j, totyind] + temp_count_lt
								grid_yearmonths_lt[v, woodland_ind, , j, totyind] = grid_yearmonths_lt[v, woodland_ind, , j, totyind] + temp_count_lt
								grid_yearmonths_lt[v, savanna_ind, , j, totyind] = grid_yearmonths_lt[v, savanna_ind, , j, totyind] + temp_count_lt
								grid_yearmonths_lt[v, developed_ind, , j, totyind] = grid_yearmonths_lt[v, developed_ind, , j, totyind] + temp_count_lt
							}	

							grid_temp_lt[forest_ind, ] = grid_temp_lt[forest_ind, ] + temp_pftwt * temp
							grid_temp_lt[woodland_ind, ] = grid_temp_lt[woodland_ind, ] + temp_pftwt * temp
							grid_temp_lt[savanna_ind, ] = grid_temp_lt[savanna_ind, ] + temp_pftwt * temp
							grid_temp_lt[developed_ind, ] = grid_temp_lt[developed_ind, ] + temp_pftwt * temp
							temp_pftwt_sum[forest_ind, ] = temp_pftwt_sum[forest_ind, ] + temp_pftwt
							temp_pftwt_sum[woodland_ind, ] = temp_pftwt_sum[woodland_ind, ] + temp_pftwt
							temp_pftwt_sum[savanna_ind, ] = temp_pftwt_sum[savanna_ind, ] + temp_pftwt
							temp_pftwt_sum[developed_ind, ] = temp_pftwt_sum[developed_ind, ] + temp_pftwt
						} # end if forest
						
						# shrub pfts (9-11)
						if (l >= 9 & l <= 11) {
							# keep track of number of months with valid data for each land type
							if ( l == 9 ) { temp_count_lt[] = 0 }
							temp_count_lt[valid_inds] = 1
							if (l == 11) {
								grid_yearmonths_lt[v, shrub_ind, , j, totyind] = grid_yearmonths_lt[v, shrub_ind, , j, totyind] + temp_count_lt
							}
							
							grid_temp_lt[shrub_ind, ] = grid_temp_lt[shrub_ind, ] + temp_pftwt * temp
							temp_pftwt_sum[shrub_ind, ] = temp_pftwt_sum[shrub_ind, ] + temp_pftwt
						} # end if shrub
						
					} else if (var_names[v] == "TOTSOMC") {
						temp_data_inds = which(cols1d_jxy[reg_lt_inds] == min_lat_ind + j - 1)
						
						lon_inds_in = cols1d_ixy[reg_lt_inds][temp_data_inds]
						for (i in 1:length(lon_inds_in)) {
							set_ind = which(lon_inds == lon_inds_in[i])
							temp[set_ind] = all_indata[reg_lt_inds][temp_data_inds][i]
						}
						
						# store this month's values for the pfts
						grid_vardata[v, l+1, , j, totmind] = temp	
					
						# keep track of number of months with valid data for each pft
						valid_inds = which(!is.na(temp))
						
						### aggregate the soil c to caland land type and year
						
						# set all the appropriate land type soil c values once
						if (l == 0) {
							# keep track of number of months with valid data
							if ( l == 0 ) { temp_count_lt[] = 0 }
							temp_count_lt[valid_inds] = 1
							grid_yearmonths_lt[v, c(barren_ind, water_ind, ice_ind, sparse_ind), , j, totyind] = 0
							grid_yearmonths_lt[v, desert_ind, , j, totyind] = grid_yearmonths_lt[v, desert_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, shrub_ind, , j, totyind] = grid_yearmonths_lt[v, shrub_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, grass_ind, , j, totyind] = grid_yearmonths_lt[v, grass_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, savanna_ind, , j, totyind] = grid_yearmonths_lt[v, savanna_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, woodland_ind, , j, totyind] = grid_yearmonths_lt[v, woodland_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, forest_ind, , j, totyind] = grid_yearmonths_lt[v, forest_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, meadow_ind, , j, totyind] = grid_yearmonths_lt[v, meadow_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, coastmarsh_ind, , j, totyind] = grid_yearmonths_lt[v, coastmarsh_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, freshmarsh_ind, , j, totyind] = grid_yearmonths_lt[v, freshmarsh_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, cultivated_ind, , j, totyind] = grid_yearmonths_lt[v, cultivated_ind, , j, totyind] + temp_count_lt
							grid_yearmonths_lt[v, developed_ind, , j, totyind] = grid_yearmonths_lt[v, developed_ind, , j, totyind] + temp_count_lt
							
							# sum the soil c data
							grid_yeardata_lt[v, c(barren_ind, water_ind, ice_ind, sparse_ind), , j, totyind] = NA
							grid_yeardata_lt[v, desert_ind, , j, totyind] = grid_yeardata_lt[v, desert_ind, , j, totyind] + temp
							grid_yeardata_lt[v, shrub_ind, , j, totyind] = grid_yeardata_lt[v, shrub_ind, , j, totyind] + temp
							grid_yeardata_lt[v, grass_ind, , j, totyind] = grid_yeardata_lt[v, grass_ind, , j, totyind] + temp
							grid_yeardata_lt[v, savanna_ind, , j, totyind] = grid_yeardata_lt[v, savanna_ind, , j, totyind] + temp
							grid_yeardata_lt[v, woodland_ind, , j, totyind] = grid_yeardata_lt[v, woodland_ind, , j, totyind] + temp
							grid_yeardata_lt[v, forest_ind, , j, totyind] = grid_yeardata_lt[v, forest_ind, , j, totyind] + temp
							grid_yeardata_lt[v, meadow_ind, , j, totyind] = grid_yeardata_lt[v, meadow_ind, , j, totyind] + temp
							grid_yeardata_lt[v, coastmarsh_ind, , j, totyind] = grid_yeardata_lt[v, coastmarsh_ind, , j, totyind] + temp
							grid_yeardata_lt[v, freshmarsh_ind, , j, totyind] = grid_yeardata_lt[v, freshmarsh_ind, , j, totyind] + temp
							grid_yeardata_lt[v, cultivated_ind, , j, totyind] = grid_yeardata_lt[v, cultivated_ind, , j, totyind] + temp
							grid_yeardata_lt[v, developed_ind, , j, totyind] = grid_yeardata_lt[v, developed_ind, , j, totyind] + temp
						} # end if l == 0
					} else {
						cat("Error: variable not supported\n")
						stop()
					} # end if else valid variable
		
				} # end for l loop over levels (pfts)
				
				# finish the npp area-weighted pft average for each land type with veg accumulation and npp scaling
				if (var_names[v] == "NPP") {
					temp_pftwt_sum[which(temp_pftwt_sum == 0.0)] = NA
					grid_temp_lt[forest_ind, ] = grid_temp_lt[forest_ind, ] / temp_pftwt_sum[forest_ind, ]
					grid_temp_lt[woodland_ind, ] = grid_temp_lt[woodland_ind, ] / temp_pftwt_sum[woodland_ind, ]
					grid_temp_lt[savanna_ind, ] = grid_temp_lt[savanna_ind, ] / temp_pftwt_sum[savanna_ind, ]
					grid_temp_lt[developed_ind, ] = grid_temp_lt[developed_ind, ] / temp_pftwt_sum[developed_ind, ]
					grid_temp_lt[shrub_ind, ] = grid_temp_lt[shrub_ind, ] / temp_pftwt_sum[shrub_ind, ]

					# add this month's npp to the annual sum
					grid_temp_lt[which(is.na(grid_temp_lt))] = 0.0
					grid_yeardata_lt[v, forest_ind, , j, totyind] = grid_yeardata_lt[v, forest_ind, , j, totyind] + 
						grid_temp_lt[forest_ind, ]
					grid_yeardata_lt[v, woodland_ind, , j, totyind] = grid_yeardata_lt[v, woodland_ind, , j, totyind] + 
						grid_temp_lt[woodland_ind, ]
					grid_yeardata_lt[v, savanna_ind, , j, totyind] = grid_yeardata_lt[v, savanna_ind, , j, totyind] + 
						grid_temp_lt[savanna_ind, ]
					grid_yeardata_lt[v, developed_ind, , j, totyind] = grid_yeardata_lt[v, developed_ind, , j, totyind] + 
						grid_temp_lt[developed_ind, ]
					grid_yeardata_lt[v, shrub_ind, , j, totyind] = grid_yeardata_lt[v, shrub_ind, , j, totyind] + 
						grid_temp_lt[shrub_ind, ]
				} # end if npp averaging across pfts
				
			} # end j loop over lat indices

		} # for v loop over variables for reading and subsetting and filling arrays
		nc_close(nc_id)
		
	} # end m loop over months

	
	#### NPP
	
	# fill missing monthly values
	# set annual npp values that are < 0 to zero
	#  this is so that the scalars are always appropriate for the positive caland baseline veg c accum values
	#  there are only a few year pixels where this happens, and it reflects where autotrophic respiration exceeds assimilation
	
	# need to fill missing npp months with the average of the valid months
	miss_inds = which(grid_yearmonths_lt[npp_ind, , , , totyind] < 12 & grid_yearmonths_lt[npp_ind, , , , totyind] > 0)
	zero_inds = which(grid_yearmonths_lt[npp_ind, , , , totyind] == 0)
	if (length(zero_inds) > 0) {
		grid_yeardata_lt[npp_ind, , , , totyind][zero_inds] = NA
	}
	if (length(miss_inds) > 0) {
		grid_yeardata_avg = grid_yeardata_lt[npp_ind, , , , totyind][miss_inds] / grid_yearmonths_lt[npp_ind, , , , totyind][miss_inds]
		grid_yeardata_lt[npp_ind, , , , totyind][miss_inds] = grid_yeardata_lt[npp_ind, , , , totyind][miss_inds] + 
			grid_yeardata_avg * (12 - grid_yearmonths_lt[npp_ind, , , , totyind][miss_inds])
	}
	
	neg_inds = which(grid_yeardata_lt[npp_ind, , , , totyind] < 0)
	grid_yeardata_lt[npp_ind, , , , totyind][neg_inds] = 0
	
	# add annual npp to the running average window years
	for (ra_ind in (totyind - npp_window_half):(totyind + npp_window_half)) {
		if ((ra_ind >= start_year_npp_runavg_ind & ra_ind <= end_year_npp_runavg_ind) | ra_ind == base_scalar_year_ind) {
			valid_inds = which(!is.na(grid_yeardata_lt[npp_ind, , , , totyind]))
			grid_runavg[npp_ind, , , , ra_ind][valid_inds] = grid_runavg[npp_ind, , , , ra_ind][valid_inds] + grid_yeardata_lt[npp_ind, , , , totyind][valid_inds]
			grid_runavg_count[npp_ind, , , , ra_ind][valid_inds] = grid_runavg_count[npp_ind, , , , ra_ind][valid_inds] + 1
		} else {
			if (ra_ind > 0 & ra_ind <= tot_years) {
				grid_runavg[npp_ind, , , , ra_ind] = NA
			}
		}
	} # end for ra_ind loop to sum for running average
	
	# calc the npp annual average over the window when the sum has completed, and the scalar
	if (totyind >= (start_year_npp_runavg_ind + npp_window_half) | totyind == (base_scalar_year_ind + npp_window_half)) {
		ra_ind = totyind - npp_window_half
		zero_inds = which(grid_runavg_count[npp_ind, , , , ra_ind] == 0)
		grid_runavg[npp_ind, , , , ra_ind] = grid_runavg[npp_ind, , , , ra_ind] / grid_runavg_count[npp_ind, , , , ra_ind]
		if (length(zero_inds) > 0) {
			grid_runavg[npp_ind, , , , ra_ind][zero_inds] = NA
		}
		zero_inds = which(grid_runavg[npp_ind, , , , base_scalar_year_ind] == 0)
		grid_scalar[npp_ind, , , , ra_ind] = grid_runavg[npp_ind, , , , ra_ind] / grid_runavg[npp_ind, , , , base_scalar_year_ind]
		grid_scalar[npp_ind, , , , ra_ind][zero_inds] = NA
		# filter the outiers by median absolute deviation (used by iESM, see bond-lamberty et al 2014)
		# based on: Davies, P.L. and Gather, U. (1993), "The identification of multiple outliers", J. Amer. Statist. Assoc., 88:782-801.
		# the default multiplier for mad() is 1.4826, which would give 1 std dev if the distribution were normal
		if(FILTER) {
			# forest, woodland, savanna, and developed
			hilim = median(grid_scalar[npp_ind, forest_ind, , , ra_ind], na.rm = TRUE) + numstddev_npp * mad(grid_scalar[npp_ind, forest_ind, , , ra_ind], na.rm = TRUE)
			lolim = median(grid_scalar[npp_ind, forest_ind, , , ra_ind], na.rm = TRUE) - numstddev_npp * mad(grid_scalar[npp_ind, forest_ind, , , ra_ind], na.rm = TRUE)
			filtinds = which(grid_scalar[npp_ind, forest_ind, , , ra_ind] < lolim | grid_scalar[npp_ind, forest_ind, , , ra_ind] > hilim)
			grid_scalar[npp_ind, forest_ind, , , ra_ind][filtinds] = 1
			grid_scalar[npp_ind, woodland_ind, , , ra_ind][filtinds] = 1
			grid_scalar[npp_ind, savanna_ind, , , ra_ind][filtinds] = 1
			grid_scalar[npp_ind, developed_ind, , , ra_ind][filtinds] = 1
			# shrub
			hilim = median(grid_scalar[npp_ind, shrub_ind, , , ra_ind], na.rm = TRUE) + numstddev_npp * mad(grid_scalar[npp_ind, shrub_ind, , , ra_ind], na.rm = TRUE)
			lolim = median(grid_scalar[npp_ind, shrub_ind, , , ra_ind], na.rm = TRUE) - numstddev_npp * mad(grid_scalar[npp_ind, shrub_ind, , , ra_ind], na.rm = TRUE)
			filtinds = which(grid_scalar[npp_ind, shrub_ind, , , ra_ind] < lolim | grid_scalar[npp_ind, shrub_ind, , , ra_ind] > hilim)
			grid_scalar[npp_ind, shrub_ind, , , ra_ind][filtinds] = 1
		} # end if filter
	} # end if calculate the running averages and the scalars for npp
	
	
	### Soil C
	
	# retain negative soil c values, but need to compare scalar signs with caland baseline soil c accum signs, and adjust
	# this adjustment needs to be done after the scalars are disaggregated to the land types
	
	# average the soil c values over the twelve months (or less if there are no data for some months)
	zero_inds = which(grid_yearmonths_lt[soilc_ind, , , , totyind] == 0)
	grid_yeardata_lt[soilc_ind, , , , totyind] = grid_yeardata_lt[soilc_ind, , , , totyind] / grid_yearmonths_lt[soilc_ind, , , , totyind]
	if (length(zero_inds) > 0) {
		grid_yeardata_lt[soilc_ind, , , , totyind][zero_inds] = NA
	}
	
	# calculate annual soil c difference and add to the running averaging window years
	# store the difference in the previous year so that the stored running average values and years are consistent with npp above
	# e.g., if the stored 2007 running average uses 2005 to 2009 values, y needs to be 2010 to get the 2009 value
	if (totyind != 1 & totyind != (num_base_years + 1)) {
		grid_yeardiff[soilc_ind, , , , totyind - 1] = grid_yeardata_lt[soilc_ind, , , , totyind] - grid_yeardata_lt[soilc_ind, , , , totyind - 1]
	
		for (ra_ind in (totyind - 1 - soilc_window_half):(totyind - 1 + soilc_window_half)) {
			if ((ra_ind >= start_year_soilc_runavg_ind & ra_ind <= end_year_soilc_runavg_ind) | ra_ind == base_scalar_year_ind) {
				valid_inds = which(!is.na(grid_yeardiff[soilc_ind, , , , totyind - 1]))
				grid_runavg[soilc_ind, , , , ra_ind][valid_inds] = grid_runavg[soilc_ind, , , , ra_ind][valid_inds] + grid_yeardiff[soilc_ind, , , , totyind - 1][valid_inds]
				grid_runavg_count[soilc_ind, , , , ra_ind][valid_inds] = grid_runavg_count[soilc_ind, , , , ra_ind][valid_inds] + 1
			} else {
				if (ra_ind > 0 & ra_ind <= tot_years) {
					grid_runavg[soilc_ind, , , , ra_ind] = NA
				}
			}
		} # end for ra_inds loop to sum for running average
		
	} else { # end if not first year of processing
			grid_runavg[soilc_ind, , , , totyind] = NA
	}
	
	# calc the soilc annual average over the window when the sum has completed, and the scalar
	# recall that y is one year ahead of the final year of an averaging window
	# there will always be one less soil c scalar year at the end of the period
	if ((totyind - 1) >= (start_year_soilc_runavg_ind + soilc_window_half) | (totyind - 1) == (base_scalar_year_ind + soilc_window_half)) {
		ra_ind = totyind - 1 - soilc_window_half
		zero_inds = which(grid_runavg_count[soilc_ind, , , , ra_ind] == 0)
		grid_runavg[soilc_ind, , , , ra_ind] = grid_runavg[soilc_ind, , , , ra_ind] / grid_runavg_count[soilc_ind, , , , ra_ind]
		if (length(zero_inds) > 0) {
			grid_runavg[soilc_ind, , , , ra_ind][zero_inds] = NA
		}
		zero_inds = which(grid_runavg[soilc_ind, , , , base_scalar_year_ind] == 0)
		grid_scalar[soilc_ind, , , , ra_ind] = grid_runavg[soilc_ind, , , , ra_ind] / grid_runavg[soilc_ind, , , , base_scalar_year_ind]
		grid_scalar[soilc_ind, , , , ra_ind][zero_inds] = NA
		# filter the outiers by median absolute deviation (used by iESM, see bond-lamberty et al 2014)
		# based on: Davies, P.L. and Gather, U. (1993), "The identification of multiple outliers", J. Amer. Statist. Assoc., 88:782-801.
		# the default multiplier for mad() is 1.4826, which would give 1 std dev if the distribution were normal
		# need to filter positive and negative scalars separately
		if(FILTER) {
			# all land types except barren, water, ice, sparse; use forest to get the values
			# positve, then negative values
			# positive
			pos_inds = which(grid_scalar[soilc_ind, forest_ind, , , ra_ind] >= 0)
			hilim = median(grid_scalar[soilc_ind, forest_ind, , , ra_ind][pos_inds], na.rm = TRUE) + numstddev_soilc * mad(grid_scalar[soilc_ind, forest_ind, , , ra_ind][pos_inds], na.rm = TRUE)
			lolim = median(grid_scalar[soilc_ind, forest_ind, , , ra_ind][pos_inds], na.rm = TRUE) - numstddev_soilc * mad(grid_scalar[soilc_ind, forest_ind, , , ra_ind][pos_inds], na.rm = TRUE)
			filtinds = which(grid_scalar[soilc_ind, forest_ind, , , ra_ind][pos_inds] < lolim | grid_scalar[soilc_ind, forest_ind, , , ra_ind][pos_inds] > hilim)
			for (i in desert_ind:developed_ind) {
				grid_scalar[soilc_ind, i, , , ra_ind][pos_inds][filtinds] = 1
			}
			# negative
			neg_inds = which(grid_scalar[soilc_ind, forest_ind, , , ra_ind] < 0)
			hilim = median(grid_scalar[soilc_ind, forest_ind, , , ra_ind][neg_inds], na.rm = TRUE) + numstddev_soilc * mad(grid_scalar[soilc_ind, forest_ind, , , ra_ind][neg_inds], na.rm = TRUE)
			lolim = median(grid_scalar[soilc_ind, forest_ind, , , ra_ind][neg_inds], na.rm = TRUE) - numstddev_soilc * mad(grid_scalar[soilc_ind, forest_ind, , , ra_ind][neg_inds], na.rm = TRUE)
			filtinds = which(grid_scalar[soilc_ind, forest_ind, , , ra_ind][neg_inds] < lolim | grid_scalar[soilc_ind, forest_ind, , , ra_ind][neg_inds] > hilim)
			for (i in desert_ind:developed_ind) {
				grid_scalar[soilc_ind, i, , , ra_ind][neg_inds][filtinds] = 1
			}
		} # end if filter
	} # end if calculate the running averages and the scalars for soilc
	
} # end y loop over years for scalar calculation

cat("End processing land type scalars on input grid", date(), "\n")
	
######### disaggregate scalars to the caland land categories

# to speed up processing:
# process only forest and shrub for npp, then copy to other land types as needed
# process only forest for soilc, then copy to ther land types
# this is because the data are duplicated above to fill out all the types
# and using the land category raster averages these data appropriately to the different land types

## use the land categories to get the proper area weighted average per land type

# read in the caland land category file and its ancillary text file
caland_rast = raster(caland_lc_rast_fname)
caland_df = read.csv(caland_lc_text_fname, stringsAsFactors = FALSE)
num_lc = length(caland_df[,1])
scalar_out = array(dim = c(num_vars, num_lt, tot_years, num_lc))
scalar_out[] = 1

df_out = NULL
# loop over the variables
for (v in vinds) {

	# loop over the caland land types
	for (l in 1:num_lt) {
		
		# loop over years again, to keep this processing separate in case it takes a long time
		totyind = 0
		for (y in years) {
			totyind = totyind + 1

			# limit this to valid years, land types, variables
			# years: only those that are within the calculation window, which can vary by variable
			# note that soil c will have one less scalar year at the end of the period
			# land types: only those that have been calculated, which can vary by variable
			# soil is not calculated for barren, water, ice, and sparse, and there is only one unique set of data for all the types
			# vegetation is calculated only for forest, savanna, woodland, and developed (which are the same), and shrub

			if ( 
				( v == npp_ind & ((totyind >= start_year_npp_runavg_ind & totyind <= end_year_npp_runavg_ind) | totyind == base_scalar_year_ind) &
					((l >= savanna_ind & l <= forest_ind) | l == developed_ind | l == shrub_ind) ) |
				( v == soilc_ind & ((totyind >= start_year_soilc_runavg_ind & totyind <= end_year_soilc_runavg_ind) | totyind == base_scalar_year_ind) &
					(l >= desert_ind & l <= developed_ind) ) 
				) {

				cat("start raster processing year", y, "; l", l, lt[l], "; v", v, comp_names[v], date(), "\n")

				# npp: do this only once for trees and once for shrub
				# soilc: do this only once for all land types
				if (l == forest_ind | (l == shrub_ind & v == npp_ind)) {

					# transform the land type scalar matrices to rasters
					scalar_rast = raster(grid_scalar[v, l, , , totyind])
					scalar_rast = flip(t(scalar_rast), direction="y")
					projection(scalar_rast)<-"+proj=longlat +datum=WGS84"
					scalar_rast <-setExtent(scalar_rast,ext=extent(min_lon_center - lon_res/2.0 - 360, max_lon_center + lon_res/2.0 - 360, min_lat_center - lat_res/2.0, max_lat_center + lat_res/2.0))
	
					# project the land type scalars to the caland grid and resample to caland resolution
					scalar_proj <- projectRaster(scalar_rast, crs= proj4_string, method="ngb")
					# resample takes about 2 h 10 min (less on constance)
					scalar_resamp <- resample(scalar_proj, caland_rast, method="ngb")
	
					# calculate the average scalars; zonal takes about 1 h 15 min (less on constance)
					scalar_lc = zonal(scalar_resamp, caland_rast, fun = "mean", digits = 4, na.rm = TRUE)
	
					# also disaggregate the soil grid_runavg for the base year to deal with negative values
					if (v == soilc_ind & totyind == base_scalar_year_ind & l == forest_ind) {
						# transform the land type scalar matrices to rasters
						runavg_rast = raster(grid_runavg[v, l, , , totyind])
						runavg_rast = flip(t(runavg_rast), direction="y")
						projection(runavg_rast)<-"+proj=longlat +datum=WGS84"
						runavg_rast <-setExtent(runavg_rast,ext=extent(min_lon_center - lon_res/2.0 - 360, max_lon_center + lon_res/2.0 - 360, min_lat_center - lat_res/2.0, max_lat_center + lat_res/2.0))
	
						# project the land type scalars to the caland grid and resample to caland resolution
						runavg_proj <- projectRaster(runavg_rast, crs= proj4_string, method="ngb")
						# resample takes about 2 h 10 min (less on constance)
						runavg_resamp <- resample(runavg_proj, caland_rast, method="ngb")
	
						# calculate the average soil c differences; zonal takes about 1 h 15 min (less on constance)
						base_runavg_lc = zonal(runavg_resamp, caland_rast, fun = "mean", digits = 4, na.rm = TRUE)
					} # end if soilc and forest and base year ind for getting the disaggregated running average
	
					# extract the land types
				
					if (l == forest_ind) {
						if (v == npp_ind) {
							for (dl in c(savanna_ind, woodland_ind, forest_ind, developed_ind)) {
								# subset the appropriate land type
								lt_inds = which((scalar_lc[,1] %/% 100) %% 1000 == lt_codes[dl])
								lc_ids = scalar_lc[,1][lt_inds]
								lc_vals = scalar_lc[,2][lt_inds]
								lc_vals[which(is.nan(lc_vals))] = 1
	
								# store each year of scalars for this land type and variable
								lc_inds = match(lc_ids, caland_df$Land_Cat_ID)
								scalar_out[v, dl, totyind, lc_inds] = lc_vals
							}
						} # end if duplicate forest npp
						
						if (v == soilc_ind) {
							for (dl in desert_ind:developed_ind) {
								# subset the appropriate land type
								lt_inds = which((scalar_lc[,1] %/% 100) %% 1000 == lt_codes[dl])
								lc_ids = scalar_lc[,1][lt_inds]
								lc_vals = scalar_lc[,2][lt_inds]
								
								lc_vals[which(is.nan(lc_vals))] = 1
								
								# check whether the value is the same sign as the caland land type base accum
								# if the base runavg is the same sign as the initial caland values, then apply scalar as is, even if negative
								# if the base runavg is different sign than initial, then calc new scalar = 2 - orig scalar
								#  this applies the scaled magnitude change appropriately to the caland base value
								lc_base_ra = base_runavg_lc[,2][lt_inds]
								if (dl %in% negative_caland_soilc_accum_inds) {
									# negative caland base values
									adj_inds = which(lc_base_ra > 0 & lc_vals != 1)
									lc_vals[adj_inds] = 2 - lc_vals[adj_inds]
								} else {
									# positive caland base values
									adj_inds = which(lc_base_ra < 0 & lc_vals != 1)
									lc_vals[adj_inds] = 2 - lc_vals[adj_inds]
								}
	
								# store each year of scalars for this land type and variable
								lc_inds = match(lc_ids, caland_df$Land_Cat_ID)
								scalar_out[v, dl, totyind, lc_inds] = lc_vals
							}
						} # end if duplicate forest soilc
						
						
					} # end if duplicate forest
				
					if (l == shrub_ind & v == npp_ind) {
						# subset the appropriate land type
						lt_inds = which((scalar_lc[,1] %/% 100) %% 1000 == lt_codes[l])
						lc_ids = scalar_lc[,1][lt_inds]
						lc_vals = scalar_lc[,2][lt_inds]
						lc_vals[which(is.nan(lc_vals))] = 1
	
						# store each year of scalars for this land type and variable
						lc_inds = match(lc_ids, caland_df$Land_Cat_ID)
						scalar_out[v, l, totyind, lc_inds] = lc_vals
					} # end if shrub npp
				
				} # end if forest or npp shrub to process rasters
				
				cat("end raster processing year", y, "; l", l, lt[l], "; v", v, comp_names[v], date(), "\n")
			} else { # end if valid scalar values
				scalar_out[v, l, totyind, ] = 1
			}
	
			# remove temporary files each year
			removeTmpFiles(h=0)
			
		} # end for y loop over years for disaggregating to caland land categories
	
		# build the output data frame here
		# but the data may not be available yet
		# so build these only when available: forest(npp and soil) and shrub (npp)
		# output only the data year range; do this full range to ensure that the columns are the same
		
		if (l == forest_ind) {
			if (v == npp_ind) {
				for (dl in c(savanna_ind, woodland_ind, forest_ind, developed_ind)) {
					# create the data frame for this land type and variable
					df_curr = caland_df[caland_df$Land_Type == lt[dl], c("Region", "Land_Type", "Ownership")]
					df_curr$Component = comp_names[v]
					for (y_ind in (num_base_years + 1):tot_years) {
						df_curr = data.frame(df_curr, scalar_out[v, dl, y_ind, which(caland_df$Land_Type == lt[dl])])
						colnames(df_curr)[ncol(df_curr)] = paste0("X", years[y_ind])
					}	
		
					# add this current data frame to the whole output data frame
					df_out = rbind(df_out, df_curr)
				}
			} # end if forest npp index
			
			if (v == soilc_ind) {
				for (dl in desert_ind:developed_ind) {
					# create the data frame for this land type and variable
					df_curr = caland_df[caland_df$Land_Type == lt[dl], c("Region", "Land_Type", "Ownership")]
					df_curr$Component = comp_names[v]
					for (y_ind in (num_base_years + 1):tot_years) {
						df_curr = data.frame(df_curr, scalar_out[v, dl, y_ind, which(caland_df$Land_Type == lt[dl])])
						colnames(df_curr)[ncol(df_curr)] = paste0("X", years[y_ind])
					}
		
					# add this current data frame to the whole output data frame
					df_out = rbind(df_out, df_curr)
				}
			} # end if forest soilc index
		} # end if forest index
		
		if (l == shrub_ind & v == npp_ind) {
			# create the data frame for this land type and variable
			df_curr = caland_df[caland_df$Land_Type == lt[l], c("Region", "Land_Type", "Ownership")]
			df_curr$Component = comp_names[v]
			for (y_ind in (num_base_years + 1):tot_years) {
				df_curr = data.frame(df_curr, scalar_out[v, l, y_ind, which(caland_df$Land_Type == lt[l])])
				colnames(df_curr)[ncol(df_curr)] = paste0("X", years[y_ind])
			}	
	
			# add this current data frame to the whole output data frame
			df_out = rbind(df_out, df_curr)
		} # end if shrub npp index
		
	} # end l loop over caland land types disaggregating to caland land categories# end v loop over the variables for disaggregating to caland land categories

} # end v loop over the variables for disaggregating to caland land categories
	
# now write the output table	
write.csv(df_out, paste0(out_name), row.names = FALSE)

cat("finish proc_iesm_climate.r", date(), "\n")