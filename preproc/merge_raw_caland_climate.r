# merge_raw_caland_climate.r

# merge several raw climate scalar input files for caland into one single file for reading by write_caland_inputs.r
# these files may have different years that need to be merged
# these files may have different records to be merged

# this is a function that takes a base name to get a list of files to merge

# NOTE: fresh marsh does not exist in the current land category files, so the scalars from proc_iesm_climate.r are 1
#  since fresh marsh comes out of cultivated, substitute the cultivated values for fresh marsh here
# Delta fresh marsh ownerships are included (with zero area) based on the Delta cultivated ownerships in calandv2_2010_landcat_area_sqm.csv,
#  which defines all the available land categories and their areas, and is used in proc_iesm_climate.r
#  so the assignment below should substitute the values correctly
#  note that only soil values exist for fresh marsh

# UPDATED 18 aug 2020:
# fix the delta cultivated scalars because proc_iesm_climate.r does not know that the soil accum values are negatvie!
# the fix is to cacluate new = 2 - input value
# since delta fresh marsh has positive soil accum values, the assignment is made the same as above

# input arguments for running without calling the function
#basename = "climate_c_scalars_iesm_rcp85_"
basename = "climate_c_scalars_iesm_rcp45_"
#basedir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp85/"
basedir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp45/"
start_year = 2010
end_year = 2086
#outname = "climate_c_scalars_iesm_rcp85.csv"
outname = "climate_c_scalars_iesm_rcp45.csv"
#outdir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp85/"
outdir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp45/"

# some info about the files
yrcol_st = 5

merge_raw_caland_climate <- function(basename = "climate_c_scalars_iesm_rcp85_", basedir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp85/", start_year = 2010, end_year = 2086, outname = "climate_c_scalars_iesm_rcp85.csv", outdir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/gis_data/iesm_climate/rcp85/") {
	
	cat("start merge_raw_caland_climate()", date(), "\n")
	
	fulloutname = paste0(outdir, outname)
	csv_tag = ".csv"
	
	# files to merge
	# they should all have the same base name
	files = list.files(basedir, pattern = paste0("^", basename, "(.*)csv$"), full.names = TRUE)
	
	# set up the output data frame
	years_out = c(start_year:end_year)
	df_out = data.frame(Region = NA, Land_Type = NA, Ownership = NA, Component = NA)
	for (y in years_out) {
		df_out = data.frame(df_out, NA)
		colnames(df_out)[ncol(df_out)] = paste0("X",y)
	}
	
	for (fn in files) {
		# data from current file
		df_curr = read.csv(fn, stringsAsFactors=FALSE)
		years_curr = as.integer(substr(names(df_curr)[yrcol_st:ncol(df_curr)], 2, 5))
		num_rec = length(df_curr[,1])
		
		# temporary df to fill with current data
		df_temp = df_curr[,1:(yrcol_st-1)]
		for (y in years_out) {
			# match the out year with a year in the current file
			curr_yind = match(y, years_curr)
			if( is.na(curr_yind) ) {
				df_temp = data.frame(df_temp, rep(1,num_rec))
			} else {
				df_temp = data.frame(df_temp, df_curr[,curr_yind + yrcol_st - 1])
			}

			colnames(df_temp)[ncol(df_temp)] = paste0("X",y)
		} # end y loop over the out years
		
		# if these land category - component records do not exist, bind these to the output data frame
		# otherwise need to replace the year columns as appropriate; meaning that only values of 1 are replaced
		
		# use loops because there isn't a clean way to get the row indices
		rep_inds = NULL
		for (t in 1:num_rec) {
			m_ind = which(df_out$Region == df_temp$Region[t] & df_out$Land_Type == df_temp$Land_Type[t] & df_out$Ownership == df_temp$Ownership[t] & df_out$Component == df_temp$Component[t])
			# keep track of df_out replacement inds
			rep_inds = c(rep_inds, m_ind)
			
			# m_ind should be of length 1 because each record should be unique for these factors; or zero if the record is not in df_out
			if (length(m_ind) == 1) {
				# replace this record's values if they are 1 in df_out
				one_inds = which(df_out[m_ind, yrcol_st:ncol(df_out)] == 1)
				df_out[m_ind, yrcol_st:ncol(df_out)][one_inds] = df_temp[t, yrcol_st:ncol(df_temp)][one_inds]
			} else if (length(m_ind) == 0) {
				# don't do anything because this record does not exist in df_out
			} else {
				cat("Error in m_ind; length =", length(m_ind), "is not zero or one\n")
			}
		} # end for t over records in current file
		
		if (length(rep_inds) == 0) { # bind all records
			df_out = rbind(df_out, df_temp)
		} else if (length(rep_inds) != num_rec) { # bind some records
			df_out = rbind(df_out, df_temp[-rep_inds,])
		}
					
	} # end for fn loop over files
	
	# remove the initial dummy record and reorder
	df_out = df_out[!is.na(df_out$Region),]
	df_out = df_out[order(df_out$Region, df_out$Land_Type, df_out$Ownership, df_out$Component),]
	
	# now substite the cultivated values for fresh marsh
	df_out[df_out$Region == "Delta" & df_out$Land_Type == "Fresh_marsh" & df_out$Component == "Soil", yrcol_st:ncol(df_out)] =
		df_out[df_out$Region == "Delta" & df_out$Land_Type == "Cultivated" & df_out$Component == "Soil", yrcol_st:ncol(df_out)]
	
	# now fix the delta cultivated scalars
	df_out[df_out$Region == "Delta" & df_out$Land_Type == "Cultivated" & df_out$Component == "Soil", yrcol_st:ncol(df_out)] =
		2 - df_out[df_out$Region == "Delta" & df_out$Land_Type == "Cultivated" & df_out$Component == "Soil", yrcol_st:ncol(df_out)]
	
	write.csv(df_out, fulloutname, row.names = FALSE)
	
	cat("end merge_raw_caland_climate()", date(), "\n")
}