# proc_ca_spatial.r
# process CA spatial data to generate useful tables

# also produce an average carbon density map delineated by the land types

# unless specified, data are in CA Teale Albers equal area projection, NAD83 datum (GRS80)
#	lat1 = 34, lat2 = 40.5, lon = -120, FN = -4000000, FE = 0, meters
# all other data have been converted and saved to this projection

# vector data had to be cleaned using GRASS GIS
#	so the needed layers have been saved as shapefiles after conversion/projection/cleaning
#	and all the pre-processing has been done in grass, so only the aggregate ownership files are read in
#	the rest of the files are still included below to indicate which data went into the processing
#	the area field didn't get written properly, so recalculate it here
#	this has been done for:
#		carb data, landfire data, usfs boundary data, frap data, ssurgo data*, cced data

# using the calfire ownership data instead of cpad because the categories are more useful

# calfire frap county data for the state and county boundaries
#	will include the mainland and the channel islands in the analysis

# do not use the landfire data
# use the landfire evt data
#	the two downlaoded ca files have been mreged and downlaoded and written to ESRI .bil format
# the landfire evt classification table has been converted to a .csv file for use here
#	this needs to be cross-walked to generic and relevant types
#	the cover data would be useful for this

# use the ca arc land cover and carbon data
# but use the biomass table (t biomsss per ha) instead of given rasters (which are in t c per ha)
#	converison is biomass*0.47=carbon
# reprojection the given rasters with bilinear interp causes problems, so try nearest neighbor
#	the ag and urban categories have been determined already for these data
#		most ruderal is ag, but some fall into urban
#		these are not consistent with the landfire descriptions, but may match the field better
#	the ag data seem ok, but the urban data are not applied completely or correctly
#	so only use the ag data, which are above ground only, and update the tabel and generate the raster
#		update the grassland ag data to the hay value also
#		some of the other non-ag types that are classified as ag have seemingly too high carbon values
#	update the urban output data table later

# use the bjorkman et al 2015 frap report and data
# discussed this with James Thorne
# these are above ground forest carbon densities for all area; other veg is not necessarily included
# state level aggregation is problematic due to variability
# use state level for now, applied to all urban area (veg and built)
# maybe update spatially later if there is time

# write out the updated biomass table with the aggregated evts also

# have created a corresponding aggregate evt classification .csv file

# the rasters created in this script are ESRI .bil format so that the binary files are available and easily readable by GIS software
#	note that the corner pixel coordinates in the .hdr file are for the center of the pixel

# the polyogns from the gridded gssurga ca geodatabase have been reprojected and saved as a shapefile
#	these just need to be linked to the valu1 table to get the soil carbon density values

######## may need to reproject and crop and save all the data (usfs, soil, nwps), then read it here!!!!!!!!!

library(rgdal)
library(raster)
library(rasterVis)
library(rgeos)
library(maptools)

cat("started proc_ca_spatial.r at", date())

# set this to TRUE to do state level, FALSE to do county level
DOSTATE = TRUE

# set these to TRUE to create the necessary rasters
# set to FALSE if the rasters already exist

# which lfc year; TRUE=2010, FALSE=2001
LFC2010 = TRUE

# these are no longer needed
NEED_CARB_EVT_RAST_2010 = FALSE
NEED_CARB_EVT_RAST_2001 = FALSE
# this will not work with the current script
NEED_LANDFIRE_EVT_RAST = FALSE		# have created this already; 2010 only; old aggregation classes

# generate aggregated evt data and add to raster attribute table
# this should always be true because is needed for carb data takes only a minute or two
AGG_EVT_RAST = TRUE

# USFS forest carbon rasters
NEED_USFS_C_RAST = FALSE

# projection: ca teale albers
PROJ4_ca_teale = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# projection: us albers
PROJ4_us_albers = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# projection: nad83 geographic
PROJ4_gcs_nad83 = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

# each cell is 30x30m, so 900 sq m
cell_area = 900
msq2ha = 1.0 / 10000

# additional unit conversion for ssurgo data
g2Mg = 1.0 / 1000000
gpmsq2Mgpha = g2Mg / msq2ha

acre2hectare = 0.404686

# maybe not needed?
# ca raster extent object (for 30m res)
#ca_extent = extent(-374000, 540100, -604500, 450030)
#ca_nrows = 35151
#ca_ncols = 30470
#raster_template = raster(x = ca_extent, ncols = ca_ncols, nrows = ca_nrows, ext = ca_extent, crs = PROJ4_ca_teale)

# cnra project directory
cnra_dir = "/Volumes/nuttallii/Users/adivi/projects/cnra_carbon/"
cnra_gis_dir = paste0(cnra_dir, "gis_data/")

# actually reading in the pre-processed spatial data

# aggregate ownership at state level
own_agg_state_dir = paste0(cnra_gis_dir, "state_own_agg/")
own_agg_state_shp = "state_own_agg"

# aggregate ownership at county level
own_agg_county_dir = paste0(cnra_gis_dir, "county_own_agg/")
own_agg_county_shp = "county_own_agg"

# right now use the carb land cover cata
if(TRUE) {

	# CA ARB carbon and land use/cover data have been extracted from the file geodatabase (LFC_LBNL.gdb)
	#	also from access database (ARB_C_LUT_v2.7.accdb)
	#	table is vegetation biomass density (MgC per ha) - several categories
	#	land cover (from landfire)
	#	these are in us albers projection
	
	carbon2biomass = 1.0 / 0.47
	biomass2carbon = 0.47
	
	lfc_dir = "/Volumes/glabrata/geodata/carb_carbon/carb_extracted/"
	lfc_use_dir = paste0(cnra_gis_dir, "carb/")
	lfc_carbon_csv = paste0(lfc_dir, "BATTLES_Biomass-LUT_01-08-10_20151029.csv")
	evt_agg_table = paste0(lfc_use_dir, "LFc_evt_aggregate_evt_evtname.csv")  	# same aggregate table for both years
	ag_lut_csv = paste0(lfc_dir, "GUNN_AG_LUTv4.csv")
	if(LFC2010) {
		lfc_bil = paste0(lfc_dir, "LFc_2010.bil")
		lfc_class_csv = paste0(lfc_dir, "LFc_2010.csv")
		lfc_agl_bil = paste0(lfc_dir, "LFc_2010_AGLwild.bil")
		lfc_tot_bil = paste0(lfc_dir, "LFc_2010_Total.bil")
		lfc_flag_bil = paste0(lfc_dir, "LFc_2010_FLAG.bil")
		# processed
		#evt_table_csv = paste0(lfc_use_dir, "LFc_evt2010_evt_evtname.csv")
		evt_agg_bil = paste0(lfc_use_dir, "LFc_evt2010_aggregate.bil")
		agc_bil = paste0(lfc_use_dir, "LFc_agc2010.bil")		# live main above ground; includes ag
		agc_se_bil = paste0(lfc_use_dir, "LFc_agc_se2010.bil")	# no ag or urban
		tc_bil = paste0(lfc_use_dir, "LFc_tc2010.bil")				# total mass biomass; no urban
		tc_se_bil = paste0(lfc_use_dir, "LFc_tc_se2010.bil")			# total mass biomass; no urban no ag
		bgc_bil = paste0(lfc_use_dir, "LFc_bgc2010.bil")	# live main below ground
		bgc_se_bil = paste0(lfc_use_dir, "LFc_bgc_se2010.bil")
		usc_bil = paste0(lfc_use_dir, "LFc_usc2010.bil")	# understory
		usc_se_bil = paste0(lfc_use_dir, "LFc_usc_se2010.bil")
		dsc_bil = paste0(lfc_use_dir, "LFc_dsc2010.bil")	# dead standing
		dsc_se_bil = paste0(lfc_use_dir, "LFc_dsc_se2010.bil")
		ddc_bil = paste0(lfc_use_dir, "LFc_ddc2010.bil")	# dead down
		ddc_se_bil = paste0(lfc_use_dir, "LFc_ddc_se2010.bil")
		ltc_bil = paste0(lfc_use_dir, "LFc_ltc2010.bil")	# litter
		ltc_se_bil = paste0(lfc_use_dir, "LFc_ltc_se2010.bil")
		# these are the processed data - not done because extract does not complete
		all_table_csv = paste0(lfc_use_dir, "all_2010.csv")
		pix_frac_csv = paste0(lfc_use_dir, "pix_frac_2010.csv")
		pix_normfrac_csv = paste0(lfc_use_dir, "pix_normfrac_2010.csv")
		
		# aggregated landfire data are in the ca albers projection
		# the evt_agg .csv files list the original evt classes that have been aggregated to each new class
		# need these for each year cuz 2001 has a couple more classes
		#	all original zero values have been changed to the -9999 nodata value
		evt_agg_nodata_csv = paste0(lfc_use_dir, "nodata_2010.csv")
		evt_agg_water_csv = paste0(lfc_use_dir, "water_2010.csv")
		evt_agg_ice_csv = paste0(lfc_use_dir, "ice_2010.csv")
		evt_agg_barren_csv = paste0(lfc_use_dir, "barren_2010.csv")
		evt_agg_sparse_csv = paste0(lfc_use_dir, "sparse_2010.csv")
		evt_agg_desert_csv = paste0(lfc_use_dir, "desert_2010.csv")
		evt_agg_shrubland_csv = paste0(lfc_use_dir, "shrubland_2010.csv")
		evt_agg_grassland_csv = paste0(lfc_use_dir, "grassland_2010.csv")
		evt_agg_savanna_csv = paste0(lfc_use_dir, "savanna_2010.csv")
		evt_agg_woodland_csv = paste0(lfc_use_dir, "woodland_2010.csv")	
		evt_agg_forest_csv = paste0(lfc_use_dir, "forest_2010.csv")	
		evt_agg_meadow_csv = paste0(lfc_use_dir, "meadow_2010.csv")
		evt_agg_marsh_csv = paste0(lfc_use_dir, "marsh_2010.csv")	
		evt_agg_wetland_csv = paste0(lfc_use_dir, "wetland_2010.csv")
		evt_agg_agriculture_csv = paste0(lfc_use_dir, "agriculture_2010.csv")
		evt_agg_developed_veg_csv = paste0(lfc_use_dir, "developed_veg_2010.csv")
		evt_agg_developed_csv = paste0(lfc_use_dir, "developed_2010.csv")
	}else{
		lfc_bil = paste0(lfc_dir, "LFc_2001.bil")
		lfc_class_csv = paste0(lfc_dir, "LFc_2001.csv")
		lfc_agl_bil = paste0(lfc_dir, "LFc_2001_AGLwild.bil")
		lfc_tot_bil = paste0(lfc_dir, "LFc_2001_Total.bil")
		lfc_flag_bil = paste0(lfc_dir, "LFc_2001_FLAG.bil")
		# processed
		#evt_table_csv = paste0(lfc_use_dir, "LFc_evt2001_evt_evtname.csv")
		evt_agg_bil = paste0(lfc_use_dir, "LFc_evt2001_aggregate.bil")
		agc_bil = paste0(lfc_use_dir, "LFc_agc2001.bil")				# no urban
		agc_se_bil = paste0(lfc_use_dir, "LFc_agc_se2001.bil")		
		tc_bil = paste0(lfc_use_dir, "LFc_tc2001.bil")				# no urban
		tc_se_bil = paste0(lfc_use_dir, "LFc_tc_se2001.bil")			
		# these are the processed data - not done because extract does not complete
		all_table_csv = paste0(lfc_use_dir, "all_2001.csv")
		pix_frac_csv = paste0(lfc_use_dir, "pix_frac_2001.csv")
		pix_normfrac_csv = paste0(lfc_use_dir, "pix_normfrac_2001.csv")
		
		# aggregated landfire data are in the ca albers projection
		# the evt_agg .csv files list the original evt classes that have been aggregated to each new class
		# need these for each year because 2001 has a couple more classes
		#	all original zero values have been changed to the -9999 nodata value
		evt_agg_nodata_csv = paste0(lfc_use_dir, "nodata_2001.csv")
		evt_agg_water_csv = paste0(lfc_use_dir, "water_2001.csv")
		evt_agg_ice_csv = paste0(lfc_use_dir, "ice_2001.csv")
		evt_agg_barren_csv = paste0(lfc_use_dir, "barren_2001.csv")
		evt_agg_sparse_csv = paste0(lfc_use_dir, "sparse_2001.csv")
		evt_agg_desert_csv = paste0(lfc_use_dir, "desert_2001.csv")
		evt_agg_shrubland_csv = paste0(lfc_use_dir, "shrubland_2001.csv")
		evt_agg_grassland_csv = paste0(lfc_use_dir, "grassland_2001.csv")
		evt_agg_savanna_csv = paste0(lfc_use_dir, "savanna_2001.csv")	
		evt_agg_woodland_csv = paste0(lfc_use_dir, "woodland_2001.csv")	
		evt_agg_forest_csv = paste0(lfc_use_dir, "forest_2001.csv")	
		evt_agg_meadow_csv = paste0(lfc_use_dir, "meadow_2001.csv")
		evt_agg_marsh_csv = paste0(lfc_use_dir, "marsh_2001.csv")	
		evt_agg_wetland_csv = paste0(lfc_use_dir, "wetland_2001.csv")
		evt_agg_agriculture_csv = paste0(lfc_use_dir, "agriculture_2001.csv")
		evt_agg_developed_veg_csv = paste0(lfc_use_dir, "developed_veg_2001.csv")
		evt_agg_developed_csv = paste0(lfc_use_dir, "developed_2001.csv")
	}
	
	#### use the 2008 standard evt so that the different years can be compared
	evt_table_csv = paste0(lfc_use_dir, "LFc_evt_2008_std.csv")

} # end TRUE for carb lfc land cover data

# these are the original carb veg biomass data
# the reprojected carb category raster has to be here because it references the biomass data
if(LFC2010) {
	# category raster
	lfc_teale_bil = paste0(lfc_use_dir, "LFc_2010_teale.bil")
	# carbon rasters - these are the only carbon unit data
	lfc_agl_teale_bil = paste0(lfc_use_dir, "LFc_2010_AGLwild_teale.bil")
	lfc_tot_teale_bil = paste0(lfc_use_dir, "LFc_2010_Total_teale.bil")
	lfc_flag_teale_bil = paste0(lfc_use_dir, "LFc_2010_FLAG_teale.bil")
	# all biomass data are in a table that keys to ARB_LF_ID, which is equivalent to 'Value' in the raster
	# extract the appropiate year
	lfc_carbon_df_out = paste0(lfc_use_dir, "biomass_update_2010.csv")
	lfc_carbon_df = read.csv(lfc_carbon_csv)
	lfc_carbon_df = lfc_carbon_df[lfc_carbon_df$Year == 2010,]
	# remove the ID column from this table because it is not needed (and it conflicts later with the rat)
	lfc_carbon_df$ID = NULL
	# add the EVT value to this table
	evt_sub = read.csv(lfc_class_csv)
	evt_sub = evt_sub[,c("ARB_LF_ID","EVT")]
	lfc_carbon_df = merge(lfc_carbon_df, evt_sub, by = "ARB_LF_ID")
}else {
	# category raster
	lfc_teale_bil = paste0(lfc_use_dir, "LFc_2001_teale.bil")
	# carbon rasters - these are the only carbon unit data
	lfc_agl_teale_bil = paste0(lfc_use_dir, "LFc_2001_AGLwild_teale.bil")
	lfc_tot_teale_bil = paste0(lfc_use_dir, "LFc_2001_Total_teale.bil")
	lfc_flag_teale_bil = paste0(lfc_use_dir, "LFc_2001_FLAG_teale.bil")
	# all biomass data are in a table that keys to ARB_LF_ID, which is equivalent to 'Value' in the raster
	# extract the appropiate year
	lfc_carbon_df_out = paste0(lfc_use_dir, "biomass_update_2001.csv")
	lfc_carbon_df = read.csv(lfc_carbon_csv)
	lfc_carbon_df = lfc_carbon_df[lfc_carbon_df$Year == 2001,]
	# remove the ID column from this table because it is not needed (and it conflicts later with the rat)
	lfc_carbon_df$ID = NULL
	# add the EVT value to this table
	evt_sub = read.csv(lfc_class_csv)
	evt_sub = evt_sub[,c("ARB_LF_ID","EVT")]
	lfc_carbon_df = merge(lfc_carbon_df, evt_sub, by = "ARB_LF_ID")
}

# gSSURGO CA - us albers
# cannot read the grid, so read the MUPOLYGON layer, which has been reprojected and saved as a shapefile
# use a mostly cleaned shapefile from GRASS (3m snap), still has 6 overlapping polygons (1696 sq meters), and 2 uncategorized areas (221 sq meters)
# soil carbon density, among other things, is in the valu1 gdb table
# this needs to be rasterized to the arb flc data grid
#	and then ratified to get the soil carbon values
#	and then the carbon values need to be averaged over the arb lfc categories and added to that table
# the desired variable in valu1 is: "soc_0_999"
#	this is g C per sq meter in total soil profile
#	nodata is a NULL value
#
# merge the valu1 table here, then rasterize and process in grass
# check the field names - may need to rewrite as filegdb
#
soil_dir = "/Volumes/glabrata/geodata/ssurgo_ca/"
soil_shape_dir = "/Volumes/glabrata/geodata/ssurgo_ca/gssurgo_ca_polys_grass/"
soil_poly_shp = "gssurgo_ca_polys_grass"
soil_use_dir = paste0(cnra_gis_dir, "gssurgo_ca/")
soil_bil = paste0(soil_use_dir, "gssurgo_ca.bil")
soil_out_dir = paste0(cnra_gis_dir, "gssurgo_ca_polys_valu1/")
soil_out_shp = "gssurgo_ca_polys_valu1"
# valu1 gdb table has been converted already
#soil_valu_gdb = "/Volumes/glabrata/geodata/ssurgo_ca/soils_GSSURGO_ca_3218279_01/valu_fy2016.gdb"
#soil_valu1 = "valu1"
#convert_valu1 = "/Volumes/nuttallii/Library/Frameworks/GDAL.framework/Programs/ogr2ogr -f CSV valu1.csv /Volumes/glabrata/geodata/ssurgo_ca/soils_GSSURGO_ca_3218279_01/valu_fy2016.gdb /Volumes/glabrata/geodata/ssurgo_ca/valu1"
#system(convert_valu1)
valu1_fname = paste0(soil_dir, "valu1.csv")
soc_tot = "soc_0_999"


# right now use the carb land cover cata
# the projections of carb and lanfire cover are not matched!
# so don't use these for carbon!
#	 unless they are resampled to the lfc grid first!
if(FALSE) {
	
# landfire data (us albers projection); arcinfo grid
#	the files in the grid directories need to be stripped of the extra text in front of the generic file names
# these files are merged into one CA data set, reprojected, and written as an ESRI .bil file
#	note that the nodata value for the .bil is 0, because that is how it appeared on reading in the original files
# the CA file is also aggregated to a small set of types, and also written to an ESRI .bil file
# note that there are 0 values also, that correspond to nodata in addition to the defined nodata pixels
landfire_dir = "/Volumes/glabrata/geodata/landfire/"
evtn_grid = paste0(landfire_dir, "lf02126095_US_120EVT/hdr.adf")
evts_grid = paste0(landfire_dir, "lf18675084_US_120EVT/hdr.adf")
evt_bil = paste0(landfire_dir, "evt2010_120.bil")
evt_table_csv = paste0(landfire_dir, "evt2010_120_value_evtname.csv")
evt_table_rdcd_csv = paste0(landfire_dir, "evt2010_120_value_evtname_reduced.csv")

# aggregated landfire data are in the ca albers projection
# the evt_agg .csv files list the original evt classes that have been aggregated to each new class
#	all original zero values have been changed to the -9999 nodata value
evt_agg_bil = paste0(landfire_dir, "evt2010_120_aggregate.bil")
evt_agg_table = paste0(landfire_dir, "evt2010_120_aggregate_value_EVT_Name.csv")
evt_agg_nodata_csv = paste0(landfire_dir, "nodata.csv")
evt_agg_water_csv = paste0(landfire_dir, "water.csv")
evt_agg_ice_csv = paste0(landfire_dir, "ice.csv")
evt_agg_barren_csv = paste0(landfire_dir, "barren.csv")
evt_agg_sparse_csv = paste0(landfire_dir, "sparse.csv")
evt_agg_desert_csv = paste0(landfire_dir, "desert.csv")
evt_agg_shrubland_csv = paste0(landfire_dir, "shrubland.csv")
evt_agg_grassland_csv = paste0(landfire_dir, "grassland.csv")
evt_agg_savanna_csv = paste0(landfire_dir, "savanna.csv")
evt_agg_woodland_csv = paste0(landfire_dir, "woodland.csv")
evt_agg_forest_csv = paste0(landfire_dir, "forest.csv")
evt_agg_meadow_csv = paste0(landfire_dir, "meadow.csv")
evt_agg_marsh_csv = paste0(landfire_dir, "marsh.csv")
evt_agg_wetland_csv = paste0(landfire_dir, "wetland.csv")
evt_agg_agriculture_csv = paste0(landfire_dir, "agriculture.csv")
evt_agg_developed_csv = paste0(landfire_dir, "developed.csv")
}

# frap county/state extent object
#ca_extent_frap = extent(-373987.9, 540082.8, -604495.8, 450023.2)

# frap fire extent object
#fire_extent_frap = extent(-373237.5, 519987.8, -604727.6, 518283.7)

# FRAP data
#	state and country boundaries: state15_1 and cnty15_1_basicplus
#	fire perimeters: fire15_1.gdb
#	ownership: ownership13_2
#		these ownership data cover cpad, in a more usable set of categories
frap_fire_gdb = "/Volumes/glabrata/geodata/frap/fire15_1.gdb"
frap_cnty_dir = "/Volumes/glabrata/geodata/frap/cnty15_1_basicplus/"
frap_state_dir = "/Volumes/glabrata/geodata/frap/state15_1/"
frap_own_dir = "/Volumes/glabrata/geodata/frap/ownership13_2/"
frap_county_shp = "cnty15_1_basicplus"
frap_state_shp = "state15_1"
frap_wildfire = "firep15_1"
frap_rxfire = "rxburn15_1"
frap_own_shp = "ownership13_2"

# National Wilderness Preservation System (WGS84 geographic, all zero parameters)
#	Most national protected areas: Wilderness_Areas_5.24.16.shp
#	this includes usfs wilderness areas and more
#	 not using this becauase only the USFS wilderness boundaries are neeed; the ownership data take care of the rest
nwps_dir = "/Volumes/glabrata/geodata/nwps/Wilderness_Areas/"
nwps_shp = "Wilderness_Areas_5.24.16"

# USFS data (NAD83 geographic)
#	National forest boundaries: S_USA.AdministrativeForest.shp
#	National grassland boundaries: S_USA.NationalGrassland.shp - not using this (the only one overlaps with a national forest)
#	Other National Protected Areas: S_USA.OtherNationalDesignatedArea.shp
#	USFS wilderness: S_USA.Wilderness.shp
usfs_nf_dir = "/Volumes/glabrata/geodata/usfs/S_USA.AdministrativeForest/"
usfs_gr_dir = "/Volumes/glabrata/geodata/usfs/S_USA.NationalGrassland/"
usfs_oa_dir = "/Volumes/glabrata/geodata/usfs/S_USA.OtherNationalDesignatedArea/"
usfs_wild_dir = "/Volumes/glabrata/geodata/usfs/S_USA.Wilderness/"
usfs_nf_shp = "S_USA.AdministrativeForest"
usfs_gr_shp = "S_USA.NationalGrassland"
usfs_oa_shp = "S_USA.OtherNationalDesignatedArea"
usfs_wild_shp = "S_USA.Wilderness"

# CA conservation easement database
#	CCED_2015a.shp
#	from the CA protected areas data portal
#	the other protected areas are covered by the FRAP ownership data and the national protected and wilderness data
# use the dissolved data because there are too many overlapping polgons to fix
cced_dir = "/Volumes/glabrata/geodata/cced/CCED2015a_dissolve/"
cced_shp = "CCED_2015a_dissolve"

# not using the ecoregions, but these are the base ones that frap wants to aggregate
# PSW-R5 ecoregions
#	converted from the .mdb format using arcgis
eco_dir = "/Volumes/glabrata/geodata/ecoregions_usfs_ca/ecoregions_ca/"
eco_shp = "ecoregions_ca_07_3"

# USFS forest gridded carbon density data (US Albers equal area projection, lat1=29.5, lat2 = 45.5, lon = -96, lat0 = 23, no FN or FE, meters)
#	rasters (250m?)
#	2000-2009
# 	erdas imagine files
# these are reprojected and converted to ESRI .bil files
usfs_gc_dir = "/Volumes/glabrata/geodata/usfs/gridded_carbon/"
usfs_totc_img = paste0(usfs_gc_dir, "Data/carbon_tot_mg_ha.img")
usfs_agc_img = paste0(usfs_gc_dir, "Data/carbon_ag_mg_ha.img") # above ground live tree
usfs_bgc_img = paste0(usfs_gc_dir, "Data/carbon_bg_mg_ha.img") # below ground live tree
usfs_ddc_img = paste0(usfs_gc_dir, "Data/carbon_dd_mg_ha.img") # dead down
usfs_ltc_img = paste0(usfs_gc_dir, "Data/carbon_lt_mg_ha.img") # litter
usfs_sdc_img = paste0(usfs_gc_dir, "Data/carbon_sd_mg_ha.img") # standing dead
usfs_soc_img = paste0(usfs_gc_dir, "Data/carbon_so_mg_ha.img") # soil organic
usfs_usc_img = paste0(usfs_gc_dir, "Data/carbon_us_mg_ha.img") # understory
usfs_totc_bil = paste0(usfs_gc_dir, "usfs_totc_mg_ha.bil")
usfs_agc_bil = paste0(usfs_gc_dir, "usfs_agc_mg_ha.bil")
usfs_bgc_bil = paste0(usfs_gc_dir, "usfs_bgc_mg_ha.bil")
usfs_ddc_bil = paste0(usfs_gc_dir, "usfs_ddc_mg_ha.bil")
usfs_ltc_bil = paste0(usfs_gc_dir, "usfs_ltc_mg_ha.bil")
usfs_sdc_bil = paste0(usfs_gc_dir, "usfs_sdc_mg_ha.bil")
usfs_soc_bil = paste0(usfs_gc_dir, "usfs_soc_mg_ha.bil")
usfs_usc_bil = paste0(usfs_gc_dir, "usfs_usc_mg_ha.bil")


# calfire harvest plan data (shapefile)
#	THPs = Timber Harvesting Plans
#	NTMPs = Non-industrial Timber Management Plans - < 2500 acres, uneven age management, 50 year plans
#	do not use the NTOs for now, (notice of timber operations) - these are continuation reports associated with ntmps
cf_harvest_dir = "/Volumes/glabrata/geodata/calfire/Harvest_Data_Statewide_20160803/"
cf_thps_shp = "THPS"
cf_ntmps_shp = "NTMPS" 

# read in and write rasters to create and write an aggregated evt raster file

if(AGG_EVT_RAST) {

	# these are the carb rasters; do one year at a time
	# this isn't just evt_rast, it is the lfc categorical raster
	# but the aggregation script works the same as for the landfire evt_rast
	if(NEED_CARB_EVT_RAST_2010) {

		# the reprojected raster exists
		if(FALSE){
		# carb categorical raster, 2010
		lfc_rast = raster(readGDAL(lfc_bil))
		lfc_df = read.csv(lfc_class_csv)

		# 2+ hours
		# 18, single-pixel ARB_LF_ID types were lost in reprojection
		evt_rast = projectRaster(lfc_rast, crs = PROJ4_ca_teale, res = 30, method = "ngb")

		# 20-30 minutes
		writeRaster(evt_rast, filename=lfc_teale_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
		# reproject the other rasters to match the categorical one
		# these were all projected to the same grid in GRASS also
		
		lfc_rast = raster(lfc_teale_bil)
		
		temp_rast = raster(lfc_agl_bil)
		out_rast = projectRaster(temp_rast, lfc_rast, method = "ngb")
		writeRaster(out_rast, filename=lfc_agl_teale_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
		
		temp_rast = raster(lfc_tot_bil)
		out_rast = projectRaster(temp_rast, lfc_rast, method = "ngb")
		writeRaster(out_rast, filename=lfc_tot_teale_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
		
		# this never completed, but it was projected using QGIS
		temp_rast = raster(lfc_flag_bil)
		out_rast = projectRaster(temp_rast, lfc_rast, method = "ngb")
		writeRaster(out_rast, filename=lfc_flag_teale_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
		# get the unique list of land types for california
		evt_df_reduced = lfc_df[,c(7,11)]
		evt_df_reduced = evt_df_reduced[!duplicated(evt_df_reduced$EVT),]
		evt_df_reduced$EVT_Name = as.character(evt_df_reduced$EVT_Name)
		evt_df_reduced[evt_df_reduced$EVT == 0,"EVT_Name"] = "Nodata"
		write.csv(evt_df_reduced, file = evt_table_csv, row.names=FALSE)
		}else{
			#evt_rast = raster(lfc_teale_bil)
			#evt_df_reduced = read.csv(evt_table_csv)
		}
	} # end get 2010 lfc data

	if(NEED_CARB_EVT_RAST_2001) {

		# the reprojected raster exists
		if(FALSE){
		# carb categorical raster, 2001
		lfc_rast = raster(readGDAL(lfc_bil))
		lfc_df = read.csv(lfc_class_csv)
		evt_rast = projectRaster(lfc_rast, crs = PROJ4_ca_teale, res = 30, method = "ngb")
		writeRaster(evt_rast, filename=lfc_teale_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
		# reproject the other rasters to match the categorical one
		# bilinear caused mixing of carbon densities acros types, so try nearest neighbor - not done yet, it stalled the first time
		
		lfc_rast = raster(lfc_teale_bil)
		
		temp_rast = raster(lfc_agl_bil)
		out_rast = projectRaster(temp_rast, lfc_rast, method = "ngb")
		writeRaster(out_rast, filename=lfc_agl_teale_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
		
		temp_rast = raster(lfc_tot_bil)
		out_rast = projectRaster(temp_rast, lfc_rast, method = "ngb")
		writeRaster(out_rast, filename=lfc_tot_teale_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
		
		temp_rast = raster(lfc_flag_bil)
		out_rast = projectRaster(temp_rast, lfc_rast, method = "ngb")
		writeRaster(out_rast, filename=lfc_flag_teale_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
		# get the unique list of land types for california
		evt_df_reduced = lfc_df[,c(7,12)]
		evt_df_reduced = evt_df_reduced[!duplicated(evt_df_reduced$EVT),]
		evt_df_reduced$EVT_Name = as.character(evt_df_reduced$EVT_Name)
		evt_df_reduced[evt_df_reduced$EVT==-9999,"EVT_Name"] = "Nodata"
		write.csv(evt_df_reduced, file = evt_table_csv, row.names=FALSE)
		}else{
			#evt_rast = raster(lfc_teale_bil)
			#evt_df_reduced = read.csv(evt_table_csv)
		}
	} # end get 2001 lfc data

	# this is the landfire evt raster, 2010
	if(NEED_LANDFIRE_EVT_RAST) {
	
		# the projected raster and reduced table have already been written
		if(FALSE){
		# first read the landfire evt data; this is in the us albers projection
		# convert it to raster, merge the two data sets
		# reproject it to the ca teale albers
		# write the new ca evt raster
		evtn_in = readGDAL(evtn_grid)
		evtn_rast = raster(evtn_in)
		evts_in = readGDAL(evts_grid)
		evts_rast = raster(evts_in)
		evt_rast = merge(evtn_rast, evts_rast) # took about 12 minutes	
		evt_rast = projectRaster(evt_rast, crs = PROJ4_ca_teale, res = 30, method = "ngb")
		writeRaster(evt_rast, filename=evt_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)

		# read the landfire existing vegetation type table
		evt_df = read.csv(evt_table_csv)
		evt_df_reduced = evt_df
	
		# first exclude obvious non-CA types:
		#	reduces 845 types to 438 types	
		#	"Caribbean," "Alaska," "Aleutian," "Arctic," "Hawai'i," "Pacific Islands," "Rocky Mountain," "Great Plains,"
		#	"Columbia," "Great Lakes," "Gulf," "Appalachian," "Acadian," "Laurentian," "Florida", "Floridian," "Atlantic"
		#	"Prairie and Barrens", "Texas", "Flatwoods", "Edwards Plateau", "Loblolly"
		#	"Eastern" has been added since the last run,
		# 		and "Rocky Mountain" and "Columbia" have been removed since the last run
		evt_df_reduced = evt_df_reduced[-grep("Caribbean", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Alaska", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Aleutian", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Arctic", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Hawai'i", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Pacific Islands", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Great Plains", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Great Lakes", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Gulf", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Appalachian", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Acadian", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Laurentian", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Florida", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Floridian", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Atlantic", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Prairie and Barrens", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Texas", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Flatwoods", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Edwards Plateau", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Loblolly", evt_df_reduced$EVT_Name),]
		evt_df_reduced = evt_df_reduced[-grep("Eastern", evt_df_reduced$EVT_Name),]
		
		evt_df_reduced$EVT = evt_df_reduced$Value
		write.csv(evt_df_reduced, file = evt_table_rdcd_csv, row.names=FALSE)
		}else {
			#evt_rast = raster(evt_bil)
			evt_df_reduced = read.csv(evt_table_rdcd_csv)
		}
	} # end read landfire data
	
	# get the joint set of 2001 and 2010 set of unique EVT_2008_STD classes
	# there are 160, but only 158 in 2001, and 155 in 2010
	#	these two are not in 2001 or 2010:
	#		"Recently Burned-Herb and Grass Cover" and "Northern Rocky Mountain Subalpine-Upper Montane Grassland"
	#	these three are in 2001 but not in 2010:
	#		"Herbaceous Semi-dry" and "Herbaceous Semi-wet" shouldn't be problem as all Herbaceous goes to meadow
	#		"Developed-Open Space" is assigned a zero carbon density so will be grouped with the intensity developed classes

	# this has been done, so just read in the 2008 standard table
	if(FALSE) {
	# need to manipulate the original carbon table
	lfc_all_df = read.csv(lfc_carbon_csv)
		
	evt_df_reduced = lfc_all_df[,c("EVT_2008_STD","Year","EVT_Name")]
	evt2001_df_reduced = evt_df_reduced[evt_df_reduced$Year==2001,]
	evt2010_df_reduced = evt_df_reduced[evt_df_reduced$Year==2010,]
	evt_df_reduced = evt_df_reduced[!duplicated(evt_df_reduced$EVT_2008_STD),]
	evt2001_df_reduced = evt2001_df_reduced[!duplicated(evt2001_df_reduced$EVT_2008_STD),]
	evt2010_df_reduced = evt2010_df_reduced[!duplicated(evt2010_df_reduced$EVT_2008_STD),]
	evt_df_reduced$EVT_2008_STD = as.character(evt_df_reduced$EVT_2008_STD)
	evt2001_df_reduced$EVT_2008_STD = as.character(evt2001_df_reduced$EVT_2008_STD)
	evt2010_df_reduced$EVT_2008_STD = as.character(evt2010_df_reduced$EVT_2008_STD)
	# so take out the 2008 only classes and set the std nodata value to "Nodata"
	evt_df_reduced = evt_df_reduced[-grep("2008", evt_df_reduced$Year),]
	evt_df_reduced[evt_df_reduced$EVT_2008_STD == "","EVT_2008_STD"] = "Nodata"
	evt2001_df_reduced[evt2001_df_reduced$EVT_2008_STD == "","EVT_2008_STD"] = "Nodata"
	evt2010_df_reduced[evt2010_df_reduced$EVT_2008_STD == "","EVT_2008_STD"] = "Nodata"
	# add the 2010 evt names becasue only the 2001 ones are selected above when duplicates are removed
	# these are only examples, not the full list, because duplicates of the std name have been removed
	evt_df_reduced = merge(evt_df_reduced, evt2010_df_reduced, by = "EVT_2008_STD", all.x = TRUE)
	write.csv(evt_df_reduced, file = evt_table_csv, row.names=FALSE)
	
	# create a full mapping list -maybe, these would be quite long
	}else {
		evt_df_reduced = read.csv(evt_table_csv)
		evt_df_reduced$EVT_2008_STD = as.character(evt_df_reduced$EVT_2008_STD)
	}
	
	# aggregate the evt data
	cat("started aggregation at", date(), "\n")

	# this fills in a new column of the lfc carbon table with the aggregate class
	# no longer write an evt raster here
	lfc_carbon_df$agg_evt = -9999
	lfc_carbon_df$agg_evt_name = "nodata"

	# the raster match doesn't work with carb data because the reduced table does not include all the Values
	# can also create the evt_rast by writing EVT from the aggregated tables
	
	# note that the carb land cover list is a subset of the landfire table
	#	so the following queries will work for both
	#	the CA land cover data do not have some eof the types listed below
	####
	#	so it is important to check for some when removing types because if they do not exist at all, an empty set is returned for the inverse
	
	# read the landfire aggregate existing vegetation type table
	evt_df_aggregate = read.csv(evt_agg_table)
	
	# first aggregate the land cover to the generic and desired types, in a raster
	# and write a .csv file for each aggregated type listing the included source types
	#evt_aggregate_rast = evt_rast
	#evt_aggregate_rast[] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "nodata","Value"]

	# nodata
	# nodata values are assigned NA, presumably, but there are 0 values that are also nodata
	#	convert the zero values to nodata=-9999, as specified in the tables
	#	this converstion -to -9999 is done by default as it the default value set above
	# the aggregated land type data will have the nodata value = -9999, as specified in evt_df_aggregate
	nodata = evt_df_reduced[grep("Nodata", evt_df_reduced$EVT_2008_STD),]
	#temp_rast = match(evt_rast, nodata$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "nodata","Value"]
	#}
	write.csv(nodata, file = evt_agg_nodata_csv, row.names=FALSE)

	# water
	#	includes aquaculture
	#	includes boreal aquatic beds (which are not in CA)
	water = evt_df_reduced[grep("Water", evt_df_reduced$EVT_2008_STD),]
	aquaculture = evt_df_reduced[grep("Aquaculture", evt_df_reduced$EVT_2008_STD),]
	bor_aquatic = evt_df_reduced[grep("Boreal Aquatic Beds", evt_df_reduced$EVT_2008_STD),]
	water = rbind(water, aquaculture, bor_aquatic)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% water$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "water","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% water$EVT_2008_STD, "agg_evt_name"] = "water"
	#temp_rast = match(evt_rast, water$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "water","Value"]
	#}
	write.csv(water, file = evt_agg_water_csv, row.names=FALSE)
	
	# snow-ice
	ice = evt_df_reduced[grep("Ice", evt_df_reduced$EVT_2008_STD),]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% ice$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "ice","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% ice$EVT_2008_STD, "agg_evt_name"] = "ice"
	#temp_rast = match(evt_rast, ice$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "ice","Value"]
	#}
	write.csv(ice, file = evt_agg_ice_csv, row.names=FALSE)
	
	# barren
	#	includes quarries/mines/pits
	#	includes bare soil and coastal sand and rock
	barren = evt_df_reduced[grep("Barren", evt_df_reduced$EVT_2008_STD),]
	if(length(barren[grep("Barrens", barren$EVT_2008_STD),]$EVT_2008_STD) > 0){
		barren = barren[-grep("Barrens", barren$EVT_2008_STD),]}
	quarries = evt_df_reduced[grep("Quarries", evt_df_reduced$EVT_2008_STD),]
	bare = evt_df_reduced[grep("Bare soil", evt_df_reduced$EVT_2008_STD),]
	rock = evt_df_reduced[grep("rock", evt_df_reduced$EVT_2008_STD),]
	barren = rbind(barren, quarries, bare, rock)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% barren$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "barren","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% barren$EVT_2008_STD, "agg_evt_name"] = "barren"
	#temp_rast = match(evt_rast, barren$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "barren","Value"]
	#}
	write.csv(barren, file = evt_agg_barren_csv, row.names=FALSE)
	
	# sparsely vegetated
	#	includes where "Desert" is also in the description
	#	includes ca alpine fellfield
	sparse = evt_df_reduced[grep("Sparse", evt_df_reduced$EVT_2008_STD),]
	ca_fell = evt_df_reduced[grep("Alpine Fell-Field", evt_df_reduced$EVT_2008_STD),]
	sparse = rbind(sparse, ca_fell)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% sparse$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "sparse","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% sparse$EVT_2008_STD, "agg_evt_name"] = "sparse"
	#temp_rast = match(evt_rast, sparse$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "sparse","Value"]
	#}
	write.csv(sparse, file = evt_agg_sparse_csv, row.names=FALSE)
	
	# shrubland (and chaparral)
	# 	includes "Peatland Shrubland" and "Shrub Peatlands" and if "Prairie" is also in description
	#	includes "Shrub" wetlands, swamps, floodplains, peatlands
	#	includes "Undeveloped Ruderal" in the description and "Recently Logged-Shrub Cover" and "Recently Burned - Shrub Cover"
	#	includes where "Desert" and "Mesic Scrub" are also in the desription
	#	includes "Sagebrush Steppe" and "Greasewood Flat"
	#	includes "Introduced Riparian Vegetation" because this is mapped to shrubland in 2010
	#	does not include where "Forest" or "Woodland" or "Meadow" or "Savanna" or "Urban" or "Developed" or "Tundra" are also in description
	shrubland = evt_df_reduced[grep("Shrub", evt_df_reduced$EVT_2008_STD),]
	chaparral = evt_df_reduced[grep("Chaparral", evt_df_reduced$EVT_2008_STD),]
	shrubland = rbind(shrubland, chaparral)
	shrubland = shrubland[-grep("Forest", shrubland$EVT_2008_STD),]
	shrubland = shrubland[-grep("Woodland", shrubland$EVT_2008_STD),]
	shrubland = shrubland[-grep("Savanna", shrubland$EVT_2008_STD),]
	shrubland = shrubland[-grep("Meadow", shrubland$EVT_2008_STD),]
	if(length(shrubland[grep("Urban", shrubland$EVT_2008_STD),]$EVT_2008_STD) > 0){
		shrubland = shrubland[-grep("Urban", shrubland$EVT_2008_STD),]}
	shrubland = shrubland[-grep("Developed", shrubland$EVT_2008_STD),]
	if(length(shrubland[grep("Tundra", shrubland$EVT_2008_STD),]$EVT_2008_STD) > 0){
		shrubland = shrubland[-grep("Tundra", shrubland$EVT_2008_STD),]}
	sage = evt_df_reduced[grep("Sagebrush Steppe", evt_df_reduced$EVT_2008_STD),]
	grease = evt_df_reduced[grep("Greasewood Flat", evt_df_reduced$EVT_2008_STD),]
	intro_rip = evt_df_reduced[grep("Introduced Riparian Vegetation", evt_df_reduced$EVT_2008_STD),]
	shrubland = rbind(shrubland, sage, grease,intro_rip)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% shrubland$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "shrubland","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% shrubland$EVT_2008_STD, "agg_evt_name"] = "shrubland"
	#temp_rast = match(evt_rast, shrubland$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "shrubland","Value"]
	#}
	write.csv(shrubland, file = evt_agg_shrubland_csv, row.names=FALSE)
	
	# woodland
	#	includes "Encinal" and "Wooded" and "Parkland", and if "Chaparral" or "Shrubland" or "Prairie" or "Desert" are also in description
	#	includes remaining "Barrens," except herbacies ones, although these may not occur in CA
	#	includes "Woody Wetland" and "Parkland"
	#	includes "Aspen-Steppe" and "Montane Riparian Systems"
	#	does not include where "Forest" or "Savanna" is also in the description
	woodland = evt_df_reduced[grep("Woodland", evt_df_reduced$EVT_2008_STD),]
	woodland = woodland[-grep("Forest", woodland$EVT_2008_STD),]
	woodland = woodland[-grep("Savanna", woodland$EVT_2008_STD),]
	if(length(woodland[grep("Parkland", woodland$EVT_2008_STD),]$EVT_2008_STD > 0)){
		woodland = woodland[-grep("Parkland", woodland$EVT_2008_STD),]
	}
	encinal = evt_df_reduced[grep("Encinal", evt_df_reduced$EVT_2008_STD),]
	barrens = evt_df_reduced[grep("Barrens", evt_df_reduced$EVT_2008_STD),]
	barrens = barrens[-grep("Woodland", barrens$EVT_2008_STD),]
	barrens = barrens[-grep("Herbaceous", barrens$EVT_2008_STD),]
	wooded = evt_df_reduced[grep("Wooded", evt_df_reduced$EVT_2008_STD),]
	woodywetland = evt_df_reduced[grep("Woody", evt_df_reduced$EVT_2008_STD),]
	parkland = evt_df_reduced[grep("Parkland", evt_df_reduced$EVT_2008_STD),]
	aspen = evt_df_reduced[grep("Aspen-Steppe", evt_df_reduced$EVT_2008_STD),]
	riparian = evt_df_reduced[grep("Montane Riparian Systems", evt_df_reduced$EVT_2008_STD),]
	woodland = rbind(woodland, encinal, wooded, barrens, woodywetland, parkland, aspen, riparian)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% woodland$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "woodland","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% woodland$EVT_2008_STD, "agg_evt_name"] = "woodland"
	#temp_rast = match(evt_rast, woodland$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "woodland","Value"]
	#}
	write.csv(woodland, file = evt_agg_woodland_csv, row.names=FALSE)
	
	# savanna
	#	includes all "Savanna": even if "Chaparral" or "Woodland" or "Praiarie" or "Grassland" are also in description
	savanna = evt_df_reduced[grep("Savanna", evt_df_reduced$EVT_2008_STD),]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% savanna$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "savanna","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% savanna$EVT_2008_STD, "agg_evt_name"] = "savanna"
	#temp_rast = match(evt_rast, savanna$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "savanna","Value"]
	#}
	write.csv(savanna, file = evt_agg_savanna_csv, row.names=FALSE)
	
	# grassland (and priarie)
	#	includes if "Desert" or "Steppe" or "are also in description
	#	includes "Undeveloped Ruderal" in the description
	#	includes "Recently Logged-Herb and Grass Cover" and "Recently Burned-Herb and Grass Cover"
	#	does not include if "Savanna" or "Woodland" or "Shrubland" or "Developed" are also in description
	grassland = evt_df_reduced[grep("Grass", evt_df_reduced$EVT_2008_STD),]
	if(length(grassland[grep("Prairie", grassland$EVT_2008_STD),]$EVT_2008_STD > 0)){
		grassland = grassland[-grep("Prairie", grassland$EVT_2008_STD),]}
	prairie = evt_df_reduced[grep("Prairie", evt_df_reduced$EVT_2008_STD),]
	grassland = rbind(grassland, prairie)
	if(length(grassland[grep("Woodland", grassland$EVT_2008_STD),]$EVT_2008_STD > 0)){
		grassland = grassland[-grep("Woodland", grassland$EVT_2008_STD),]}
	if(length(grassland[grep("Savanna", grassland$EVT_2008_STD),]$EVT_2008_STD > 0)){
		grassland = grassland[-grep("Savanna", grassland$EVT_2008_STD),]}
	if(length(grassland[grep("Shrubland", grassland$EVT_2008_STD),]$EVT_2008_STD > 0)){
		grassland = grassland[-grep("Shrubland", grassland$EVT_2008_STD),]}
	if(length(grassland[grep("Developed", grassland$EVT_2008_STD),]$EVT_2008_STD > 0)){
		grassland = grassland[-grep("Developed", grassland$EVT_2008_STD),]}
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% grassland$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "grassland","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% grassland$EVT_2008_STD, "agg_evt_name"] = "grassland"
	#temp_rast = match(evt_rast, grassland$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "grassland","Value"]
	#}
	write.csv(grassland, file = evt_agg_grassland_csv, row.names=FALSE)
	
	# developed (could still have some veg)
	#	includes "Developed" high, med, low intensity
	#	includes developed open space, which is only listed in 2001, with 0 carbon
	#	includes roads, even they are not in urban areas
	#	does not include Urban with vegetation identifiers (forest, shrubland, herbaceous)
	#	does not include "Developed Ruderal" because this is mostly agriculture
	#	does not include "Developed-Open Space" - presumably vegetation, this class is not in CA
	developed = evt_df_reduced[grep("Developed", evt_df_reduced$EVT_2008_STD),]
	if(length(developed[grep("Developed Ruderal", developed$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed = developed[-grep("Developed Ruderal", developed$EVT_2008_STD),]}
	if(length(developed[grep("Herbaceous", developed$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed = developed[-grep("Herbaceous", developed $EVT_2008_STD),]}
	if(length(developed[grep("Forest", developed$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed = developed[-grep("Forest", developed$EVT_2008_STD),]}
	if(length(developed[grep("Shrubland", developed$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed = developed[-grep("Shrubland", developed$EVT_2008_STD),]}
	urban = evt_df_reduced[grep("Urban", evt_df_reduced$EVT_2008_STD),]
	urban = urban[-grep("Forest", urban$EVT_2008_STD),]
	urban = urban[-grep("Shrubland", urban$EVT_2008_STD),]
	urban = urban[-grep("Herbaceous", urban$EVT_2008_STD),]
	developed = rbind(developed, urban)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% developed$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "developed","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% developed$EVT_2008_STD, "agg_evt_name"] = "developed"
	#temp_rast = match(evt_rast, developed$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "developed","Value"]
	#}
	write.csv(developed, file = evt_agg_developed_csv, row.names=FALSE)
	
	# developed vegetation (this is urban forest, shrubland, herbaceous)
	#	2010: the only "Urban" classes in CA are the veg classes
	#	2008 these are "Developed" Forest, Herbaceous, Shurbland
	#	does not include 2010 "Developed Ruderal" which is mostly agriculture
	#	does not include open space
	developed_veg = evt_df_reduced[grep("Developed", evt_df_reduced$EVT_2008_STD),]
	if(length(developed_veg[grep("Developed Ruderal", developed_veg$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed_veg = developed_veg[-grep("Developed Ruderal", developed_veg$EVT_2008_STD),]}
	if(length(developed_veg[grep("Roads", developed_veg$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed_veg = developed_veg[-grep("Roads", developed_veg$EVT_2008_STD),]}
	if(length(developed_veg[grep("Intensity", developed_veg$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed_veg = developed_veg[-grep("Intensity", developed_veg$EVT_2008_STD),]}
	if(length(developed_veg[grep("Open Space", developed_veg$EVT_2008_STD),]$EVT_2008_STD > 0)){
		developed_veg = developed_veg[-grep("Open Space", developed_veg$EVT_2008_STD),]}
	urban = evt_df_reduced[grep("Urban", evt_df_reduced$EVT_2008_STD),]
	if(length(urban[urban$EVT_2008_STD == "Urban",]$EVT_2008_STD > 0)){
		urban = urban[-urban $EVT_2008_STD == "Urban",]}
	developed_veg = rbind(developed_veg, urban)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% developed_veg$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "developed_veg","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% developed_veg$EVT_2008_STD, "agg_evt_name"] = "developed_veg"
	#temp_rast = match(evt_rast, developed_veg$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "developed_veg","Value"]
	#}
	write.csv(developed_veg, file = evt_agg_developed_veg_csv, row.names=FALSE)
	
	# desert (and scrub)
	#	includes "Thornscrub" and "Upland Scrub" and "Coastal Scrub" and "Sand Flat Scrub"
	#	does not include if "Sparse" or "Shrub" or "Chaparral" or "Grassland" or "Woodland" or "Forest" or "Herbaceous" are also in description
	desert = evt_df_reduced[grep("Desert", evt_df_reduced$EVT_2008_STD),]
	if(length(desert[grep("Thornscrub", desert$EVT_2008_STD),]$EVT_2008_STD > 0)){
		desert = desert[-grep("Thornscrub", desert$EVT_2008_STD),]}
	desert = desert[-grep("Sparse", desert$EVT_2008_STD),]
	desert = desert[-grep("Shrub", desert$EVT_2008_STD),]
	desert = desert[-grep("Chaparral", desert$EVT_2008_STD),]
	desert = desert[-grep("Grassland", desert$EVT_2008_STD),]
	if(length(desert[grep("Forest", desert$EVT_2008_STD),]$EVT_2008_STD > 0)){
		desert = desert[-grep("Forest", desert$EVT_2008_STD),]}		# this needs to be done before "Woodland"
	desert = desert[-grep("Woodland", desert$EVT_2008_STD),]
	if(length(desert[grep("Herbaceous", desert$EVT_2008_STD),]$EVT_2008_STD > 0)){
		desert = desert[-grep("Herbaceous", desert$EVT_2008_STD),]}
	thornscrub = evt_df_reduced[grep("Thornscrub", evt_df_reduced$EVT_2008_STD),]
	uplandscrub = evt_df_reduced[grep("Upland Scrub", evt_df_reduced$EVT_2008_STD),]
	coastscrub = evt_df_reduced[grep("Coastal Scrub", evt_df_reduced$EVT_2008_STD),]
	sandscrub = evt_df_reduced[grep("Sand Flat Scrub", evt_df_reduced$EVT_2008_STD),]
	desert = rbind(desert, thornscrub, uplandscrub, coastscrub, sandscrub)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% desert$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "desert","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% desert$EVT_2008_STD, "agg_evt_name"] = "desert"
	#temp_rast = match(evt_rast, desert$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "desert","Value"]
	#}
	write.csv(desert, file = evt_agg_desert_csv, row.names=FALSE)
	
	# agriculture
	#	includes all crops, agriculture, pasture, and hayland
	#	includes "Wheat" and "berries" and "Orchard" and "Vinyard"
	#	includes "Developed Ruderal"
	crop = evt_df_reduced[grep("Crop", evt_df_reduced$EVT_2008_STD),]
	agriculture = evt_df_reduced[grep("Agriculture", evt_df_reduced$EVT_2008_STD),]
	agriculture = agriculture[-grep("Crop", agriculture$EVT_2008_STD),]
	pasture = evt_df_reduced[grep("Pasture", evt_df_reduced$EVT_2008_STD),]
	if(length(pasture[grep("Agriculture", pasture$EVT_2008_STD),]$EVT_2008_STD > 0)){
		pasture = pasture[-grep("Agriculture", pasture$EVT_2008_STD),]}
	wheat = evt_df_reduced[grep("Wheat", evt_df_reduced$EVT_2008_STD),]
	berries = evt_df_reduced[grep("berries", evt_df_reduced$EVT_2008_STD),]
	orchard = evt_df_reduced[grep("Orchard", evt_df_reduced$EVT_2008_STD),]
	vineyard = evt_df_reduced[grep("Vineyard", evt_df_reduced$EVT_2008_STD),]
	dev_rud = evt_df_reduced[grep("Developed Ruderal", evt_df_reduced$EVT_2008_STD),]	
	agriculture = rbind(crop, agriculture, pasture, wheat, berries, orchard, vineyard, dev_rud)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% agriculture$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "agriculture","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% agriculture$EVT_2008_STD, "agg_evt_name"] = "agriculture"
	#temp_rast = match(evt_rast, agriculture$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "agriculture","Value"]
	#}
	write.csv(agriculture, file = evt_agg_agriculture_csv, row.names=FALSE)
	
	# meadow (and herbaceous)
	#	includes all "Meadow" in the description
	# 	includes "Herbaceous" exceipt if "Grass", "Wooded", "Urban", "Developed" are in the description
	#	includes herbaceous wetland - the descriptions are more meadow-like than wetland, and are inland
	#	includes peatland, floodplain, glade, and recently burned herb cover; so includes valley, coastal, and desert as well
	#	includes "Tundra" and "Boreal Peatlands"
	#	includes upland "Forbland"
	#	includes "Rocky Mountain Wetland-Herbaceous"
	meadow = evt_df_reduced[grep("Meadow", evt_df_reduced$EVT_2008_STD),]
	if(length(meadow[grep("Herbaceous", meadow$EVT_2008_STD),]$EVT_2008_STD > 0)){
		meadow = meadow[-grep("Herbaceous", meadow$EVT_2008_STD),]}
	herb = evt_df_reduced[grep("Herb", evt_df_reduced$EVT_2008_STD),]
	if(length(herb[grep("Grass", herb$EVT_2008_STD),]$EVT_2008_STD > 0)){
		herb = herb[-grep("Grass", herb$EVT_2008_STD),]}
	herb = herb[-grep("Wooded", herb$EVT_2008_STD),]
	if(length(herb[grep("Urban", herb$EVT_2008_STD),]$EVT_2008_STD > 0)){
		herb = herb[-grep("Urban", herb$EVT_2008_STD),]}
	if(length(herb[grep("Developed", herb$EVT_2008_STD),]$EVT_2008_STD > 0)){
		herb = herb[-grep("Developed", herb$EVT_2008_STD),]}
	tundra = evt_df_reduced[grep("Tundra", evt_df_reduced$EVT_2008_STD),]
	bor_peat = evt_df_reduced[grep("Boreal Peatlands", evt_df_reduced$EVT_2008_STD),]
	forb = evt_df_reduced[grep("Forb", evt_df_reduced$EVT_2008_STD),]
	forb = forb[-grep("Grass", forb$EVT_2008_STD),]
	alpine_turf = evt_df_reduced[grep("Alpine Turf", evt_df_reduced$EVT_2008_STD),]
	meadow = rbind(meadow, herb, tundra, forb, bor_peat, alpine_turf)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% meadow$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "meadow","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% meadow$EVT_2008_STD, "agg_evt_name"] = "meadow"
	#temp_rast = match(evt_rast, meadow$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "meadow","Value"]
	#}
	write.csv(meadow, file = evt_agg_meadow_csv, row.names=FALSE)

	# wetland
	#	the only wetlands included in the data are inland meadows and woody swamp
	#	maybe some coastal non-salt marsh wetland can be separated later
	
	# salt marsh
	#	includes all "Marsh" in the description (coastal, tidal, coastal aquatic beds)
	#	there is only "Pacific Coastal Marsh" in CA, and includes mud flats, salt marsh, and other coastal wetlands that may not always be saline
	marsh = evt_df_reduced[grep("Marsh", evt_df_reduced$EVT_2008_STD),]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% marsh$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "marsh","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% marsh$EVT_2008_STD, "agg_evt_name"] = "marsh"
	#temp_rast = match(evt_rast, marsh$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "marsh","Value"]
	#}
	write.csv(marsh, file = evt_agg_marsh_csv, row.names=FALSE)

	# forest 
	#	includes all types with "Forest" and "Tree"
	#	which includes plantations, wetlands, recently logged tree cover
	#	also includes types with additional designations such as woodland, shrubland, desert, wetland, floodplain, peatland
	#	includes "Ruderal" undeveloped forest
	#	includes "North Pacific Swamp Systems" and "Conifer Swamp", which are forest types
	#	does not include "Treeline" as a descriptor or "Developed"
	#	does not include "Urban" or "Developed" or "Tundra"
	forest = evt_df_reduced[grep("Forest", evt_df_reduced$EVT_2008_STD),]
	if(length(forest[grep("Urban", forest$EVT_2008_STD),]$EVT_2008_STD > 0)){
		forest = forest[-grep("Urban", forest$EVT_2008_STD),]}
	forest = forest[-grep("Developed", forest$EVT_2008_STD),]
	if(length(forest[grep("Tundra", forest$EVT_2008_STD),]$EVT_2008_STD > 0)){
		forest = forest[-grep("Tundra", forest$EVT_2008_STD),]}
	tree = evt_df_reduced[grep("Tree", evt_df_reduced$EVT_2008_STD),]
	tree = tree[-grep("Treeline", tree$EVT_2008_STD),]
	conifer_swamp = evt_df_reduced[grep("Conifer Swamp", evt_df_reduced$EVT_2008_STD),]
	pacific_swamp = evt_df_reduced[grep("North Pacific Swamp Systems", evt_df_reduced$EVT_2008_STD),]
	forest = rbind(forest, tree, conifer_swamp, pacific_swamp)
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% forest$EVT_2008_STD, "agg_evt"] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "forest","Value"]
	lfc_carbon_df[lfc_carbon_df$EVT_2008_STD %in% forest$EVT_2008_STD, "agg_evt_name"] = "forest"
	#temp_rast = match(evt_rast, forest$Value)
	#temp_inds = Which(!is.na(temp_rast), cells = TRUE)
	#if(length(temp_inds > 0)) {
	#	evt_aggregate_rast[temp_inds] = evt_df_aggregate[evt_df_aggregate$EVT_Name == "forest","Value"]
	#}
	write.csv(forest, file = evt_agg_forest_csv, row.names=FALSE)
	
	# check that all source types are covered
	cat("nodata count", length(nodata$EVT_2008_STD),"\n")
	cat("water count", length(water$EVT_2008_STD),"\n")
	cat("ice count", length(ice$EVT_2008_STD),"\n")
	cat("barren count", length(barren$EVT_2008_STD),"\n")
	cat("sparse count", length(sparse$EVT_2008_STD),"\n")
	cat("shrubland count", length(shrubland$EVT_2008_STD),"\n")
	cat("woodland count", length(woodland$EVT_2008_STD),"\n")
	cat("savanna count", length(savanna$EVT_2008_STD),"\n")
	cat("grassland count", length(grassland$EVT_2008_STD),"\n")
	cat("developed count", length(developed$EVT_2008_STD),"\n")
	cat("developed_veg count", length(developed_veg$EVT_2008_STD),"\n")
	cat("desert count", length(desert$EVT_2008_STD),"\n")
	cat("crop count", length(agriculture$EVT_2008_STD),"\n")
	cat("meadow count", length(meadow$EVT_2008_STD),"\n")
	#cat("wetland count", length(wetland$EVT_2008_STD),"\n")
	cat("marsh count", length(marsh$EVT_2008_STD),"\n")
	cat("forest count", length(forest$EVT_2008_STD),"\n")
	sum_counts = length(nodata$EVT_2008_STD)+length(water$EVT_2008_STD)+length(ice$EVT_2008_STD)+length(barren$EVT_2008_STD)+length(sparse$EVT_2008_STD)+length(shrubland$EVT_2008_STD)+length(woodland$EVT_2008_STD)+length(savanna$EVT_2008_STD)+length(grassland$EVT_2008_STD)+length(developed$EVT_2008_STD)+length(developed_veg$EVT_2008_STD)+length(desert$EVT_2008_STD)+length(agriculture$EVT_2008_STD)+length(meadow$EVT_2008_STD)+length(marsh$EVT_2008_STD)+length(forest$EVT_2008_STD)
	cat("sum aggrregated count", sum_counts,"\n")
	cat("evt_df_reduced count", length(evt_df_reduced$EVT_2008_STD),"\n")
	
	# bind the aggregated types into one data set for comparison with source
	all_aggregated = rbind(nodata, water, ice, barren, sparse, shrubland, woodland, savanna, grassland, developed, developed_veg, desert, agriculture, meadow, marsh, forest)
	cat("aggregated count", length(all_aggregated$EVT_2008_STD),"\n")
	
	# check for duplicates
	n <- data.frame(table(all_aggregated$EVT_2008_STD))
	n[n$Freq > 1,]
	all_aggregated[all_aggregated$EVT_2008_STD %in% n$Var1[n$Freq > 1],]
	
	# check if any are missing
	# for the carb lfc data EVT_2008_STD is the raster category
	evt_only = !evt_df_reduced$EVT_2008_STD %in% all_aggregated$EVT_2008_STD
	evt_df_reduced[evt_only,]
	
	#####
	# now write the aggregated land type raster
	#writeRaster(evt_aggregate_rast, filename=evt_agg_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	cat("finished aggregation at at", date())
} else {
	# read the aggregate existing vegetation type table
	evt_df_aggregate = read.csv(evt_agg_table)
	
	# read the aggregated evt raster
	#evt_aggregate_rast <- raster(evt_agg_bil)
	
} # end generate or read the aggreate the evt data


# spatial processing was done in GRASS GIS because R had some issues
# so read in the spatial data for the desired ownership categories
# the corresponding R spatial processing flow for the state aggregation is below, but not all of it works or has been tested
# and now it is somewhat out of order

# see below for continued processing


if (FALSE) {
	# read in the frap political and ownership data
	# includes the channel islands
	county <- readOGR(dsn=frap_cnty_dir, layer=frap_county_shp)
	# state
	state <- readOGR(dsn=frap_state_dir, layer=frap_state_shp)
	# ownership
	own <- readOGR(dsn=frap_own_dir, layer=frap_own_shp)

	# dissolve the ownership layer into 2 aggregate categories: protected and USFS
	# aggregate with FUN = length gives two columns: Group.1=aggergate category and x = # of features dissolved into the Group.1 categories
	# union() merges layers (as OR) and preserves attrubutes, and there shouldn't be any outside-state features in these data
	# but unionSpatialPolygons() dissolves polygons and does not preserve attributes
	# need to create the 'private' polygons and category by unioning with the state boundary afterward
	dissolve_inds = which(own@data$Own_Group != "USDA Forest Service")
	owncats = as.character(own@data$Own_Group)
	owncats[dissolve_inds] = "Protected"
	owncats[-dissolve_inds] = "USFS"
	own@data$dissolve = owncats
	own_df = as(own, "data.frame")
	own_df_dissolve = aggregate(own_df$dissolve, list(own_df$dissolve), FUN = length)
	row.names(own_df_dissolve) <- as.character(own_df_dissolve$Group.1)
	own_dissolve = unionSpatialPolygons(own, IDs = own@data$dissolve)
	own_dissolve = SpatialPolygonsDataFrame(own_dissolve, own_df_dissolve)
	# this should give categories to all land in the state
	own_dissolve = union(state,own_dissolve)
	own_dissolve@data$owncats = own_dissolve@data$Group.1
	private_inds = which(is.na(own_dissolve@data$owncats))
	own_dissolve@data$owncats[private_inds] = "Private"

	# read the nwps data and reproject and crop
	# not needed because the usfs wilderness layer is enough; the ownership data take care of the rest
	# only needed if separating reserved land in non-usfs ownsership
	# nwps_in <- readOGR(dsn=nwps_dir, layer=nwps_shp)
	# nwps <- spTransform(nwps_in, CRS(PROJ4_ca_teale))
	# nwps = crop(nwps, state)

	# read in the usfs boundaries and reproject and crop
	# only if aggregating to forest units for checking forest carbon
	#nf_in <- readOGR(dsn=usfs_nf_dir, layer=usfs_nf_shp)
	#natfor <- spTransform(nf_in, CRS(PROJ4_ca_teale))
	#natfor = crop(natfor, state)

	# don't need this; the only grassland overlaps a forest unit
	#gr_in <- readOGR(dsn=usfs_gr_dir, layer=usfs_gr_shp)
	#natgrass <- spTransform(gr_in, CRS(PROJ4_ca_teale))

	# read the usfs protected areas
	# these have been cleaned, reprojected and cropped in GRASS
	oa_in <- readOGR(dsn=usfs_oa_dir, layer=usfs_oa_shp)
	natother <- spTransform(oa_in, CRS(PROJ4_ca_teale))
	natother = crop(natother, state)
	usfs_wild_in <- readOGR(dsn=usfs_wild_dir, layer=usfs_wild_shp)
	usfs_wild <- spTransform(usfs_wild_in, CRS(PROJ4_ca_teale))
	usfs_wild = crop(usfs_wild, state)

	natother@data$dissolve = "Protected"
	natother_df = as(natother, "data.frame")	
	natother_df_dissolve = aggregate(natother_df$dissolve, list(natother_df$dissolve), FUN = length)
	row.names(natother_df_dissolve) <- as.character(natother_df_dissolve$Group.1)
	natother_dissolve = unionSpatialPolygons(natother, IDs = natother@data$dissolve)
	natother_dissolve = SpatialPolygonsDataFrame(natother_dissolve, natother_df_dissolve)

	usfs_wild@data$dissolve = "Protected"
	usfs_wild_df = as(usfs_wild, "data.frame")
	usfs_wild_df_dissolve = aggregate(usfs_wild_df$dissolve, list(usfs_wild_df$dissolve), FUN = length)
	row.names(usfs_wild_df_dissolve) <- as.character(usfs_wild_df_dissolve$Group.1)
	usfs_wild_dissolve = unionSpatialPolygons(usfs_wild, IDs = usfs_wild@data$dissolve)
	usfs_wild_dissolve = SpatialPolygonsDataFrame(usfs_wild_dissolve, usfs_wild_df_dissolve)

	# read dissolved conservation easement data
	# cleaning and dissolving done in GRASS
	# but change the ease_label to "Protected"
	cced <- readOGR(dsn=cced_dir, layer=cced_shp)
	cced@data$ease_label = "Protected"

	# don't need to do this; already done in GRASS	
	# dissolve all the cced polygons and all other national wilderness polygons and all the usfs wilderness polygons first
	# then union them with the dissolved (3 categories) ownership data
	# then dissolve the newly added protected areas into the existing protected category
	#cced@data$dissolve = "Protected"
	#cced_df = as(cced, "data.frame")
	#cced_df_dissolve = aggregate(cced_df$dissolve, list(cced_df$dissolve), FUN = length)
	#row.names(cced_df_dissolve) <- as.character(cced_df_dissolve$Group.1)
	#cced_dissolve = unionSpatialPolygons(cced, IDs = cced@data$dissolve)
	#cced_dissolve = SpatialPolygonsDataFrame(cced_dissolve, cced_df_dissolve)

	# actually, need to clip the usfs data to state first, but not sure how to do this without creating a bunch of new polygons
	# probably need to use intersect, then delete the polygons without labels
	# not sure if this works
	natother_dissolve = intersect(state, natother_dissolve)
	usfs_wild_dissolve = intersect(state, usfs_wild_dissolve)
	natother = natother[natother@data$dissolve="Protected",]
	natother = natother[natother@data$dissolve="Protected",]

	# now merge all the ownership/protected polygons and dissolve all the protected into one category
	own_dissolve = union(own_dissolve, cced_dissolve)
	own_dissolve = union(own_dissolve, natother_dissolve)
	own_dissolve = union(own_dissolve, usfs_wild_dissolve)

	dissolve_inds = which(is.na(own_dissolve@data$owncats))	
	own_dissolve@data$owncats[dissolve_inds] = "Protected"
	own_dissolve_df = as(own_dissolve, "data.frame")
	own_dissolve_df_dissolve = aggregate(own_dissolve_df$owncats, list(own_dissolve_df$owncats), FUN = function(x) (x))
	row.names(own_dissolve_df_dissolve) <- as.character(own_dissolve_df_dissolve$Group.1)
	own_agg_state = unionSpatialPolygons(own_dissolve, IDs = own_dissolve@data$owncats)
	own_agg_state = SpatialPolygonsDataFrame(own_agg_state, own_agg_state_df_dissolve)

	# get the total area of each ownership category feature
	own_agg_state@data$area_sqm = gArea(own_agg_state, byid = TRUE)

} # end FALSE for R spatial processing

# read in the ca arb categorical raster data
# carbon table was read in at the beginning
# need to link this to the carbon table - see below
lfc_rast = raster(lfc_teale_bil)

# this was done here, except for rasterizing and filling the table
# the vector data with the valu1 was rasterized in grass for processing there
if(FALSE){
# add the soil carbon data to the lfc data table

# read in the soil carbon densities
#	gssurgo ca projected polygon data
# these data are just for the mainland
soil_in <- readOGR(dsn=soil_shp_dir, layer=soil_poly_shp)
#names(soil_in@data) <- tolower(names(soil_in@data))

# read in the table with the soil carbon data
soil_valu1 <- read.csv(valu1_fname)

# now merge the table with the spatial layer; make sure that package sp is loaded via raster
# keep all the spatial data rows
soil_in@data = data.frame(soil_in@data, merge(soil_in, soil_valu1, by.x = "MUKEY", by.y = "mukey", all.x = TRUE))

# write the soil data with the merged attributes
writeOGR(soil_in, dsn=soil_out_dir, layer=soil_out_shp, driver = "ESRI Shapefile", overwrite_layer=TRUE)

# now rasterize the desired soil carbon data to the same grid as the lfc data
# if overlapping polygons, then the last polygon is used as the value by default
# the unit conversion is gpmsq2Mgpha
soil_rast = lfc_rast
soil_rast[] = NA
soil_rast = rasterize(soil_in, soil_rast, field = soc_tot)

# fill the lfc table wth the soil carbon values
# soil_rast has to be the extact same grid as lfc_rast, and aligned geographically
lfc_carbon_df$soc_tot_sum = NA
lfc_carbon_df$soc_tot_cnt = 0
num_pix=length(lfc_rast)
for(p in 1:num_pix) {
	if(is.na(lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_sum"])){
		if(!is.na(soil_rast[p])){
			lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_sum"] = soil_rast[p]
			lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_cnt"] =
				lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_cnt"] + 1
		}
	}else {
		if(!is.na(soil_rast[p])){
			lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_sum"] =
				lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_sum"] + soil_rast[p]
			lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_cnt"] =
				lfc_carbon_df[lfc_carbon_df$ARB_LF_ID == lfc_rast[p], "soc_tot_cnt"] + 1
		}
	}
} # end for p loop over raster pixels
lfc_carbon_df$soc_tot_avg = lfc_carbon_df$soc_tot_sum / lfc_carbon_df$soc_tot_cnt
lfc_carbon_df$soc_tot_avg[lfc_carbon_df$soc_tot_avg == Inf] = NA
}

# update the biomass table to include the ag values for above ground
# replace the two grassland values with the max value for hay/pasture
# need to convert these values to biomass
ag_df = read.csv(ag_lut_csv, stringsAsFactors = FALSE)
ag_df[grep("Grassland", ag_df$EVT), "AG_MTCha"] = max(ag_df[grep("Hay", ag_df$EVT), "AG_MTCha"])
lfc_carbon_df[lfc_carbon_df$EVT_Name %in% ag_df$EVT, "above.main.biomass"] = ag_df[match(lfc_carbon_df[lfc_carbon_df$EVT_Name %in% ag_df$EVT, "EVT_Name"], ag_df$EVT),"AG_MTCha"] * carbon2biomass

# calculate the below ground values if necessary, and set negative values back to zero (only one negative value for ARB_LF_ID==27 in 2010)
zinds = which(lfc_carbon_df$below.main.biomass==0 & !is.na(lfc_carbon_df$below.main.biomass))
lfc_carbon_df[zinds,"below.main.biomass"] = lfc_carbon_df[zinds,"total.main.biomass"] - lfc_carbon_df[zinds,"above.main.biomass"]
lfc_carbon_df[zinds,"below.main.SE"] = sqrt((lfc_carbon_df[zinds,"total.main.SE"])^2 + (lfc_carbon_df[zinds,"above.main.SE"])^2)
ninds = which(lfc_carbon_df$below.main.biomass<0 & !is.na(lfc_carbon_df$below.main.biomass))
lfc_carbon_df[ninds,"below.main.biomass"] = 0
lfc_carbon_df[ninds,"below.main.SE"] = 0

# write the updated biomass table
write.csv(lfc_carbon_df, file=lfc_carbon_df_out, row.names=FALSE)

# the lfc data need to be ratified so that the tables can be linked and the data values can be exported
# ratify takes about 10 minutes
lfc_rast = ratify(lfc_rast)
all_rat = levels(lfc_rast)[[1]]
length(all_rat$ID)
length(lfc_carbon_df$ARB_LF_ID)
# 18, single-pixel ARB_LF_ID types were lost in reprojection for the 2010 data
# so keep only rows that are in both the raster and the table
# the resulting rat has no ARB_LF_ID column because it is the same as the rat ID column
# the all_rat table can be treated as a normal data frame after the merge
all_rat = merge(all_rat, lfc_carbon_df, by.x = "ID", by.y = "ARB_LF_ID")
levels(lfc_rast) = all_rat

# aggregated evt
# ~3 hours 45 minutes
#agg_evt_rast = deratify(lfc_rast, att = "agg_evt")
# ~18 minutes
#writeRaster(agg_evt_rast, filename=evt_agg_bil, datatype = "INT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)

# the table is in metric tons biomass per hectare
# carbon = 0.47*biomass

# above ground live main biomass - no urban
# the ag has been added here
# this should be the same as the provided raster, otherwise, except for reprojection issues
#cat("starting deratify at", date(), "\n")
#agc_rast = deratify(lfc_rast, att = "above.main.biomass")
#writeRaster(agc_rast, filename=agc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# total mass biomass - no urban, no ag (because root biomass is unknown)
#cat("starting deratify at", date(), "\n")
#tc_rast = deratify(lfc_rast, att = "total.mass")
#writeRaster(tc_rast, filename=tc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

### still use these SE values for what is available

# above ground live main biomass SE - no urban or ag
#cat("starting deratify at", date(), "\n")
#agc_se_rast = deratify(lfc_rast, att = "above.main.SE")
#writeRaster(agc_se_rast, filename=agc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# total mass biomass SE - no urban, no ag
#cat("starting deratify at", date(), "\n")
#tc_se_rast = deratify(lfc_rast, att = "total.mass.SE")
#writeRaster(tc_se_rast, filename=tc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

#### the rest of the carbon variables do not include ag or urban
# total dead is the sum of standing dead, down dead, and litter
# total main biomass is the sum of above and below main biomass
# total biomass is the sum of above main, below main, and understory

# below ground zero values have been replaced by the difference between the total main and the above main
#	with appropriate SE propgation

# below ground live tree carbon
#cat("starting deratify at", date(), "\n")
#bgc_rast = deratify(lfc_rast, att = "below.main.biomass")
#writeRaster(bgc_rast, filename=bgc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# below ground live tree carbon SE
#cat("starting deratify at", date(), "\n")
#bgc_se_rast = deratify(lfc_rast, att = "below.main.SE")
#writeRaster(bgc_se_rast, filename=bgc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# understory carbon
#cat("starting deratify at", date(), "\n")
#usc_rast = deratify(lfc_rast, att = "understory")
#writeRaster(usc_rast, filename=usc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# understory carbon SE
#cat("starting deratify at", date(), "\n")
#usc_se_rast = deratify(lfc_rast, att = "understory.SE")
#writeRaster(usc_se_rast, filename=usc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# dead standing carbon
#cat("starting deratify at", date(), "\n")
#dsc_rast = deratify(lfc_rast, att = "dead.standing")
#writeRaster(dsc_rast, filename=dsc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# dead standing carbon SE
#cat("starting deratify at", date(), "\n")
#dsc_se_rast = deratify(lfc_rast, att = "dead.standing.SE")
#writeRaster(dsc_se_rast, filename=dsc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# dead down carbon
#cat("starting deratify at", date(), "\n")
#ddc_rast = deratify(lfc_rast, att = "dead.down")
#writeRaster(ddc_rast, filename=ddc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# dead down carbon SE
#cat("starting deratify at", date(), "\n")
#ddc_se_rast = deratify(lfc_rast, att = "dead.down.SE")
#writeRaster(ddc_se_rast, filename=ddc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# litter carbon
#cat("starting deratify at", date(), "\n")
#ltc_rast = deratify(lfc_rast, att = "litter")
#writeRaster(ltc_rast, filename=ltc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# litter carbon SE
#cat("starting deratify at", date(), "\n")
#ltc_se_rast = deratify(lfc_rast, att = "litter.SE")
#writeRaster(ltc_se_rast, filename=ltc_se_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
#cat("finshed write at", date(), "\n")

# don't need the following because extraction takes too long
# do all this in grass
if(FALSE) {

# either do the state or county level aggregation
# need to add the area because the field didn't write correctly - check this
if (DOSTATE) {
	own_agg <- readOGR(dsn=own_agg_state_dir, layer=own_agg_state_shp)
} else {
	own_agg <- readOGR(dsn=own_agg_county_dir, layer=own_agg_county_shp)
}
own_agg@data$area_sqm = gArea(own_agg, byid = TRUE)

# i stopped this extraction at about 24 hours!

# extraction will be by lfc category, then these pixel weights can be merged with the tables for calculating the ownership aggregated values
# if the landfire evt data are desired, their categories need to be aggregated and merged with the lfc tables prior to this step (not implemented)
# get the fractions of cells within each polygon in a data frame (ID is the polygon, layer is the pixel value, weight is fraction or normalized frac)
# pixel fractions for area calcs
cat("starting extract at", date(), "\n")
own_agg_frac = extract(lfc_rast, own_agg, weights = TRUE, normalizeWeights = FALSE, df = TRUE)
cat("finish extract at", date(), "\n")
write.csv(own_agg_frac, file = pix_frac_csv, row.names=FALSE)
# normalized pixel fractions for carbon calcs
own_agg_norm = extract(lfc_rast, own_agg, weights = TRUE, normalizeWeights = TRUE, df = TRUE)
write.csv(own_agg_norm, file = pix_normfrac_csv, row.names=FALSE)

# merge the extracted tables with the data table
own_agg_frac

# first build the area tables

cat("starting area tables at", date(), "\n")

# calculate the area of each land type in each ownership, statewide
# get the fractions of cells within each polygon in a data frame (ID is the polygon, layer is the pixel value, weight is fraction)
#agg_vals = extract(evt_aggregate_rast, own_agg, weights = TRUE, normalizeWeights = FALSE, df = TRUE)
agg_vals$area_ha = agg_vals$weight
agg_area = aggregate(area_ha ~ ID + layer, l, FUN = function(x) sum(x*cell_area*msq2ha))
agg_area = merge(agg_area, evt_df_aggregate, by.x = "layer", by.y = "Value")
# need to check the "ID" column, but
agg_area = merge(agg_area, own_agg@data, by.x = "ID", by.y ="Group.1")
# then need to clean this up, otherwise, ready to output!
write.csv(agg_area, file = outdir_and_filename)

# sort example:
#agg_area[with(agg_area,order(EVT_Name,Group.1)),]
# change column order example:
#agg_area[,c("layer", "EVT_Name", "ID","area_ha")]
# change column name example:
#colnames(agg_area) <- c("Class_ID", "EVT_Name", "Own_ID","area_ha")

cat("Finished state area tables at", date(), "\n")

# calculate forest carbon densities from usfs data from state and county breakdowns

######################	
if(NEED_USFS_C_RAST) {
		
	####
	# read the usfs carbon rasters and resample to the ca projection and crop to ca
	#	these projections use bilinear interpolation
	usfs_totc_rast = raster(usfs_totc_img)
	usfs_totc_rast = projectRaster(usfs_totc_rast, lfc_rast)
	writeRaster(usfs_totc_rast, filename=usfs_totc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_agc_rast = raster(usfs_agc_img)
	usfs_agc_rast = projectRaster(usfs_agc_rast, lfc_rast)
	writeRaster(usfs_agc_rast, filename= usfs_agc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_bgc_rast = raster(usfs_bgc_img)
	usfs_bgc_rast = projectRaster(usfs_bgc_rast, lfc_rast)
	writeRaster(usfs_bgc_rast, filename= usfs_bgc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_ddc_rast = raster(usfs_ddc_img)
	usfs_ddc_rast = projectRaster(usfs_ddc_rast, lfc_rast)
	writeRaster(usfs_ddc_rast, filename= usfs_ddc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_ltc_rast = raster(usfs_ltc_img)
	usfs_ltc_rast = projectRaster(usfs_ltc_rast, lfc_rast)
	writeRaster(usfs_ltc_rast, filename= usfs_ltc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_sdc_rast = raster(usfs_sdc_img)
	usfs_sdc_rast = projectRaster(usfs_sdc_rast, lfc_rast)
	writeRaster(usfs_sdc_rast, filename= usfs_sdc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_soc_rast = raster(usfs_soc_img)
	usfs_soc_rast = projectRaster(usfs_soc_rast, lfc_rast)
	writeRaster(usfs_soc_rast, filename= usfs_soc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
	usfs_usc_rast = raster(usfs_usc_img)
	usfs_usc_rast = projectRaster(usfs_usc_rast, lfc_rast)
	writeRaster(usfs_usc_rast, filename= usfs_usc_bil, datatype = "FLT4S", overwrite=TRUE, NAflag = -9999, prj=TRUE)
	
} else {
	# read the usfs carbon rasters
	
	# these do not exist yet
	if(FALSE){
	usfs_totc_rast <- raster(usfs_totc_bil)
	usfs_agc_rast <- raster(usfs_agc_bil)
	usfs_bgc_rast <- raster(usfs_bgc_bil)
	usfs_ddc_rast <- raster(usfs_ddc_bil)
	usfs_ltc_rast <- raster(usfs_ltc_bil)
	usfs_sdc_rast <- raster(usfs_sdc_bil)
	usfs_soc_rast <- raster(usfs_soc_bil)
	usfs_usc_rast <- raster(usfs_usc_bil)
	}
}

} # end false for extraction and table processing

###################

######## process the fire data

# read in the frap perimeter data
# wildfire
wildfire <- readOGR(dsn=frap_fire_gdb, layer=frap_wildfire)
# prescribed fire
rx_fire <- readOGR(dsn=frap_fire_gdb, layer=frap_rxfire)

# this is aspatial processing for now
# so there could be double counting of overlapping burn area in the same year
wildfire_df = wildfire@data
rx_fire_df = rx_fire@data

wf_ann = aggregate(GIS_ACRES ~ YEAR_, wildfire_df, FUN=sum)
rf_ann = aggregate(GIS_ACRES ~ YEAR_, rx_fire_df, FUN=sum)
wf_ann$YEAR_ = as.numeric(as.character(wf_ann$YEAR_))
wf_ann$GIS_ACRES = as.numeric(wf_ann$GIS_ACRES)
rf_ann$YEAR_ = as.numeric(as.character(rf_ann$YEAR_))
rf_ann$GIS_ACRES = as.numeric(rf_ann$GIS_ACRES)

# Calc stats of 2000 - 2015, and convert to hectares
wf_ann_mean = mean(wf_ann[wf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
wf_ann_stddev = sd(wf_ann[wf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
wf_ann_min = min(wf_ann[wf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
wf_ann_max = max(wf_ann[wf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
rf_ann_mean = mean(rf_ann[rf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
rf_ann_stddev = sd(rf_ann[rf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
rf_ann_min = min(rf_ann[rf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
rf_ann_max = max(rf_ann[rf_ann$YEAR_ > 1999, "GIS_ACRES"], na.rm = TRUE) * acre2hectare

# calc stats for 2001-2010
mean(wf_ann[wf_ann$YEAR_ >= 2001 & wf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
sd(wf_ann[wf_ann$YEAR_ >= 2001 & wf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
min(wf_ann[wf_ann$YEAR_ >= 2001 & wf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
max(wf_ann[wf_ann$YEAR_ >= 2001 & wf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
mean(rf_ann[rf_ann$YEAR_ >= 2001 & rf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
sd(rf_ann[rf_ann$YEAR_ >= 2001 & rf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
min(rf_ann[rf_ann$YEAR_ >= 2001 & rf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare
max(rf_ann[rf_ann$YEAR_ >= 2001 & rf_ann$YEAR_ <= 2010, "GIS_ACRES"], na.rm = TRUE) * acre2hectare


######## process the calfire 'private' management/harvest data

# read the management/harvest data
#cf_thps <- readOGR(dsn= cf_harvest_dir, layer= cf_thps_shp)
#cf_ntmps <- readOGR(dsn= cf_harvest_dir, layer= cf_ntmps_shp)

######## these are for comparison with other carbon data

# also categorize by just national forest unit

# this comparison is extra:
# also by usfs, np, blm, other fed, state, local, private - this breaks out several of the 'protected' categories
#	and by reserved vs non-reserved - need the nwps data for this

#########








cat("finished proc_ca_spatial.r at", date())