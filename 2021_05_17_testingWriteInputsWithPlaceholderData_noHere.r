#load XLconnect
library(XLConnect)

#load the write_caland_inputs function to memory
source("write_caland_inputs.r")

write_caland_inputs(
    c_file = "2021_05_14_carbon_input_nwland_testS.xls", # output file name
    inputs_dir = "2021_05_14_carbon_input_nwland_testS", # output file directory
    parameter_file = "proto_lc_params.xls",
    scenarios_file = "proto_nwl_scenarios_v01_ha.xls",
    units_scenario = "ha", # can be "ac" or "ha", we use hectares for NWLAND
    climate_c_file = "climate_c_scalars_nmco_null_2010_2100.csv", 
    fire_area_file = "fire_area_RDS-2020-0016_averageannual_2001-2100.csv",
    mortality_file = "mortality_annual_testing.csv",
    area_gis_files_new = "NWLAND_Area_Changes_2015_to_2050.csv",
    land_change_method = "Landuse_Avg_Annual",
    area_gis_files_orig = c("proto_area2001_sqm_stats.csv", "proto_area2016_sqm_stats.csv"),
    carbon_gis_files = c("soc_tpha_2017_stats.csv",
        "inventory_proto_agc_tpha.csv", 
        "inventory_proto_bgc_tpha.csv", 
        "inventory_proto_ddc_tpha.csv", 
        "inventory_proto_dsc_tpha.csv", 
        "inventory_proto_ltc_tpha.csv", 
        "inventory_proto_usc_tpha.csv"),
    scen_tag = "protoA",
    start_year = 2016,
    end_year = 2050,
    CLIMATE = "HIST",
    forest_mort_fact = 2, #what does this do?
    forest_mort_adj_first = 2015,
    forest_mort_adj_last = 2024,
    control_wildfire_lulcc_file = "individual_proposed_sims_control_lulcc_wildfire_aug2018.csv", #not used becuse following line is false
    control_wildfire_lulcc = FALSE
)
