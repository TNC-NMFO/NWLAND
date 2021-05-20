#options(nwarnings = 10000)

#load XLconnect
library(XLConnect)

#load the write_caland_inputs function to memory
source("write_caland_inputs.r")

# regions
reg_names = c("C08001", "C08003", "C08005", "C08007", "C08009", "C08011", "C08013", "C08014", "C08015", "C08017", "C08019", "C08021", "C08023", "C08025", "C08027", "C08029", "C08031", "C08033", "C08035", "C08037", "C08039", "C08041", "C08043", "C08045", "C08047", "C08049", "C08051", "C08053", "C08055", "C08057", "C08059", "C08061", "C08063", "C08065", "C08067", "C08069", "C08071", "C08073", "C08075", "C08077", "C08079", "C08081", "C08083", "C08085", "C08087", "C08089", "C08091", "C08093", "C08095", "C08097", "C08099", "C08101", "C08103", "C08105", "C08107", "C08109", "C08111", "C08113", "C08115", "C08117", "C08119", "C08121", "C08123", "C08125", "C35001", "C35003", "C35005", "C35006", "C35007", "C35009", "C35011", "C35013", "C35015", "C35017", "C35019", "C35021", "C35023", "C35025", "C35027", "C35028", "C35029", "C35031", "C35033", "C35035", "C35037", "C35039", "C35041", "C35043", "C35045", "C35047", "C35049", "C35051", "C35053", "C35055", "C35057", "C35059", "C35061")
num_reg = length(reg_names)

write_caland_inputs(
    c_file = "2021_05_14_carbon_input_nwland_testAN.xls", # output file name
    inputs_dir = "2021_05_14_carbon_input_nwland_testAN", # output file directory
    parameter_file = "proto_lc_params.xls",
    scenarios_file = "proto_nwl_scenarios_v01_ha.xls",
    units_scenario = "ha", # can be "ac" or "ha", we use hectares for NWLAND
    climate_c_file = "climate_c_scalars_nmco_null_2010_2100.csv", 
    fire_area_file = "fire_area_RDS-2020-0016_averageannual_2001-2100.csv",
    mortality_file = "mortality_annual_testing.csv",
    area_gis_files_new = "NWLAND_Area_Changes_2015_to_2050.csv",
    land_change_method = "Landcover",
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
