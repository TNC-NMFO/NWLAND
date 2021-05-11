#load the write_caland_inputs function to memory
source("~/GitHub/NWLAND/write_caland_inputs.r")


# I don't think inline comments work in R, but we'll see..
#commented lines below do not have inputs for CO/NM yet
write_caland_inputs(
    c_file = "2021_05_11_carbon_input_nwland_test.xls", # output file name
    inputs_dir = "2021_05_11_carbon_input_nwland_test", # output file directory
#    parameter_file = "lc_params.xls",
#    scenarios_file = "nwl_scenarios_v6_ac.xls",
    units_scenario = "ha", # can be "ac" or "ha", we use hectares for NWLAND
    climate_c_file = "climate_c_scalars_nmco_null_2010_2100.csv", 
#    fire_area_file = "fire_area_canESM2_85_bau_2001_2100.csv",
    mortality_file = "mortality_annual_testing.csv",
    area_gis_files_new = "NWLAND_Area_Changes_2015_to_2050.csv",
    land_change_method = "Landuse_Avg_Annual",
#    carbon_gis_files = c("gss_soc_tpha_sp9_own9_2010lt15_stats.csv",
        "lfc_agc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_agc_tpha_sp9_own9_2010lt15_stats.csv", 
        "lfc_bgc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_bgc_tpha_sp9_own9_2010lt15_stats.csv", 
        "lfc_ddc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_ddc_tpha_sp9_own9_2010lt15_stats.csv", 
        "lfc_dsc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_dsc_tpha_sp9_own9_2010lt15_stats.csv", 
        "lfc_ltc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_ltc_tpha_sp9_own9_2010lt15_stats.csv", 
        "lfc_usc_se_tpha_sp9_own9_2010lt15_stats.csv", "lfc_usc_tpha_sp9_own9_2010lt15_stats.csv"),
#   scen_tag = "default",
    start_year = 2016,
    end_year = 2050,
    CLIMATE = "HIST",
#    forest_mort_fact = 2, #what does this do?
#    forest_mort_adj_first = 2015,
#    forest_mort_adj_last = 2024,
#    control_wildfire_lulcc_file = "individual_proposed_sims_control_lulcc_wildfire_aug2018.csv",
    control_wildfire_lulcc = FALSE
)

#why didnt this make it to the other workstation? am I not pushing/pulling to the correct repo?
