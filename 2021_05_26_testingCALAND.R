#load XLConnect (which should also load its dependency rJava)
library(XLConnect)

# this enables java to use up to 16GB of memory for reading and writing excel files
options(java.parameters = "-Xmx32g" )  # originally set to "-Xmx8g" 

#load the write_caland_inputs function to memory
source("CALAND.r")

CALAND(
  scen_file_arg = "NWL_Proto_v1_Hist_protoA_hist_noOcean.xls", 
  c_file_arg = "2021_05_14_carbon_input_nwland_testB1_noOcean.xls", 
  indir = "2021_05_14_carbon_input_nwland_testB1",
  outdir = "2021_05_14_carbon_input_nwland_testB1", 
  start_year = 2016, 
  end_year = 2051, 
  value_col_dens = 7, 
  ADD_dens = TRUE, 
  value_col_accum = 7, 
  ADD_accum = TRUE, 
  value_col_soilcon=8, 
  ADD_soilcon = TRUE, 
  NR_Dist = 120, 
  WRITE_OUT_FILE = TRUE, 
  blackC = FALSE
)