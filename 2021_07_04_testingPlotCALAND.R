# this enables java to use up to 16GB of memory for reading and writing excel files
options(java.parameters = "-d64 -Xms50g -Xmx200g" )  # originally set to "-Xmx8g" 

install.packages('ggplot2')
install.packages('gtable')
install.packages('munsell')


#load the write_caland_inputs function to memory
source("plot_caland.r")

plot_caland(
  scen_fnames = c("NWL_Proto_v1_Hist_protoA_hist_output_mean_BC1_NR120.xls", 
                  "NWL_Proto_v1_Alt_A_protoA_hist_output_mean_BC1_NR120.xls"),
  scen_snames = c("Baseline","A"),
  data_dir = "./outputs/a",
  reg = c("All_region"), 
  lt = c("All_land", "Barren", "Cultivated", "Desert", "Developed_all", "Forest", "Grassland", "Ice", "Meadow", "Riparian", "Savanna", "Shrubland", "Sparse", "Water", "Wetland", "Woodland"),
  own = c("All_own"),
  figdir = "figures"
)