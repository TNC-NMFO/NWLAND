Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_291')
install.packages('devtools')

Sys.setenv(JAVA_HOME="")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_291')

install.packages('rJava')
require(devtools)
install_version("rJava", version = "0.9-13", repos = "http://cran.us.r-project.org")
install_version("XLConnect", version = "1.0.1", repos = "http://cran.us.r-project.org")
#may have worked but returns:
  #error: JAVA_HOME cannot be determined from the Registry


install.packages("XLConnect", type="source", INSTALL_opts = c("--no-multiarch"))

source("~/GitHub/NWLAND/write_caland_inputs.r")


#install Rtools

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")


install.packages("C:/Users/sbassett.TNC/Downloads/XLConnect.zip")




