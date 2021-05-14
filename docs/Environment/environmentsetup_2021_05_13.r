install.packages('devtools')
install.packages('rJava')
require(devtools)
#install_version("rJava", version = "0.9-13", repos = "http://cran.us.r-project.org")
install_version("XLConnect", version = "1.0.1", repos = "http://cran.us.r-project.org")
#may have worked but returns:
  #error: JAVA_HOME cannot be determined from the Registry