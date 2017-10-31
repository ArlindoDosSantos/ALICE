tools::package_dependencies(c("btb"
                              , "shiny"
                              , "Shinyjs"
                              , "leaflet"
                              , "Rcpp"
                              , "RSQLite"
                              , "Cartography"
                              , "Properties"
                              , "log4r"
                              , "rgdal"
                              , "sp"
                              , "stringr"
                              , "DBI", recursive = TRUE
))

## LOCAL
cheminPackages <- "D:/programmation/R/shiny/packages/2017_01_17/"
cheminDestination <- "D:/programmation/R/tmp"
listePackages <- list.files(path = cheminPackages, recursive = TRUE)
listePackages <- paste0(cheminPackages, listePackages)
install.packages(listePackages, cheminDestination, type = "source", repos = NULL)

couche75_L93 <- rgdal::readOGR("U:/fonds_de_cartes/communes/dep75_L93/ComD75.shp", "ComD75")
couche75_L2 <- sp::spTransform(couche75_L93, CRS = sp::CRS("+init=epsg:27572")) # transformation en WGS84 pour le leaflet
rgdal::writeOGR(couche75_L2, "D:/temp/ComD75_L2.shp", "QPV", driver = "ESRI Shapefile", overwrite_layer = "TRUE")


tools::package_dependencies("btb", recursive = TRUE)  # Rcpp
tools::package_dependencies("htmlwidgets", recursive = TRUE) # Rcpp
tools::package_dependencies("raster", recursive = TRUE)# Rcpp
tools::package_dependencies("plyr", recursive = TRUE)# Rcpp
tools::package_dependencies("scales", recursive = TRUE)# Rcpp
tools::package_dependencies("leaflet", recursive = TRUE)# Rcpp
tools::package_dependencies("shiny", recursive = TRUE)# Rcpp
tools::package_dependencies("miniUI", recursive = TRUE)# Rcpp
tools::package_dependencies("shinyjs", recursive = TRUE)# Rcpp
tools::package_dependencies("RSQLite", recursive = TRUE)# Rcpp
tools::package_dependencies("cartography", recursive = TRUE)# Rcpp
tools::package_dependencies("dichromat", recursive = TRUE)
tools::package_dependencies("labeling", recursive = TRUE)
tools::package_dependencies("colorspace", recursive = TRUE)
tools::package_dependencies("munsell", recursive = TRUE)
tools::package_dependencies("maptools", recursive = TRUE)
tools::package_dependencies("sp", recursive = TRUE)
tools::package_dependencies("Rcpp", recursive = TRUE)
tools::package_dependencies("base64enc", recursive = TRUE)
tools::package_dependencies("yaml", recursive = TRUE)
tools::package_dependencies("jsonlite", recursive = TRUE)
tools::package_dependencies("digest", recursive = TRUE)
tools::package_dependencies("magrittr", recursive = TRUE)
tools::package_dependencies("mime", recursive = TRUE)
tools::package_dependencies("markdown", recursive = TRUE)
tools::package_dependencies("png", recursive = TRUE)
tools::package_dependencies("RColorBrewer", recursive = TRUE)
tools::package_dependencies("e1071", recursive = TRUE)
tools::package_dependencies("classInt", recursive = TRUE)
tools::package_dependencies("DBI", recursive = TRUE)
tools::package_dependencies("BH", recursive = TRUE)
tools::package_dependencies("R6", recursive = TRUE)
tools::package_dependencies("lattice", recursive = TRUE)
tools::package_dependencies("log4r", recursive = TRUE)
tools::package_dependencies("memoise", recursive = TRUE)
tools::package_dependencies("plogr", recursive = TRUE)
tools::package_dependencies("properties", recursive = TRUE)
tools::package_dependencies("rgdal", recursive = TRUE)
tools::package_dependencies("sourcetools", recursive = TRUE)
tools::package_dependencies("xtable", recursive = TRUE)
tools::package_dependencies("rgeos", recursive = TRUE)
tools::package_dependencies("abind", recursive = TRUE)
tools::package_dependencies("jpeg", recursive = TRUE)
tools::package_dependencies("rjson", recursive = TRUE)
tools::package_dependencies("codetools", recursive = TRUE)
tools::package_dependencies("iterators", recursive = TRUE)
tools::package_dependencies("foreach", recursive = TRUE)
tools::package_dependencies("rosm", recursive = TRUE)

# packages présents par défaut en recette
tools::package_dependencies("manipulate", recursive = TRUE)
tools::package_dependencies("rstudio", recursive = TRUE)
tools::package_dependencies("rgdal", recursive = TRUE)
tools::package_dependencies("sp", recursive = TRUE)
tools::package_dependencies("KernSmooth", recursive = TRUE)
tools::package_dependencies("MASS", recursive = TRUE)
tools::package_dependencies("Matrix", recursive = TRUE)
tools::package_dependencies("base", recursive = TRUE)
tools::package_dependencies("boot", recursive = TRUE)
tools::package_dependencies("class", recursive = TRUE)
tools::package_dependencies("cluster", recursive = TRUE)
tools::package_dependencies("codetools", recursive = TRUE)
tools::package_dependencies("compiler", recursive = TRUE)
tools::package_dependencies("datasets", recursive = TRUE)
tools::package_dependencies("foreign", recursive = TRUE)
tools::package_dependencies("grDevices", recursive = TRUE)
tools::package_dependencies("graphics", recursive = TRUE)
tools::package_dependencies("grid", recursive = TRUE)
tools::package_dependencies("lattice", recursive = TRUE)
tools::package_dependencies("methods", recursive = TRUE)
tools::package_dependencies("mgcv", recursive = TRUE)
tools::package_dependencies("nlme", recursive = TRUE)
tools::package_dependencies("nnet", recursive = TRUE)
tools::package_dependencies("parallel", recursive = TRUE)
tools::package_dependencies("rpart", recursive = TRUE)
tools::package_dependencies("spatial", recursive = TRUE)
tools::package_dependencies("splines", recursive = TRUE)
tools::package_dependencies("stats", recursive = TRUE)
tools::package_dependencies("stats4", recursive = TRUE)
tools::package_dependencies("survival", recursive = TRUE)
tools::package_dependencies("tcltk", recursive = TRUE)
tools::package_dependencies("tools", recursive = TRUE)
tools::package_dependencies("translations", recursive = TRUE)
tools::package_dependencies("utils", recursive = TRUE)


write(as.character(packageVersion("Rcpp")), file = "version.txt")
write(as.character(packageDescription("Rcpp")), file = "description.txt")

library(httpuv)
detach(package:httpuv)
detach(package:htmltools)
detach(package:shiny)
detach(package:Rcpp)

library(btb)
detach(package:btb)
packageVersion("btb")
find.package("btb")
getwd()
Sys.getenv("R_LIBS_USER")
Sys.setenv(R_LIBS_USER =  "D:\\Program_Files\\R\\R-3.3.1\\library")
D:\Program_Files\R\R-3.3.1\library


getwd()
setwd("D:/programmation/R/shiny/packagesZip/2017_01_17")
getwd()

.libPaths()
.libPaths("D:/programmation/R/shiny/packages")
.libPaths()

remove.packages("rgdal", "D:/programmation/R/shiny/packages")
remove.packages("btb", "D:/programmation/R/shiny/packages")

install.packages("D:/programmation/R/btb_0.1.14.tar.gz", lib = c("D:/programmation/R/shiny/packages"), type = "source", repos = NULL)
install.packages("D:/programmation/R/shiny/packagesZip/2017_01_17/rgdal_1.2-5.tar.gz", c("D:/programmation/R/shiny/packages"), type = "source", repos = NULL)
install.packages("D:/programmation/R/shiny/packagesZip/2017_01_17/log4r_0.2.tar.gz", c("D:/programmation/R/shiny/packages"), type = "source", repos = NULL)
