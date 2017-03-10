################ MODE 1 ##################################
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/QPV/QPV.shp", "QPV")))                                 # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/AU/Nantes/au_nantes_L93.shp", "au_nantes_L93")))       # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/communes/dep75_L93/ComD75.shp", "ComD75")))            # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/DOM/DepD971_region.shp", "DepD971_region")))           # 32620
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/DOM/DepD972_region.shp", "DepD972_region")))           # 32620
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/DOM/DepD973_region.shp", "DepD973_region")))           # 26922
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/DOM/DepD974_region.shp", "DepD974_region")))           # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/DOM/DepD976_region.shp", "DepD976_region")))           # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/fr_metro_regions", "france-metropolitaine-regions")))  # 4326
showEPSG(proj4string(readOGR("U:/fonds_de_cartes", "fraf")))                                            # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/UU", "UU10f")))                                        # OGRERR_UNSUPPORTED_SRS

showEPSG(proj4string(readOGR("U:/fonds_de_cartes/communes/dep35", "comdep35")))                         # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("U:/fonds_de_cartes/communes/dep35_L2e", "communesdep35")))                # OGRERR_UNSUPPORTED_SRS

showEPSG(proj4string(readOGR("D:/temp", "coucheADS")))                                                  # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("D:/temp", "coucheADS26922")))                                             # 26922
showEPSG(proj4string(readOGR("D:/temp", "coucheADSQGIS2154")))                                          # OGRERR_UNSUPPORTED_SRS
showEPSG(proj4string(readOGR("D:/programmation/R/shiny/packages/rgdal/vectors/cities.shp", "cities")))                                     # 4326 
showEPSG(proj4string(readOGR("D:/programmation/R/shiny/packages/rgdal/vectors/kiritimati_primary_roads.shp", "kiritimati_primary_roads"))) # 32604
showEPSG(proj4string(readOGR("D:/programmation/R/shiny/packages/rgdal/vectors/scot_BNG.shp", "scot_BNG")))                                 # OGRERR_UNSUPPORTED_SRS

showEPSG(proj4string(readOGR("Z:/france", "Payf_region")))                                              # OGRERR_UNSUPPORTED_SRS                        

################ MODE 2 ##################################
showEPSG(ogrInfo("U:/fonds_de_cartes/QPV", "QPV")["p4s"]$p4s)                                           # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/AU/Nantes", "au_nantes_L93")["p4s"]$p4s)                           # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/communes/dep75_L93", "ComD75")["p4s"]$p4s)                         # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/DOM", "DepD971_region")["p4s"]$p4s)                                # 32620
showEPSG(ogrInfo("U:/fonds_de_cartes/DOM", "DepD972_region")["p4s"]$p4s)                                # 32620
showEPSG(ogrInfo("U:/fonds_de_cartes/DOM", "DepD973_region")["p4s"]$p4s)                                # 26922
showEPSG(ogrInfo("U:/fonds_de_cartes/DOM", "DepD974_region")["p4s"]$p4s)                                # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/DOM", "DepD976_region")["p4s"]$p4s)                                # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/fr_metro_regions")["p4s"]$p4s)                                     # 4326
showEPSG(ogrInfo("U:/fonds_de_cartes", "fraf")["p4s"]$p4s)                                              # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/UU", "UU10f")["p4s"]$p4s)                                          # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("U:/fonds_de_cartes/arrondissement_paris_apur", "ARRONDISSEMENT")["p4s"]$p4s)          # OGRERR_UNSUPPORTED_SRS

showEPSG(ogrInfo("D:/temp", "coucheADS")["p4s"]$p4s)                                                         # OGRERR_UNSUPPORTED_SRS 
showEPSG(ogrInfo("D:/temp", "coucheADS26922")["p4s"]$p4s)                                                    # 26922 
showEPSG(ogrInfo("D:/temp", "coucheADSQGIS2154")["p4s"]$p4s)                                                 # OGRERR_UNSUPPORTED_SRS
showEPSG(ogrInfo("D:/programmation/R/shiny/packages/rgdal/vectors", "cities")["p4s"]$p4s)                    # 4326 
showEPSG(ogrInfo("D:/programmation/R/shiny/packages/rgdal/vectors", "kiritimati_primary_roads")["p4s"]$p4s)  # 32604
showEPSG(ogrInfo("D:/programmation/R/shiny/packages/rgdal/vectors", "scot_BNG")["p4s"]$p4s)                  # OGRERR_UNSUPPORTED_SRS

showEPSG(ogrInfo("Z:/france", "Payf_region")["p4s"]$p4s)                         # OGRERR_UNSUPPORTED_SRS

################ centroide DOM ##################################
# Metropole
couche <- rgdal::readOGR("U:/fonds_de_cartes/fr_metro_regions", "france-metropolitaine-regions")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox971 <- sp::bbox(couche)
centroideMetropole <- rgeos::gCentroid(couche)

# Guadeloupe
couche <- rgdal::readOGR("U:/fonds_de_cartes/DOM", "DepD971_region")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox971 <- sp::bbox(couche)
centroide971 <- rgeos::gCentroid(couche)

# Martinique
couche <- rgdal::readOGR("U:/fonds_de_cartes/DOM", "DepD972_region")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox972 <- sp::bbox(couche)
centroide972 <- rgeos::gCentroid(couche)

# Guyane
couche <- rgdal::readOGR("U:/fonds_de_cartes/DOM", "DepD973_region")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox973 <- sp::bbox(couche)
centroide973 <- rgeos::gCentroid(couche)

# Reunion
couche <- rgdal::readOGR("U:/fonds_de_cartes/DOM", "DepD974_region")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox974 <- sp::bbox(couche)
centroide974 <- rgeos::gCentroid(couche)

# Mayotte
couche <- rgdal::readOGR("U:/fonds_de_cartes/DOM", "DepD976_region")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox976 <- sp::bbox(couche)
centroide976 <- rgeos::gCentroid(couche)

rgeos::gDistance(centroideMetropole, centroideMetropole)
rgeos::gDistance(centroide971, centroide971)
rgeos::gDistance(centroide972, centroide972)
rgeos::gDistance(centroide973, centroide973)
rgeos::gDistance(centroide974, centroide974)
rgeos::gDistance(centroide976, centroide976)

centroideMetropole@coords
centroide971@coords
centroide972@coords
centroide973@coords
centroide974@coords
centroide976@coords

# Nantes
couche <- rgdal::readOGR("U:/fonds_de_cartes/AU/Nantes", "au_nantes_L93")
couche <- sp::spTransform(couche, CRSobj = sp::CRS("+init=epsg:4326"))  # transformation en WGS84
# bbox971 <- sp::bbox(couche)
centroideNantes <- rgeos::gCentroid(couche)

vecteurCentroides <- c(centroideMetropole, centroide971, centroide972, centroide973, centroide974, centroide976)
distances <- lapply(vecteurCentroides, rgeos::gDistance, centroideNantes)

v <- c(sp::SpatialPoints(matrix(c(2.55189, 46.55859), ncol = 2)))
distances <- lapply(v, rgeos::gDistance, centroideNantes)
