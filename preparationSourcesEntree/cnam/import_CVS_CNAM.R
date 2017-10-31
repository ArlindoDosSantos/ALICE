source("U:/ALICE/preparationDesSources/regionaliser.R")
library(data.table)
library(RSQLite)

iMillesime <- 2016
repBases <- "D:/S3QCEA/bases/"

#### lecture du fichier csv ####
# colonnes attendues : nais sexe reg x93 y93 qualitexy
cnam2016 <- data.table::fread("C:/Users/S3QCEA/Desktop/cnam2016.csv", colClasses = c("integer", "integer", "integer", "integer", "integer", "factor"))
cnam2016 <- subset(cnam2016, select = -c(qualitexy))
colnames(cnam2016) <- c("nais", "sexe", "codeRegion", "x", "y")
gc()

#### suppression des lignes non géolocalisées ####
cnam2016 <- cnam2016[!is.na(x), ]
cnam2016 <- cnam2016[!is.na(y), ]
gc()

# suppression des observations sans année de naissance
cnam2016 <- cnam2016[!is.na(cnam2016$nais), ]

#### arrondi des coordonnées à l'unité ####
cnam2016$x <- as.integer(cnam2016$x)
cnam2016$y <- as.integer(cnam2016$y)
gc()

# séparer métropole et DOM
cnam2016DOM <- cnam2016[codeRegion < 7, ]
cnam2016Metropole <- cnam2016[codeRegion > 7, ]
rm(cnam2016)
gc()

# scinder en régions
regionaliser(dtBase = cnam2016Metropole, cNomSource = "cnam", iMillesime = iMillesime, cRepDestination = repBases)
regionaliser(dtBase = cnam2016DOM, cNomSource = "cnam", iMillesime = iMillesime, cRepDestination = "D:/S3QCEA/bases/")

rm(cnam2016Metropole)
rm(cnam2016DOM)
gc()

# ajouter les variables statistiques pour chacune des régions
iNoRegion <- 0
for(repRegion in list.files("D:/S3QCEA/bases/"))
{
  iNoRegion <- iNoRegion + 1
  cat(iNoRegion, "/", length(list.files("D:/S3QCEA/bases/")),  " - préparation ", repRegion, "\n")
  nomFichierBaseRegionCNAM <- paste0("D:/S3QCEA/bases/", repRegion, "/cnam/", iMillesime, ".db")
  if(file.exists(nomFichierBaseRegionCNAM))
  {
    sqlite <- DBI::dbDriver("SQLite")
    cnxBDD <- DBI::dbConnect(sqlite, nomFichierBaseRegionCNAM) 
    vNomTables <- DBI::dbListTables(cnxBDD)
    sRequete <- paste("select * from", vNomTables[1])
    dfRegionCNAM <- DBI::dbGetQuery(cnxBDD, sRequete)
    
    # préparation des variables statistique
    dfRegionCNAM$homme <- 0L
    dfRegionCNAM$femme <- 0L
    dfRegionCNAM[dfRegionCNAM$sexe == 1, "homme"] <- 1L
    dfRegionCNAM[dfRegionCNAM$sexe == 2, "femme"] <- 1L
    gc()
    
    # tranches d'âge
    dfRegionCNAM$age_0_5     <- 0L
    dfRegionCNAM$age_5_10    <- 0L
    dfRegionCNAM$age_10_15   <- 0L
    dfRegionCNAM$age_15_20   <- 0L
    dfRegionCNAM$age_20_25   <- 0L
    dfRegionCNAM$age_25_35   <- 0L
    dfRegionCNAM$age_35_45   <- 0L
    dfRegionCNAM$age_45_55   <- 0L
    dfRegionCNAM$age_55_65   <- 0L
    dfRegionCNAM$age_65_75   <- 0L
    dfRegionCNAM$age_plus75  <- 0L
    dfRegionCNAM$ageH_plus75 <- 0L
    dfRegionCNAM$ageF_plus75 <- 0L
    dfRegionCNAM[                                         (iMillesime - dfRegionCNAM$nais) <  5, "age_0_5"]     <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >=  5 & (iMillesime - dfRegionCNAM$nais) < 10, "age_5_10"]    <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 10 & (iMillesime - dfRegionCNAM$nais) < 15, "age_10_15"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 15 & (iMillesime - dfRegionCNAM$nais) < 20, "age_15_20"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 20 & (iMillesime - dfRegionCNAM$nais) < 25, "age_20_25"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 25 & (iMillesime - dfRegionCNAM$nais) < 35, "age_25_35"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 35 & (iMillesime - dfRegionCNAM$nais) < 45, "age_35_45"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 45 & (iMillesime - dfRegionCNAM$nais) < 55, "age_45_55"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 55 & (iMillesime - dfRegionCNAM$nais) < 65, "age_55_65"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 65 & (iMillesime - dfRegionCNAM$nais) < 75, "age_65_75"]   <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 75                                        , "age_plus75"]  <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 75 & dfRegionCNAM$sexe == 1               , "ageH_plus75"] <- 1L
    dfRegionCNAM[(iMillesime - dfRegionCNAM$nais) >= 75 & dfRegionCNAM$sexe == 2               , "ageF_plus75"] <- 1L
    dfRegionCNAM <- subset(dfRegionCNAM, select = -c(sexe))
    gc()
    
    # ajouter la colonne pour gérer le secret statistique
    dfRegionCNAM$nbObsLisse <- 1L
    
    dbWriteTable(cnxBDD, vNomTables[1], dfRegionCNAM, overwrite = TRUE)
    
    DBI::dbDisconnect(cnxBDD)
  }
}
