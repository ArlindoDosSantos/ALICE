options(encoding = "UTF-8")
# préparation des bases régionales à partir d'une base nationale pour utilisation dans ALICE
#
# en entrée : un Rdata de nom dtBase avec au moins les colonnes x, y
#               soit il y a déjà la colonne "codeRegion" renseignée (code région)
#               soit il y a une colonne "codePostal" ou "dep"
#
# en sortie : les données éclatées par région et sauvegardées dans le répertoire indiqué au format SQLlite 
#             (on déborde à chaque fois de 50 km)
#
# Attention : Si les données existent déjà dans le répertoire destination, elles sront écrasées.

# exemple d'appel
#
# regionaliser(    dtBase = dtBase
#                , nomSource = "cnaf"
#                , iMillesime = 2015
#                , repDestination = "D:/S3QCEA/CNAF/sources/basesRegionales/"
#                , iDistanceDebordement = 50000)

regionaliser <- function(dtBase, cNomSource, iMillesime, cRepDestination, iDistanceDebordement = 50000)
{
  # Vérification de validité de la base en entrée
  if (!exists("dtBase"))
    stop("L'objet dtBase n'a pas été trouvé")
  
  if (!("x" %in% colnames(dtBase)) | !("y" %in% colnames(dtBase)))
    stop("L'objet dtBase ne contient pas de coordonnées")
  
  if (!("codeRegion" %in% colnames(dtBase)) & !("codePostal" %in% colnames(dtBase)) & !("dep" %in% colnames(dtBase)))
    stop("L'objet dtBase ne contient pas de colonne codePostal")
  
  if (!("codeRegion" %in% colnames(dtBase)))
  {
    # association codes département/région pour la métropole
    dfAssocDepReg <- data.frame(reg = "11", dep = c("75", "77", "78", "91", "92", "93", "94", "95")) # ile de france
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "52", dep = c("44", "49", "53", "72", "85"))) # Pays de la Loire
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "53", dep = c("22", "29", "35", "56"))) # Bretagne
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "27", dep = c("21", "25", "39", "58", "70", "71", "89", "90"))) # Bourgogne-Franche-Comté 
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "94", dep = c("2A", "2B"))) # Corse
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "24", dep = c("18", "28", "36", "37", "41", "45"))) # Centre-Val de Loire
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "75", dep = c("16", "17", "19", "23", "24", "33", "40", "47", "64", "79", "86", "87"))) # Nouvelle-Aquitaine
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "28", dep = c("14", "27", "50", "61", "76"))) # Normandie
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "76", dep = c("09", "11", "12", "30", "31", "32", "34", "46", "48", "65", "66", "81", "82"))) # Occitanie
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "44", dep = c("08", "10", "51", "52", "54", "55", "57", "67", "68", "88"))) # Grand Est
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "93", dep = c("04", "05", "06", "13", "83", "84"))) # Provence-Alpes-CÃ´te d'Azur
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "32", dep = c("02", "59", "60", "62", "80"))) # Hauts-de-France
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "84", dep = c("01", "03", "07", "15", "26", "38", "42", "43", "63", "69", "73", "74"))) # Auvergne-RhÃ´ne-Alpes
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "01", dep = c("971"))) # Guadeloupe
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "02", dep = c("972"))) # Martinique
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "03", dep = c("973"))) # Guyane
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "04", dep = c("974"))) # La Réunion
    dfAssocDepReg <- rbind(dfAssocDepReg, data.frame(reg = "06", dep = c("976"))) # Mayotte
    
    if (!("dep" %in% colnames(dtBase)))
    {
      dtBase[, "dep"] <- substr(dtBase$codePostal, 1, 3)
      dtBase[substr(dtBase$codePostal, 1, 2) != "97", "dep"] <- substr(dtBase[substr(dtBase$codePostal, 1, 2) != "97", "dep"], 1, 2)
    }
  
    # enrichissement de chaque observation avec son code région
    dtBase[, "codeRegion"] <- dfAssocDepReg$reg[match(dtBase$dep, dfAssocDepReg$dep)]
    
    dtBase <- subset(dtBase, select = -c(dep, codePostal))
    rm(dfAssocDepReg)
  }
  
  # vecteurs des régions
  vCodeRegions <- c("01","02","03","04","06","11","24","27","28","32","44","52","53","75","76","84","93","94")
  vNomRegions <- c("Guadeloupe","Martinique","Guyane","Reunion","Mayotte","IleDeFrance", "CentreValDeLoire", "BourgogneFrancheComte", "Normandie", "HautsDeFrance", "GrandEst", "PaysDeLaLoire", "Bretagne", "NouvelleAquitaine", "Occitanie", "AuvergneRhoneAlpes", "ProvenceAlpesCoteAzur", "Corse")
  
  library(RSQLite)
  
  for (i in 1:length(vCodeRegions))
  {
    baseReg <- dtBase[dtBase$codeRegion == vCodeRegions[i], ]
    xmin <- min(baseReg$x) - iDistanceDebordement
    xmax <- max(baseReg$x) + iDistanceDebordement
    ymin <- min(baseReg$y) - iDistanceDebordement
    ymax <- max(baseReg$y) + iDistanceDebordement
    rm(baseReg)
    gc()

    baseRegEtendue <- dtBase[dtBase$x >= xmin & dtBase$x <= xmax & dtBase$y >= ymin & dtBase$y <= ymax, ]
    baseRegEtendue <- subset(x = baseRegEtendue, select = -c(codeRegion))
  
    if (nrow(baseRegEtendue) > 0)
    {
      # sauvegarde au format SQLite
      chemin <- paste0(cRepDestination, vNomRegions[i], "/", cNomSource, "/")
      dir.create(chemin, recursive = TRUE)
      nomTable <- paste0(vNomRegions[i], cNomSource, iMillesime)
      sqlite <- DBI::dbDriver("SQLite")
      cnxBDD <- DBI::dbConnect(sqlite, paste0(chemin, iMillesime, ".db"))
      dbWriteTable(cnxBDD, nomTable, baseRegEtendue, overwrite = TRUE)
      DBI::dbDisconnect(cnxBDD)  
  
      cat(i , "/", length(vCodeRegions), " - Enregistrement base régionale", paste0(chemin, iMillesime, ".db"), "(", nrow(baseRegEtendue), "observations)", "\n")
      
      rm(baseRegEtendue)
      gc()
    }
    else
    {
      warning("pas de données pour la région ", vNomRegions[i], "\n")
    }
  }
}

# # test fonction
# library(data.table)
# base <- data.table(x = c(1, 2), y = c(5, 6), codePostal = c("92240", "97314"))
# 
# regionaliser(  dtBase = base
#              , cNomSource = "cnaf"
#              , iMillesime = 2015
#              , cRepDestination = "D:/S3QCEA/CNAF/sources/basesRegionales/"
#              , iDistanceDebordement = 50000)
