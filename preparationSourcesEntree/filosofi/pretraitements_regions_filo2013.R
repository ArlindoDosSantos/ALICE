#################
#PRETRAITEMENTS #
#################

library(RSQLite)

setwd("V:/PSAR-AU/AR/ALICE/2_lissage/data_filo2013") #repertoire de travail

nomSource <- "filosofi"
millesime <- "2013"

liste_region <- c("02","04","11","24","27","28","32","44","52","53","75","76","84","93","94")
listeNomRegions <- c("Martinique","LaReunion","IleDeFrance", "CentreValDeLoire", "BourgogneFrancheComte", "Normandie", "HautsDeFrance", "GrandEst", "PaysDeLaLoire", "Bretagne", "NouvelleAquitaine", "Occitanie", "AuvergneRhoneAlpes", "ProvenceAlpesCoteAzur", "Corse")

load(file = "V:/PSAR-AU/AR/ALICE/1_preparation_base/data/filo_2013.Rdata")

for (i in 1:length(liste_region))
{
  region <- liste_region[i]
  basereg <- filo[filo$REG16 == region,]
  xmin <- min(basereg$x) - 50000
  xmax <- max(basereg$x) + 50000
  ymin <- min(basereg$y) - 50000
  ymax <- max(basereg$y) + 50000
  
  base <- filo[filo$x >= xmin & filo$x <= xmax & filo$y >= ymin & filo$y <= ymax,]
  base <- base[base$nivviem >= 0 & base$revdecm >= 0 & base$revdispm >= 0, ]
  
  #calcul revenu * population
  base$niv_x_pop <- base$nivviem * base$pop
  base$dis_x_pop <- base$revdispm * base$pop
  base$dec_x_pop <- base$revdecm * base$pop
  
  #exclusion des variables pas a lisser
  pourliss <- base[,!names(base) %in% c("REG16", "depcom", "nivviem", "revdispm", "revdecm")]
  
  #conversion en integer de toutes les colonnes
  pourliss <- as.data.frame(lapply(pourliss, as.integer))
  
  # ajout de la colonne nbObsLisse
  pourliss$nbObsLisse <- 1
  
  # sauvegarde RData
  chemin <- paste0("RData/", listeNomRegions[i], "/", nomSource)
  dir.create(chemin, recursive = TRUE)
  save(pourliss, file = paste0(chemin, "/", millesime, ".Rdata"))
  
  # sauvegarde SQLite
  chemin <- paste0("SQLite/", listeNomRegions[i], "/", nomSource)
  dir.create(chemin, recursive = TRUE)
  nomTable <- paste0(listeNomRegions[i], nomSource, millesime)
  sqlite <- DBI::dbDriver("SQLite")
  cnxBDD <- DBI::dbConnect(sqlite, paste0(chemin, "/", millesime, ".db"))
  dbWriteTable(cnxBDD, paste0(listeNomRegions[i], nomTable), pourliss, overwrite = TRUE)
  DBI::dbDisconnect(cnxBDD)
  
  #suppressions des dataframe temporaires
  rm(pourliss)
  rm(base)
  rm(basereg)
}


#tests sur les DOM
test <- get(load(file="V:/PSAR-AU/AR/ALICE/2_lissage/data_filo2013/RData/LaReunion/filosofi/2013.Rdata"))
martinique = filo[filo$REG16=="02",]
