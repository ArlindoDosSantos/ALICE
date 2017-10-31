options(encoding = "UTF-8")
options(shiny.maxRequestSize = 1 * 1024 ^ 2) # taille maximum pour le upload des fichiers fond de carte (1 Mo)
options(shiny.trace = FALSE)
options(shiny.reactlog = FALSE)
source("utils.R")

#### Application ALICE (Application web de LIssage CartographiquE) - permet d'appeler le package de carroyage/lissage "btb"
#
# date        version         auteur                    commentaire
# 2016/10/27  0.0.1      Arlindo Dos Santos
# 2017/05/19  0.0.2      Arlindo Dos Santos
# 
# RG 00: Toujours travailler en local avec une version de R identique à celle de REC et Prod afin de se prémunir de problèmes de compatibilité de versions de packages
# RG 01: Le fichier .DB doit comporter une seule table
# RG 02: Le fichier .DB doit comporter des coordonnées en Lambert 93  => RG obsolete ?
# RG 03: Le fichier .DB doit comporter une colonne nbObsLisse à 1 pour toutes les observations
# RG 04: Les carreaux ayant moins de NB_OBS_MIN observations ne sont ni affichés ni exportés (secret statistique)
# RG 05: Seules les carreaux ayant un nbObsLisse > NB_OBS_MIN et les NB_MAX_CARREAUX carreaux ayant le plus grand nbObsLisse sont affichés
# RG 06: Les variables contenant au moins un NA dans leur modalité sont supprimées
# RG 07: Les variables non numériques sont supprimées
# RG 08: Le nom du répertoire contenant les sources de données doit être identique au nom  => RG obsolete ?
# RG 09: Les valeurs des categories sont arondies à l'unité; il faut donc multiplier les pourcentatges pour les mettre sur base 100
# RG 10: Catégorisation k-means pour des questions de performance
# RG 11: Taille maximale du fichier fond de carte uploadé: 10 Mo (estimation empirique)
# RG 12: S'il y a des suppressions de colonnes à effectuer, elles seront déclarées dans le traitement post lissage
# RG 13: Ne pas utiliser de cat; lui préférer l'appel à la fonction trace définie dans utils.R
####

WINDOWS <- "windows"
platformOS <- .Platform$OS.type
NB_MAX_CARREAUX <- 5000
MAX_RAYON <- 4000
RAYON_INITIAL <- 400
NB_OBS_MIN <- 10
# vNuancesRouge <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")
vNuancesRouge <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")

bibliotheque <- paste0(getwd(), "/", "packages")
.libPaths(bibliotheque)  # mettre dans le path le chemin où sont stockées les bibliothèques compilées

library(shiny, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(DBI, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(RSQLite, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(cartography, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rgdal, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(sp, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(scales, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(leaflet, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(DT, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(classInt, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(btb, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

urlFondDeCarte <- 'http://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png'
cheminBasesDonnees <- "D:/S3QCEA/bases/"

coucheQPVMetropole <- NULL

pbMessageParDefaut <- "Traitement en cours:"
pbDetailParDefaut <- "aucun"

nomFichierConfSource <- "sources.properties"
##################### fonctions commune à toutes les sessions ##########################################################

contextualisation <- function()
{
  if (platformOS == WINDOWS)
  {
    # ceci n'est pas à faire sur les env de recette et prod car la commande zip existe par défaut sur ces environnements
    Sys.setenv(R_ZIPCMD = "D:/S3QCEA/Program_Files/Rtools/bin/zip")
    
    # stop en debug en cas d'erreur
    options(shiny.error = browser)
    
    setwd("D:/S3QCEA/programmation/R/ALICE") # local (debug)

    # output back to the console
    sink(type = "message")
    sink()
  
    # # output to a file
    # dateHeure <- substr(x = Sys.time(), start = 1, stop = 19)
    # dateHeure <- Sys.Date()
    # dateHeure <- gsub(pattern = ":", replacement = "_", x = dateHeure)
    # dateHeure <- gsub(pattern = " ", replacement = "_", x = dateHeure)
    # newStdout <- file(paste0(getwd(), "/logs/server_", dateHeure, ".log"), open = "a", encoding = "UTF-8", blocking = FALSE)
    # sink(newStdout)
    # sink(newStdout, type = "message", append = TRUE)
  }
  else
  {
    cheminBasesDonnees <<- paste0(getwd(), "/bases")

    # output to a file
    dateHeure <- substr(x = Sys.time(), start = 1, stop = 19)
    dateHeure <- Sys.Date()
    dateHeure <- gsub(pattern = ":", replacement = "_", x = dateHeure)
    dateHeure <- gsub(pattern = " ", replacement = "_", x = dateHeure)
    newStdout <- file(paste0(getwd(), "/logs/server_", dateHeure, ".log"), open = "a", encoding = "UTF-8", blocking = FALSE)
    sink(newStdout)
    sink(newStdout, type = "message", append = TRUE)
  }
  cheminFichierQPV <- paste0(getwd(), "/QPV/QPV.shp")
  coucheQPVMetropole <<- readOGR(cheminFichierQPV, "QPV")
  coucheQPVMetropole <<- spTransform(coucheQPVMetropole, CRS = CRS("+init=epsg:4326")) # transformation en WGS84 pour le leaflet
  
  serverPropertyFileName <- paste0(getwd(), "/properties/server.properties")
  NB_MAX_CARREAUX <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("NB_MAX_CARREAUX"))[[1]])
  MAX_RAYON <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("MAX_RAYON"))[[1]])
  RAYON_INITIAL <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("RAYON_INITIAL"))[[1]])
  NB_OBS_MIN <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("NB_OBS_MIN"))[[1]])
  traceLevel <<- fctReadProperty(serverPropertyFileName, fields = c("trace.level"))
  
  # calculer les centroides de toutes les zones
  epsgPivot <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("epsgPivot"))[[1]])
  nbZones <- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("nbZones"))[[1]])
  vNbZones <- seq(from = 1, to = nbZones)
  
  vCleZonesNom <- paste0("zone.", vNbZones, ".nom")
  vCleZonesEPSG <- paste0("zone.", vNbZones, ".epsg")
  vZonesNom <- as.character(mapply(fctReadProperty, serverPropertyFileName, vCleZonesNom))
  vZonesEPSG <<- as.character(mapply(fctReadProperty, serverPropertyFileName, vCleZonesEPSG))
  
  vCouches <- mapply(rgdal::readOGR, paste0(getwd(), "/zones"), vZonesNom)
  vCouches <- mapply(sp::spTransform, vCouches, paste0("+init=epsg:", epsgPivot))
  vCentroideZones <<- mapply(rgeos::gCentroid, vCouches)
}

getListUsersAutorises <- function()
{
  propertyFileName <- paste0(getwd(), "/properties/authentification.properties")
  fctReadProperty(propertyFileName)
}

contextualisation()
listeUsersAutorises <<- getListUsersAutorises()

####################### fonctions propre à chaque session ##############################################################
shinyServer(
  function(input, output, session)
  {
    # vérification de l'autorisation d'accès
    output$autorisation <- eventReactive(input$btnConnexion, {
      if (toupper(input$login) %in% toupper(names(listeUsersAutorises)))
      {
        index <- which(toupper(names(listeUsersAutorises)) == toupper(input$login))
        
        nom <- strsplit(x = listeUsersAutorises[[index]], split = ";")[[1]][1]
        motDePasseAttendu <- strsplit(x = listeUsersAutorises[[index]], split = ";")[[1]][2]

        if (input$password == motDePasseAttendu)
        {
          trace(traceLevel, "autorisation TRUE", "DEBUG")
          output$nomUser <- renderUI({div(nom, style = "font-weight: bold")})
          "TRUE"
        }else{
          trace(traceLevel, "autorisation FALSE", "DEBUG")
          output$messageConnexion <- renderText({"mot de passe incorrect"})
          "FALSE"
        }
      }
      else{
        trace(traceLevel, "autorisation FALSE", "DEBUG")
        output$messageConnexion <- renderText({"login inconnu"})
        "FALSE"
      }
    })
    outputOptions(output, "autorisation", suspendWhenHidden = FALSE) # pour forcer l'évaluation de la variable autorisation
    
    # affichage du menu des libellés des variables
    output$menuVariables <- DT::renderDataTable({

      propertyFileName <- paste0(getwd(), "/properties/sources.properties")
      iNbSources <- fctReadProperty(propertyFileName, "nbSources")[[1]]

      dfToutesSources <- data.frame(source = c(), variable = c(), descriptif = c(), stringsAsFactors = FALSE)

      for (iSource in 1:iNbSources)
      {
        nomSource <- fctReadProperty(propertyFileName, paste0("source.", iSource))[[1]]
        lLibelles <- fctReadProperty(propertyFileName, paste0(nomSource, ".variables"))[[1]]
        lLibelles <- gsub(pattern = "\t", replacement = "", x = lLibelles)
        vCleLibelles <- strsplit(x = lLibelles, split = ";")[[1]]

        vVariable <- unlist(lapply(1:length(vCleLibelles), function(i) {
          return(strsplit(x = vCleLibelles[[i]], split = "=>")[[1]][1])
        }))

        vDiffusible <- unlist(lapply(1:length(vCleLibelles), function(i) {
          return(strsplit(x = vCleLibelles[[i]], split = "=>")[[1]][2])
        }))
        
        vDescriptif <- unlist(lapply(1:length(vCleLibelles), function(i) {
          return(strsplit(x = vCleLibelles[[i]], split = "=>")[[1]][3])
        }))
        
        df <- data.frame(source = nomSource, variable = vVariable, diffusible = vDiffusible, descriptif = vDescriptif, stringsAsFactors = FALSE)

        dfToutesSources <- rbind(dfToutesSources, df)
      }

      DT::datatable(dfToutesSources)
    })
    
    # affichage du menu "version de R"
    output$versionDeR <- renderUI({
      message <- paste0("<tr>", "<td>", names(R.version), ": </td><td><b>", R.version, "</b></td></tr>")
      message <- toString(message)
      message <- gsub(", ", "", message)
      message <- paste0("<table>", message, "</table>")
      HTML(message)
      }
    )
    
    updateRayonMin <- eventReactive(input$pas, {ceiling(input$pas / 2 ^ 0.5)})
    updateRayonMax <- eventReactive(input$pas, {input$pas * 20})
    
    # le nouveau style ne fonctionne pas en recette; certainement dû à une autre version du package shiny 
    # qui serait chargée mais qui ne me serait pas accessible.
    # progress <- shiny::Progress$new(style = "notification")    
    progress <- shiny::Progress$new()
    progress$set(value = 0, message = pbMessageParDefaut, detail = pbDetailParDefaut)

    ####### fonction de mise à jour de l'avancée de la barre de progression #######
    updateProgress <- function(value = NULL, message = NULL) 
    {
      if (value == 1)
      { 
        progress$set(value = value / 100)
      }
      else if (value >= 99)
      {
        trace(traceLevel, message, "INFO")
        progress$set(value = 0, detail = message)
      }
      else
      {
        trace(traceLevel, message, "DEBUG")
        progress$set(value = (value / 100), detail = message)
      }
    }
    
    ####### affichage des packages chargés en mémoire #######
    output$tableDesPackages <- DT::renderDataTable({
      trace(traceLevel, "############ affichage des packages chargés en mémoire ############", "DEBUG")
      
      listePackages <- search()
      listePackages <- listePackages[listePackages != "Autoloads"]
      listePackages <- listePackages[listePackages != "tools:rstudio"]
      listePackages <- listePackages[listePackages != ".GlobalEnv"]
      listePackages <- gsub("package:", "", listePackages)
      listePackages <- sort(listePackages)

      packageDeBase <- function(nomPackage) 
      {
        if (is.null(packageDescription(nomPackage)))
          return(FALSE)
        
        if (is.null(packageDescription(nomPackage)$Priority))
          return(FALSE)
          
        if (packageDescription(nomPackage)$Priority != "base")
          return(FALSE)
          
          return(TRUE)
      }
      dfListePackages <- data.frame(nomPackage = listePackages, base = "", stringsAsFactors = FALSE)
      dfListePackages$base <- sapply(dfListePackages$nomPackage, packageDeBase)
      dfListePackages <- dfListePackages[dfListePackages$base == FALSE, ]
      
      if (platformOS == WINDOWS)
        DT::datatable(installed.packages()[dfListePackages$nomPackage, c("Package", "LibPath", "Version", "Depends", "NeedsCompilation", "Built")])
      else
		    DT::datatable(installed.packages(lib.loc = paste0(getwd(), "/packages"))[dfListePackages$nomPackage, c("Package", "LibPath", "Version", "Depends", "NeedsCompilation", "Built")])
    })
    
    ####### affichage du curseur pour le rayon de lissage #######
    observe({
      # utiliser le update plutôt que de refaire à chaque fois le slider permet d'éviter l'oscillation du slider
      if (is.null(input$sliderRayon))
        rayonCourant <- RAYON_INITIAL
      else if (input$sliderRayon < updateRayonMin())
        rayonCourant <- updateRayonMin()
      else if (input$sliderRayon > updateRayonMax())
        rayonCourant <- updateRayonMax()
      else
        rayonCourant <- input$sliderRayon
      
      min <- updateRayonMin() + 50 - updateRayonMin() %% 50
      updateSliderInput(session, "sliderRayon", value = rayonCourant, min = min, max = updateRayonMax(), step = 50)
    })

    ####### affichage du sélecteur de base de donnée #######
    observeEvent(input$inputFiles,
     {
       trace(traceLevel, "############ affichage du sélecteur de base de donnée ############", "DEBUG")

       # TODO: vérifier la présence des 4 fichiers
       output$inputDatas <- renderUI(
         {
           listeNomFichiersBases <- list.files(cheminBasesDonnees, pattern = ".db", recursive = TRUE)
           listeBases <- strsplit(listeNomFichiersBases, "\\.")
           vNomBases <- unlist(listeBases)
           vNomBases <- vNomBases[vNomBases != "db"]
           vNomBases <- cbind("",  vNomBases)
           
           selectInput('inputDatas', 'Source de données :', choices = vNomBases) 
         }
       )
     }
    )
    
    ####### sélection du territoire étudié #######
    territoire <- eventReactive(input$inputFiles, 
     {
       trace(traceLevel, "############ sélection du territoire étudié ############", "DEBUG")
        dir <- dirname(input$inputFiles[1, 4])   # pour récuperer le shp
        for (i in 1:nrow(input$inputFiles))      # on renomme les fichiers dans le répertoire temporaire du serveur
          file.rename(input$inputFiles[i, 4], paste0(dir, "/", input$inputFiles[i, 1]))
        
        shpFilename <- list.files(dir, pattern = "*.shp", full.names = TRUE)
        nomCouche <- strsplit(input$inputFiles[1, 1], "\\.")[[1]][1]
        
        shape <- readOGR(shpFilename, nomCouche)
        shape <- sp::spTransform(shape, CRSobj = CRS(paste0("+init=epsg:", epsgPivot)))
        
        shape
     }
    )
    
    ####### recherche du système de projection cible pour le fond de carte fourni ####### 
    epsgCible <- eventReactive(input$inputFiles, 
     {
       trace(traceLevel, "recherche du système de projection cible pour le fond de carte fourni", "DEBUG")
       distances <- lapply(vCentroideZones, rgeos::gDistance, territoire())
       index <- which.min(distances)
       epsg <- vZonesEPSG[[index]]
       epsg
     }
   )                                  
    
    ############ affichage du bouton Lisser ############
    observeEvent(input$inputDatas,
    {
      if (is.null(input$inputDatas) | input$inputDatas == "") 
        return(NULL)
      
      trace(traceLevel, "############ affichage du bouton Lisser ############", "DEBUG")
      output$btnDynLisser <- renderUI({
        actionButton(inputId = "btnDynLisser", label = "Lisser", class = "btn-primary")
      })
    })
    
    ####### sélection base de données ######
    currentTable <- eventReactive(input$inputDatas, 
      {
        trace(traceLevel, "############ sélection base de données ############", "DEBUG")
        if (is.null(input$inputDatas) | input$inputDatas == "")
          return(NULL)

        coucheTerritoire <- sp::spTransform(territoire(), CRSobj = CRS(paste0("+init=epsg:", epsgCible())))
        bboxshp <- bbox(coucheTerritoire)
        xMin <- bboxshp[1, 1]
        xMax <- bboxshp[1, 2]
        yMin <- bboxshp[2, 1]
        yMax <- bboxshp[2, 2]
        
        sqlite <- DBI::dbDriver("SQLite")
        cnxBDD <- DBI::dbConnect(sqlite, paste0(cheminBasesDonnees, "/", input$inputDatas, ".db"), flags = SQLITE_RO) 
        vNomTables <- DBI::dbListTables(cnxBDD)
        
        if (!exists("vNomTables"))
        {
          DBI::dbDisconnect(cnxBDD)
          message <- paste("Pas de table trouvée dans le fichier ", input$inputDatas)
          session$sendCustomMessage(type = 'msgboxError', message = message)
          warning(type = 'msgboxError', message = paste("Pas de table trouvée dans le fichier ", input$inputDatas))
          return(NULL)
        }
        
        if (length(vNomTables) != 1)
        {
          session$sendCustomMessage(type = 'msgboxError', message = paste("La base de données", vNomTables, " doit contenir une et une seule table."))
          DBI::dbDisconnect(cnxBDD)
          return(NULL)
        }
        
        # Filtrer les colonnes en fonction de ce qui est indiqué dans le fichier properties - ne pas oublier x et y
        nomSource <- strsplit(as.character(input$inputDatas[1]), "/")[[1]][2]
        millesime <- strsplit(as.character(input$inputDatas[1]), "/")[[1]][3]
        cle <- paste0(nomSource, ".", millesime, ".colonnesLissageClassique")
        listeColonnes <- fctReadProperty(paste0(getwd(), "/properties/", nomFichierConfSource), fields = c(cle))
        sRequete <- paste("select", listeColonnes, "from", vNomTables[1])
        sRequete <- paste(sRequete,  "where x >=", xMin, "and x <=", xMax, "and y >=", yMin, "and y<=", yMax)
        trace(traceLevel, paste("Requête :", sRequete))
        progress$set(value = 0.5, detail = 'Sélection des données correspondant au fond de carte')

        tryCatch({
          debutRequete <- Sys.time()
          dfBase <- DBI::dbGetQuery(cnxBDD, sRequete)
          trace(traceLevel, paste0("Durée requête : ", round(Sys.time() - debutRequete), "s\n"))
        }, warning = function(w) {
        }, error = function(e) {
          message <- paste("Erreur lors de la lecture de la table. Requete:", sRequete, e, sep = "\n")
          trace(traceLevel, message, "ERROR")
          stop(message)
        }, finally = {
          DBI::dbDisconnect(cnxBDD)
        })

        # suppression des colonnes ayant au moins un NA
        dfBase <- dfBase[ , colSums(is.na(dfBase)) == 0]
        
        # suppression des colonnes non numériques
        dfBase <- dfBase[ , sapply(dfBase, class) == "numeric" | sapply(dfBase, class) == "integer" ]
        
        if (nrow(dfBase) == 0)
        { 
          message <- paste("Aucune donnée ne correspond à l'intersection données / fond de carte")
          session$sendCustomMessage(type = 'msgboxError', message = message)
          trace(message, "WARN")
          
          # TODO: supprimer le bouton lisser
          return(NULL)
        }
        
        dfBase
      }
    )
    
    ####### lissage des données #######
    dfLisse <- eventReactive(input$btnDynLisser, {
      trace(traceLevel, "############ lissage des données ############", "DEBUG")
      if (is.null(input$btnDynLisser))
       return(NULL)
     
     if (input$btnDynLisser == 0)
       return(NULL)

        debutLissage <- Sys.time()
        dfLisse <- btb::kernelSmoothing( dfObservations = currentTable()
                                      , cellSize = input$pas
                                      , bandwidth = input$sliderRayon
                                      , fUpdateProgress = updateProgress)
        trace(traceLevel, paste0("Durée lissage : ", round(Sys.time() - debutLissage), "s"))
        
        nbObsAvantElagage <- nrow(dfLisse)
        dfLisse <- dfLisse[dfLisse$nbObsLisse >= NB_OBS_MIN, ]
        trace(traceLevel, paste("Elagage pour secret - nb carreaux supprimés:", nbObsAvantElagage - nrow(dfLisse), "- avant :", nbObsAvantElagage, "- après:", nrow(dfLisse), "- seuil:", NB_OBS_MIN, "obs - nb obs min:", min(dfLisse$nbObsLisse), "- nb obs max:", max(dfLisse$nbObsLisse)))
        
       # traitement post-lissage
        nomSource <- strsplit(as.character(input$inputDatas[1]), "/")[[1]][2]
        millesime <- strsplit(as.character(input$inputDatas[1]), "/")[[1]][3]
        cle <- paste0(nomSource, ".", millesime, ".traitementPostLissage")
        instructions <- fctReadProperty(paste0(getwd(), "/properties/", nomFichierConfSource), fields = c(cle))
        instructions <- gsub(pattern = "\t", replacement = "", x = instructions)

       tryCatch({
         debutPostLissage <- Sys.time()
         eval(parse(text = instructions))
         trace(traceLevel, paste0("Durée post-lissage : ", round(Sys.time() - debutPostLissage), "s"))
       }, warning = function(w) {
       }, error = function(e) {
         message <- paste("Erreur lors de l'évaluation du traitement post-lissage. Ce traitement sera ignoré.\n", instructions, e, sep = "\n")
         session$sendCustomMessage(type = 'msgboxError', message = message)
         trace(traceLevel, message, "ERROR")
       }, finally = {
       })
        
      dfLisse
    })

    ####### affichage et traitement du bouton download #######
    observeEvent(input$btnDynLisser,
     {
       trace(traceLevel, "############ affichage et traitement du bouton download ############", "DEBUG")
       output$btnDownload <- downloadHandler(
        filename = function() { 
            trace(traceLevel, "############ filename download ############", "DEBUG")
            fond <- strsplit(basename(input$inputFiles[1, 1]), "\\.")[[1]][1]
            source <- as.character(input$inputDatas[1])
            source <- gsub("/", "_", source)
            pas <- input$pas
            rayon <- input$sliderRayon
            nomArchiveSansExt <- paste(fond, source, pas, rayon, sep = '_') 
            nomArchiveAvecExt <- paste0(nomArchiveSansExt, ".zip") 
            trace(traceLevel, paste("Téléchargement de", nomArchiveAvecExt))
            nomArchiveAvecExt
        },
        content = function(file) {
          trace(traceLevel, "############ content download ############", "DEBUG")
          progress$set(value = 0.2, detail = "Préparation du téléchargement")
          dfLisse <- dfLisse()
          fond <- strsplit(basename(input$inputFiles[1, 1]), "\\.")[[1]][1]
          source <- as.character(input$inputDatas[1])
          source <- gsub("/", "_", source)  # TODO: regarder s'il y a moyen d'améliorer le code
          pas <- input$pas
          rayon <- input$sliderRayon
          nomVariableCourante <- input$selectVar
          nomArchiveSansExt <- paste(fond, source, pas, rayon, nomVariableCourante, sep = '_') 

          if (platformOS == WINDOWS)
            tmpDir <- Sys.getenv("TMP")
          else
            tmpDir <- Sys.getenv("R_SESSION_TMPDIR")
          
          repCouche <- paste0(tmpDir, "/", nomArchiveSansExt)
          if (!file.exists(repCouche))
          {
            dir.create(file.path(repCouche))
          }
          
          repTravailSauve <- getwd()
          setwd(repCouche)
          filePathNameLayer <- paste0(repCouche, "/", nomArchiveSansExt, ".shp")
          
          progress$set(value = 0.4, detail = "Préparation de la grille")
          rgdal::writeOGR(grille(), filePathNameLayer, nomVariableCourante, driver = "ESRI Shapefile", overwrite_layer = "TRUE")
          
          progress$set(value = 0.6, detail = "Préparation des données")
          save(dfLisse, file = paste0(repCouche, "/", nomArchiveSansExt, ".RData"))
          liste_export <- paste0(nomArchiveSansExt, c(".dbf", ".shp", ".shx", ".prj", ".RData"))
          
          progress$set(value = 0.8, detail = "Compression des données")
          archiveZip <- zip(zipfile = file, files = liste_export)
          setwd(repTravailSauve)
          unlink(c(repCouche), recursive = TRUE)
          trace(traceLevel, paste("Téléchargement - Taille du zip :", round(file.info(file)[, "size"]/1024), "Ko"), "INFO")

          progress$set(value = 0, detail = pbDetailParDefaut)
          archiveZip
          },
          contentType = "application/zip"
       )
       output$avertissement <- renderUI({div("Veillez à respecter le secret statistique avant diffusion.", style = "color:red")})
       
       # la version avec le javascript "onclick" fonctionne en local mais pas en recette, vraisemblablement à cause d'une différence sur la version du package shiny (comme pour aspect e la barre de progression)
       # output$telecharger <- renderUI({downloadButton(outputId = 'btnDownload', label = 'Télécharger', onclick = "return confirm('Je reconnais avoir lu et accepté les règles indiquées dans l onglet Secret statistique.');")})
       output$telecharger <- renderUI({downloadButton(outputId = 'btnDownload', label = 'Télécharger')})
      }
    )

    ####### affichage des paramètres de lissage utilisés #######
    observeEvent(input$carteAffichee, {
     trace(traceLevel, "############ retour du serveur via javascript ############", "DEBUG")
      if ( is.null(input$selectVar) )
        return(NULL)
      progress$set(value = 0, detail = pbDetailParDefaut)

     message <- paste("<b>Paramètres du lissage affiché</b>")
     message <- paste(message, "<pre>Fond de carte: \t\t<b>", strsplit(basename(input$inputFiles[1, 1]), "\\.")[[1]][1], "</b>")
     message <- paste(message, "<br>Source: \t\t<b>", as.character(input$inputDatas[1]), "</b>")
     message <- paste(message, "<br>Pas: \t\t\t<b>", input$pas, "</b>")
     message <- paste(message, "<br>Rayon: \t\t\t<b>", input$sliderRayon, "</b>")
     message <- paste(message, "<br>Min obs par carreau: \t<b>", NB_OBS_MIN, "</b>")
     message <- paste(message, "<br>Max carreaux affichés:\t<b>", NB_MAX_CARREAUX, "</b>")
     
     trace(traceLevel, paste("Paramètres lissage - carte:", strsplit(basename(input$inputFiles[1, 1]), "\\.")[[1]][1], "- Source:", as.character(input$inputDatas[1]), "- pas:", input$pas, "- rayon:", input$sliderRayon))
     output$parametresUtilises <- renderUI({HTML(message)})
    })
    
    ####### calcul de la grille #######
    grille <- reactive({
      trace(traceLevel, "############ calcul de la grille ############", "DEBUG")
      dfLisse <- dfLisse()

      # création de la grille
      grille <- btb::smoothingToGrid(grid = dfLisse, epsg = epsgCible(), fUpdateProgress = updateProgress)   # L93
      
      # Ne conserver que les carreaux réellement dans le territoire étudié
      nbCarreauxAvantElagageSecret <- nrow(grille)
      coucheTerritoire <- sp::spTransform(territoire(), CRSobj = CRS(paste0("+init=epsg:", epsgCible())))
      grille <- grille[coucheTerritoire, ]
      
      trace(traceLevel, paste0("Nb carreaux entre territoire et rectangle englobant: ", nbCarreauxAvantElagageSecret - nrow(grille)), "INFO")
      
      grille
    })
    
    ####### affichage de la liste de sélection pour la carte #######
    observeEvent(grille(),{
      trace(traceLevel, "############ affichage de la liste de sélection pour la carte ############", "DEBUG")
      listeColonnes <- setdiff(colnames(dfLisse()), c("x", "y", "nbObsLisse"))
      output$selectVar <- renderUI({fluidRow( selectInput(inputId = "selectVar", label = NULL, choices = listeColonnes))})
    })

    ####### affichage de la carte WSG84 #######
    output$map <- renderLeaflet({
      # observeEvent(input$selectVar, {
      trace(traceLevel, "############ affichage de la carte WSG84 ############", "DEBUG")

      if ( is.null(input$selectVar) )
        return(NULL)
      
      fond <- grille()   # L93
      
      # Elagage pour performances d'affichage
      progress$set(value = 0.05, detail = "élagage pour performance d'affichage")
      if ("nbObsLisse" %in% colnames(fond@data))
      {
        nbCarreauxAvantElagage <- nrow(fond)
        if (nbCarreauxAvantElagage > NB_MAX_CARREAUX)
        {
          fond <- fond[with(fond@data, order(-nbObsLisse)), ]
          limiteNbObs <- fond$nbObsLisse[NB_MAX_CARREAUX]
          fond <- fond[fond$nbObsLisse >= limiteNbObs, ]
          trace(traceLevel, paste("Elagage pour performance - nb carreaux supprimés:", nbCarreauxAvantElagage - nrow(fond), "- avant :", nbCarreauxAvantElagage, "- après:", nrow(fond), "- seuil:", NB_MAX_CARREAUX, "carreaux - nb Obs min:", min(fond$nbObsLisse), "- nb Obs max:", max(fond$nbObsLisse)))
        }
      }
      else
      {
        trace(traceLevel, "Pas d'élagage de performance car colonne nbObsLisse non trouvée.", "WARNING")
      }

      progress$set(value = 0.10, detail = "Préparation de la couche lissée")
      fond <- spTransform(fond, CRS = CRS("+init=epsg:4326")) # transformation en WGS84 pour le leaflet

      progress$set(value = 0.20, detail = "Préparation catégories")
      variable <- paste0("fond@data$", input$selectVar)
      
      # pour supprimer les cases dont le calcul a donné des valeurs non représentables (comme un sex ratio infini si n hommes / 0 femme)
      nbCasesAvant <- nrow(fond)
      fond <- fond[(eval(parse(text = variable))) != -Inf & (eval(parse(text = variable))) != Inf, ]
      if(nrow(fond) < nbCasesAvant)
        trace(traceLevel, paste(nbCasesAvant - nrow(fond), "cases supprimées car non représentables pour la variable", input$selectVar), niveau = "WARNING")
      
      classeIntervalles <- classInt::classIntervals(eval(parse(text = variable)), style = "kmeans", n = 5)
      fond@data$colonneCouleurs <- findColours(classeIntervalles, vNuancesRouge)

      progress$set(value = 0.40, detail = "Sélection des QP")

      # Pour ne garder que les QP de la zone étudiée  - permet de ne plus avoir de "server disconnected" ou d'éloigner le seuil
      coucheQPV <- coucheQPVMetropole[fond, ]

      bboxshp <- bbox(fond)
      xMin <- bboxshp["x", "min"]
      xMax <- bboxshp["x", "max"]
      yMin <- bboxshp["y", "min"]
      yMax <- bboxshp["y", "max"]
      
      borneInfCategorie <- round(classeIntervalles$brks[1:(length(classeIntervalles$brks) - 1)])
      borneSupCategorie <- round(classeIntervalles$brks[2:length(classeIntervalles$brks)])
      labelCategories <- paste(borneInfCategorie, "-", borneSupCategorie)
      
      # masquer les bornes extrêmes
      labelCategories[1] <- gsub(paste0(strsplit(labelCategories[1], " - ")[[1]][1], " - "), "- de ", labelCategories[1])
      labelCategories[length(classeIntervalles$brks) - 1] <- gsub(paste0(" - ", strsplit(labelCategories[length(classeIntervalles$brks) - 1], " - ")[[1]][2]), "", labelCategories[length(classeIntervalles$brks) - 1])
      labelCategories[length(classeIntervalles$brks) - 1] <- paste0("+ de ", labelCategories[length(classeIntervalles$brks) - 1])

      heureDebut <- Sys.time()
      progress$set(value = 0.60, detail = "Préparation de la carte")
      carte <-
        leaflet() %>% fitBounds(xMin, yMin, xMax, yMax) %>%
        # addRectangles(lng1 = xMin, lat1 = yMin, lng2 = xMax, lat2 = yMax, fill = FALSE, weight = 3) %>%
        addPolygons(data = fond, fillColor = ~colonneCouleurs, stroke = FALSE, fillOpacity = 0.7) %>%
        addPolygons(data = coucheQPV, fillOpacity = 0, color = "black", weight = 1, opacity = 1) %>%
        addPolygons(data = territoire(), weight = 3, color = "black", opacity = 1, fillOpacity = 0) %>%
        addLegend(colors = vNuancesRouge, values = eval(parse(text = variable)), position = "bottomleft", title = "Catégories", labels = labelCategories, opacity = 0.7)
      
      isolate(
      if (input$afficheOSM == TRUE)
        carte <- carte %>% addTiles(urlTemplate = urlFondDeCarte) %>% addTiles()
      )
      
      # pour conserver le niveau de zoom et la position de la carte
      isolate(
        if (input$conserverZoom == "TRUE")
        {
          if (!is.null((input$map_zoom)) & !is.null(input$map_bounds))
          {
            xMinZoom <- input$map_bounds$west
            xMaxZoom <- input$map_bounds$east
            yMinZoom <- input$map_bounds$south
            yMaxZoom <- input$map_bounds$north
            xCenter <- (xMaxZoom + xMinZoom) / 2
            yCenter <- (yMaxZoom + yMinZoom) / 2
            carte <- carte %>% setView(lng = xCenter, lat = yCenter, zoom = input$map_zoom)
          }
        }
      )
      
      trace(traceLevel, paste0("Durée préparation carte : ", round(Sys.time() - heureDebut), "s"), "DEBUG")
      progress$set(value = 0.80, detail = "Préparation de l'affichage par votre navigateur")

      debutGC <- Sys.time()
      gc()
      trace(traceLevel, paste("durée gc() :", Sys.time() - debutGC), "DEBUG")
      trace(traceLevel, paste("object.size(carte) :", round(object.size(carte) / 1048576, digits = 3), "Mo"), "DEBUG")
      trace(traceLevel, paste("memory.size() :", memory.size(), "Mo"), "DEBUG")
      trace(traceLevel, paste("memory.limit() :", memory.limit(), "Mo"), "DEBUG")

      # output$map <- renderLeaflet({carte})
      carte
    })
 
    ####### affichage de la couche openStreetMap
    observeEvent(input$afficheOSM, {
        if (input$afficheOSM == TRUE)
        {
          leafletProxy("map")  %>% addTiles(urlTemplate = urlFondDeCarte)  %>%
            addTiles()
        }
        else
        {
          leafletProxy("map") %>%
            clearTiles()
        }
      }
    )

  }
)