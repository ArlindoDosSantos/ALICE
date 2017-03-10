options(shiny.maxRequestSize = 10 * 1024 ^ 2) # taille maximum pour le upload des fichiers fond de carte (10 Mo)
options(encoding = "UTF-8")
options(shiny.trace = FALSE)
options(shiny.reactlog = FALSE)
########################################################################################################################
#
# date        version         auteur                    commentaire
# 2016/10/27  0.0.1      Arlindo Dos Santos
# 
# RG 00: Toujours travailler en local avec une version de R identique à celle de REC et Prod afin de se prémunir de problèmes de compatibilité de versions de packages
# RG 01: Le fichier .DB doit comporter une seule table ayant le même nom que son fichier .DB => RG obsolete ?
# RG 02: Le fichier .DB comporte des coordonnées en Lambert 93  => RG obsolete ?
# RG 03: Le fichier .DB comporte une colonne nbObsLisse à 1 pour toutes les observations
# RG 04: Les carreaux ayant moins de NB_OBS_MIN observations ne sont pas affichés ni exportées (secret statistique)
# RG 05: les variables contenant au moins un NA dans leur modalité sont supprimées
# RG 06: les variables non numériques sont supprimées
# RG 07: Le nom du répertoire contenant les sources de données doit être identique au nom 
# RG 08: Seules les données qui ont un nbObsLisse > NB_OBS_MIN sont exportées
# RG 09: Seules les données qui ont un nbObsLisse > NB_OBS_MIN et les NB_MAX_CARREAUX carreaux ayant le plus grand nbObsLisse sont affichées
# RG 10: Les valeurs des categories sont arondies à l'unité; il faut donc multiplier les pourcentatges pour les mettre sur base 100
# RG 11: Catégorisation k-means pour des questions de performance
# RG 12: taille maximale du fichier fond de carte uploadé: 10 Mo
# RG 13: s'il y a des suppressions de colonnes à effectuer, elles seront déclarées dans le traitement post lissage
########################################################################################################################

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
library(log4r, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rgdal, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(sp, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(scales, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(leaflet, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(DT, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(classInt, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(btb, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

logger <- log4r::create.logger()

urlFondDeCarte <- 'http://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png'
cheminBasesDonnees <- "D:/bases/"

coucheQPVMetropole <- NULL

pbMessageParDefaut <- "Traitement en cours:"
pbDetailParDefaut <- "aucun"

nomFichierConfSource <- "sources.properties"
##################### fonctions commune à toutes les sessions ##########################################################

# fct permettant de lire un couple cle-valeur dans un fichier property
# inspiré de properties::read.properties mais modifié pour nos besoins 
fctReadProperty <- function(file, fields = NULL, encoding = "UTF-8") 
{
  if (is.character(file)) {
    file <- file(file, "r", encoding = encoding)
    on.exit(close(file))
  }
  
  if (!inherits(file, "connection")) 
    stop("'file' must be a character string or connection")
  
  lines <- readLines(file)
  commentedLines <- grepl("^#.*$", lines)
  lines <- lines[!commentedLines]
  line_is_not_empty <- !grepl("^[[:space:]]*$", lines)
  lines <- lines[line_is_not_empty]
  line_has_tag <- grepl("^[^[:blank:]][^=:]*:=", lines)
  
  tuples <- c()
  for (i in length(lines):1 )
  {
    if (!line_has_tag[i])
      lines[i - 1] <- paste0(lines[i - 1], ";", lines[i]) # le tuple se poursuit sur plusieurs lignes
    else
      tuples <- c(lines[i], tuples)
  }
  
  keys <- gsub("^([^:=]+):=.*$", "\\1", tuples)
  values <- gsub("^[^:=]+:=(.*)$", "\\1", tuples)
  names(values) <- keys
  out <- as.list(values)
  
  out <- if (!is.null(fields)) 
    out[fields]
  else out
  return(out)
}

contextualisation <- function()
{
  if (platformOS == WINDOWS)
  {
    # ceci n'est pas à faire sur les env de recette et prod car la commande zip existe par défaut sur ces environnements
    Sys.setenv(R_ZIPCMD = "D:/Program_Files/Rtools/bin/zip")
    
    # stop en debug en cas d'erreur
    options(shiny.error = browser)
    
    setwd("D:/programmation/R/ALICE") # local (debug)

    # output back to the console
    # sink(type = "message")
    # sink()
  }
  else
  {
    cheminBasesDonnees <<- paste0(getwd(), "/bases")

    # output to a file
    newStdout <- file(paste0(getwd(), "/logs/server.log"), open = "wt")
    sink(newStdout)
    sink(newStdout, type = c("output", "message"), split = TRUE)
  }

  cheminFichierQPV <- paste0(getwd(), "/QPV/QPV.shp")
  coucheQPVMetropole <<- readOGR(cheminFichierQPV, "QPV")
  coucheQPVMetropole <<- spTransform(coucheQPVMetropole, CRS = CRS("+init=epsg:4326")) # transformation en WGS84 pour le leaflet
  
  serverPropertyFileName <- paste0(getwd(), "/properties/server.properties")
  NB_MAX_CARREAUX <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("NB_MAX_CARREAUX"))[[1]])
  MAX_RAYON <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("MAX_RAYON"))[[1]])
  RAYON_INITIAL <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("RAYON_INITIAL"))[[1]])
  NB_OBS_MIN <<- as.numeric(fctReadProperty(serverPropertyFileName, fields = c("NB_OBS_MIN"))[[1]])
  logLevel <- fctReadProperty(serverPropertyFileName, fields = c("log4R.level"))
  
  log4r::logfile(logger) <<- paste0(getwd(), "/logs/server.log")
  log4r::level(logger) <<- logLevel[[1]]
  
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

contextualisation()

####################### fonctions propre à chaque session ##############################################################
shinyServer(
  function(input, output, session)
  {
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
        progress$set(value = 0, detail = message)
      }
      else
      {
        progress$set(value = (value / 100), detail = message)
      }
    }
    
    ####### affichage des packages chargés en mémoire #######
    output$tableDesPackages <- DT::renderDataTable({
      cat("\n ############ affichage des packages chargés en mémoire ############\n")
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
    output$outputSliderRayon <- renderUI({
      cat("\n ############ affichage du curseur pour le rayon de lissage ############\n")
      if (is.null(input$sliderRayon))
        rayonCourant <- RAYON_INITIAL
      else if (input$sliderRayon < updateRayonMin())
        rayonCourant <- updateRayonMin()
      else if (input$sliderRayon > updateRayonMax())
        rayonCourant <- updateRayonMax()
      else 
        rayonCourant <- input$sliderRayon

      min <- updateRayonMin() + 50 - updateRayonMin() %% 50
      sliderInput("sliderRayon", "Rayon de lissage (en mètres) :", min = min, max = updateRayonMax(), value = rayonCourant, step = 50)
    })

    ####### affichage du sélecteur de base de donnée #######
    observeEvent(input$inputFiles,
     {
       cat("\n ############ affichage du sélecteur de base de donnée ############\n")
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
       cat("\n ############ sélection du territoire étudié ############\n")
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
       cat("\n recherche du système de projection cible pour le fond de carte fourni \n")
       distances <- lapply(vCentroideZones, rgeos::gDistance, territoire())
       index <- which.min(distances)
       epsg <- vZonesEPSG[[index]]
       epsg
     }
   )                                  
    
    ####### sélection base de données ######
    currentTable <- eventReactive(input$inputDatas, 
      {
        cat("\n ############ sélection base de données ############\n")
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

        log4r::info(logger, paste("Requête :", sRequete))
        progress$set(value = 0.5, detail = 'Sélection des données correspondant au fond de carte')

        tryCatch({
          debutRequete <- Sys.time()
          dfBase <- DBI::dbGetQuery(cnxBDD, sRequete)
          log4r::info(logger, paste0("Durée requête : ", round(Sys.time() - debutRequete), "s"))
        }, warning = function(w) {
        }, error = function(e) {
          message <- paste("Erreur lors de la lecture de la table. Requete:", sRequete, e, sep = "\n")
          log4r::error(logger, message)
          stop(message)
        }, finally = {
          progress$set(value = 0, detail = pbDetailParDefaut)
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
          log4r::warn(logger, message)

          # TODO: supprimer le bouton lisser
          return(NULL)
        }
        
        output$btnDynLisser <- renderUI({
          actionButton(inputId = "btnDynLisser", label = "Lisser", class = "btn-primary")
        })

        dfBase
      }
    )
    
    ####### lissage des données #######
    dfLisse <- eventReactive(input$btnDynLisser, {
      cat("\n ############ lissage des données ############\n")
      if (is.null(input$btnDynLisser))
       return(NULL)
     
     if (input$btnDynLisser == 0)
       return(NULL)

        debutLissage <- Sys.time()
        dfLisse <- btb::kernelSmoothing( dfObservations = currentTable()
                                      , cellSize = input$pas
                                      , bandwidth = input$sliderRayon
                                      , fUpdateProgress = updateProgress)
        log4r::info(logger, paste0("Durée lissage : ", round(Sys.time() - debutLissage), "s"))
        
        nbObsAvantElagage <- length(dfLisse$nbObsLisse)
        dfLisse <- dfLisse[dfLisse$nbObsLisse >= NB_OBS_MIN, ]
        log4r::info(logger, paste("Elagage pour secret - nb carreaux supprimés:", nbObsAvantElagage - length(dfLisse$nbObsLisse), "- avant :", nbObsAvantElagage, "- après:", length(dfLisse$nbObsLisse), "- seuil:", NB_OBS_MIN, "obs - nb obs min:", min(dfLisse$nbObsLisse), "- nb obs max:", max(dfLisse$nbObsLisse)))
        
       # traitement post-lissage
        nomSource <- strsplit(as.character(input$inputDatas[1]), "/")[[1]][2]
        millesime <- strsplit(as.character(input$inputDatas[1]), "/")[[1]][3]
        cle <- paste0(nomSource, ".", millesime, ".traitementPostLissage")
        instructions <- fctReadProperty(paste0(getwd(), "/properties/", nomFichierConfSource), fields = c(cle))

       tryCatch({
         debutPostLissage <- Sys.time()
         eval(parse(text = instructions))
         log4r::info(logger, paste0("Durée post-lissage : ", round(Sys.time() - debutPostLissage), "s"))
       }, warning = function(w) {
       }, error = function(e) {
         message <- paste("Erreur lors de l'évaluation du traitement post-lissage. Ce traitement sera ignoré.\n", instructions, e, sep = "\n")
         session$sendCustomMessage(type = 'msgboxError', message = message)
          log4r::error(logger, message)
       }, finally = {
       })
       
       dfLisse
    }
    )

    ####### affichage de la liste de sélection pour la carte #######
    output$selectVar <- renderUI({
      cat("\n ############ affichage de la liste de sélection pour la carte ############\n")
      if (is.null(currentTable()) | is.null(dfLisse()))
        return(NULL)

      listeColonnes <- setdiff(colnames(dfLisse()), c("x", "y"))
      fluidRow( selectInput(inputId = "selectVar", label = "", choices = listeColonnes))
    })

    ####### affichage et traitement du bouton download #######
    observeEvent(input$selectVar,
     {
       cat("\n ############ affichage et traitement du bouton download ############\n")
       output$btnDownload <- downloadHandler(
        filename = function() { 
            # cat("\n ############ filename download ############\n")
            fond <- strsplit(basename(input$inputFiles[1, 1]), "\\.")[[1]][1]
            source <- as.character(input$inputDatas[1])
            source <- gsub("/", "_", source)
            pas <- input$pas
            rayon <- input$sliderRayon
            var <- input$selectVar
            nomArchiveSansExt <- paste(fond, source, pas, rayon, var, sep = '_') 
            nomArchiveAvecExt <- paste0(nomArchiveSansExt, ".zip") 
            log4r::info(logger, paste("Téléchargement de ", nomArchiveAvecExt))
            nomArchiveAvecExt
        },
        content = function(file) {
          # cat("\n ############ content download ############\n")
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
          
          # Remarque: on effectue un nouveau smoothingToGrid car on veut les carreaux éliminés par l'élagage de performance
          grid = btb::smoothingToGrid(grid = dfLisse, epsg = epsgCible(), fUpdateProgress = updateProgress)
          progress$set(value = 0, detail = pbDetailParDefaut)
          rgdal::writeOGR(grid, filePathNameLayer, nomVariableCourante, driver = "ESRI Shapefile", overwrite_layer = "TRUE")

          save(dfLisse, file = paste0(repCouche, "/", nomArchiveSansExt, ".RData"))
          liste_export <- paste0(nomArchiveSansExt, c(".dbf", ".shp", ".shx", ".prj", ".RData"))
          
          archiveZip <- zip(zipfile = file, files = liste_export)
          setwd(repTravailSauve)
          unlink(c(repCouche), recursive = TRUE)
          archiveZip
          },
        contentType = "application/zip"
       )
       output$avertissement <- renderUI({div("Attention ! Veillez à respecter le secret statistique avant toute diffusion.", style = "color:red")})
       output$telecharger <- renderUI({downloadButton('btnDownload', 'Télécharger')})
      }
    )

    ####### affichage des paramètres de lissage utilisés #######
    observeEvent(input$carteAffichee, {
                           cat("############ retour du serveur via javascript ############")
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
     
     log4r::info(logger, paste("Paramètres lissage - carte:", strsplit(basename(input$inputFiles[1, 1]), "\\.")[[1]][1], "- Source:", as.character(input$inputDatas[1]), "- pas:", input$pas, "- rayon:", input$sliderRayon))
     output$parametresUtilises <- renderUI({HTML(message)})
    })
    
    ####### calcul de la grille élaguée #######
    grilleElaguee <- reactive({
      cat("\n ############ calcul de la grille élaguee ############\n")
      dfLisse <- dfLisse()
      nbCarreauxAvantElagage <- length(dfLisse$nbObsLisse)
      if (length(dfLisse$nbObsLisse) > NB_MAX_CARREAUX)
      {
        vNbObsTrie <- sort(dfLisse$nbObsLisse, decreasing = TRUE)
        iLimite <- vNbObsTrie[NB_MAX_CARREAUX]
        dfLisse <- dfLisse[dfLisse$nbObsLisse >= iLimite, ]
      }
      log4r::info(logger, paste("Elagage pour performance - nb carreaux supprimés:", nbCarreauxAvantElagage - length(dfLisse$nbObsLisse), "- avant :", nbCarreauxAvantElagage, "- après:", length(dfLisse$nbObsLisse), "- seuil:", NB_MAX_CARREAUX, "carreaux - nb Obs min:", min(dfLisse$nbObsLisse), "- nb Obs max:", max(dfLisse$nbObsLisse)))
      
      grilleElaguee <- btb::smoothingToGrid(grid = dfLisse, epsg = epsgCible(), fUpdateProgress = updateProgress)   # L93
      grilleElaguee
    })
    
    ####### affichage de la carte WSG84 #######
    output$map <- renderLeaflet({
      cat("\n ############ affichage de la carte WSG84 ############\n")
      if ( is.null(input$selectVar) )
        return(NULL)

      progress$set(value = 0.10, detail = "Préparation de la couche lissée")
      fond <- grilleElaguee()   # L93
      fond <- spTransform(fond, CRS = CRS("+init=epsg:4326")) # transformation en WGS84 pour le leaflet
      # territoire <- spTransform(territoire(), CRS = CRS("+init=epsg:4326")) # transformation en WGS84 pour le leaflet

      progress$set(value = 0.20, detail = "Préparation catégories")
      variable <- paste0("fond@data$", input$selectVar)
      classeIntervalles <- classInt::classIntervals(eval(parse(text = variable)), style = "kmeans", n = 5)
      fond@data$colonneCouleurs <- findColours(classeIntervalles, vNuancesRouge)

      progress$set(value = 0.40, detail = "Sélection des QP")

      # Pour ne garder que les QP de la zone étudiée  - permet de ne plus avoir de "server disconnected" ou d'éloigner le seuil
      coucheQPV <- coucheQPVMetropole[fond, ]

      bboxshp <- bbox(fond)
      xMin <- bboxshp[1, 1]
      xMax <- bboxshp[1, 2]
      yMin <- bboxshp[2, 1]
      yMax <- bboxshp[2, 2]
      
      borneInfCategorie <- round(classeIntervalles$brks[1:(length(classeIntervalles$brks) - 1)])
      borneSupCategorie <- round(classeIntervalles$brks[2:length(classeIntervalles$brks)])
      labelCategories <- paste(borneInfCategorie, "-", borneSupCategorie)
      
      heureDebut <- Sys.time()
      progress$set(value = 0.60, detail = "Préparation de la carte")
      carte <-
        leaflet() %>% addTiles(urlTemplate = urlFondDeCarte)  %>%
          fitBounds(xMin, yMin, xMax, yMax) %>%
          # addRectangles(lng1 = xMin, lat1 = yMin, lng2 = xMax, lat2 = yMax, fill = FALSE, weight = 3) %>%
          addPolygons(data = fond, fillColor = ~colonneCouleurs, stroke = FALSE, smoothFactor = 1, fillOpacity = 0.7) %>%
          addPolygons(data = coucheQPV, stroke = TRUE, smoothFactor = 1, fillOpacity = 0, color = "black", weight = 1, opacity = 1) %>%
          addPolygons(data = territoire(), weight = 3, color = "black", opacity = 1, fillOpacity = 0) %>%
          addLegend(colors = vNuancesRouge, values = eval(parse(text = variable)), position = "bottomleft", title = "Catégories", labels = labelCategories, opacity = 0.7)
      
      # pour conserver le niveau de zoom et la position de la carte
      isolate(
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
      )
      
      log4r::info(logger, paste0("Durée préparation carte : ", round(Sys.time() - heureDebut), "s"))
      progress$set(value = 0.80, detail = "Préparation de l'affichage par votre navigateur")

      carte
    })
  }
)