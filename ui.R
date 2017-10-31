options(encoding = "UTF-8")
#### Application ALICE (Application web de LIssage CartographiquE) - permet d'appeler le package de carroyage/lissage "btb"
#
# date        version         auteur                    commentaire
# 2016/10/27  0.0.1      Arlindo Dos Santos
# 2017/05/19  0.0.2      Arlindo Dos Santos
# 

WINDOWS <- "windows"
platformOS <- .Platform$OS.type

if (platformOS == WINDOWS)
  tmpDir <<- Sys.getenv("TMP") else
  tmpDir <<- Sys.getenv("R_SESSION_TMPDIR")

bibliotheque <- paste0(getwd(), "/", "packages")
.libPaths(bibliotheque)  # mettre dans le path le chemin où sont stockés les bibliothèques compilées

library(shiny, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(shinyjs, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(leaflet, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(DT, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rmarkdown, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

shinyUI(
  fluidPage(
    includeScript("www/scripts.js"),
    conditionalPanel(condition = "typeof output.autorisation === 'undefined' || output.autorisation == 'FALSE'", 
                     mainPanel(
                       textInput(inputId = "login", label = "Identifiant"),
                       passwordInput(inputId = "password", label = "Mot de passe"),
                       actionButton(inputId = "btnConnexion", label = "connexion", class = "btn-primary"),
                       textOutput(outputId = "messageConnexion")
                     )),
    conditionalPanel(condition = "output.autorisation == 'TRUE'", 
              navbarPage(
                title = "ALICE - Application de LIssage CartographiquE",
                tabPanel("Lissage",
                          sidebarPanel(
                            width = "3",
                            fileInput('inputFiles', "Carte (4 fichiers : dbf + shp + shx + prj) :", accept = c('.shp','.dbf','.sbx','.shx',".prj"), multiple = TRUE),
                            uiOutput('inputDatas'),
                            sliderInput("pas", "Longueur d'un carreau (en mètres) :", min = 200, max = 1000, value = 200, step = 50, animate = animationOptions(interval = 1000, loop = TRUE)),
                            sliderInput("sliderRayon", "Rayon de lissage (en mètres) :", min = 200, max = 20000, value = 400, step = 50, animate = animationOptions(interval = 1000, loop = TRUE)),
                            fluidRow(
                              column(width = 3, uiOutput("btnDynLisser")),
                              column(width = 6, checkboxInput("conserverZoom", "Conserver zoom", value = TRUE)),
                              column(width = 2, checkboxInput("afficheOSM", "OSM", value = TRUE))
                            ),
                            uiOutput("selectVar"),
                            htmlOutput("parametresUtilises"),
                            fluidRow(
                              column(width = 7, htmlOutput("avertissement")),
                              column(width = 2, uiOutput("telecharger"))
                            )
                          ),
                          mainPanel(leafletOutput("map", width = "115%", height = 780))
                 ),
                
                navbarMenu("Aide",
                           tabPanel("Présentation d'ALICE", HTML(readLines(rmarkdown::render("www/presentation.md", output_file = paste0(tmpDir, "/", "presentation.html"))))),
                           tabPanel("Précautions d'usage", HTML(readLines(rmarkdown::render("www/precautions.md", output_file = paste0(tmpDir, "/", "precautions.html"))))),
                           tabPanel("Lissage", HTML(readLines(rmarkdown::render("www/lissage.md", output_file = paste0(tmpDir, "/", "lissage.html"))))),
                           tabPanel("FAQ", HTML(readLines(rmarkdown::render("www/faq.md", output_file = paste0(tmpDir, "/", "faq.html")))))
                ),
                
                tabPanel("Secret stat", HTML(readLines(rmarkdown::render("www/secretStat.md", output_file = paste0(tmpDir, "/", "secretStat.html"))))),
                
                tabPanel("Variables",  DT::dataTableOutput("menuVariables")),
                navbarMenu("Version",
                           tabPanel("R", htmlOutput("versionDeR")),
                           tabPanel("Packages", DT::dataTableOutput("tableDesPackages"))
                ),
                tabPanel(
                          div(img(src = "logoInsee.svg", height = 20, width = 20, alt = "Insee"), "Auteurs"),
                           mainPanel(
                             "Le package BTB et l'interface ALICE ont étés proposés, conçus et réalisés par l'équipe PSAR Analyse Urbaine."
                             , br()
                             , "De gauche à droite : Thierry Cornely, François Sémécurbe, Cynthia Faivre, Auriane Renaud et Arlindo Dos Santos."
                             , br()
                             , "Pour toutes vos questions, vous pouvez envoyer un message à l'adresse "
                             , a(":DG75-PSAR-Analyse-Urbaine.", href = "mailto:DG75-PSAR-Analyse-Urbaine.")
                             , img(src = "psar.jpg", height = 500, width = 800, alt = "Psar")
                             , br()
                             , "Intranet de la division: "
                            , a("https://www.agora.insee.fr/cms/sites/intranetddar/home/acces-par-unite/departement-de-laction-regionale/division-statistiques-et-analyse.html", href = "https://www.agora.insee.fr/cms/sites/intranetddar/home/acces-par-unite/departement-de-laction-regionale/division-statistiques-et-analyse.html")
                           )
                 )
                 , tabPanel(htmlOutput("nomUser"))
      )
    )
  )
)
