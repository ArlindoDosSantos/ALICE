options(encoding = "UTF-8")

bibliotheque <- paste0(getwd(), "/", "packages")
.libPaths(bibliotheque)  # mettre dans le path le chemin où sont stockés les bibliothèques compilées

library(shiny, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(shinyjs, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(leaflet, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(DT, lib.loc = bibliotheque, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

shinyUI(
  fluidPage(
    includeScript("www/scripts.js"),
    navbarPage(title = "ALICE - Application web de LIssage CartographiquE",
               tabPanel("Lissage",
                        
                        # parametres de lissage
                        sidebarPanel(
                          width = "3",
                          fileInput('inputFiles', "Fond de carte (fichiers dbf, shp, shx et prj) :", accept = c('.shp','.dbf','.sbx','.shx',".prj"), multiple = TRUE),
                          uiOutput('inputDatas'),
                          sliderInput("pas", "Longueur d'un carreau (en mètres) :", min = 200, max = 1000, value = 200, step = 50),
                          uiOutput("outputSliderRayon"),
                          uiOutput("btnDynLisser"),
                          uiOutput("selectVar"),
                          htmlOutput("parametresUtilises"),
                          htmlOutput("avertissement"),
                          uiOutput("telecharger")
                        ),
                        mainPanel(leafletOutput("map", width = "110%", height = 750))
               ),
               tabPanel("Présentation", mainPanel(includeMarkdown("www/sample.md"))),
               tabPanel("FAQ", mainPanel(includeMarkdown("www/sample.md"))),
               tabPanel("Packages utilisés", DT::dataTableOutput("tableDesPackages")),
               tabPanel("Version de R", htmlOutput("versionDeR")),
               tabPanel("Auteurs",
                         mainPanel(
                           "Le package BTB et l'interface ALICE ont étés proposés, conçus et réalisés par l'équipe PSAR Analyse Urbaine."
                           , br()
                           , "De gauche à droite : Thierry Cornely, François Sémécurbe, Cynthia Faivre, Auriane Renaud et Arlindo Dos Santos."
                           , br()
                           , "Pour toutes vos questions, vous pouvez envoyer un message à l'adresse "
                           , a(":DG75-PSAR-Analyse-Urbaine.", href = "mailto:DG75-PSAR-Analyse-Urbaine.")
                           , img(src = "psar.jpg", height = 500, width = 800)
                           , br()
                           , "Intranet de la division: "
                          , a("http://www.agora.insee.fr/jahia/Jahia/site/intranetddar/pid/104457", href = "http://www.agora.insee.fr/jahia/Jahia/site/intranetddar/pid/104457")
                         )
               )
              , tabPanel(img(src = "logoInsee.svg", height = 20, width = 20))
    )
  )
)
