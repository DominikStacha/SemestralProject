library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Analýza dat"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    includeCSS("assets/style.css"),
    tabItems(
      tabItem(tabName = "fileTab",
              wellPanel(
                fileInput("inputFile", "Vyberte soubor CSV", placeholder = "Soubor nebyl vybrán", accept = ".csv"),
                checkboxInput("hasHeader", "Soubor obsahuje hlavičku", TRUE)
              )
      ),

      tabItem(tabName = "summaryTab",
              wellPanel(
                # tableOutput("summary"),
                # tableOutput("frequency")
                uiOutput("summary")
              )
      ),

      tabItem(tabName = "dataTableTab",
              wellPanel(
                dataTableOutput("dataTable")
              )
      ),

      tabItem(tabName = "missingDataTab",
              wellPanel(
                tableOutput("missingDataTable"),
                htmlOutput("missingDataReport"),
                br(),
                radioButtons("missingValueAction", "Chybějící hodnoty:", list(
                  "Vynechat" = "omit",
                  "Nahradit průměrnou hodnotou" = "average"
                ))
              )
      ),

      tabItem(tabName = "clusteringTab",
              wellPanel(
                selectInput("clusteringAlgo", "Algoritmus:", list(
                  "K-means" = "kmeans",
                  "Hierarchické aglomerativní shlukování" = "hierarchicalClustering"
                )),
                uiOutput("clusteringUI"),
                actionButton("doClustring", "Spustit"),
                htmlOutput("clusteringError")
              ),
              uiOutput("clusteringResult")
      ),

      tabItem(tabName = "classificationTab",
              wellPanel(
                selectInput("classificationAlgo", "Algoritmus:", list(
                  "Naive Bayes classifier" = "bayes",
                  "k-nearest neighbors" = "knn"
                )),
                uiOutput("classificationUI"),
                actionButton("doClassification", "Spustit"),
                htmlOutput("classificationError")
              ),
              # wellPanel(
              #   verbatimTextOutput("classificationResultText")
              # )
              uiOutput("classificationResult")
      )
    )
  ),
)