library(shiny)
library(shinydashboard)
library(glue)
library(tidyverse)
library(ggplot2)
# library(ggdendro)
# library(psych)
library(dplyr)
library(caret)
# library(e1071)
# library(klaR)
library(factoextra)
library(vegan)
library(stats)
library(dendextend)
# library(caTools)
library(scatterplot3d)
library(graphics)

distMethods <- list(
  "Eukleidovská" = "euclidean",
  "Manhattanská" = "manhattan",
  "Jaccardova" = "jaccard",
  "Canberrova" = "canberra"
)

hclustMethods <- list(
  "Single-linkage" = "single",
  "Complete-linkage" = "complete",
  "Average-linkage" = "average",
  "Centroid-linkage" = "centroid"
)

missingValueActions <- list(
  "Vyloučit" = "omit",
  "Nahradit průměrem" = "mean",
  "Nahradit mediánem" = "median",
  "Nahradit minimem" = "min",
  "Nahradit maximem" = "max"
)

normalizeMethods <- list(
  "Standardizace" = "standardization",
  "Robust Scalar" = "robust",
  "Min-max" = "minMax",
  "Mean" = "mean",
  "Unit Length" = "unitLength"
)

robust_scalar <- function(x) {
  (x - median(x)) / (quantile(x, probs = .75) - quantile(x, probs = .25))
}

norm_minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

mean_norm_minmax <- function(x) {
  (x - mean(x)) / (max(x) - min(x))
}

unit_length <- function(x) {
  x / sqrt(sum(x^2))
}

get_confusion_matrix <- function(model, x, y) {
  train_predict <- predict(model, x)
  return(confusionMatrix(train_predict, y))
}

format_clusters <- function(clusters) {
  data.frame(id = seq.int(length(clusters)), clusters)
}

format_prediction <- function(prediction) {
  data.frame(id = seq.int(length(prediction$class)), prediction$class)
}

server <- function(input, output) {
  dataPure <- reactive({
    file <- input$inputFile

    if (is.null(file))
      return(NULL)

    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Byl nahrán neplatný soubor."))
    data = read.csv(file$datapath, header = input$hasHeader, stringsAsFactors = TRUE)
    return(data)
  })

  dataSummary <- reactive({
    data <- dataPure()
    if (input$missingValueAction == "omit") {
      data <- na.omit(data)
    }
    return(data)
  })

  data <- reactive({
    data <- dataPure()

    for (colName in colnames(data)) {
      excludeAttrValue <- input[[glue('excludeAttr_{colName}')]]
      normalizeAttrValue <- input[[glue('normalizeAttr_{colName}')]]
      normalizeAttrMethodValue <- input[[glue('normalizeAttrMethod_{colName}')]]
      missingValueActionAttrValue <- input[[glue("missingValueActionAttr_{colName}")]]

      if (!is.null(excludeAttrValue) && excludeAttrValue) {
        data <- data[, !names(data) %in% c(colName)]
        next
      }

      if (!is.null(normalizeAttrValue) && normalizeAttrValue) {
        if (normalizeAttrMethodValue == "standardization") {
          data[colName] = as.data.frame(scale(data[colName], center = TRUE, scale = TRUE))
        } else if (normalizeAttrMethodValue == "robust") {
          data[colName] = as.data.frame(lapply(data[colName], robust_scalar))
        } else if (normalizeAttrMethodValue == "minMax") {
          data[colName] = as.data.frame(lapply(data[colName], norm_minmax))
        } else if (normalizeAttrMethodValue == "mean") {
          data[colName] = as.data.frame(lapply(data[colName], mean_norm_minmax))
        } else if (normalizeAttrMethodValue == "unitLength") {
          data[colName] = as.data.frame(lapply(data[colName], unit_length))
        }
      }

      if (!is.null(missingValueActionAttrValue)) {
        if (missingValueActionAttrValue == "mean") {
          data[is.na(data[, colName]), colName] <- mean(data[, colName], na.rm = TRUE)
        } else if (missingValueActionAttrValue == "median") {
          data[is.na(data[, colName]), colName] <- median(data[, colName], na.rm = TRUE)
        } else if (missingValueActionAttrValue == "min") {
          data[is.na(data[, colName]), colName] <- min(data[, colName], na.rm = TRUE)
        } else if (missingValueActionAttrValue == "max") {
          data[is.na(data[, colName]), colName] <- max(data[, colName], na.rm = TRUE)
        }
      }
    }

    if (input$missingValueAction == "omit") {
      data <- na.omit(data)
    }
    return(data)
  })

  output$menu <- renderMenu({
    if (is.null(data())) {
      sidebarMenu(
        menuItem("Soubor", tabName = "fileTab", icon = icon("file"))
      )
    } else {
      sidebarMenu(
        menuItem("Soubor", tabName = "fileTab", icon = icon("file")),
        menuItem("Souhrn", tabName = "summaryTab", icon = icon("table"), selected = TRUE),
        menuItem("Datová tabulka", tabName = "dataTableTab", icon = icon("table")),
        menuItem("Shlukování", tabName = "clusteringTab", icon = icon("object-ungroup")),
        menuItem("Klasifikace", tabName = "classificationTab", icon = icon("question"))
      )
    }
  })

  ########################################################
  # Souhrn
  ########################################################

  output$summary <- renderUI({
    summaryUI <- colnames(dataPure()) %>%
      map(function(colName) {
        attr <- dataSummary()[[colName]]
        attrPure <- dataPure()[[colName]]
        plotColumn <- column(3, renderPlot(histPlot, height = 250))

        optionsColumnUI <- list(
          p("Možnosti", class = "title"),
          checkboxInput(glue('excludeAttr_{colName}'), "Vyloučit atribut")
        )

        missingValuesColumn <-
          column(2,
                 p("Chybějící hodnoty", class = "title"),
                 fluidRow(
                   column(6,
                          p("Platné"),
                          p("Chybějící")
                   ),
                   column(6,
                          p(sum(complete.cases(attrPure))),
                          p(sum(!complete.cases(attrPure)))
                   )
                 ),
                 selectInput(glue("missingValueActionAttr_{colName}"), "Akce", missingValueActions)
          )

        if (is.numeric(attr)) {
          optionsColumnUI <- append(list(
            checkboxInput(glue('normalizeAttr_{colName}'), "Normalizovat atribut"),
            selectInput(glue('normalizeAttrMethod_{colName}'), "Metoda normalizace", normalizeMethods)
          ), optionsColumnUI, after = 0)

          quantiles <- unname(quantile(attr))
          bw <- 2 * IQR(attr) / length(attr)^(1 / 3)
          histPlot <- dataSummary() %>%
            ggplot(aes_string(x = colName)) +
            geom_histogram(fill = "#69b3a2", color = "#e9ecef", binwidth = bw) +
            scale_y_continuous(name = "Absolutní četnost")
          list(
            h3(colName),
            fluidRow(
              plotColumn,
              column(2,
                     optionsColumnUI
              ),
              missingValuesColumn,
              column(1,
                     p("Minimum"),
                     p("Maximum"),
                     p("Průměr"),
                     p("Medián"),
                     p("Rozptyl"),
                     p("Směrodatná odchylka")
              ),
              column(1,
                     p(round(min(attr), 2)),
                     p(round(max(attr), 2)),
                     p(round(mean(attr), 2)),
                     p(round(median(attr), 2)),
                     p(round(var(attr), 2)),
                     p(round(sd(attr, 2)))
              ),
              column(1,
                     p("Kvantily"),
                     p("0%"),
                     p("25%"),
                     p("50%"),
                     p("75%"),
                     p("100%")
              ),
              column(1,
                     p(HTML('&nbsp;')),
                     quantiles %>% map(function(quantile) {
                       p(quantile)
                     })
              )
            ),
            hr()
          )
        } else {
          attr <- dataSummary()[[colName]]
          absolute_frequency = table(attr)
          relative_frequency = round(absolute_frequency / length(attr) * 100, 2)
          histPlot <- dataSummary() %>%
            ggplot(aes_string(x = colName)) +
            geom_bar(fill = "#69b3a2", color = "#e9ecef") +
            scale_y_continuous(name = "Absolutní četnost")

          list(
            h3(colName),
            fluidRow(
              plotColumn,
              column(2,
                     optionsColumnUI
              ),
              missingValuesColumn,
              column(1,
                     p("Četnosti"),
                     names(absolute_frequency) %>% map(function(val) {
                       p(val)
                     })
              ),
              column(1,
                     p("Absolutní"),
                     names(absolute_frequency) %>% map(function(val) {
                       p(absolute_frequency[val])
                     })
              ),
              column(1,
                     p("Relativní"),
                     names(absolute_frequency) %>% map(function(val) {
                       p(glue('{relative_frequency[val]}%'))
                     })
              )
            ),
            hr()
          )
        }
      })
    summaryUI
  })

  ########################################################
  # Datová tabulka
  ########################################################

  output$dataTable <- renderDataTable(data(), options = list(
    pageLength = 10
  ))

  ########################################################
  # Shlukování
  ########################################################

  observeEvent(input$clusteringAlgo, {
    if (input$clusteringAlgo == "kmeans") {
      output$clusteringUI <- renderUI({
        list(
          numericInput("clusteringK", "k:", value = 3),
          selectInput("attributes", "Vyberte atributy:", colnames(data()), multiple = TRUE)
        )
      })
    } else if (input$clusteringAlgo == "hierarchicalClustering") {
      output$clusteringUI <- renderUI({
        list(
          selectInput("distMethods", "Podobnost:", distMethods),
          selectInput("hclustMethod", "Metoda:", hclustMethods),
          numericInput("hclustK", "Počet shluků:", 3),
          selectInput("attributes", "Vyberte atributy:", colnames(data()), multiple = TRUE)
        )
      })
    }
  })

  observeEvent(input$doClustring, {
    output$clusteringResultUI <- NULL
    selectedData <- data()[input$attributes]
    output$clusteringError <- NULL

    if (input$clusteringAlgo == "kmeans") {
      if (length(input$attributes) < 2) {
        output$clusteringError <- renderUI({
          h4("Vyberte alespoň 2 atributy.", class = "error")
        })
        return(NULL)
      }
      
      if (input$clusteringK < 1) {
        output$clusteringError <- renderUI({
          h4("Hodnota k musí být větší než 0.", class = "error")
        })
        return(NULL)
      }
      

      kmeansResult <- kmeans(selectedData, input$clusteringK)
      kmeansClusters <<- kmeansResult$cluster
      clusteringResultUI <- list(
        h3("Optimální počet shluků"),
        renderPlot(
          fviz_nbclust(selectedData, kmeans, method = "wss"),
          width = 800
        ),
        # h3("Centroidy"),
        # renderPrint(kmeansResult$centers),
        h3("Velikosti shluků"),
        renderPrint({
          kmeansResult$size
        }),
        h3("Within cluster sum of squares"),
        renderPrint({
          kmeansResult$withinss
        }),
        h3("Shluky"),
        renderPrint({
          head(kmeansResult$cluster, 1000)
        }),
        downloadButton("downloadKmeansClusters", "Stáhnout")
      )


      clusteringResultUI <- append(list(
        h3("Graf"),
        renderPlot({
          isolate({
            if (length(input$attributes) == 2) {
              ggplot() +
                geom_point(data = selectedData, aes_string(x = input$attributes[1], y = input$attributes[2]), col = kmeansResult$cluster, size = 4) +
                geom_point(mapping = aes_string(x = kmeansResult$centers[, input$attributes[1]],
                                                y = kmeansResult$centers[, input$attributes[2]]),
                           color = "red", size = 4) +
                geom_text(mapping = aes_string(x = kmeansResult$centers[, input$attributes[1]],
                                               y = kmeansResult$centers[, input$attributes[2]], label = 1:input$clusteringK), size = 8)
            } else {
              pairs(selectedData, col = (kmeansResult$cluster))
              # scatterplot3d(data()[, 2:4], pch = 20, color = rainbow(3)[kmeansResult$cluster])
            }
          })

        }, width = 1000, height = 800),
        br()
      ), clusteringResultUI, after = 2)

      output$clusteringResult <- renderUI({
        wellPanel(
          clusteringResultUI
          # renderPrint({
          #   kmeansResult$size
          # }),
          # renderPrint({
          #   kmeansResult$cluster
          # }),
          # renderPrint({
          #   kmeansResult$withinss
          # })
        )
      })
    } else if (input$clusteringAlgo == "hierarchicalClustering") {
      if (length(input$attributes) < 1) {
        output$clusteringError <- renderUI({
          h4("Vyberte alespoň 1 atribut.", class = "error")
        })
        return(NULL)
      }

      d <- vegdist(selectedData, method = input$distMethods)
      hc <- hclust(d, method = input$hclustMethod)

      hclustUI <- list(
        h3("Shluky"),
        renderPrint({
          head(hclustClusters, 1000)
        }),
        downloadButton("downloadHclustClusters", "Stáhnout")
      )

      dend <- as.dendrogram(hc)
      k <- input$hclustK
      hclustClusters <<- cutree(hc, k = k)

      if (nrow(data()) < 200) {

        dendrogramPlot <<- function() {
          plot(dend, yaxt = 'n')
          rect.dendrogram(dend, k = k, border = "red")
        }

        hclustUI <- append(list(
          h3("Dendrogram"),
          renderPlot(dendrogramPlot(), height = 1200),
          downloadButton("downloadDendrogram", "Stáhnout")
        ), hclustUI, after = length(hclustUI))
      }

      output$clusteringResult <- renderUI({
        wellPanel(
          hclustUI
        )
      })
    }
  })

  ########################################################
  # Klasifikace
  ########################################################

  classificationTestOptions <- renderUI({
    if (input$testOptions == "trainingSet") {
      list(
      )
    } else if (input$testOptions == "crossValidation") {
      list(
        numericInput("classificationFolds", "Folds:", value = 10)
      )
    } else if (input$testOptions == "percentageSplit") {
      list(
        numericInput("classificationPercentage", "Trénovací sada (%):", value = 66)
      )
    }
  })

  classificationBaseUI <- reactive({
    list(
      radioButtons("testOptions", "Způsob trénování", list(
        "Křížová validace" = "crossValidation",
        "Procentuální rozdělení" = "percentageSplit",
        "Použít vstupní datovou sadu" = "trainingSet"
      )),
      classificationTestOptions,
      selectInput("classificationClass", "Třída:", colnames(data()), selected = tail(colnames(data()), n = 1))
    )
  })

  observeEvent(input$classificationAlgo, {
    if (input$classificationAlgo == "bayes") {
      output$classificationUI <- renderUI({
        append(list(

        ), classificationBaseUI())
      })
    } else if (input$classificationAlgo == "knn") {
      output$classificationUI <- renderUI({
        append(list(
          numericInput("classificationKnnK", "k:", value = 3)
        ), classificationBaseUI())
      })
    }
  })

  observeEvent(input$doClassification, {
    output$classificationResult <- NULL
    class <- input$classificationClass
    x <- data()[, !names(data()) %in% c(class)]
    y <- unlist(data()[class])

    output$classificationError <- NULL
    if (is.numeric(y)) {
      output$classificationError <- renderUI({
        h4("Třída musí být kategoriální atribut.", class = "error")
      })
      return(NULL)
    }


    if (input$testOptions == "trainingSet") {
      trainMethod <- trainControl(method = 'boot')
    } else  if (input$testOptions == "crossValidation") {
      if (input$classificationFolds < 2) {
        output$classificationError <- renderUI({
          h4("Hodnota Folds musí být větší než 2.", class = "error")
        })
        return(NULL)
      }
      trainMethod <- trainControl(method = 'cv', number = input$classificationFolds)
    } else  if (input$testOptions == "percentageSplit") {
      if (input$classificationPercentage < 10 || input$classificationPercentage > 90) {
        output$classificationError <- renderUI({
          h4("Hodnota % musí být v rozsahu od 10 do 90.", class = "error")
        })
        return(NULL)
      }
      trainMethod <- trainControl(method = 'LGOCV', p = input$classificationPercentage / 100)
    }

    if (input$classificationAlgo == "bayes") {
      model <<- train(x, y, 'nb', trControl = trainMethod)
      output$classificationResult <- renderUI({
        wellPanel(
          renderPrint({ model }),
          renderPrint({ get_confusion_matrix(model, x, y) }),
          # renderPrint({ model$results }),
          # renderPrint({ model$finalModel$apriori }),
          # renderPrint({ model$finalModel$tables$Id }),
          # renderPrint({ model$resample }),
          downloadButton("downloadPrediction", "Stáhnout")
        )
      })
    } else if (input$classificationAlgo == "knn") {
      if (input$classificationKnnK < 1) {
        output$classificationError <- renderUI({
          h4("Hodnota k musí být větší než 0.", class = "error")
        })
        return(NULL)
      }

      if (input$classificationKnnK %% 1 != 0) {
        output$classificationError <- renderUI({
          h4("Hodnota k musí být celé číslo.", class = "error")
        })
        return(NULL)
      }
      model <<- train(x, y, 'knn', trControl = trainMethod, tuneGrid = expand.grid(k = input$classificationKnnK))
      output$classificationResult <- renderUI({
        wellPanel(
          renderPrint({ model }),
          renderPrint({ get_confusion_matrix(model, x, y) }),
          # renderPrint({ model$results }),
          # renderPrint({ model$resample }),
          downloadButton("downloadPrediction", "Stáhnout")
        )
      })
    }
    prediction <<- predict(model$finalModel, x)
  })

  output$downloadDendrogram <- downloadHandler(
    filename = "export.png",
    content = function(file) {
      png(file, width = 2000, height = 2000)
      dendrogramPlot()
      dev.off()
    }
  )

  output$downloadKmeansClusters <- downloadHandler(
    filename = "kmeans-clusters.csv",
    content = function(file) {
      write.table(format_clusters(kmeansClusters), file, sep = ";", col.names = FALSE, row.names = FALSE)
    }
  )

  output$downloadHclustClusters <- downloadHandler(
    filename = "hclust-clusters.csv",
    content = function(file) {
      write.table(format_clusters(hclustClusters), file, sep = ";", col.names = FALSE, row.names = FALSE)
    }
  )

  output$downloadPrediction <- downloadHandler(
    filename = "prediction.csv",
    content = function(file) {
      write.table(format_prediction(prediction), file, sep = ";", col.names = FALSE, row.names = FALSE)
    }
  )
}

