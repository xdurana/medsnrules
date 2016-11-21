library(shiny)
library(arules)
library(arulesViz)
library(dplyr)

library(visNetwork)
library(igraph)

data(medsnconditions)
data(dictionary)

tr <- as(dataset, 'transactions')

function(input, output) {
  
  rules <- reactive({
    
    arAll <- apriori(tr,
                     parameter = list(support=input$supp,
                                    confidence=input$conf,
                                    minlen=input$minL,
                                    maxlen=input$maxL))

    colsL <- colnames(dataset)
    colsR <- colnames(dataset)

    if (!is.null(input$lhs)) {
      colsL <- dictionary %>%
        filter(category %in% input$lhs) %>%
        select(variable)
      colsL <- colsL$variable
    }
    
    if (!is.null(input$rhs)) {
      colsR <- dictionary %>%
        filter(category %in% input$rhs) %>%
        select(variable)
      colsR <- colsR$variable
    }

    varsL <- character()
    for(i in 1:length(colsL)) {
      tmp <- colsL[i]  
      if (!is.logical(dataset[, colsL[i]])) {
        tmp <- with(dataset, paste(colsL[i], '=', levels(as.factor(get(colsL[i]))), sep=''))
      }
      varsL <- c(varsL, tmp)
    }
    
    varsR <- character()
    for(i in 1:length(colsR)) {
      tmp <- colsR[i]  
      if (!is.logical(dataset[, colsR[i]])) {
        tmp <- with(dataset, paste(colsR[i], '=', levels(as.factor(get(colsR[i]))), sep=''))
      }
      varsR <- c(varsR, tmp)
    }
    
    ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)
    
    quality(ar)$chiSquare <- interestMeasure(ar, measure='chiSquare', transactions=tr)
    quality(ar)$conviction <- interestMeasure(ar, measure='conviction', transactions=tr)
    ar
  })
  
  # Rule length
  nR <- reactive({
    nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
  })
  
  ## Grouped Plot #########################
  output$groupedPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
  }, height=800, width=800)
  
  ## Graph Plot ##########################
  output$graphPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='graph', control=list(type=input$graphType))
  }, height=800, width=800)
  
  ## Scatter Plot ##########################
  output$scatterPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
  }, height=800, width=800)
  
  ## Rules Data Table ##########################
  output$rulesDataTable <- renderDataTable({
    rulesdt <- as(rules(), "data.frame")
    rulesdt
  })
  
  ## Rules Printed ########################
  output$rulesTable <- renderPrint({
    ar <- rules()
    inspect(sort(ar, by=input$sort))
  })
  
  ## Download data to csv ########################
  output$downloadData <- downloadHandler(
    filename = 'arules_data.csv',
    content = function(file) {
      write.csv(as(rules(), "data.frame"), file)
    }
  )
  
  ## Download report ########################
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', 'pdf')
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', pdf_document())
      file.rename(out, file)
    }
  )  
}