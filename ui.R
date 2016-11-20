library(shiny)

supp <- 0.001
conf <- 0.8

shinyUI(pageWithSidebar(
  
  headerPanel("Association Rules"),
  
  sidebarPanel(

    selectizeInput(
      'lhs', 'LHS', choices = unique(dictionary$category),
      multiple = TRUE, options = list()
    ),
    
    selectizeInput(
      'rhs', 'RHS', choices = unique(dictionary$category),
      multiple = TRUE, options = list()
    ),
    
    conditionalPanel(
      condition = "input.samp=='Sample'",
      numericInput("nrule", 'Number of Rules', 5), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab=='graph'",
      radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
    ),
    
    conditionalPanel(
      condition = "input.mytab=='grouped'",
      sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
    ),
    
    conditionalPanel(
      condition = "true", 
      radioButtons('samp', label='Sample', choices=c('Sample', 'All Rules'), inline=T), br(),
      sliderInput("supp", "Support:", min = 0, max = 1, value = supp , step = 1/10000), br(),
      sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf , step = 1/10000), br(),
      selectInput('sort', label='Sorting Criteria:', choices = c('chiSquare', 'lift', 'confidence', 'support')), br(), br(),
      numericInput("minL", "Min. items per set:", 2), br(), 
      numericInput("maxL", "Max. items per set::", 3), br(),
      downloadButton('downloadData', 'Download Rules as CSV')
    )
    
  ),
  
  mainPanel(
    tabsetPanel(id='mytab',
                tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%')),
                tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
    )
  )
  
))