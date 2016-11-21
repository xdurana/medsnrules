library(shiny)
library(shinythemes)
library(shinydashboard)

header <- dashboardHeader(
  title = "GCAT",
  disable = TRUE
)

sidebar <- dashboardSidebar(
  disable = TRUE
)

body <- dashboardBody(
  fluidRow(
    column(
      width = 9,
      box(
        width = NULL,
        title='Association rules',
        status='primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        tabsetPanel(
          tabPanel(
            title='Graph',
            value='graph',
            plotOutput("graphPlot", width='100%', height='100%'),
            radioButtons('graphType', label='Graph Type', choices=c('items','itemsets'), inline=T)
          ),
          tabPanel(
            title='Grouped',
            plotOutput("groupedPlot", width='100%', height='100%'),
            sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15)
          ),
          tabPanel(
            title='Scatter',
            value='scatter',
            plotOutput("scatterPlot", width='100%', height='100%')
          ),
          tabPanel(
            title='Data Table',
            value='datatable',
            dataTableOutput("rulesDataTable")
          )
        )
      )
    ),
    column(
      width = 3,
      box(
        width = NULL,
        title = 'Configuration',
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        numericInput("nrule", 'Number of Rules', 15),
        radioButtons('samp', label='Sample', choices=c('Sample', 'All Rules'), inline=T), br(),
        sliderInput("supp", "Support:", min = 0, max = 1, value = 0.001 , step = 1/10000), br(),
        sliderInput("conf", "Confidence:", min = 0, max = 1, value = 0.8 , step = 1/10000), br(),
        selectInput('sort', label='Sorting Criteria:', choices = c('chiSquare', 'lift', 'confidence', 'support')), br(), br(),
        numericInput("minL", "Min. items per set:", 2), br(), 
        numericInput("maxL", "Max. items per set::", 3), br(),
        selectizeInput(
          'lhs', 'LHS', choices = unique(dictionary$category),
          multiple = TRUE, options = list()
        ),
        selectizeInput(
          'rhs', 'RHS', choices = unique(dictionary$category),
          multiple = TRUE, options = list()
        ),
        downloadButton('downloadData', 'Download Rules as CSV'),
        br(),
        downloadButton('downloadReport')
      )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)