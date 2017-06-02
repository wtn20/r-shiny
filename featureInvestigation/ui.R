list.of.packages <- c("shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)

shinyUI(
  navbarPage(
    "Features",
    tabPanel("Load Feature Data",  
             sidebarLayout(
               sidebarPanel(
                 fileInput("dataFile", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 checkboxInput("filterEmptyFeatures", "Filter constant features", TRUE)
               ),
               mainPanel(
                 h2("Feature Summary"),
                 dataTableOutput("featureSummary"),
                 h2("Example data"),
                 tableOutput("featureExample")
               )
             )
    ),
    tabPanel("Individual Features",  
             sidebarLayout(
               sidebarPanel(
                 sliderInput("threshold",
                             "Model threshold",
                             min = 0,
                             max = 1,
                             value = 0.75),
                 uiOutput("featureSelector"),
                 checkboxInput("useInteractionTerms", "Use interactions", FALSE),
                 uiOutput("featureSelectorInteraction"),
                 uiOutput("xMax1"),
                 checkboxInput("showRatios", "Plot ratios", FALSE),
                 uiOutput("ratioMin")
               ),
             mainPanel(
               plotOutput("trueVsNegativePlot"),
               plotOutput("featurePlot"),
               plotOutput("featureRatioPlot")
               )
             )
    ),
    tabPanel("Pairs of Features",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("2dThreshold",
                             "Model threshold",
                             min = 0,
                             max = 1,
                             value = 0.75),
                 uiOutput("featureSelector1"),
                 uiOutput("featureSelector2"),
                 uiOutput("xMax2"),
                 uiOutput("xMax3"),
                 uiOutput("histBins"),
                 sliderInput("histMax",
                   "Maximum value of 2D historgram",
                   min = 10,
                   max = 250,
                   value = 10),
                 sliderInput("histBins",
                             "Number of bins for 2D historgram",
                             min = 10,
                             max = 250,
                             value = 25)
               ),
               mainPanel(
                 h3("Features by prediction success"),
                 plotOutput("twoDimTrueVsFalse"),
                 h3("Features by positive/ negative"),
                 plotOutput("twoDimPositiveVsNegative"),
                 h3("Features distribution"),
                 plotOutput("twoDimHist")
               )
               )
    )
  )
)
  
