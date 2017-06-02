list.of.packages <- c("shiny", "gplots", "RColorBrewer", "broom")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(gplots)
library(RColorBrewer)
library(broom)

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

options(shiny.maxRequestSize=1250*1024^2) 

shinyServer(function(input, output) {
  
  # Read in the feature data
  featureData <- reactive({
    inFile <- input$dataFile
    
    rawFeatureData <- read.csv(inFile$datapath,
                          header = TRUE,
                          sep = ',',
                          nrows = 500)
    if (input$filterEmptyFeatures){
      filteredFeatureData <- rawFeatureData[,apply(rawFeatureData, 2, var, na.rm=TRUE) != 0]
    } else {
      filteredFeatureData <- rawFeatureData
    }
    filteredFeatureData
  })
  
  output$featureSummary <- renderDataTable({
    if (is.null(input$dataFile))
      return(NULL)
    summaryData <- tidy(apply(featureData(), 2, summary))
    transposeSD <- t(summaryData)
    colnames(transposeSD) <- transposeSD[1,]

    tidy(transposeSD)[-1,]
  })
  
  output$featureExample <- renderTable({
    if (is.null(input$dataFile))
      return(NULL)
    
    head(featureData())
  })
  
  output$featureSelector <- renderUI({
    if (is.null(input$dataFile))
      return(NULL)
    selectInput("Feature", "Select feature 1", as.list(colnames(featureData())))
    })
  
  output$featureSelectorInteraction <- renderUI({
    if (is.null(input$dataFile) | !input$useInteractionTerms)
      return(NULL)
    selectInput("FeatureInteraction", "Select feature 2", as.list(colnames(featureData())))
  })
  
  output$featureSelector1 <- renderUI({
    if (is.null(input$dataFile))
      return(NULL)
    selectInput("Feature1", "Select feature 1", as.list(colnames(featureData())))
  })

  output$featureSelector2 <- renderUI({
    if (is.null(input$dataFile))
      return(NULL)
    selectInput("Feature2", "Select feature 2", as.list(colnames(featureData())))
  })
  
  output$ratioMin <- renderUI({
    if (input$showRatios){
      sliderInput("minDensity",
                  "Minimum density to use for ratios",
                  min = 0,
                  max = 1.0,
                  value = 0.05)
    } else {
      return(NULL)
    }
    
  })
  
  output$xMax1 <- renderUI({
    if (is.null(input$dataFile))
      return(NULL)
    
    featureData <- featureData()
    featureName1 <- input$Feature
    featureName2 <- input$FeatureInteraction
    
    if (input$useInteractionTerms) 
    {
      max_x_range = max(featureData[,featureName1] * featureData[,featureName2])
      
    } else {
      max_x_range = max(featureData[,featureName1])
    }
    
    sliderInput("featureMax",
                             "Feature 1 maximum value",
                             min = 0,
                             max = max_x_range,
                             value = 25)
  })
  
  output$xMax2 <- renderUI({
    if (is.null(input$dataFile))
      return(NULL)
    
    featureData <- featureData()
    featureName <- input$Feature1
    
    sliderInput("feature1max",
                "Feature 1 maximum value",
                min = 0,
                max = max(featureData[,featureName]),
                value = 25)
  })
  
  output$xMax3 <- renderUI({
    if (is.null(input$dataFile))
      return(NULL)
    
    featureData <- featureData()
    featureName <- input$Feature2
    sliderInput("feature2max",
                "Feature 2 maximum value",
                min = 0,
                max = max(featureData[,featureName]),
                value = 25)
  })
  
  output$featurePlot <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    
    featureData <- featureData()
    
    featureData$DECISION_TYPE = ifelse(featureData$Prediction > input$threshold & featureData$Y_Value == 1, "TP",
                                ifelse(featureData$Prediction <= input$threshold & featureData$Y_Value == 1, "FN",
                                ifelse(featureData$Prediction <= input$threshold & featureData$Y_Value == 0, "TN",
                                ifelse(featureData$Prediction > input$threshold & featureData$Y_Value == 0, "FP", NA
                                ))))
    
    if (input$useInteractionTerms) {
      featureName1 <- input$Feature
      featureName2 <- input$FeatureInteraction
      featureName <- paste(featureName1, featureName2, sep = " * ")
      
      xRangeMin <- min(featureData[,featureName1] * featureData[,featureName2])
      xRangeMax <- min(max(featureData[,featureName1] * featureData[,featureName2]), input$featureMax)
      
      d_tp <- density(
        featureData[featureData$DECISION_TYPE == "TP",][,featureName1] * featureData[featureData$DECISION_TYPE == "TP",][,featureName2],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_fn <- density(
        featureData[featureData$DECISION_TYPE == "FN",][,featureName1] * featureData[featureData$DECISION_TYPE == "FN",][,featureName2],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_tn <- density(
        featureData[featureData$DECISION_TYPE == "TN",][,featureName1] * featureData[featureData$DECISION_TYPE == "TN",][,featureName2], 
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_fp <- density(
        featureData[featureData$DECISION_TYPE == "FP",][,featureName1] * featureData[featureData$DECISION_TYPE == "FP",][,featureName2],
        from=xRangeMin, 
        to=xRangeMax,
        adjust=1.5)
      
    }
    else {
      featureName <- input$Feature

      xRangeMin <- min(featureData[,featureName])
      xRangeMax <- min(max(featureData[,featureName]), input$featureMax)
      
      d_tp <- density(
        featureData[featureData$DECISION_TYPE == "TP",][,featureName],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_fn <- density(
        featureData[featureData$DECISION_TYPE == "FN",][,featureName],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_tn <- density(
        featureData[featureData$DECISION_TYPE == "TN",][,featureName], 
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_fp <- density(
        featureData[featureData$DECISION_TYPE == "FP",][,featureName],
        from=xRangeMin, 
        to=xRangeMax,
        adjust=1.5)
      
    }
    
    yRangeMin <- min(min(d_tp$y), min(d_fn$y), min(d_tn$y), min(d_fp$y))
    yRangeMax <- max(max(d_tp$y), max(d_fn$y), max(d_tn$y), max(d_fp$y))
   
    par(oma = c(2,2,2,5))
  
    plot(d_tp$x,
          d_tp$y,
         type="l",
         col="red",
         xlim <- c(xRangeMin, xRangeMax),
         ylim <- c(yRangeMin, yRangeMax),
         main = "Distribution density by prediction class",
         xlab = featureName,
         ylab = "Distribution density")
    lines(d_fn$x, d_fn$y, col="green")
    lines(d_tn$x, d_tn$y, col="cyan")
    lines(d_fp$x, d_fp$y, col="blue")
    
    par(fig=c(0,1,0,1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE, xpd=TRUE)
    plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    
    legend("topright",
           c("TP", "FN", "TN", "FP"),
           lty = c(1,1,1,1),
           col = c("red", "green", "cyan", "blue")
           )

  })
  
  output$trueVsNegativePlot <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    
    featureData <- featureData()
    
    featureData$DECISION_TYPE = ifelse(featureData$Prediction > input$threshold & featureData$Y_Value == 1, "TP",
                                ifelse(featureData$Prediction <= input$threshold & featureData$Y_Value == 1, "FN",
                                ifelse(featureData$Prediction <= input$threshold & featureData$Y_Value == 0, "TN",
                                ifelse(featureData$Prediction > input$threshold & featureData$Y_Value == 0, "FP", NA
                                ))))
    
    if (input$useInteractionTerms){
      featureName1 <- input$Feature
      featureName2 <- input$FeatureInteraction
      featureName <- paste(featureName1, featureName2, sep = " * ")
      
      xRangeMin <- min(featureData[,featureName1] * featureData[,featureName2])
      xRangeMax <- min(max(featureData[,featureName1] * featureData[,featureName2]), input$featureMax)
      
      d_t <- density(
        featureData[featureData$DECISION_TYPE == "TP" | featureData$DECISION_TYPE == "TN",][,featureName1] * 
          featureData[featureData$DECISION_TYPE == "TP" | featureData$DECISION_TYPE == "TN",][,featureName2],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_f <- density(
        featureData[featureData$DECISION_TYPE == "FP" | featureData$DECISION_TYPE == "FN",][,featureName1] * 
          featureData[featureData$DECISION_TYPE == "FP" | featureData$DECISION_TYPE == "FN",][,featureName2], 
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
    } else {
      featureName <- input$Feature
      
      xRangeMin <- min(featureData[,featureName])
      xRangeMax <- min(max(featureData[,featureName]), input$featureMax)
      
      d_t <- density(
        featureData[featureData$DECISION_TYPE == "TP" | featureData$DECISION_TYPE == "TN",][,featureName],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d_f <- density(
        featureData[featureData$DECISION_TYPE == "FP" | featureData$DECISION_TYPE == "FN",][,featureName], 
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
    }
    
    d_frac <- function(true_density, false_density) {
      mapply(function(y1, y2) { 
        if(y1 < input$minDensity | y2 < input$minDensity) {
          0
        } else {
          abs((y2 - y1)/y1)
        }    
      }, true_density, false_density)
    }
    
    yRangeMin <- min(min(d_t$y), min(d_f$y), 0)
    if (input$showRatios){
      yRangeMax <- max(max(d_t$y), max(d_f$y), max(d_frac(d_t$y, d_f$y)))
    } else {
      yRangeMax <- max(max(d_t$y), max(d_f$y))
    }
    
    par(oma = c(2,2,2,5))
    
    plot(d_f$x,
         d_f$y,
         type="l",
         col="red",
         xlim <- c(xRangeMin, xRangeMax),
         ylim <- c(yRangeMin, yRangeMax),
         main = "Distribution density by prediction success",
         xlab = featureName,
         ylab = "Distribution density")
    lines(d_t$x, d_t$y, col="blue")
    if (input$showRatios){
      lines(d_t$x, d_frac(d_t$y, d_f$y), type="l", col="green")
    }
    
    par(fig=c(0,1,0,1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE, xpd=TRUE)
    plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    
    legend("topright",
           if (input$showRatios){
              c("Correct prediction (C)", "Incorrect prediction (I)", "|I-C|/C")
           } else {
             c("Correct prediction (C)", "Incorrect prediction (I)")
           },
           lty = c(1,1,1,1),
           col = if (input$showRatios){
            c("red", "blue", "green")
           } else {
             c("red", "blue")
           }
           
    )
  })
  
  output$featureRatioPlot <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    
    featureData <- featureData()
    
    if (input$useInteractionTerms) {
      featureName1 <- input$Feature
      featureName2 <- input$FeatureInteraction
      featureName <- paste(featureName1, featureName2, sep = " * ")
      
      xRangeMin <- min(featureData[,featureName1] * featureData[,featureName1])
      xRangeMax <- min(max(featureData[,featureName1] * featureData[,featureName2]), input$featureMax)
      
      d1 <- density(
        featureData[featureData$Y_Value == 0,][,featureName1] * featureData[featureData$Y_Value == 0,][,featureName2],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d2 <- density(
        featureData[featureData$Y_Value == 1,][,featureName1] * featureData[featureData$Y_Value == 1,][,featureName2],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
    } else {
      featureName <- input$Feature
      
      xRangeMin <- min(featureData[,featureName])
      xRangeMax <- min(max(featureData[,featureName]), input$featureMax)
      
      d1 <- density(
        featureData[featureData$Y_Value == 0,][,featureName],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
      d2 <- density(
        featureData[featureData$Y_Value == 1,][,featureName],
        from=xRangeMin,
        to=xRangeMax,
        adjust=1.5)
    }
    
    d_frac <- function(density1, density2) {
      mapply(function(y1, y2) { 
        if(y1 < input$minDensity | y2 < input$minDensity) {
          0
        } else {
          abs((y2 - y1)/y1)
        }    
      }, density1, density2)
    }
    
    yRangeMin <- min(min(d1$y), min(d2$y), 0)
    if (input$showRatios){
      yRangeMax <- max(max(d1$y), max(d2$y), max(d_frac(d2$y, d1$y)))
    } else {
      yRangeMax <- max(max(d1$y), max(d2$y))
    }
    
    par(oma = c(2,2,2,5))    
    plot(d1$x,
         d1$y,
         type = "l",
         col="red",
         xlim <- c(xRangeMin, xRangeMax),
         ylim <- c(yRangeMin, yRangeMax),
         main = "Distribution density by positives/ negatives",
         xlab = featureName,
         ylab = "Distribution density")
    lines(d1$x, d2$y, col="blue")
    if (input$showRatios){
      lines(d1$x, d_frac(d2$y, d1$y), type="l", col="green")
    }
    
    par(fig=c(0,1,0,1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE,  xpd=TRUE)
    plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    
    legend("topright",
           if (input$showRatios){
             c("Negatives (N)", "Positives (P)", "|N - P|/P")
           } else {
             c("Negatives (N)", "Positives (P)")
           },
           lty = c(1,1),
           col = if (input$showRatios){
             c("red", "blue", "green")
           } else {
             c("red", "blue")
           }
    )
  })
  
  twoDimData <- reactive({
    feature1Name <- input$Feature1
    feature2Name <- input$Feature2
    
    featureData <- featureData()
    
    tFdata <- featureData[c(feature1Name, feature2Name)]
    tFdata
  })
  
  
  twoDimDataWithYvalue <- reactive({
    feature1Name <- input$Feature1
    feature2Name <- input$Feature2
    
    featureData <- featureData()
    
    t2FdataPred <- featureData[c(feature1Name, feature2Name, "Y_Value")]
    t2FdataPred
  })
  
  output$twoDimPositiveVsNegative <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    
    feature1Name <- input$Feature1
    feature2Name <- input$Feature2
    
    tFdata <- twoDimDataWithYvalue()
    
    par(pty="s")
    
    plot(tFdata[tFdata$Y_Value == 1,][,feature1Name],
         tFdata[tFdata$Y_Value == 1,][,feature2Name],
         col="green",
         xlab = feature1Name, 
         ylab = feature2Name, 
         xlim = c(0,input$feature1max),
         ylim = c(0,input$feature2max)
         )
    
    points(tFdata[tFdata$Y_Value == 0,][,feature1Name],
           tFdata[tFdata$Y_Value == 0,][,feature2Name],
           col = "red")
  })
  
  twoDimDataWithYvalueAndPrediction <- reactive({
    feature1Name <- input$Feature1
    feature2Name <- input$Feature2
    
    featureData <- featureData()
    
    t2FdataPred <- featureData[c(feature1Name, feature2Name, "Prediction", "Y_Value")]
    t2FdataPred
  })
  
  output$twoDimTrueVsFalse <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    
    feature1Name <- input$Feature1
    feature2Name <- input$Feature2
    
    tFdata <- twoDimDataWithYvalueAndPrediction()
    
    par(pty="s")
    
    plot(tFdata[tFdata$Y_Value == 1 & tFdata$Prediction >= input$threshold |
                  tFdata$Y_Value == 0 & tFdata$Prediction < input$threshold,][,feature1Name],
         tFdata[tFdata$Y_Value == 1 & tFdata$Prediction >= input$threshold |
                  tFdata$Y_Value == 0 & tFdata$Prediction < input$threshold,][,feature2Name],
         col="green",
         xlab = feature1Name, 
         ylab = feature2Name, 
         xlim = c(0,input$feature1max),
         ylim = c(0,input$feature2max)
    )
    
    points(tFdata[tFdata$Y_Value == 1 & tFdata$Prediction < input$threshold |
                    tFdata$Y_Value == 0 & tFdata$Prediction >= input$threshold,][,feature1Name],
           tFdata[tFdata$Y_Value == 1 & tFdata$Prediction < input$threshold |
                    tFdata$Y_Value == 0 & tFdata$Prediction >= input$threshold,][,feature2Name],
           col = "red")
  })
  
  output$twoDimHist <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    
    feature1Name <- input$Feature1
    feature2Name <- input$Feature2
    
    par(pty="s")
    
    tFdata <- twoDimData()
    hist2d(tFdata, 
                    nbins=input$histBins, 
                    col=r,
                    xlab = feature1Name, 
                    ylab = feature2Name, 
                    xlim = c(0,input$feature1max),
                    ylim = c(0,input$feature2max),
                    zlim = c(0,input$histMax))
  })
  
})

