

library(shiny)
library(RColorBrewer)
library(plotrix)

source("globals.R")

shinyServer(function(input, output, session) {
 
  processed.data <- reactive({
    if(!is.null(input$file)) {
      data <- loadData(file = input$file$datapath)
      
      return(scaleData(trimData(data), nx = 400))
    } else {
      return(NULL)
    }
  })
  
  fft.data <- reactive({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    # TODO: window functions will go here
    data <- dft(data)
    output <- list (
      power = apply(data, 1:2, function(x){Mod(x)^2}),
      phase = apply(data, 1:2, Arg)
    )
    return(output)
  })
  
  wavelet.data <- reactive({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    return(wavelet(data, 4, 8))
  })

  output$raw <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    zmax = max(abs(data))
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotmat(data, contour = FALSE, col = brewer.pal(100, "RdBu"), zlim=c(-zmax, zmax),
            xlab="")
    plotmat(data, contour = TRUE, col = c("black"), add = TRUE, zlim=c(-zmax, zmax),
            xlab="")
  })  
    
  output$raw.x <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotspc(data, func = sum, plot.args = list(xaxs="i", yaxs="i"))
    title(ylab = "integrated intensity")
  })
  
  output$raw.y <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(2,2,0,0))
    par(oma=c(0,0,0,0))
    
    plot(x = apply(data[[]], 1, sum), y = data@data$x, type = "l", xaxs="i", 
         yaxs="i")
    if(!is.null(input$raw.brush)){
      trimmed.data <- data[[l = input$raw.brush$xmin:input$raw.brush$xmax]]
      lines(x = apply(trimmed.data, 1, sum), y = data@data$x, type = "l", 
           xaxs="i", yaxs="i", col = 'blue', lwd=8)
    }
  })
  
  
  
  output$fft.power <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    data <- data$power
    zmax = max(abs(data))
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotmat(data, contour = FALSE, col = brewer.pal(100, "YlOrRd"),
            xlab="")
    plotmat(data, contour = TRUE, col = c("black"), add = TRUE,
            xlab="")
  })
  
  output$fft.power.x <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    data <- data$power
    
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotspc(data, func = sum, plot.args = list(xaxs="i", yaxs="i"))
    title(ylab = "integrated intensity")
  })
  
  output$fft.power.y <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    data <- data$power
    
    par(mar=c(2,2,0,0))
    par(oma=c(0,0,0,0))
    x = apply(data[[]], 1, sum)
    plot(x = x, y = data@data$x, type = "l", xaxs="i", 
         yaxs="i", xlim = c(0, max(x)))
    if(!is.null(input$fft.brush)){
      trimmed.data <- data[[l = input$fft.brush$xmin:input$fft.brush$xmax]]
      lines(x = apply(trimmed.data, 1, sum), y = data@data$x, type = "l", 
            xaxs="i", yaxs="i", col = 'blue', lwd=8)
    }
  })
  
  
  
  output$fft.phase <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    data <- data$phase
    zmax = max(abs(data))
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotmat(data, contour = FALSE, col = brewer.pal(100, "RdBu"),
            xlab="")
    plotmat(data, contour = TRUE, col = c("black"), add = TRUE,
            xlab="")
  })  
  
  output$fft.phase.x <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    data <- data$phase
    
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotspc(data, func = sum, plot.args = list(xaxs="i", yaxs="i"))
    title(ylab = "integrated intensity")
  })
  
  output$fft.phase.y <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    data <- data$phase
    
    par(mar=c(2,2,0,0))
    par(oma=c(0,0,0,0))
    x = apply(data[[]], 1, sum)
    plot(x = x, y = data@data$x, type = "l", xaxs="i", 
         yaxs="i")
    if(!is.null(input$fft.phase.brush)){
      trimmed.data <- data[[l = input$fft.phase.brush$xmin:input$fft.phase.brush$xmax]]
      lines(x = apply(trimmed.data, 1, sum), y = data@data$x, type = "l", 
            xaxs="i", yaxs="i", col = 'blue', lwd=8)
    }
  })
  
  output$wavelet <- renderPlot({
    data <- wavelet.data()
    if(is.null(wavelet.data())) {
      return(NULL)
    }
    if(!is.null(input$wavelet.brush)) {
      data <- data[l = input$wavelet.brush$xmin:input$wavelet.brush$xmax]
    }
    plotmap(data, aspect = "fill", contour = TRUE, 
            col.regions = brewer.pal(9, "YlOrRd"), cuts = 8)
  })
  
  output$wavelet.selector <- renderPlot({
    plotspc(fft.data()$power, func = sum, plot.args = list(xaxs="i", yaxs="i"))
  })
})