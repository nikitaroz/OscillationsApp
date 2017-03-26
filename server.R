library(shiny)
library(RColorBrewer)
library(plotrix)

source("globals.R")
options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session) {
 
  processed.data <- reactive({
    if(!is.null(input$file)) {
      data <- loadData(file = input$file$datapath)
      if(input$x.axis == 2){
        data@label$.wavelength = expression(Wavenumber (cm^{-1}))
      }
      
      data <- scaleData(trimData(data), nx = 400)
      data@data$x <- data@data$x * as.numeric(input$time)
      return(data)
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
    par(mar=c(4,4,1,6))
    plotmat(data, y = "x", contour = FALSE, col = brewer.pal(100, "RdBu"), zlim=c(-zmax, zmax))
    plotmat(data, y = "x", contour = TRUE, col = c("black"), add = TRUE, zlim=c(-zmax, zmax))
  })  
    
  output$raw.x <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(4,4,1,6))
    integrated.data <- colSums(data, label.spc = expression("integrated intensity"))
    plotspc(integrated.data, plot.args = list(xaxs="i"))
    
  })
  
  output$raw.y <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(4,4,1,2))
    
    plot(x = rowSums(data[[]]), y = data@data$x, yaxs = "i", type = "l", 
         xlab = "Integrated intensity", ylab = "Time (s)")
    if(!is.null(input$raw.brush)){
      l = wl2i(data, input$raw.brush$xmin):wl2i(data, input$raw.brush$xmax)
      trimmed.data <- data[[l = l, wl.index = TRUE]]
      lines(x = rowSums(trimmed.data), y = data@data$x, type = "l", 
            col = 'blue', lwd=8)
    }
  })
  
  
  
  output$fft <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    
    par(mar=c(4,5,1,6))
    
    if(input$fft.datatype == 1) {
      data <- data$power
      palette <- "YlOrRd"
    } else {
      data <- data$phase
      palette <- "RdBu" 
    }

    par(mar=c(4,5,1,6))
    plotmat(data, y = "x", contour = FALSE, col = brewer.pal(100, palette))
    plotmat(data, y = "x", contour = TRUE, col = c("black"), add = TRUE)
  
  })
  
  output$fft.x <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    if(input$fft.datatype == 1) {
      data <- data$power
    } else {
      data <- data$phase
    }
    
    par(mar=c(2,2,0,5))
    par(oma=c(0,0,0,0))
    plotspc(data, func = sum, plot.args = list(xaxs="i"))
    title(ylab = "integrated intensity")
  })
  
  output$fft.y <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    
    if(input$fft.datatype == 1) {
      data <- data$power
      x.data <- rowSums(data[[]])
      xlim <- c(0, max(x.data))
    } else {
      data <- data$phase
      x.data <- rowSums(data[[]])
      datalim <- max(abs(x.data))
      xlim <- c(-datalim, datalim)
    }
    
    par(mar=c(2,2,0,0))
    
    
    plot(x = x.data, y = data@data$x, xlim = xlim, type = "l", 
         yaxs="i"
    )
    if(!is.null(input$fft.brush)){
      l = wl2i(data, input$fft.brush$xmin):wl2i(data, input$fft.brush$xmax)
      trimmed.data <- data[[l = l, wl.index = TRUE]]
      lines(x = rowSums(trimmed.data), y = data@data$x, type = "l", 
            col = 'blue', lwd=8)
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