library(shiny)
library(signal)
library(RColorBrewer)
library(plotrix)

source("globals.R")
options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session) {
 
  processed.data <- reactive({
  if(!is.null(input$file) && input$example == "none") {
      data <- loadData(file = input$file$datapath)
    } else if(input$example == "experimental"){
      data <- loadData(file = "data/experimental.csv")
    } else if(input$example == "simulated") {
      data <- loadData(file = "data/simulated.csv")
    } else {
      return(NULL)
    }
    
    if(input$x.axis == "wavenumber"){
      data@label$.wavelength = expression(Wavenumber~(cm^{-1}))
    }
    
    data <- scaleData(trimData(data), nx = 400)
    # picoseconds are default times
    data@data$x <- data@data$x * as.numeric(input$time)
    return(data)
  })
  
  fft.filter <- reactive({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    ntime <- length(data@data$x)
    
    w <- switch(input$fft.filter, 
                 "boxcar" = boxcar(ntime),
                 "bartlett" = bartlett(ntime),
                 "blackman" = blackman(ntime),
                 "gausswin" = gausswin(ntime),
                 "hamming" = hamming(ntime),
                 "hanning" = hanning(ntime),
                 "triang" = triang(ntime)
    )
    return(w)
  })
  
  fft.data <- reactive({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    w <- fft.filter()
    if(!is.null(w)) {
      data <- apply(data, 2, function(x){x * w})
    }
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
    return(wavelet(data, input$noctaves, input$nvoices, w0 = input$w0 * 2*pi))
  })

  output$raw <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    zmax = max(abs(data))
    par(mar=c(4.5,5,1,7.5))
    
    plotmat(data, y = "x", contour = FALSE, col = brewer.pal(100, "RdBu"), zlim=c(-zmax, zmax))
    plotmat(data, y = "x", contour = TRUE, col = c("black"), add = TRUE, zlim=c(-zmax, zmax))
    nxticks <<- length(axTicks(1))
  })  
    
  output$raw.x <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(1.5,5,1,7.5))
    integrated.data <- colSums(data, label.spc = "integrated intensity")
    plotspc(integrated.data, func = sum, plot.args = list(xaxs="i"), 
            title.args = list(xlab = ""), axis.args = list(x = list(labels = FALSE)), 
            bty="o", nxticks = nxticks)
  })
  
  output$raw.y <- renderPlot({
    data <- processed.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(4.5,1.5,1,1.5))
    
    
    plot(x = rowSums(data[[]]), y = data@data$x, yaxs = "i", type = "l", 
         xlab = "Integrated intensity", ylab = "", yaxt = "n")
    axis(side = 2, labels = FALSE)
    abline(v = 0, lty = 2)
    
    if(!is.null(input$raw.brush)){
      l = wl2i(data, input$raw.brush$xmin):wl2i(data, input$raw.brush$xmax)
      trimmed.data <- data[[l = l, wl.index = TRUE]]
      lines(x = rowSums(trimmed.data), y = data@data$x, type = "l", 
            col = '#e95420', lwd = 5)
    }
  })
  
  
  output$fft <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    
    if(input$fft.datatype == "power") {
      data <- data$power
      palette <- "YlOrRd"
    } else if(input$fft.datatype == "phase") {
      data <- data$phase
      palette <- "RdBu" 
    } else {
      return(NULL)
    }

    par(mar=c(4.5,5,1,7.5))
    plotmat(data, y = "x", contour = FALSE, col = brewer.pal(100, palette))
    plotmat(data, y = "x", contour = TRUE, col = c("black"), add = TRUE)
  
  })
  
  output$fft.x <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    if(input$fft.datatype == "power") {
      data <- data$power
    } else if(input$fft.datatype == "phase"){
      data <- data$phase
    } else {
      return(NULL)
    }
    
    par(mar=c(1.5,5,1,7.5))
    integrated.data <- colSums(data, label.spc = "integrated intensity")
    plotspc(integrated.data, plot.args = list(xaxs="i"),
            title.args = list(xlab = ""), axis.args = list(x = list(labels = FALSE)), 
            nxticks = nxticks)
  })
  
  output$fft.y <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    
    if(input$fft.datatype == "power") {
      data <- data$power
      x.data <- rowSums(data[[]])
      xlim <- c(0, max(x.data))
    } else if(input$fft.datatype == "phase"){
      data <- data$phase
      x.data <- rowSums(data[[]])
      datalim <- max(abs(x.data))
      xlim <- c(-datalim, datalim)
    } else {
      return(NULL)
    }
    
    par(mar=c(4.5,1.5,1,1.5))

    plot(x = rowSums(data[[]]), y = data@data$x, xlim = xlim, yaxs = "i", 
         type = "l", xlab = "Integrated intensity", ylab = "", yaxt = "n")
    axis(side = 2, labels = FALSE)
    abline(v = 0, lty = 2)
    
    if(!is.null(input$fft.brush)){
      l = wl2i(data, input$fft.brush$xmin):wl2i(data, input$fft.brush$xmax)
      trimmed.data <- data[[l = l, wl.index = TRUE]]
      lines(x = rowSums(trimmed.data), y = data@data$x, type = "l", 
            col = '#e95420', lwd = 5)
    }
  })
  
  output$fft.filter.plot <- renderPlot({
    w <- fft.filter()
    if(is.null(w)) {
      return(NULL)
    }
    par(mar=c(4, 2, 0.2, 0.2))
    plot(w, xlab = "Filter", ylab = "", type = "l", ylim = c(0, 1.05))
  })
  
  
  output$wavelet <- renderPlot({
    data <- wavelet.data()
    if(is.null(wavelet.data())) {
      return(NULL)
    }
    if(!is.null(input$wavelet.brush)) {
      data <- data[l = input$wavelet.brush$xmin:input$wavelet.brush$xmax]
    }
    par(mar=c(4,5,1,6))
    
    plotmap(data, aspect = "fill", contour = TRUE, 
            col.regions = brewer.pal(9, "YlOrRd"), cuts = 8)
  })
  
  output$wavelet.selector <- renderPlot({
    data <- fft.data()
    if(is.null(data)) {
      return(NULL)
    }
    par(mar=c(4,5,1,1))
    integrated.data <- colSums(data$power, label.spc = "FFT integrated\n power")
    
    plotspc(integrated.data, plot.args = list(xaxs="i"))
  })
})