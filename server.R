library(shiny)
library(signal)
library(rmarkdown)

source("helpers.R")
source("plotters.R")
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

shinyServer(function(input, output, session) {
  processed.data <- reactive({
    if (!is.null(input$file) && input$example == "none") {
      data <- loadData(file = input$file$datapath)
    } else if (input$example == "experimental") {
      data <- loadData(file = "data/experimental.csv")
    } else if (input$example == "simulated") {
      data <- loadData(file = "data/simulated.csv")
    } else {
      return(NULL)
    }
    
    if (input$x.axis == "wavenumber") {
      data@label$.wavelength = expression(Wavenumber ~ (cm ^ {
        -1
      }))
    } else {
      
    }
    data <- scaleData(trimData(data, uniform = TRUE), nx = 600)
    # picoseconds are default times
    data@data$x <- data@data$x * as.numeric(input$time)
    return(data)
  })
  
  fft.filter <- reactive({
    data <- processed.data()
    if (is.null(data)) {
      return(NULL)
    }
    ntime <- length(data@data$x)
    
    w <- switch(
      input$fft.filter,
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
    if (is.null(data)) {
      return(NULL)
    }
    w <- fft.filter()
    if (!is.null(w)) {
      data <- apply(data, 2, function(x) {
        x * w
      })
    }
    data <- dft(data)
    output <- list (power = apply(data, 1:2, function(x) {
      Mod(x) ^ 2
    }),
    phase = apply(data, 1:2, Arg))
    return(output)
  })
  
  wavelet.data <- reactive({
    data <- processed.data()
    if (is.null(data)) {
      return(NULL)
    }
    return(wavelet(data, input$noctaves, input$nvoices, w0 = input$w0 * 2 *
                     pi))
  })
  
  output$raw <- renderPlot({
    plot.raw(processed.data())
  })
  
  output$raw.x <- renderPlot({
    plot.raw.x(processed.data())
  })
  
  output$raw.y <- renderPlot({
    plot.raw.y(processed.data(), input$raw.brush, input$x.axis)
  })
  
  output$fft <- renderPlot({
    plot.fft(fft.data(), input$fft.datatype)
  })
  
  output$fft.x <- renderPlot(plot.fft.x(fft.data(), input$fft.datatype, input$fft.projection))
  
  output$fft.y <- renderPlot({
    plot.fft.y(
      fft.data(),
      input$fft.datatype,
      input$fft.projection,
      input$fft.brush,
      input$x.axis
    )
  })
  
  output$fft.filter.plot <- renderPlot({
    w <- fft.filter()
    if (is.null(w)) {
      return(NULL)
    }
    par(mar = c(4, 2, 0.2, 0.2))
    plot(
      w,
      xlab = "Filter",
      ylab = "",
      type = "l",
      ylim = c(0, 1.05)
    )
  })
  
  output$wavelet <- renderPlot({
    plot.wavelet(wavelet.data(), input$wavelet.brush, input$x.axis)
  })
  
  output$wavelet.selector <- renderPlot({
    plot.wavelet.selector(fft.data())
  })
  
})
