
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("../R/src.R")

shinyServer(function(input, output, session) {
  rv <- reactiveValues()
  callback <- observeEvent( input$file, {
    if (is.null(input$file)){
      rv$data <- NULL
      rv$trimmed.data <- NULL
      rv$frequency <- NULL
      rv$wavelet <- NULL
    } else {
      rv$data <- loadData(file = input$file$datapath)
      rv$trimmed.data <- trimData(rv$data, uniform = TRUE)
      interp.data <- scaleData(x = rv$trimmed.data@time,
                                        y = rv$trimmed.data@wavenumber, 
                                        z = rv$trimmed.data@data,
                                        ny = 500)
      rv$trimmed.data@data <- interp.data$z * 1E3
      rv$trimmed.data@wavenumber <- interp.data$y
      rv$trimmed.data@time <- interp.data$x
      rv$dft <- dft(rv$trimmed.data)
      rv$wavelet <- wavelet(rv$trimmed.data)
      updateSliderInput(session, inputId = "data.select", 
                        value = round(rv$trimmed.data@wavenumber[1]),
                        min = round(rv$trimmed.data@wavenumber[1]),
                        max = round(tail(rv$trimmed.data@wavenumber, n = 1)),
                        step = 1)
    }
  })

  
  output$data.plot <- renderPlotly({
    if(is.null(rv$trimmed.data)) {
      return(NULL)
    }
    if(input$is.surface) {
      rv$plot.type <- "surface"
    } else {
      rv$plot.type <- "contour"
    }
    imshow(rv$trimmed.data@wavenumber, rv$trimmed.data@time, rv$trimmed.data@data, 
           type = rv$plot.type, interactive = TRUE, showscale = FALSE)
  })
  output$fft.intensity <- renderPlotly({
    if(is.null(rv$dft)) {
      return(NULL)
    }
    imshow(rv$dft@wavenumber, rv$dft@frequency, rv$dft@data, 
         type = rv$plot.type, interactive = TRUE, component = "intensity")
  })
  output$fft.phase <- renderPlotly({
    if(is.null(rv$dft)) {
      return(NULL)
    }
    imshow(rv$dft@wavenumber, rv$dft@frequency, rv$dft@data, 
         type = rv$plot.type, interactive = TRUE, component = "phase")
  })
  output$wavelet.intensity <- renderPlotly({
    if(is.null(rv$wavelet)) {
      return(NULL)
    }
    rv$wavelet.idx = which.min(abs(rv$data@wavenumber - input$data.select))
    data <- t(rv$wavelet@data[, , rv$wavelet.idx])
    imshow(rv$wavelet@time, rv$wavelet@frequency, data, 
         type = rv$plot.type, interactive = TRUE, component = "intensity")
  })
  output$wavelet.phase <- renderPlotly({
    if(is.null(rv$wavelet)){
      return(NULL)
    }
    data <- t(rv$wavelet@data[, , rv$wavelet.idx])
    imshow(rv$wavelet@time, rv$wavelet@frequency, data, 
         type = rv$plot.type, interactive = TRUE, component = "phase")
  })
})