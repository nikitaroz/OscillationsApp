

library(shiny)
library(ggplot2)

library(plotly)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


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
      rv$processed.data <- scaleData(trimData(rv$data), nx = 500)
      rv
      if(FALSE){
      rv$dft <- dft(rv$trimmed.data)
      
      rv$wavelet <- wavelet(rv$trimmed.data)
      updateSliderInput(session, inputId = "data.select", 
                        value = round(rv$trimmed.data@wavenumber[1]),
                        min = round(rv$trimmed.data@wavenumber[1]),
                        max = round(tail(rv$trimmed.data@wavenumber, n = 1)),
                        step = 1)
      }
    }
  })

  
  output$data.plot <- renderPlotly({
    if(is.null(rv$processed.data)) {
      return(NULL)
    }
    g <- ggplot(as.long.df(rv$processed.data), aes(x = .wavelength, y = x)) + 
      geom_raster(aes(fill = spc), interpolate = TRUE, show.legend = FALSE) +
      scale_fill_distiller(palette = "RdBu") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      theme_linedraw()
    return(ggplotly(g) %>% toWebGL())
  })
  if(FALSE){
  output$fft.intensity <- plotly::renderPlotly({
    if(is.null(rv$dft)) {
      return(NULL)
    }
    imshow(rv$dft@wavenumber, rv$dft@frequency, rv$dft@data, 
         type = rv$plot.type, interactive = TRUE, component = "intensity")
  })
  output$fft.phase <- plotly::renderPlotly({
    if(is.null(rv$dft)) {
      return(NULL)
    }
    imshow(rv$dft@wavenumber, rv$dft@frequency, rv$dft@data, 
         type = rv$plot.type, interactive = TRUE, component = "phase")
  })
  output$wavelet.intensity <- plotly::renderPlotly({
    if(is.null(rv$wavelet)) {
      return(NULL)
    }
    rv$wavelet.idx = which.min(abs(rv$trimmed.data@wavenumber - input$data.select))
    data <- t(rv$wavelet@data[, , rv$wavelet.idx])
    imshow(rv$wavelet@time, rv$wavelet@frequency, data, 
         type = rv$plot.type, interactive = TRUE, component = "intensity")
  })
  }
})