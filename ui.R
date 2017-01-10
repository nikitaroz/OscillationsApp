
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(plotly)

shinyUI(

  navbarPage(title = "Oscillations in Spectroscopy",
                tabPanel("Data Input", 
                          sidebarLayout(
                            sidebarPanel(
                            fileInput('file', NULL,
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv'))
                           ),
                           mainPanel(
                             plotlyOutput("data.plot", height = "600px")
                           ) 
                            
                          )),
                tabPanel("FFT", 
                  splitLayout(
                    plotly::plotlyOutput("fft.intensity"),
                    plotly::plotlyOutput("fft.phase")
                  )         
                ),
                tabPanel("Wavelets", 
                  fluidPage(
                    sliderInput(inputId = "data.select", label = NULL, width = "100%",
                                min = 0, max = 100, value = 10, step = 1),
                    splitLayout(
                      plotly::plotlyOutput("wavelet.intensity")
                  )
                  )
                )
   )
)

  
