
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(

  navbarPage(title = "Oscillations in Spectroscopy",
                tabPanel("Data Input", 
                          sidebarLayout(
                            sidebarPanel(
                            fileInput('file', NULL,
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv')),
                            checkboxInput(inputId = "is.surface", label = "Surface plot")
                           ),
                           mainPanel(
                             plotlyOutput("data.plot", height = "600px")
                           ) 
                            
                          )),
                tabPanel("FFT", 
                  splitLayout(
                    plotlyOutput("fft.intensity"),
                    plotlyOutput("fft.phase")
                  )         
                ),
                tabPanel("Wavelets", 
                  fluidPage(
                    sliderInput(inputId = "data.select", label = NULL, width = "100%",
                                min = 0, max = 100, value = 10, step = 1),
                    splitLayout(
                      plotlyOutput("wavelet.intensity"),
                      plotlyOutput("wavelet.phase")
                  )
                  )
                )
   )
)

  
