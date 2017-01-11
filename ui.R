
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(plotly)

shinyUI(
  navbarPage(title = "Oscillations in Spectroscopy",
    tabPanel("Data Input", 
        fluidPage(
          fluidPage(
            fileInput(
              'file', NULL, accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                    '.csv')
            )
          ),
            fluidRow(
              column(9,
                plotOutput("raw",
                           brush = brushOpts(
                            id = "raw.brush",
                            direction = c("x") 
                           ), width = "100%"
                           ),
                plotOutput("raw.x", width = "100%", height = "150px")
              ),
              column(3,
                plotOutput("raw.y", width = "100%")
              )
            ) 
        )
      ),
      tabPanel("FFT", 
               fluidPage(
                 fluidRow(
                   column(9,
                          plotOutput("fft.power",
                                     brush = brushOpts(
                                       id = "fft.brush",
                                       direction = c("x") 
                                     ), width = "100%"
                          ),
                          plotOutput("fft.power.x", width = "100%", height = "150px")
                   ),
                   column(3,
                          plotOutput("fft.power.y", width = "100%")
                   )
                 ) 
               ),
               fluidPage(
                 fluidRow(
                   column(9,
                          plotOutput("fft.phase",
                                     brush = brushOpts(
                                       id = "fft.phase.brush",
                                       direction = c("x") 
                                     ), width = "100%"
                          ),
                          plotOutput("fft.phase.x", width = "100%", height = "150px")
                   ),
                   column(3,
                          plotOutput("fft.phase.y", width = "100%")
                   )
                 ) 
               ) 
      ),
      tabPanel("Wavelets", 
        fluidPage(
          splitLayout(
            plotOutput("wavelet.intensity")
          )
        )
    )
  )
)

  
