
shinyUI(
  navbarPage(title = "Oscillations in Spectroscopy",
    tabPanel("Data Input", 
        fluidPage(
            fluidRow(
              column(3, 
                wellPanel(
                  fileInput(
                    'file', NULL, accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')
                  ),
                  selectInput("x.axis", label = "label", selected = 1,
                              choices = list(
                                "wavelength" = 1,
                                "wavenumber" = 2
                              )
                  ),
                  selectInput("time", label = "time scale", selected = 1E-12,
                              choices = list(
                                "picoseconds (ps)" = 1E-12,
                                "femtoseconds (fs)" = 1E-15,
                                "seconds (s)" = 1
                              ) 
                  )
                )
              ),
              column(7,
                plotOutput("raw",
                           brush = brushOpts(
                            id = "raw.brush",
                            direction = c("x") 
                           ), width = "100%"
                ),
                plotOutput("raw.x", width = "100%", height = "150px")
              ),
              column(2,
                plotOutput("raw.y", width = "100%")
              )
            ) 
        )
      ),
      tabPanel("FFT", 
              fluidPage(
                fluidRow(
                   column(3,
                      wellPanel(
                        selectInput("fft.datatype", label = "FFT output", 
                                    selected = "power", choices = list(
                                      "power" = 1,
                                      "phase" = 2
                                    )
                        ),
                        selectInput("filter", label = "filter", selected = "none",
                                    choices = list(
                                      "none" = 1
                                    )
                        )
                      )
                  ),
                  column(7,
                          plotOutput("fft",
                                     brush = brushOpts(
                                       id = "fft.brush",
                                       direction = c("x") 
                                     ), width = "100%"
                          ),
                          plotOutput("fft.x", width = "100%", height = "150px")
                   ),
                   column(2,
                          plotOutput("fft.y", width = "100%")
                   )
                 ) 
               )
      ),
      tabPanel("Wavelets", 
        fluidPage(
          fluidRow(
            column(3,
              sliderInput("noctaves", label = "octaves", 1, 10, value = 4),
              sliderInput("nvoices", label = "voices", 1, 16, value = 8),
              sliderInput("w0", label = "central frequency (ratio of 2\U03C0)", 
                          0.5, 5.0, value = 1.0, step = 0.1)
            ),
            column(9,
              plotOutput("wavelet"),
              plotOutput("wavelet.selector", brush = brushOpts(
                id = "wavelet.brush",
                direction = c("x") 
                ), height = "150px"
              )
            )
          )
        )
      )
  )
)
