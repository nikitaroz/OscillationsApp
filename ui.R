library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(title = "Oscillations in Spectroscopy",
    tabPanel("Data Input",
        fluidPage(
            fluidRow(
              column(3, 
                wellPanel(
                  fileInput(
                    'file', NULL, 
                    accept = c(
                      'text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv'
                    )
                  ),
                  selectInput(
                    "example", label = "example datasets", selected = "none",
                    choices = list(
                      "none" = "none",
                      "experimental" = "experimental",
                      "simulated" = "simulated"
                    )
                  ),
                  selectInput("x.axis", label = "label", 
                              selected = "wavenumber",
                              choices = list(
                                "wavenumber" = "wavenumber",
                                "wavelength" = "wavelength"
                              )
                  ),
                  selectInput("time", label = "time scale", selected = 1,
                              choices = list(
                                "picoseconds (ps)" = 1,
                                "femtoseconds (fs)" = 1E-3,
                                "seconds (s)" = 1E12
                              ) 
                  ),
                  includeMarkdown("markdown/data-input.md")
                  
                )
              ),
              column(7,
                plotOutput("raw",
                           brush = brushOpts(
                            id = "raw.brush",
                            direction = c("x") 
                           )
                ),
                plotOutput("raw.x", height = "150px")
              ),
              column(2,
                plotOutput("raw.y")
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
                                      "power" = "power",
                                      "phase" = "phase"
                                    )
                        ),
                        selectInput("fft.filter", label = "filter", 
                                    selected = "boxcar",
                                    choices = list(
                                      "none" = "boxcar",
                                      "Bartlett" = "bartlett",
                                      "Blackman" = "blackman",
                                      "Gaussian" = "gausswin",
                                      "Hamming" = "hamming",
                                      "Hanning" = "hanning",
                                      "Triangular" = "triang"
                                    )
                        ),
                        plotOutput("fft.filter.plot", height = "200px"),
                        br(),
                        includeMarkdown("markdown/fft.md")
                      )
                  ),
                  column(7,
                          plotOutput("fft",
                                     brush = brushOpts(
                                       id = "fft.brush",
                                       direction = c("x") 
                                     )
                          ),
                          plotOutput("fft.x", height = "150px")
                   ),
                   column(2,
                          plotOutput("fft.y")
                   )
                 ) 
               )
      ),
      tabPanel("Wavelets", 
        fluidPage(
          fluidRow(
            column(3,
              wellPanel(
                sliderInput("noctaves", label = "octaves", 1, 10, value = 4),
                sliderInput("nvoices", label = "voices", 1, 16, value = 8),
                sliderInput("w0", 
                            label = "central frequency (ratio of 2\U03C0)", 
                            0.5, 2.0, value = 1.0, step = 0.1),
                includeMarkdown("markdown/wavelets.md")
                
              )
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
      ), 
      tabPanel("About",
        includeMarkdown("markdown/about.md")
      ),
    theme = shinytheme("united"), 
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css")
    )
  )
)
