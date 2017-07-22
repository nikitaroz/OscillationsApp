library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)

shinyUI(dashboardPage(
  dashboardHeader(title = "Spectroscopy App"),
  
  ##Side Bade Code: menuItem() + conditionalPanel() = New Menu
  dashboardSidebar(
    sidebarMenu(
      id = "mytabs",
      menuItem(
        "Data Input",
        tabName = "input",
        icon = icon("dashboard")
      ),
      conditionalPanel(
        "input.mytabs == 'input'",
        fileInput(
          'file',
          label = 'upload a file',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        selectInput(
          'example',
          label = "example datasets",
          selected = "none",
          choices = list(
            "none" = "none",
            "experimental" = "experimental",
            "simulated" = "simulated",
            "parallel" = "parallel"
          )
        ),
        selectInput(
          "x.axis",
          label = "label",
          selected = "wavenumber",
          choices = list("wavenumber" = "wavenumber",
                         "wavelength" = "wavelength")
        ),
        selectInput(
          "time",
          label = "time scale",
          selected = 1,
          choices = list(
            "picoseconds (ps)" = 1,
            "femtoseconds (fs)" = 1E-3,
            "seconds (s)" = 1E12
          )
        ),
        selectInput(
          "proj.style",
          label = "projection",
          selected = "max",
          choices = list("max" = "max",
                         "integrated" = "int")
        )
      ),
      menuItem(
        "Fast Fourier Transform",
        tabName = "fft",
        icon = icon("area-chart")
      ),
      conditionalPanel(
        "input.mytabs == 'fft'",
        #fixed
        selectInput(
          "fft.datatype",
          label = "FFT output",
          selected = "power",
          choices = list("power" = "power",
                         "phase" = "phase")
        ),
        selectInput(
          "fft.filter",
          label = "filter",
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
        plotOutput("fft.filter.plot", height = "200px")
      ),
      menuItem(
        "Wavelet Transform",
        tabName = "wavelet",
        icon = icon("superscript")
      ),
      conditionalPanel(
        "input.mytabs == 'wavelet'",
        sliderInput("noctaves", label = "octaves", 1, 10, value = 4),
        sliderInput("nvoices", label = "voices", 1, 16, value = 8),
        sliderInput(
          "w0",
          label = "central frequency (ratio of 2\U03C0)",
          0.5,
          2.0,
          value = 1.0,
          step = 0.1
        )
      ),
      menuItem(
        "Fluorescence Anisotropy",
        tabName = "anisotropy",
        icon = icon("lightbulb-o")
      ),
      conditionalPanel(
        "input.mytabs == 'anisotropy'",
        fileInput(
          'file2',
          label = 'upload a file',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        selectInput(
          'example2',
          label = "example datasets",
          selected = "none",
          choices = list(
            "none" = "none",
            "perpendicular" = "perpendicular"
          )
        )
      ),
      menuItem(
        "Export Data",
        tabName = "export",
        icon = icon("cloud-download")
      ),
      conditionalPanel(
        "input.mytabs == 'export'",
        selectInput(
          "export.graph",
          label = "filter",
          choices = list(
            "Main Plot" = "main",
            "X-Projection" = "x",
            "Y-Projection" = "y"
          )
        ),
        selectInput(
          "export.type",
          label = "export type",
          selected = "CSV",
          choices = list("CSV" = "CSV",
                         "PNG" = "PNG")
        ),
        actionButton('export', 'Export the Plot Presented', width = "88%")
      ),
      menuItem("About", tabName = "about", icon = icon("user-circle-o"))
    )
  ),
  
  ##Dashboard Body: tabItem corresponds to a specific menuItem
  dashboardBody(tabItems(
    tabItem(
      "input",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/data-input.md"))))),
      box(
        title = "Data Input",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotlyOutput("raw"),
                          plotlyOutput("raw.x")),
                   column(width = 5,
                          plotlyOutput("raw.y")))
      )
      )
    ),
    tabItem(
      "fft",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/fft.md"))))),
      box(
        title = "Fast Fourier Transform",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotlyOutput("fft"),
                          plotlyOutput("fft.x")),
                   column(width = 5,
                          plotlyOutput("fft.y"))))
      )
    ),
    
    tabItem(
      "wavelet",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/wavelets.md"))))),
      box(
        title = "Wavelet Transform",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotlyOutput("wavelet"),
                          plotlyOutput("wavelet.x")),
                   column(width = 5,
                          plotlyOutput("wavelet.y")))
      )
    )),
    tabItem(
      "anisotropy",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/anisotropy.md"))))),
      box(
        title = "Broadband Fluorescence Anisotropy",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotlyOutput("anisotropy"),
                          plotlyOutput("anisotropy.x")),
                   column(width = 5,
                          plotlyOutput("anisotropy.y"))))
      )
    ),
    tabItem("about",
            includeMarkdown("markdown/about.md"))
  ))
  
  
))
