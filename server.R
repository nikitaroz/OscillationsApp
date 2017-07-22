library(shiny)
library(signal)
library(RColorBrewer)

source("globals.R")
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

shinyServer(function(input, output, session) {
  
  #Process data for all menus
  processed.data <- reactive({
    validate(need((input$example != "none") ||
                    (is.null(input$file) == FALSE), ""))
    if (is.null(input$file) == FALSE) {
      df <- load(file = input$file$datapath)
    } else if (input$example == "experimental") {
      df <- load(file = "data/experimental.csv")
    } else if (input$example == "simulated") {
      df <- load(file = "data/simulated.csv")
    } else if (input$example == "parallel") {
      df <- load(file = "data/parallel.csv")
    } else {
      return(NULL)
    }
    if (input$x.axis == "wavenumber") {
      df@label$.wavelength = expression("Wavenumber (cm^-1)")
    }
    if (input$mytabs == 'input' ||
        input$mytabs == 'anisotropy' ||
        input$mytabs == 'export') {
      df <- scaleData(df, nx = 600)
    } else if (input$mytabs == 'fft' ||
               input$mytabs == 'wavelet' ||
               input$mytabs == 'export') {
      df <- scaleData(trimData(df, uniform = TRUE), nx = 600)
    }
    df@data$x <- df@data$x * as.numeric(input$time)
    return(df)
  })
  
  #Process fft Filter
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
  
  #Process fft data
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
    out <- list (power = apply(data, 1:2, function(x) {
      Mod(x) ^ 2
    }),
    phase = apply(data, 1:2, Arg))
    return(out)
  })
  
  #Process wavelet data
  wavelet.data <- reactive({
    return (wavelet(
      processed.data(),
      input$noctaves,
      input$nvoices,
      w0 = input$w0 * 2 * pi
    ))
  })
  
  #Process fluorescence anisotropy data
  anisotropy.data <- reactive({
    validate(need((input$example2 != "none") ||
                    (is.null(input$file2) == FALSE), ""))
    if (is.null(input$file2) == FALSE) {
      df <- load(file = input$file2$datapath)
    }
    else if (input$example2 == "perpendicular") {
      df <- load(file = "data/perpendicular.csv")
    }
    df <- scaleData(df, nx = 600)
    df@data$x <- df@data$x * as.numeric(input$time)
    return(df)
  })
  
  #Output input tab main plot
  output$raw <- renderPlotly({
    dat <- processed.data()
    brush <- input.brush()
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
      dat = brush(dat, min(brush[, 4]), max(brush[, 4]), FALSE)
    }
    
    df = longDF_rmNA(dat)
    
    p <- plot_ly(x = df[, 2],
                 y = df[, 4],
                 source = "A") %>%
      add_contour(
        x = dat@wavelength,
        y = dat@data$x,
        z = dat@data$spc,
        visible = TRUE
      ) %>%
      add_markers(
        x = df[, 2],
        y = df[, 4],
        opacity = 0.01,
        hoverinfo = "x"
      ) %>%
      hide_colorbar() %>%
      layout(
        xaxis = list(title = eval(dat@label$.wavelength)),
        yaxis = list(title = eval(dat@label$x)),
        dragmode = "select",
        showlegend = FALSE
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'main') {
        if (input$export.type == 'CSV') {
          export(df, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output input tab x plot
  output$raw.x <- renderPlotly({
    dat <- processed.data()
    brush = input.brush()
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
    }
    
    if (input$proj.style == "max") {
      df <- apply(dat, 2, max)
      df@label$spc <- "Max Intensity"
    } else if (input$proj.style == "int") {
      df <- colSums(dat, label.spc = "Integrated Intensity")
    } else {
      return(NULL)
    }
    
    df_l = longDF_rmNA(df)
    
    p <- plot_ly(
      df_l,
      x = ~ .wavelength,
      y = ~ spc,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(
          title = eval(dat@label$.wavelength),
          showticklabels = T
        ),
        yaxis = list(title = df@label$spc, showticklabels = T)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'x') {
        if (input$export.type == 'CSV') {
          export(df_l, "csv", "Main.csv")
          exportdisp()
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
          exportdisp()
        }
        exportdisp()
      }
    }
    
    return (p)
    
  })
  
  #Output input tab y plot
  output$raw.y <- renderPlotly({
    dat <- processed.data()
    brush = input.brush()
    
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 4]), max(brush[, 4]), FALSE)
    }
    
    if (input$proj.style == "max") {
      df <- apply(dat, 1, max)
      df@label$wavelength = "Max Intensity"
    } else if (input$proj.style == "int") {
      df <- rowSums(dat, label.wavelength = "Integrated Intensity")
      print(df)
    }
    
    df_l = longDF_rmNA(df)
    
    p <- plot_ly(
      df_l,
      x = ~ spc,
      y = ~ x,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(title = df@label$wavelength , showticklabels = T),
        yaxis = list(title = "", showticklabels = F)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'y') {
        if (input$export.type == 'CSV') {
          export(df_l, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output fft tab main plot
  output$fft <- renderPlotly({
    dat <- fft.data()
    brush <- fft.brush()
    
    if (input$fft.datatype == "power") {
      dat <- dat$power
    } else if (input$fft.datatype == "phase") {
      dat <- dat$phase
    } else {
      return(NULL)
    }
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
      dat = brush(dat, min(brush[, 4]), max(brush[, 4]), FALSE)
    }
    
    df = longDF_rmNA(dat)
    p <- plot_ly(x = df[, 2],
                 y = df[, 4],
                 source = "B") %>%
      add_contour(
        x = dat@wavelength,
        y = dat@data$x,
        z = dat@data$spc,
        visible = TRUE
      ) %>%
      add_markers(
        x = df[, 2],
        y = df[, 4],
        opacity = 0.01,
        hoverinfo = "x+y"
      ) %>%
      hide_colorbar() %>%
      layout(
        xaxis = list(title = "Wavenumbers (cm^-1)"),
        yaxis = list(title = eval(dat@label$x)),
        dragmode = "select",
        showlegend = FALSE
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'main') {
        if (input$export.type == 'CSV') {
          export(df, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output fft tab x plot
  output$fft.x <- renderPlotly({
    dat <- fft.data()
    brush = fft.brush()
    
    if (input$fft.datatype == "power") {
      dat <- dat$power
    } else if (input$fft.datatype == "phase") {
      dat <- dat$phase
    }
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
    }
    
    if (input$proj.style == "max") {
      df <- apply(dat, 2, max)
      df@label$spc <- "Max Intensity"
    } else if (input$proj.style == "int") {
      df <- colSums(dat, label.spc = "Integrated Intensity")
    } else {
      return(NULL)
    }
    
    df_l = longDF_rmNA(df)
    
    p <- plot_ly(
      df_l,
      x = ~ .wavelength,
      y = ~ spc,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(title = "Wavenumbers (cm^-1)", showticklabels = T),
        yaxis = list(title = df@label$spc, showticklabels = T)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'x') {
        if (input$export.type == 'CSV') {
          export(df_l, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output fft tab y plot
  output$fft.y <- renderPlotly({
    dat <- fft.data()
    brush <- fft.brush()
    if (input$fft.datatype == "power") {
      dat <- dat$power
    } else if (input$fft.datatype == "phase") {
      dat <- dat$phase
    }
    
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 4]), max(brush[, 4]), FALSE)
    }
    
    if (input$proj.style == "max") {
      df <- apply(dat, 1, max)
      df@label$wavelength  = "Max Intensity"
    } else if (input$proj.style == "int") {
      df = rowSums(dat , label.wavelength  = "Integrated Intensity")
    }
    
    df_l = longDF_rmNA(df)
    
    p <- plot_ly(
      df_l,
      x = ~ spc,
      y = ~ x,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(title = df@label$wavelength , showticklabels = T),
        yaxis = list(title = "", showticklabels = F)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'y') {
        if (input$export.type == 'CSV') {
          export(df_l, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output fft filter plot
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
  
  #Output wavelet main plot
  output$wavelet <- renderPlotly({
    dat <- wavelet.data()
    brush <- wavelet.brush()
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
    }
    dat <- apply(dat, 1, mean)
    df <- longDF_rmNA(dat)
    p <- plot_ly(
      df,
      x = ~ x,
      y = ~ y,
      z = ~ spc,
      type = "contour",
      showscale = FALSE
    ) %>%
      layout(
        xaxis = list(title = "Time Delay (ps)"),
        yaxis = list(title = "|FT|^2 Frequency"),
        dragmode = "select",
        showlegend = FALSE
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'main') {
        if (input$export.type == 'CSV') {
          export(df, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output wavelet x plot
  output$wavelet.x <- renderPlotly({
    dat <- fft.data()
    brush <- wavelet.brush()
    dat <- dat$power
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
    }
    
    df <- apply(dat, 2, max)
    df@label$spc <- "Max Intensity"
    
    df_l = longDF_rmNA(df)
    
    p <- plot_ly(x = df_l[, 2],
                 y = df_l[, 3],
                 source = "C") %>%
      add_markers(
        x = df_l[, 2],
        y = df_l[, 3],
        opacity = 0.01,
        hoverinfo = "x"
      ) %>%
      add_lines(x = df_l[, 2],
                y = df_l[, 3])  %>%
      layout(
        xaxis = list(title = "Frequency (cm^-1)"),
        yaxis = list(title = df@label$spc),
        dragmode = "select",
        showlegend = FALSE
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'x') {
        if (input$export.type == 'CSV') {
          export(df_l, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output wavelet y plot
  output$wavelet.y <- renderPlotly({
    dat <-  wavelet.data()
    brush = wavelet.brush()
    
    if (!is.null(brush)) {
      dat = brush(dat, min(brush[, 3]), max(brush[, 3]), TRUE)
    }
    
    dat <- apply(dat, 1, mean)
    dm <-
      matrix(dat@data$spc,
             nrow = length(unique(dat@data$x)),
             ncol = length(unique(dat@data$y)))
    df <- apply(dm, 2, max)
    df_l <- data.frame(df, unique(dat@data$y))
    p <- plot_ly(
      df_l,
      x = df_l[, 1],
      y = df_l[, 2],
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(title = "Max Intensity", showticklabels = T),
        yaxis = list(title = "", showticklabels = F)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'y') {
        if (input$export.type == 'CSV') {
          export(df_l, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output anisotropy main plot
  output$anisotropy <- renderPlotly({
    perp <- anisotropy.data()
    par <- processed.data()
    brush <- anisotropy.brush()
    
    if (length(par@wavelength) == length(par@wavelength))  {
      df <- par
      df@data$spc <-
        (par@data$spc[, ] - perp@data$spc[, ]) / (par@data$spc[, ] + 2 * perp@data$spc[, ])
    }
  
    if (!is.null(brush)) {
      df = brush(df, min(brush[, 3]), max(brush[, 3]), TRUE)
      df = brush(df, min(brush[, 4]), max(brush[, 4]), FALSE)
    }
    
    df_l = longDF_rmNA(df)
    p <- plot_ly(x = df_l[, 2],
                 y = df_l[, 4],
                 source = "D") %>%
      add_contour(
        x = df@wavelength,
        y = df@data$x,
        z = df@data$spc,
        visible = TRUE
      ) %>%
      add_markers(
        x = df_l[, 2],
        y = df_l[, 4],
        opacity = 0.01,
        hoverinfo = "x"
      ) %>%
      hide_colorbar() %>%
      layout(
        xaxis = list(title = "Wavelength"),
        yaxis = list(title = "Time Delay (ps)"),
        dragmode = "select",
        showlegend = FALSE
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'main') {
        if (input$export.type == 'CSV') {
          export(df, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output anisotropy x plot
  output$anisotropy.x <- renderPlotly({
    perp <- anisotropy.data()
    par <- processed.data()
    brush <- anisotropy.brush()
    
    if (length(par@wavelength) == length(perp@wavelength))  {
      df <- par
      df$data$spc <-
        (par@data$spc[, ] - perp@data$spc[, ]) / (par@data$spc[, ] + 2 * perp@data$spc[, ])
    }
    
    if (!is.null(brush)) {
      df = brush(df, min(brush[, 3]), max(brush[, 3]), TRUE)
    }
    
    if (input$proj.style == "max") {
      df <- apply(df, 2, max)
      df@label$spc <- "Max Intensity"
    } else if (input$proj.style == "int") {
      df <- colSums(df, label.spc = "Integrated Intensity")
    } else {
      return(NULL)
    }
    
    df_l = longDF_rmNA(df)
    
    p <- plot_ly(
      df_l,
      x = ~ .wavelength,
      y = ~ spc,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(
          title = eval(df@label$.wavelength),
          showticklabels = T
        ),
        yaxis = list(title = df@label$spc, showticklabels = T)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'main') {
        if (input$export.type == 'CSV') {
          export(df, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Output anisotropy y plot
  output$anisotropy.y <- renderPlotly({
    perp <- anisotropy.data()
    par <- processed.data()
    brush <- anisotropy.brush()
    if (length(par@wavelength) == length(perp@wavelength))  {
      df <- par
      df$data$spc <- (par@data$spc[, ] - perp@data$spc[, ]) / (par@data$spc[, ] + 2 * perp@data$spc[, ])
    }
    
    if (!is.null(brush)) {
      df = brush(df, min(brush[, 4]), max(brush[, 4]), FALSE)
    }
    
    if (input$proj.style == "max") {
      df <- apply(df, 1, max)
      df@label$wavelength  = "Max Intensity"
    } else if (input$proj.style == "int") {
      df = rowSums(df, label.wavelength = "Integrated Intensity")
    }
    
    df_l = longDF_rmNA(df)
    p <- plot_ly(
      df_l,
      x = ~ spc,
      y = ~ x,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        xaxis = list(title = df@label$wavelength, showticklabels = T),
        yaxis = list(title = "", showticklabels = F)
      )
    
    if (input$export == TRUE) {
      if (input$export.graph == 'main') {
        if (input$export.type == 'CSV') {
          export(df, "csv", "Main.csv")
        } else if (input$export.type == 'PNG') {
          export(p, "png", "Main.png")
        }
        exportdisp()
      }
    }
    
    return (p)
  })
  
  #Brush for input tab main plot
  input.brush <- reactive({
    b <- event_data("plotly_selected", source = "A")
    if (is.null(b)) {
      
    } else
      return(b)
  })
  
  #Brush for fft tab main plot
  fft.brush <- reactive({
    b <- event_data("plotly_selected", source = "B")
    if (is.null(b)) {
      
    } else
      return(b)
  })
  
  #Brush for wavelet tab x plot
  wavelet.brush <- reactive({
    b <- event_data("plotly_selected", source = "C")
    if (is.null(b)) {
      
    } else
      return(b)
  })
  
  #Brush for anisotropy tab main plot
  anisotropy.brush <- reactive({
    b <- event_data("plotly_selected", source = "D")
    if (is.null(b)) {
      
    } else
      return(b)
  })
  
  #Modal for import confirmation
  dataModal <- function(failed = FALSE) {
    modalDialog("Remember to input a file.",
                easyClose = TRUE,
                footer = actionButton("ok", "OK"))
  }
  
  #Modal for import confirmation - Force to work only once
  observeEvent(input$mytabs, {
    if (is.null(input$ok) == TRUE) {
      showModal(dataModal())
    }
  })
  
  #Modal for import confirmation - Recieve exit
  observeEvent(input$mytabs, {
    if (is.null(input$ok) == TRUE) {
      showModal(dataModal())
    }
  })
  
  #Modal for export confirmation
  exportdisp <- reactive({
    showModal(modalDialog(
      "You exported a file.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  observeEvent(input$ok, {
    removeModal()
  })
  
  
})
