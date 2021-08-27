library(hyperSpec)
library(fields)
library(Rwave)
library(plyr)
library(RColorBrewer)
library(plotrix)

plot.raw <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  zmax <- max(abs(data))
  par(mar = c(4.5, 5, 1.5, 7.5))
  plotmat(
    data,
    y = "x",
    contour = FALSE,
    col = brewer.pal(9, "RdBu"),
    zlim = c(-zmax, zmax)
  )
  plotmat(
    data,
    y = "x",
    contour = TRUE,
    col = c("black"),
    add = TRUE,
    zlim = c(-zmax, zmax),
    drawlabels = FALSE
  )
  # par(cex=1.0)
  nxticks <<- length(axTicks(1))
}

plot.raw.x <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  par(mar = c(1.5, 5, 1, 7.5))
  integrated.data <-
    colSums(data, label.spc = "Integrated\n intensity")
  plotspc(
    integrated.data,
    func = sum,
    plot.args = list(xaxs = "i"),
    title.args = list(xlab = ""),
    axis.args = list(x = list(labels = FALSE)),
    nxticks = nxticks
  )
}

plot.raw.y <- function(data, brush, x.axis) {
  if (is.null(data)) {
    return(NULL)
  }
  par(mar = c(4.5, 1.5, 1.5, 1.5))
  
  plot(
    x = rowSums(data[[]]),
    y = data@data$x,
    yaxs = "i",
    type = "l",
    xlab = "Integrated\n intensity",
    ylab = "",
    yaxt = "n"
  )
  axis(side = 2, labels = FALSE)
  abline(v = 0, lty = 2)
  
  if (!is.null(brush)) {
    l = wl2i(data, brush$xmin):wl2i(data, brush$xmax)
    trimmed.data <- data[[l = l, wl.index = TRUE]]
    lines(
      x = rowSums(trimmed.data),
      y = data@data$x,
      type = "l",
      col = '#e95420',
      lwd = 5
    )
    if (x.axis == "wavenumber") {
      label <- bquote(.(as.integer(brush$xmin)) ~ -~
                        .(as.integer(brush$xmax)) ~ cm ^ {
                          -1
                        })
    } else {
      label <- bquote(.(as.integer(brush$xmin)) ~ -~
                        .(as.integer(brush$xmax)) ~ nm)
    }
    mtext(label,
          col = '#e95420',
          font = 2,
          cex = 1.2)
  }
}

plot.fft <- function(data, datatype) {
  if (is.null(data)) {
    return(NULL)
  }
  if (datatype == "power") {
    data <- data$power
    palette <- "YlOrRd"
  } else if (datatype == "phase") {
    data <- data$phase
    palette <- "RdBu"
  } else {
    return(NULL)
  }
  
  par(mar = c(4.5, 5, 1.5, 7.5))
  plotmat(data,
          y = "x",
          contour = FALSE,
          col = brewer.pal(9, palette))
  plotmat(
    data,
    y = "x",
    contour = TRUE,
    col = c("black"),
    add = TRUE,
    drawlabels = FALSE
  )
}

plot.fft.x <- function(data, datatype, projection) {
  if (is.null(data)) {
    return(NULL)
  }
  if (datatype == "power") {
    data <- data$power
  } else if (datatype == "phase") {
    data <- data$phase
  } else {
    return(NULL)
  }
  
  par(mar = c(1.5, 5, 1, 7.5))
  if (projection == "max") {
    integrated.data <- apply(data, 2, max)
    integrated.data@label$spc = "Max Intensity"
    
  }
  else if (projection == "integrated") {
    integrated.data <- colSums(data, label.spc = "Integrated Intensity")
  } else {
    return(NULL)
  }
  plotspc(
    integrated.data,
    plot.args = list(xaxs = "i"),
    title.args = list(xlab = ""),
    axis.args = list(x = list(labels = FALSE)),
    nxticks = nxticks
  )
}

plot.fft.y <- function(data, datatype, projection, brush, x.axis) {
  if (is.null(data)) {
    return(NULL)
  }
  
  if (datatype == "power") {
    data <- data$power
    x.data <- rowSums(data[[]])
    xlim <- c(0, max(x.data))
  } else if (datatype == "phase") {
    data <- data$phase
    x.data <- rowSums(data[[]])
    datalim <- max(abs(x.data))
    xlim <- c(-datalim, datalim)
  } else {
    return(NULL)
  }
  
  if (projection == "max") {
    x.data <- apply(data[[]], 1, max)
    xlim <- c(0, max(x.data))
    x.label = "Max Intensity"
    
  } else if (projection == "integrated") {
    x.data = rowSums(data[[]])
    xlim <- c(0, max(x.data))
    x.label = "Integrated Intensity"
  } else {
    return(NULL)
  }
  
  par(mar = c(4.5, 1.5, 1.5, 1.5))
  
  plot(
    x = x.data,
    y = data@data$x,
    xlim = xlim,
    yaxs = "i",
    type = "l",
    xlab = x.label,
    ylab = "",
    yaxt = "n"
  )
  axis(side = 2, labels = FALSE)
  abline(v = 0, lty = 2)
  
  if (!is.null(brush)) {
    l = wl2i(data, brush$xmin):wl2i(data, brush$xmax)
    trimmed.data <- data[[l = l, wl.index = TRUE]]
    if (projection == "max") {
      trimmed.data <- apply(data[[l = l, wl.index = TRUE]], 1, max)
    } else if (projection == "integrated") {
      trimmed.data = rowSums(trimmed.data)
    } else {
      return(NULL)
    }
    lines(
      x = trimmed.data,
      y = data@data$x,
      type = "l",
      col = '#e95420',
      lwd = 5
    )
    
    if (x.axis == "wavenumber") {
      label <- bquote(.(as.integer(brush$xmin)) ~ -~
                        .(as.integer(brush$xmax)) ~ cm ^ {
                          -1
                        })
    } else {
      label <- bquote(.(as.integer(brush$xmin)) ~ -~
                        .(as.integer(brush$xmax)) ~ nm)
    }
    mtext(label,
          col = '#e95420',
          font = 2,
          cex = 1.2)
  }
}

plot.wavelet <- function(data, brush, x.axis) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!is.null(brush)) {
    l = wl2i(data, brush$xmin):wl2i(data, brush$xmax)
    
    data <- data[l = l, wl.index = TRUE]
    lims <- c(as.integer(brush$xmin),
              as.integer(brush$xmax))
  } else {
    lims <- c(as.integer(min(data@wavelength)),
              as.integer(max(data@wavelength)))
  }
  if (x.axis == "wavenumber") {
    label <- bquote(.(lims[1]) ~ -~
                      .(lims[2]) ~ cm ^ {
                        -1
                      })
  } else {
    label <- bquote(.(lims[1]) ~ -~
                      .(lims[2]) ~ nm)
  }
  par(mar = c(4, 5, 1, 6))
  pnl <- function(..., label) {
    panel.levelplot(...)
    grid.text(
      label,
      unit(0.99, "npc"),
      unit(0.99, "npc"),
      gp = gpar(fontsize = 24, fontface = "bold"),
      just = c("right", "top")
    )
  }
  plotmap(
    data,
    aspect = "fill",
    contour = TRUE,
    col.regions = brewer.pal(9, "YlOrRd"),
    cuts = 8,
    label = label,
    panel = pnl
  )
}

plot.wavelet.selector <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  par(mar = c(4, 5, 1, 1))
  integrated.data <-
    colSums(data$power, label.spc = "FFT integrated\n power")
  
  plotspc(integrated.data, plot.args = list(xaxs = "i"))
}