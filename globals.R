library(hyperSpec)
library(fields)
library(Rwave)
library(plyr)

setClass("Spectra", 
  contains = "hyperSpec"
)

loadData <- function(file = file.choose(), sep = ",", ...) {
  data <- read.table(file, sep = ",", stringsAsFactors = FALSE, ...)
  
  time <- as.numeric(data[-1, 1])
  wavelength <- as.vector(t(data[1, -1]))
  data <- as.matrix(data[-1, -1])

  if (!is.unsorted(wavelength)) {
    
  } else if (!is.unsorted(rev(wavelength))) {
    wavelength <- rev(wavelength)
    data <- t(apply(data, 1, rev))
  } else {
    stop("Dataset wavelengths are not ordered")
  }
  bad.wavelengths <- vector()
  sapply(seq(from = 1, to = length(wavelength), by = 1), 
         function(i) {
           if (all(data[, i] == 0)) {
             bad.wavelengths <<- c(bad.wavelengths, -i)
           }
         }
  )
  
  if (length(bad.wavelengths) != 0) {
    wavelength <- wavelength[bad.wavelengths]
    data <- data[, bad.wavelengths]
  }
  
  dimnames(data) <- list(x = time, y = wavelength)
  spec.data <- new ("Spectra", 
    data = data.frame(
      x = time
    ),
    spc = data,
    wavelength = wavelength,
    label = list(
      x = expression("Time (ps)"),
      spc = expression("Stimulated Raman gain"),
      .wavelength = expression("Wavelength (nm)")
    )                
  )
  return(spec.data)
}

trimData <- function(object, 
                     remove.negative = TRUE, 
                     uniform = TRUE, 
                     start.time = NA, 
                     end.time = NA) {
  
  # handles NA values for start.time
  if (is.na(start.time)) {
    # if remove.negative is set
    if (remove.negative) {
      start.time <- head(object@data$x[object@data$x >= 0], n = 1)
    } else {
      start.time <- head(object@data$x, n = 1)
    }
  }
  # NA value for end.time
  if (is.na(end.time)) {
    end.time <- tail(object@data$x, n = 1)
  }
  # times to indicies
  start.idx <- which.min(abs(object@data$x - start.time))
  end.idx <- which.min(abs(object@data$x - end.time))
  # if the data is supposed to be uniform
  if (uniform) {
    delta <- abs(object@data$x[start.idx + 1] - object@data$x[start.idx])
    
    for (i in (start.idx + 1):end.idx) {
      difference <- abs(object@data$x[i - 1] - object@data$x[i])
      if (abs(difference - delta) > 50e-15) {
        end.idx <- i - 1
        (break)()
      }
    }
  }
  
  trimmed <- object[start.idx:end.idx, ]
  return(trimmed)
}

scaleData <- function(object, nx = length(object@wavelength), 
                      ny = length(object@data$x)) {
  
  data <- list(x = object@wavelength, y = object@data$x, z = t(object@data$spc))
  x.interp <- seq(from = min(data$x), to = max(data$x), length.out = nx)
  y.interp <- seq(from = min(data$y), to = max(data$y), length.out = ny)
  interp.list <- list(x = x.interp, y = y.interp)
  interp.grid <- interp.surface.grid(data, interp.list)
  interp <- new ("Spectra", 
    data = data.frame(
      x = y.interp
    ),
    spc = t(interp.grid$z),
    wavelength = x.interp,
    label = object@label
  )
  return(interp)
}

time2invcm <- function(x) {
  # time in picoseconds
  c <- 299792458
  fs <- 1E12 / mean(diff(x))
  l = ceiling(length(x)/2) + 1
  f <- seq(from = 0, by = 1, length.out = l) * fs / (length(x))
  invcm <- f / (100 * c)
  return(invcm)
}

dft <- function(object) {
  f <- apply(object, 2, fft)
  
  trimmed <- f[1:(ceiling(length(f@data$x)/2) + 1), ]
  times <- object@data$x
  trimmed@data$x <- time2invcm(times)[1:(ceiling(length(f@data$x)/2) + 1)]
  trimmed@label$x <- expression(Frequency (cm^{-1}))
  return(trimmed)
}

wavelet <- function(input, noctave, nvoice = 1, w0 = 2 * pi, twoD = TRUE) {
  # time in picoseconds
  
  data <- apply(input[[]], 2, function(x){
    Mod(cwt(x, noctave, nvoice = nvoice, w0 = w0, plot = FALSE))^2
    }
  )
  
  scales <- (2^(1/nvoice))^(0:(noctave * nvoice - 1))
  
  dt <- mean(diff(input@data$x)) * 1E-12
  frequency <- w0 / (scales * dt * 4 * pi)
  c <- 299792458
  invcm <- frequency / (100 * c)
  
  
  spectra <- new("Spectra", 
    data = data.frame(
      x = rep(input@data$x, times = length(invcm)),
      y = rep(invcm, each = length(input@data$x))
    ),
    spc = data,
   wavelength = input@wavelength,
   labels = list(
     x = input@label$x,
     y = expression(paste("Frequency (cm" ^ "-1", ")")),
     .wavelength = input@label$.wavelength
   )
  )
}
