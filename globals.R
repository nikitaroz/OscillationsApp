library(hyperSpec)
library(fields)

setClass("Spectra", 
  contains = "hyperSpec"
)

loadData <- function(file = file.choose(), sep = ",", ...) {
  data <- read.table(file, sep = ",", ...)
  time <- as.numeric(data[-1, 1])
  wavelength <- as.vector(t(data[1, -1]))
  data <- as.matrix(data)[-1, -1]
  
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
  d = data.frame(
    x = rep(wavelength, times = length(time)),
    y = rep(time, each = length(wavelength))
  )
  dimnames(data) <- list(x = time, y = wavelength)
  spec.data <- new ("Spectra", 
                    data = data.frame(
                      x = time
                    ),
                    spc = data,
                    wavelength = wavelength
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
      if (abs(difference - delta) > 1e-05) {
        end.idx <- i - 1
        (break)()
      }
    }
  }
  
  trimmed <- new ("Spectra", 
                    data = data.frame(
                      x = object@data$x[start.idx:end.idx]
                    ),
                    spc = object[start.idx:end.idx, ],
                    wavelength = object@wavelength
  )
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
                    wavelength = x.interp
  )
  return(interp)
}


