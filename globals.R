library(hyperSpec)

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
                     uniform = FALSE, 
                     start.time = NA, 
                     end.time = NA) {
  
  # handles NA values for start.time
  if (is.na(start.time)) {
    # if remove.negative is set
    if (remove.negative) {
      start.time <- head(object@time[object@time >= 0], n = 1)
    } else {
      start.time <- head(object@time, n = 1)
    }
  }
  # NA value for end.time
  if (is.na(end.time)) {
    end.time <- tail(object@time, n = 1)
  }
  # times to indicies
  start.idx <- which.min(abs(object@time - start.time))
  end.idx <- which.min(abs(object@time - end.time))
  # if the data is supposed to be uniform
  if (uniform) {
    delta <- abs(object@time[start.idx + 1] - object@time[start.idx])
    
    for (i in (start.idx + 1):end.idx) {
      difference <- abs(object@time[i - 1] - object@time[i])
      if (abs(difference - delta) > 1e-05) {
        end.idx <- i - 1
        (break)()
      }
    }
  }
  
  trimmed <- new("Spectra")
  trimmed@time <- object@time[start.idx:end.idx]
  trimmed@wavenumber <- object@wavenumber
  trimmed@data <- object@data[start.idx:end.idx, ]
  if (uniform) {
    trimmed@is.uniform <- TRUE
  } else {
    trimmed@is.uniform <- FALSE
  }
  return(trimmed)
}