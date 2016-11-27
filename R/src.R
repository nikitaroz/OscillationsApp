library(plotly)
library(lomb)
library(signal)
library(Rwave)
library(fields)
library(doMC)
registerDoMC(cores=4)

# functions: 
# for NOW only implement trimmed data functions
# fft (trimmed)
# lomb (trimmed)
# wavelet 


# SpecData should only contain one instance of data
# methods to extract data will return new instances of the data that can
# trimmed, etc. 
# trimmed should be a parameter, that will be accessed by the function that
# calls the object, instead of passed as a kwarg

# setClass("SpecData", representation(
#   data = "array",
#   time = "vector",
#   wavenumber = "vector",
#   trimmed.time = "vector",
#   trimmed.data = "matrix",
#   ntime = "numeric",
#   nwavenumber = "numeric",
#   trimmed.ntime = "numeric"
# ))

setClass("SpecData", representation(
  data = "array",
  time = "vector",
  wavenumber = "vector",
  is.uniform = "logical"
))

setClass("WaveletResult", representation(
  data = "array",
  wavenumber = "vector",
  time = "vector",
  frequency = "vector"
))

setClass("FrequencyResult", representation(
  data = "array",
  frequency = "vector",
  wavenumber = "vector"
))

setGeneric("dft", function(object, window = NA)
  standardGeneric("dft")
)

setGeneric("wavelet", function(object, noctave = 2, nvoice = 8, fc = 1, ...)
  standardGeneric("wavelet")
)

setGeneric("lomb", function(object, from = NULL, to = NULL)
  standardGeneric("lomb")
)

setGeneric("imshow", function(x = seq(0, 1, length.out = nrow(z)), 
                              y = seq(0, 1, length.out = ncol(z)), 
                              z, type = "contour", 
                              interactive = FALSE, component = NA, ...)
  standardGeneric("imshow")
)

setMethod("wavelet", signature("SpecData"),
          function(object, noctave = 2, nvoice = 8, fc = 1, ...){
            if(!object@is.uniform) {
              stop("data needs to be uniform")
            }
            data <- object@data
            ntime <- length(object@time)
            nwavenumber <- length(object@wavenumber)
            times <- object@time
            
            scales <- (2^(1/nvoice))^(0:(noctave*nvoice - 1))
            w0 <- fc * (2 * pi)
            dt <- abs(times[2] - times[1]) * 1E-12
            frequency <- fc / (scales * dt * 2)
            c <- 299792458
            frequency <- (frequency/c) / 100
            result <- new("WaveletResult")
            result@time <- times
            result@wavenumber <- object@wavenumber
            result@frequency <- rev(frequency)
            result@data <- array(dim = c(ntime, 
                                         length(result@frequency), 
                                         nwavenumber))
            for(i in 1:nwavenumber){
              coefs <- cwt(data[, i], noctave = noctave, 
                  nvoice = nvoice, w0 = w0, plot = FALSE)
              
              result@data[, , i] <- t(apply(coefs, 1, function(y) rev(y)))
            }
            return(result)
          }
)

setMethod("lomb", signature("SpecData"), 
          function(object, from = NULL, to = NULL){
            data <- object@data
            ntime <- length(object@time)
            nwavenumber <- length(object@wavenumber)
            times <- object@time
            
            result <- new("FrequencyResult")
            
            if(!is.null(from)) {
              from = from / ( (1e+12 / 100) / 299792458 )
            } 
            if(!is.null(to)) {
              to = to / ( (1e+12 / 100) / 299792458 )
            }
            # TODO: for loop
            for(i in 1:nwavenumber){
              p <- lsp(data[, i], times = times, from = from, to = to, plot = FALSE)
              if(i == 1) {
                result@data <- array(dim = c(p$n.out, nwavenumber))
              }
              result@data[, i] <- p$power
            }
            result@frequency <- p$scanned * (1e+12 / 100) / 299792458
            result@wavenumber <- object@wavenumber
            
            return(result)
          }
)

setMethod("dft", signature("SpecData"), 
          function(object, window = NA){
            
            if(!object@is.uniform){
              stop("Data needs to be uniform")
            }
            data <- object@data
            ntime <- length(object@time)
            nwavenumber <- length(object@wavenumber)
            
            if(!is.na(window)){
              if(window == "hamming"){
                w = hamming(ntime)
              }
              data <- apply(data, 2, function(x) x * w)
            }
            
            result <- new("FrequencyResult")
            result@data <- array(dim = c(ntime, nwavenumber))
            # TODO: for loop
            for(i in 1:nwavenumber){
              result@data[, i] <- fft(data[, i])
            }
            result@data <- result@data[1:(ceiling(ntime/2) + 1), ]
            
            c <- 299792458
            fs <- 1e+12 / mean(diff(object@time))

            f <- fs * (0:ceiling(ntime/2))/ntime
            result@frequency <- f / (c * 100)
            result@wavenumber <- object@wavenumber
            return(result)
          }
)

loadData <- function(file = file.choose(), sep = ",", ...) {
  data <- read.table(file, sep = ",", ...)
  spec.data <- new("SpecData")
  spec.data@time <- data[-1, 1]
  spec.data@wavenumber <- as.vector(t(data[1, -1]))
  spec.data@data <- as.matrix(data)[-1, -1]
  spec.data@is.uniform <- FALSE
  if(!is.unsorted(spec.data@wavenumber)){
    
  } else if(!is.unsorted(rev(spec.data@wavenumber))){
    spec.data@wavenumber <- rev(spec.data@wavenumber)
    spec.data@data <- t(apply(spec.data@data, 1, rev))
  }
  else {
    stop("Dataset wavelengths are not ordered")
  }
  bad.wavenumbers <- vector()
  sapply(seq(from = 1, to = length(spec.data@wavenumber), by = 1), 
         function(i) {
           if(all(spec.data@data[ , i] == 0)){
             bad.wavenumbers <<- c(bad.wavenumbers, -i)
           }
         }
  )
  if(length(bad.wavenumbers) != 0) {
  spec.data@wavenumber <- spec.data@wavenumber[bad.wavenumbers]
  spec.data@data <- spec.data@data[, bad.wavenumbers]
  }
  colnames(spec.data@data) <- NULL
  
  return(spec.data)
}

trimData <- function(object, remove.negative = TRUE, 
                     uniform = FALSE, 
                     start.time = NA, 
                     end.time = NA){
  
  # handles NA values for start.time
  if(is.na(start.time)) {
    # if remove.negative is set
    if(remove.negative) {
      start.time <- head(object@time[object@time >= 0], n = 1)
    } else {
      start.time <- head(object@time, n = 1)
    }
  }
  # NA value for end.time
  if(is.na(end.time)) {
    end.time <- tail(object@time, n = 1)
  }
  # times to indicies
  start.idx <- which.min(abs(object@time - start.time))
  end.idx <- which.min(abs(object@time - end.time))
  # if the data is supposed to be uniform
  if(uniform) {
    delta <- abs(object@time[start.idx + 1] - object@time[start.idx])
    
    for(i in (start.idx + 1):end.idx) {
      difference <- abs(object@time[i - 1] - object@time[i])
      if(abs(difference - delta) > 1E-5) {
        end.idx <- i - 1
        break()
      }
    }
  }
  
  trimmed <- new("SpecData")
  trimmed@time <- object@time[start.idx:end.idx]
  trimmed@wavenumber <- object@wavenumber
  trimmed@data <- object@data[start.idx:end.idx, ]
  if(uniform) {
    trimmed@is.uniform <- TRUE
  } else {
    trimmed@is.uniform <- FALSE
  }
  return(trimmed)
}

setMethod("as.data.frame", signature("WaveletResult"),
          function(x, row.names = NULL, optional = FALSE, ...){
            data <- as.vector(x@data)
            nwavenumber <- length(x@wavenumber)
            ntime <- length(x@time)
            nfrequency <- length(x@frequency)
            wavenumber <- rep(x@wavenumber, each = nfrequency*ntime)
            time <- rep(rep(x@time, each = nfrequency), times = nwavenumber)
            frequency <- rep(x@frequency, times = nwavenumber*ntime)
            df <- data.frame(wavenumber, time, frequency, data)
            colnames(df) <- c("wavenumber", "time", "frequency", "wavelet")
            return(df)
          }
)

setMethod("as.data.frame", signature("FrequencyResult"), 
          function(x, row.names = NULL, optional = FALSE, ...) {
            data <- as.vector(x@data)
            nwavenumber <- length(x@wavenumber)
            nfrequency <- length(x@frequency)
            wavenumber <- rep(x@wavenumber, each = nfrequency)
            frequency <- rep(x@frequency, times = nwavenumber)
            df <- data.frame(wavenumber, frequency, data)
            colnames(df) <- c("wavenumber", "frequency", "fft")
            return(df)
          }
)

scaleData <- function(x, y, z, nx = nrow(z), ny = ncol(z)){
  if(nx == nrow(z) & ny == ncol(z)){
    return(list(x = x, y = y, z = z))
  }
  data <- list(x = x, y = y, z = z)
  x.interp <- seq(from = min(x), to = max(x), length.out = nx)
  y.interp <- seq(from = min(y), to = max(y), length.out = ny)
  interp.list <- list(x = x.interp, y = y.interp)
  interp.grid <- interp.surface.grid(data, interp.list)
  interp.grid$z <- as.matrix(interp.grid$z, nrow = nx, ncol = ny)
  return(interp.grid)
}

setMethod("imshow", signature(z = "matrix"),
          function(x = seq(0, 1, length.out = nrow(z)), 
                   y = seq(0, 1, length.out = ncol(z)), 
                   z, type = "contour", interactive = FALSE, 
                   component = NA, ...){
            
            if(is.na(component)) {
              
            } else if(component == "intensity"){
              z <- Mod(z)^2
            } else if (component == "phase") {
              z <- Arg(z)
            }
            
            if(type == "contour" & !interactive){
              return(filled.contour(x = x, y = y, z = z, 
                                    color.palette = colorRamps::matlab.like, ...))
            } else if(type == "contour" & interactive){
              return(plot_ly(x = x, y = y, z = z, type = "contour", 
                             line = list(width = 0), 
                             colors = colorRamps::matlab.like(8), ...))
            } else if(type == "surface" & interactive){
              return(plot_ly(x = x, y = y, z = z, type = "surface", 
                             colors = colorRamps::matlab.like(8), ...))
            }
          }
)

# Desired plot types:
# interactive and static
# contour, surface, and image