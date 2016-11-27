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


#' Title
#'
#' @param object 
#' @param window 
#'
#' @return
#' @export
#'
#' @examples
setGeneric("dft", function(object, window = NA)
  standardGeneric("dft")
)


#' Title
#'
#' @param object 
#' @param noctave 
#' @param nvoice 
#' @param fc 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setGeneric("wavelet", function(object, noctave = 2, nvoice = 8, fc = 1, ...)
  standardGeneric("wavelet")
)

#' Title
#'
#' @param object 
#' @param from 
#' @param to 
#'
#' @return
#' @export
#'
#' @examples
setGeneric("lomb", function(object, from = NULL, to = NULL)
  standardGeneric("lomb")
)

#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#' @param type 
#' @param interactive 
#' @param component 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setGeneric("imshow", function(x = seq(0, 1, length.out = nrow(z)), 
                              y = seq(0, 1, length.out = ncol(z)), 
                              z, type = "contour", 
                              interactive = FALSE, component = NA, ...)
  standardGeneric("imshow")
)


#' Title
#'
#' @param file 
#' @param sep 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param object 
#' @param remove.negative 
#' @param uniform 
#' @param start.time 
#' @param end.time 
#'
#' @return
#' @export
#'
#' @examples
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





#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#' @param nx 
#' @param ny 
#'
#' @return
#' @export
#'
#' @examples
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