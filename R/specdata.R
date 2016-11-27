

# SpecData should only contain one instance of data
# methods to extract data will return new instances of the data that can
# trimmed, etc. 
# trimmed should be a parameter, that will be accessed by the function that
# calls the object, instead of passed as a kwarg


#' Title
#'
#' @slot data array. 
#' @slot time vector. 
#' @slot wavenumber vector. 
#' @slot is.uniform logical. 
#'
#' @return
#' @export
#'
#' @examples
setClass("SpecData", representation(
  data = "array",
  time = "vector",
  wavenumber = "vector",
  is.uniform = "logical"
))

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

