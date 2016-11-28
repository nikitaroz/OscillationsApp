
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

setMethod("wavelet", signature("SpecData"), 
          function(object, noctave = 2, nvoice = 8, fc = 1, ...) {
            if (!object@is.uniform) {
              stop("data needs to be uniform")
            }
            data <- object@data
            ntime <- length(object@time)
            nwavenumber <- length(object@wavenumber)
            times <- object@time
            
            scales <- (2^(1/nvoice))^(0:(noctave * nvoice - 1))
            w0 <- fc * (2 * pi)
            dt <- abs(times[2] - times[1]) * 1e-12
            frequency <- fc/(scales * dt * 2)
            c <- 299792458
            frequency <- (frequency/c)/100
            result <- new("WaveletResult")
            result@time <- times
            result@wavenumber <- object@wavenumber
            result@frequency <- rev(frequency)
            result@data <- array(dim = c(ntime, length(result@frequency), nwavenumber))
            for (i in 1:nwavenumber) {
              coefs <- cwt(data[, i], noctave = noctave, nvoice = nvoice, w0 = w0, 
                           plot = FALSE)
              
              result@data[, , i] <- t(apply(coefs, 1, function(y) rev(y)))
            }
            return(result)
          })
