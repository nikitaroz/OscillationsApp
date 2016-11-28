

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
#' @param object 
#' @param window 
#'
#' @return
#' @export
#'
#' @examples
setGeneric("dft", function(object, window = NA) standardGeneric("dft"))


setMethod("lomb", signature("SpecData"), 
          function(object, from = NULL, to = NULL) {
            data <- object@data
            ntime <- length(object@time)
            nwavenumber <- length(object@wavenumber)
            times <- object@time
            
            result <- new("FrequencyResult")
            
            if (!is.null(from)) {
              from = from/((1e+12/100)/299792458)
            }
            if (!is.null(to)) {
              to = to/((1e+12/100)/299792458)
            }
            # TODO: for loop
            for (i in 1:nwavenumber) {
              p <- lsp(data[, i], times = times, from = from, to = to, plot = FALSE)
              if (i == 1) {
                result@data <- array(dim = c(p$n.out, nwavenumber))
              }
              result@data[, i] <- p$power
            }
            result@frequency <- p$scanned * (1e+12/100)/299792458
            result@wavenumber <- object@wavenumber
            
            return(result)
          }
)

setMethod("dft", signature("SpecData"), 
          function(object, window = NA) {
            if (!object@is.uniform) {
              stop("Data needs to be uniform")
            }
            data <- object@data
            ntime <- length(object@time)
            nwavenumber <- length(object@wavenumber)
            
            if (!is.na(window)) {
              if (window == "hamming") {
                w = hamming(ntime)
              }
              data <- apply(data, 2, function(x) x * w)
            }
            
            result <- new("FrequencyResult")
            result@data <- array(dim = c(ntime, nwavenumber))
            # TODO: for loop
            for (i in 1:nwavenumber) {
              result@data[, i] <- fft(data[, i])
            }
            result@data <- result@data[1:(ceiling(ntime/2) + 1), ]
            
            c <- 299792458
            fs <- 1e+12/mean(diff(object@time))
            
            f <- fs * (0:ceiling(ntime/2))/ntime
            result@frequency <- f/(c * 100)
            result@wavenumber <- object@wavenumber
            return(result)
          }
)

