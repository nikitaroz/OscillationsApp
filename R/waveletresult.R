


#' Title
#'
#' @slot data array. 
#' @slot wavenumber vector. 
#' @slot time vector. 
#' @slot frequency vector. 
#'
#' @return
#' @export
#'
#' @examples
setClass("WaveletResult", representation(
  data = "array",
  wavenumber = "vector",
  time = "vector",
  frequency = "vector"
))

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