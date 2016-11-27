

#' Title
#'
#' @slot data array. 
#' @slot frequency vector. 
#' @slot wavenumber vector. 
#'
#' @return
#' @export
#'
#' @examples
setClass("FrequencyResult", representation(
  data = "array",
  frequency = "vector",
  wavenumber = "vector"
))


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