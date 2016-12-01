#' @importFrom methods new
#' @importFrom utils read.table
NULL


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
  spec.data <- new ("hyperSpec", 
    data = data.frame(
      x = time
    ),
    spc = data,
    wavelength = wavelength
  )
  return(spec.data)
}
