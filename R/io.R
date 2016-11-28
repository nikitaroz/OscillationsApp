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
  spec.data <- new("SpecData")
  spec.data@time <- data[-1, 1]
  spec.data@wavenumber <- as.vector(t(data[1, -1]))
  spec.data@data <- as.matrix(data)[-1, -1]
  spec.data@is.uniform <- FALSE
  if (!is.unsorted(spec.data@wavenumber)) {
    
  } else if (!is.unsorted(rev(spec.data@wavenumber))) {
    spec.data@wavenumber <- rev(spec.data@wavenumber)
    spec.data@data <- t(apply(spec.data@data, 1, rev))
  } else {
    stop("Dataset wavelengths are not ordered")
  }
  bad.wavenumbers <- vector()
  sapply(seq(from = 1, to = length(spec.data@wavenumber), by = 1), 
         function(i) {
           if (all(spec.data@data[, i] == 0)) {
             bad.wavenumbers <<- c(bad.wavenumbers, -i)
           }
         }
  )
  
  if (length(bad.wavenumbers) != 0) {
    spec.data@wavenumber <- spec.data@wavenumber[bad.wavenumbers]
    spec.data@data <- spec.data@data[, bad.wavenumbers]
  }
  colnames(spec.data@data) <- NULL
  return(spec.data)
}
