
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
setClass("FrequencyResult", 
  representation(
    data = "array", 
    frequency = "vector", 
    wavenumber = "vector"
  )
)

