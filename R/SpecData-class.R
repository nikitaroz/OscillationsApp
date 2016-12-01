#' @import hyperSpec
NULL

# SpecData should only contain one instance of data methods to extract
# data will return new instances of the data that can trimmed, etc.
# trimmed should be a parameter, that will be accessed by the function
# that calls the object, instead of passed as a kwarg

# callNextMethod

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
setClass("SpecData", 
  contains = "hyperSpec",
  representation = representation(
    is.uniform = "logical",
    type = "character"
  )
)


