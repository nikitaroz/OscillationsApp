

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
