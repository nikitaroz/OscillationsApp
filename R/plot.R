
# TOOD: This is a terrible API (change to plot?)
#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#' @param type 
#' @param interactive 
#' @param component 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setGeneric("imshow", function(x = seq(0, 1, length.out = nrow(z)), 
                              y = seq(0, 1, length.out = ncol(z)), 
                              z, 
                              type = "contour", 
                              interactive = FALSE, 
                              component = NA, ...) 
  standardGeneric("imshow")
)


setMethod("imshow", signature(z = "matrix"), 
          function(x = seq(0, 1, length.out = nrow(z)), 
                   y = seq(0, 1, length.out = ncol(z)), 
                   z, type = "contour", interactive = FALSE, component = NA, ...) {
            
            if (is.na(component)) {
              
            } else if (component == "intensity") {
              z <- Mod(z)^2
            } else if (component == "phase") {
              z <- Arg(z)
            }
            
            if (type == "contour" & !interactive) {
              return(
                filled.contour(x = x, y = y, z = z, 
                               color.palette = colorRamps::matlab.like, 
                               ...)
              )
            } else if (type == "contour" & interactive) {
              return(
                plot_ly(x = x, y = y, z = z, 
                        type = "contour", 
                        line = list(width = 0), 
                        colors = colorRamps::matlab.like(8), ...)
              )
            } else if (type == "surface" & interactive) {
              return(
                plot_ly(x = x, y = y, z = z, 
                        type = "surface", 
                        colors = colorRamps::matlab.like(8), ...)
              )
            }
          }
)