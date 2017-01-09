#' @importFrom fields interp.surface.grid
NULL

#' Title
#'
#' @param object 
#' @param remove.negative 
#' @param uniform 
#' @param start.time 
#' @param end.time 
#'
#' @return
#' @export
#'
#' @examples



#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#' @param nx 
#' @param ny 
#'
#' @return
#' @export
#'
#' @examples


# TODO: bad API, interpolation requires additional package
# TODO: reimplement for each class
scaleData <- function(x, y, z, nx = nrow(z), ny = ncol(z)) {
  if (nx == nrow(z) & ny == ncol(z)) {
    return(list(x = x, y = y, z = z))
  }
  data <- list(x = x, y = y, z = z)
  x.interp <- seq(from = min(x), to = max(x), length.out = nx)
  y.interp <- seq(from = min(y), to = max(y), length.out = ny)
  interp.list <- list(x = x.interp, y = y.interp)
  interp.grid <- interp.surface.grid(data, interp.list)
  interp.grid$z <- as.matrix(interp.grid$z, nrow = nx, ncol = ny)
  return(interp.grid)
}


