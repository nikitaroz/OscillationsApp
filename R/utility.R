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
trimData <- function(object, 
                     remove.negative = TRUE, 
                     uniform = FALSE, 
                     start.time = NA, 
                     end.time = NA) {
  
  # handles NA values for start.time
  if (is.na(start.time)) {
    # if remove.negative is set
    if (remove.negative) {
      start.time <- head(object@time[object@time >= 0], n = 1)
    } else {
      start.time <- head(object@time, n = 1)
    }
  }
  # NA value for end.time
  if (is.na(end.time)) {
    end.time <- tail(object@time, n = 1)
  }
  # times to indicies
  start.idx <- which.min(abs(object@time - start.time))
  end.idx <- which.min(abs(object@time - end.time))
  # if the data is supposed to be uniform
  if (uniform) {
    delta <- abs(object@time[start.idx + 1] - object@time[start.idx])
    
    for (i in (start.idx + 1):end.idx) {
      difference <- abs(object@time[i - 1] - object@time[i])
      if (abs(difference - delta) > 1e-05) {
        end.idx <- i - 1
        (break)()
      }
    }
  }
  
  trimmed <- new("SpecData")
  trimmed@time <- object@time[start.idx:end.idx]
  trimmed@wavenumber <- object@wavenumber
  trimmed@data <- object@data[start.idx:end.idx, ]
  if (uniform) {
    trimmed@is.uniform <- TRUE
  } else {
    trimmed@is.uniform <- FALSE
  }
  return(trimmed)
}


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


