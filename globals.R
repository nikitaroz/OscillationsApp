library(hyperSpec)
library(fields)
library(Rwave)
library(plyr)

setClass("Spectra", 
  contains = "hyperSpec"
)


Sys.setenv("plotly_username" = "taylormi")
Sys.setenv("plotly_api_key" = "0L3Lxpk8yErNctZ1p1QA")

#Load Data from comma delimited file
load <- function(file = file.choose(), sep = ",", ...) {
  data <- read.table(file, sep = ",", stringsAsFactors = FALSE, ...)
  
  time <- as.numeric(data[-1, 1])
  wavelength <- as.vector(t(data[1, -1]))
  data <- as.matrix(data[-1, -1])

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
  
  dimnames(data) <- list(x = time, y = wavelength)
  
  spec.data <- spec_object(x = time,
                y = rep(0, length(time)),
                spc =  data,
                wavelength = wavelength,
              xlabel = expression("Time (ps)"),
              ylabel = expression("Stimulated Raman gain"),
              wavelabel = expression("Wavelength (nm)"))
  
  return(spec.data)
}

#Trim Data to even spacing
trimData <- function(object, 
                     remove.negative = TRUE, 
                     uniform = TRUE, 
                     start.time = NA, 
                     end.time = NA) {
  
  # handles NA values for start.time
  if (is.na(start.time)) {
    # if remove.negative is set
    if (remove.negative) {
      start.time <- head(object@data$x[object@data$x >= 0], n = 1)
    } else {
      start.time <- head(object@data$x, n = 1)
    }
  }
  # NA value for end.time
  if (is.na(end.time)) {
    end.time <- tail(object@data$x, n = 1)
  }
  # times to indicies
  start.idx <- which.min(abs(object@data$x - start.time))
  end.idx <- which.min(abs(object@data$x - end.time))
  # if the data is supposed to be uniform
  if (uniform) {
    delta <- abs(object@data$x[start.idx + 1] - object@data$x[start.idx])
    
    for (i in (start.idx + 1):end.idx) {
      difference <- abs(object@data$x[i - 1] - object@data$x[i])
      if (abs(difference - delta) > 50e-15) {
        end.idx <- i - 1
        (break)()
      }
    }
  }
  
  trimmed <- object[start.idx:end.idx, ]
  return(trimmed)
}

#Scale Data to fit in spec object
scaleData <- function(object, nx = length(object@wavelength), 
                      ny = length(object@data$x)) {
  
  data <- list(x = object@wavelength, y = object@data$x, z = t(object@data$spc))
  x.interp <- seq(from = min(data$x), to = max(data$x), length.out = nx)
  y.interp <- seq(from = min(data$y), to = max(data$y), length.out = ny)
  interp.list <- list(x = x.interp, y = y.interp)
  interp.grid <- interp.surface.grid(data, interp.list)
  interp <- spec_object(y.interp ,rep(0, length(time)),t(interp.grid$z),x.interp, object@label, object@label, object@label)
  return(interp)
}

#Converts time to inverse units
time2invcm <- function(x) {
  # time in picoseconds
  c <- 299792458
  fs <- 1E12 / mean(diff(x))
  l = ceiling(length(x)/2) + 1
  f <- seq(from = 0, by = 1, length.out = l) * fs / (length(x))
  invcm <- f / (100 * c)
  return(invcm)
}

#Discrete FT
dft <- function(object) {
  f <- apply(object, 2, fft)
  
  trimmed <- f[1:(ceiling(length(f@data$x)/2) + 1), ]
  times <- object@data$x
  trimmed@data$x <- time2invcm(times)[1:(ceiling(length(f@data$x)/2) + 1)]
  trimmed@label$x <- expression("Frequency (s^-1)")
  return(trimmed)
}

#Wavelet Transform
wavelet <- function(input, noctave, nvoice = 1, w0 = 2 * pi, twoD = TRUE) {
  # time in picoseconds
  
  data <- apply(input[[]], 2, function(x){
    Mod(cwt(x, noctave, nvoice = nvoice, w0 = w0, plot = FALSE))^2
    }
  )
  
  scales <- (2^(1/nvoice))^(0:(noctave * nvoice - 1))
  
  dt <- mean(diff(input@data$x)) * 1E-12
  frequency <- w0 / (scales * dt * 4 * pi)
  c <- 299792458
  invcm <- frequency / (100 * c)
  spec_object(x = rep(input@data$x, times = length(invcm)),
              y = rep(invcm, each = length(input@data$x)),
              spc =  data,
              wavelength = input@wavelength,
              xlabel = (input@label$x),
              ylabel = expression("|FT|^2 Intensity"),
              wavelabel = input@label$.wavelength)            
}

#Spec function to be consistent between functions
spec_object <- function(x,y,spc,wavelength, xlabel, ylabel, wavelabel){
  spectra <- new("Spectra", 
                 data = data.frame(
                   x = x,
                   y = y
                 ),
                 spc = spc,
                 wavelength = wavelength,
                 labels = list(
                   x = xlabel,
                   y = ylabel,
                   .wavelength = wavelabel
                 )
  )
}

#Spec Object to Long dataframe with no NAs
longDF_rmNA <- function(obj) {
  df <- hyperSpec::as.long.df(obj, rownames = TRUE, na.rm = FALSE)
  df <- df[!is.na(df$spc), , drop = FALSE]
  return(df)
}

#Time units to index.
time2i = function (spectrum, x = stop ("You need a time point!")){
  chk.hy (spectrum)
  validObject (spectrum)
  
  ## special in formula
  max = max (spectrum$x)
  min = min (spectrum$x)
  
  envir = attr (spectrum, ".Environment")
  
  `~` = function (e1, e2){
    if (missing (e2))              # happens with formula ( ~ end)
      stop ("Time must be a both-sided formula")
    
    if ((Re (e1) < min (spectrum@x) && Re (e2) < min (spectrum@x)) ||
        (Re (e1) > max (spectrum@x) && Re (e2) > max (spectrum@x))){
      NULL                       # Time points completely outside the wl. range of x
    } else {
      e1 = .getindex (x, Re (e1)) + Im (e1)
      e2 = .getindex (x, Re (e2)) + Im (e2)
      
      if (e1 <= 0 || e2 <= 0|| e1 > length (spectrum@x) || e2 > length (spectrum@x))
        warning ("time2i: formula yields indices outside the object.")
      
      seq (e1, e2)
    }
  }
  
  .conv.range = function (range){
    if (is.numeric (range)){
      .getindex (spectrum, range, extrapolate = FALSE)
    } else
      eval (range)
  }
  
  if (is.list (x)) {
    unlist (lapply (x, .conv.range))
  } else {
    .conv.range (x)
  }
}

#Helper function for time2i
.getindex = function (spectrum, x, extrapolate = TRUE){
  if (! extrapolate) {
    x[x < min (spectrum$x)] = NA
    x[x > max (spectrum$x)] = NA
  }
  tmp = x [! is.na (x)]
  if (length (tmp) > 0) {
    tmp = sapply (tmp,
                  function (x, y) which.min (abs (x  - y)),
                  spectrum$x)
    x[! is.na (x)] = tmp
  }
  x
}

#Brush function to find indices for both waves and time
brush <- function(df, min, max, wave = TRUE){
  if(wave){
    l = wl2i(df, min):wl2i(df, max)
    return (df[l = l, wl.index = TRUE])
  } else{
    t = time2i(df, min):time2i(df, max)
    return (df[t])
  }
}

#Export Function
export <- function(file, type, name){
  if(type == "csv"){
    write.csv(file, file = name)
  }else if(type == "png"){
    plotly_IMAGE(file, out_file = name)
  }
}
