#' @import shiny
#' @import plotly
NULL

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
app <- function(...) {
  runApp(appDir = system.file("app", package = "OscillationsApp"), ...)
}