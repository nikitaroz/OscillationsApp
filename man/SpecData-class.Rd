\name{SpecData-class}
\Rdversion{1.1}
\docType{class}
\alias{SpecData-class}
\alias{dft,SpecData-method}
\alias{lomb,SpecData-method}
\alias{wavelet,SpecData-method}

\title{Class \code{"SpecData"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpecData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"array"} ~~ }
    \item{\code{time}:}{Object of class \code{"vector"} ~~ }
    \item{\code{wavenumber}:}{Object of class \code{"vector"} ~~ }
    \item{\code{is.uniform}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{dft}{\code{signature(object = "SpecData")}: ... }
    \item{lomb}{\code{signature(object = "SpecData")}: ... }
    \item{wavelet}{\code{signature(object = "SpecData")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("SpecData")
}
\keyword{classes}
