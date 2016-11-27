\name{loadData}
\alias{loadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadData(file = file.choose(), sep = ",", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file = file.choose(), sep = ",", ...) 
{
    data <- read.table(file, sep = ",", ...)
    spec.data <- new("SpecData")
    spec.data@time <- data[-1, 1]
    spec.data@wavenumber <- as.vector(t(data[1, -1]))
    spec.data@data <- as.matrix(data)[-1, -1]
    spec.data@is.uniform <- FALSE
    if (!is.unsorted(spec.data@wavenumber)) {
    }
    else if (!is.unsorted(rev(spec.data@wavenumber))) {
        spec.data@wavenumber <- rev(spec.data@wavenumber)
        spec.data@data <- t(apply(spec.data@data, 1, rev))
    }
    else {
        stop("Dataset wavelengths are not ordered")
    }
    bad.wavenumbers <- vector()
    sapply(seq(from = 1, to = length(spec.data@wavenumber), by = 1), 
        function(i) {
            if (all(spec.data@data[, i] == 0)) {
                bad.wavenumbers <<- c(bad.wavenumbers, -i)
            }
        })
    if (length(bad.wavenumbers) != 0) {
        spec.data@wavenumber <- spec.data@wavenumber[bad.wavenumbers]
        spec.data@data <- spec.data@data[, bad.wavenumbers]
    }
    colnames(spec.data@data) <- NULL
    return(spec.data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
