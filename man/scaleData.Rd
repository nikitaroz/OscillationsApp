\name{scaleData}
\alias{scaleData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scaleData(x, y, z, nx = nrow(z), ny = ncol(z))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{nx}{
%%     ~~Describe \code{nx} here~~
}
  \item{ny}{
%%     ~~Describe \code{ny} here~~
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
function (x, y, z, nx = nrow(z), ny = ncol(z)) 
{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
