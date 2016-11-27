\name{trimData}
\alias{trimData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trimData(object, remove.negative = TRUE, uniform = FALSE, start.time = NA, end.time = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{remove.negative}{
%%     ~~Describe \code{remove.negative} here~~
}
  \item{uniform}{
%%     ~~Describe \code{uniform} here~~
}
  \item{start.time}{
%%     ~~Describe \code{start.time} here~~
}
  \item{end.time}{
%%     ~~Describe \code{end.time} here~~
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
function (object, remove.negative = TRUE, uniform = FALSE, start.time = NA, 
    end.time = NA) 
{
    if (is.na(start.time)) {
        if (remove.negative) {
            start.time <- head(object@time[object@time >= 0], 
                n = 1)
        }
        else {
            start.time <- head(object@time, n = 1)
        }
    }
    if (is.na(end.time)) {
        end.time <- tail(object@time, n = 1)
    }
    start.idx <- which.min(abs(object@time - start.time))
    end.idx <- which.min(abs(object@time - end.time))
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
    }
    else {
        trimmed@is.uniform <- FALSE
    }
    return(trimmed)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
