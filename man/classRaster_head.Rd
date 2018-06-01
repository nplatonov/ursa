\name{head}
\alias{head.ursaRaster}
\alias{tail.ursaRaster}
\alias{series}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract first and last bands of raster image
}
\description{
Functions to extract first bands (\code{head}), last bands (\code{tail}) and first+last bands (\code{series}) of raster image.
}
\usage{
\method{head}{ursaRaster}(x, n = 3L, ...)

\method{tail}{ursaRaster}(x, n = 3L, ...)

series(x, n = 3L, s=170, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{n}{
Positive integer. Number of extracted bands.
}
  \item{s}{
Positive numeric. Maximal size of memory in MB for extracted raster image in the assumption that class of values is \code{\link[base]{numeric}}.
}
  \item{\dots}{
Not used.
}
}
\details{
Function \code{series} combines consequtive calling \code{head(x); tail(x)} with checking the size of extracted part of raster image. If size exceeds specified value of the argument \code{s}, then number of extracted bands \code{n} is decreased.
}
\value{
Object of class \code{ursaRaster}
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/8))
a <- ursa_dummy(nband=101)
print(head(a))
print(tail(a))
print(series(a,2))
print(series(a[1:5]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
