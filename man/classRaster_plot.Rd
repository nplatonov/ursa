\name{plot}
\alias{plot.ursaRaster}
\alias{image.ursaRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple display of raster images
}
\description{
Function \code{image} for \code{ursaRaster} object calls generic function \code{\link[graphics]{image}}.
\cr
Function \code{plot} for \code{ursaRaster} object calls function \code{\link[graphics]{filled.contour}}.
\cr
\link[ursa:classColorTable]{Color tables} are supported.
}
\usage{
\method{plot}{ursaRaster}(x, ...)

\method{image}{ursaRaster}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{\dots}{
Other parameters. Are passed to or \code{\link[graphics]{filled.contour}} or to generic function \code{\link[graphics]{image}}.
}
}
\details{
Usage of both these functions is justified for low-level control of plotting. It is recommended to use high-level function \code{\link[ursa]{display}}. It is flexible and power instrument for raster images visualization.

Function \code{\link[ursa:classStack]{as.list}} for \code{ursaRaster} object transforms single band of raster image to a suitable object for plotting via function \code{\link[graphics]{image}} from package \pkg{graphics}
}
\value{
Returned value from \code{\link[graphics]{image}} or \code{\link[graphics]{filled.contour}} (both functions are in the package \pkg{graphics})
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

\seealso{
\code{\link[ursa]{display}}
}
\examples{
session_grid(NULL)
a <- pixelsize()
plot(a,asp=1)
image(a,asp=1)
b <- colorize(a,ncolor=15)
plot(b,asp=1)
image(b,asp=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
