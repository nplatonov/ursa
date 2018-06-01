\name{as.array}
\alias{as.array.ursaRaster}
\alias{.as.array}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export raster object to multidimensional array
}
\description{
In the \code{ursaRaster} object the 3-dimensional image data are presented in 2-dimensional matrix. \code{as.array} transforms internal 2-dimensional data to the usual 3-dimansional data. \code{as.matrix} just extracts image data in internal 2-dimensional format.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{as.array}{ursaRaster}(x, ...)

## non-public
.as.array(x, drop = FALSE, flip = FALSE, permute = FALSE, dim = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Arguments, which are passed to \code{.as.array}.
}
  \item{x}{
\code{ursaRaster} object
}
  \item{drop}{
Logical. If \code{drop=TRUE} then single-band images are presented without third dimension.
}
  \item{permute}{
Logical. If \code{permute=FALSE} then returned array has dimension \emph{(samples, lines, bands)}. If \code{permute=TRUE} then returned array has dimension \emph{(lines, samples, bands)}.
}
  \item{flip}{
Logical. If \code{flip=TRUE} then vertical flip (reverse coordinates for dimension #2) is applied for output image. 
}
  \item{dim}{
Logical. If \code{dim=TRUE} then array's dimension is returned.
}
}
\details{
Use \code{permute=TRUE} to create an object of class \code{\link[grDevices:as.raster]{raster}}: \code{as.raster(as.array(...))}

The spatial reference system is lost.
}
\value{
If \code{dim=FALSE} then \code{as.array} returns object of class \code{\link[base]{array}}.\cr
If \code{dim=TRUE} then \code{as.array} returns dimension of array.
\cr
\code{as.matrix} returns object of class \code{\link[base]{matrix}}.
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
\code{\link[ursa:classRaster_as.raster]{as.raster}} is a function to direct export to the object of class \code{\link[grDevices:as.raster]{raster}}.
\cr
\code{\link[ursa:classRaster_as.matrix]{as.matrix}} with argument/value \code{coords=TRUE} and \code{\link[ursa:classRaster_as.data.frame]{as.data.frame}} for object of class \code{ursaRaster} keep spatial reference system.
}
\examples{
session_grid(NULL)
a <- pixelsize()
a <- (a-global_min(a))/(global_max(a)-global_min(a))
b <- c(entire=a,half=a/2,double=a*2)
str(m <- as.matrix(b))
str(d1 <- as.array(b))
str(d2 <- as.array(b[1],drop=FALSE))
str(d3 <- as.array(b[1],drop=TRUE))
contour(d3)
filled.contour(d3)
d4 <- as.array(b,perm=TRUE)/global_max(b)
d4[is.na(d4)] <- 0
str(d4 <- as.raster(d4))
plot(d4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
