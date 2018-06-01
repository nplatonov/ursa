\name{dim}
\alias{dim.ursaRaster}
\alias{dim<-.ursaRaster}
\title{
Dimension of multiband raster image
}
\description{
Retrieve the dimension of an object of class \code{ursaRaster}. The replacement function is dummy; it doesn't change raster dimension.
}
\usage{
\method{dim}{ursaRaster}(x)
\method{dim}{ursaRaster}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{value}{
Any. Ignored
}
}
\details{
Use extract operator \code{\link[ursa:classRaster.Extract]{[]}} and combine function \code{\link[ursa:classRaster_c]{c}} to change third (e.g., temporal) dimension of raster.\cr
%%~ Use \code{\link[ursa:resizeBilinear]{bilinear}}, \code{\link[ursa:resizeContract]{contract}}, \code{\link[ursa:resizeExpand]{expand}} functions for change spatial dimension.
Use \code{\link[ursa]{regrid}} function to change grid parameters and to resize/resample raster into new grid.
}
\value{
The 'Extract' function \code{dim} returns named integer vector of length three: 1) number of lines/rows, 2) number of samples/columns, 3) number of bands/channels/layers.\cr
The 'Replacement' function \code{dim<-} returns \code{ursaRaster} object without changes.
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
session_grid(regrid(mul=1/16))
a <- ursa_dummy(nband=3)
ursa_info(a)
print(dim(a))
dim(a) <- c(25,00,34)
print(dim(a))
b <- create_envi("tmp1",bandname=letters[1:5],compress=FALSE)
print(dim(b))
close(b)
envi_remove("tmp1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
