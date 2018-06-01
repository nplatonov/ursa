\name{as.Raster}
%%~ \alias{raster}
\alias{as.Raster}
\alias{as.Raster.ursaRaster}
\alias{as.Raster.list}
\alias{as.Raster.ursaStack}
\alias{as.Raster.NULL}
\title{
Coercion to package 'raster' objects
}
\description{
\code{as.Raster} converts singe-band \code{ursaRaster} object to \emph{raster}, multi-band \code{ursaRaster} object to \emph{brick} and list of \code{ursaRaster} objects to \emph{stack}. S4 classes \dQuote{raster}, \dQuote{brick}, and \dQuote{stack} are defined in package \pkg{raster}.
}
\usage{
as.Raster(obj)

\method{as.Raster}{ursaRaster}(obj)

\method{as.Raster}{list}(obj)

\method{as.Raster}{ursaStack}(obj)

\method{as.Raster}{NULL}(obj)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster} or list of \code{ursaRaster} objects
}
}
\details{
Package \pkg{raster} is required for conversions. 

The uppercase \verb{as.\bold{R}aster} is important, because \code{\link[ursa:classRaster_as.raster]{as.raster}} is used in internal functions for coersion to object of class \code{\link[grDevices:as.raster]{raster}}.

Single-banded \code{ursaRaster} object (with or without colortable) is coerced to \code{\link[raster:raster]{RasterLayer}}. Colortables are kept.
\cr
Multi-banded \code{ursaRaster} object is coerced to \code{\link[raster:raster]{RasterBrick}}. Colortables are destroyed.
\cr
Multi-layered object (list of \code{ursaRaster} objects) is coerced to \code{\link[raster:raster]{RasterStack}}. Colortables are destroyed.
\cr
}
\value{
Either \code{\link[raster:raster]{RasterLayer}}, \code{\link[raster:brick]{RasterBrick}}, or \code{\link[raster:stack]{RasterStack}} object.

If package \pkg{raster} is not installed then return value is \code{NULL}
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Package \pkg{raster} is marked as "Suggested".
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
if (requireNamespace("raster")) {
   require(methods) ## need to 'raster::plot'
   msk <- ursa_dummy(1,min=0,max=100)>40
   a1 <- ursa_dummy(1,min=200,max=500)[msk]
   a2 <- colorize(a1,ramp=FALSE)
   a3 <- as.integer(ursa_dummy(3,min=0,max=255.99))
   a4 <- ursa_stack(a3[msk])
   if (isLayer <- TRUE) {
      print(a1)
      r1 <- as.Raster(a1)
      message(as.character(class(r1)))
      print(r1)
      raster::plot(r1)
      b1 <- as.ursa(r1)
      print(c(exported=a1,imported=b1,failed=b1-a1))
      print(c(theSameValue=identical(ursa_value(a1),ursa_value(b1))
             ,rheSameGrid=identical(ursa_grid(a1),ursa_grid(b1))))
   }
   if (isLayerColortable <- TRUE) {
      r2 <- as.Raster(a2)
      message(as.character(class(r2)))
      print(r2)
      raster::plot(r2)
      b2 <- as.ursa(r2)
      print(c(theSameValue=identical(ursa_value(a2),ursa_value(b2))
             ,rheSameGrid=identical(ursa_grid(a2),ursa_grid(b2))))
   }
   if (isBrickOrRGB <- TRUE) {
      r3 <- as.Raster(a3)
      message(as.character(class(r3)))
      print(r3)
      raster::plot(r3)
      raster::plotRGB(r3)
      b3 <- as.ursa(r3)
      print(c(theSameValue=identical(ursa_value(a3),ursa_value(b3))
             ,rheSameGrid=identical(ursa_grid(a3),ursa_grid(b3))))
   }
   if (isStack <- TRUE) {
      r4 <- as.Raster(a4)
      message(as.character(class(r4)))
      print(r4)
      raster::plot(r4)
      b4 <- as.ursa(r4)
      print(c(theSameValue=identical(ursa_value(a4),ursa_value(b4))
             ,theSameGrid=identical(ursa_grid(a4),ursa_grid(b4))))
   }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
