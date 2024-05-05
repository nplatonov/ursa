\name{ursaCRS}
\alias{class-ursaCRS}
\alias{print.ursaCRS}
\alias{str.ursaCRS}
\title{
Coordinate Reference System (CRS) for raster images.
}
\description{
Class \code{ursaCRS} is a part of class \code{ursaGrid}. It defines map projection.
}
\usage{
\method{print}{ursaCRS}(x, ...)
\method{str}{ursaCRS}(object, ...)
}
\arguments{
  \item{x}{
\code{ursaCRS} object in function \code{print}.
}
  \item{object}{
\code{ursaCRS} object in function \code{str}.
}
  \item{\dots}{
Further arguments passed to generic functions \code{\link[base]{print}}, and \code{\link[utils]{str}}.
}
}
\value{
Functions print information about CRS and return invisible \code{NULL} value. 
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
session_grid(NULL)
a <- ursa_dummy()
crs <- ursa_crs(a)
print(c('Is proj4string used?'=p4 <- isTRUE(getOption("ursaProj4Legacy"))))
print(crs)
str(crs)
op <- options(ursaProj4Legacy=!p4)
print(c('Is proj4string used?'=p4 <- isTRUE(getOption("ursaProj4Legacy"))))
session_grid(NULL)
a <- ursa_dummy()
crs <- ursa_crs(a)
print(crs)
str(crs)
options(op)
}
\keyword{classes}
\keyword{print}
