\name{as_stars}
\alias{as_stars}
\title{
Raster coercion to \code{`stars`}
}
\description{
Coercion from raster \code{ursaRaster} object to raster \code{stars} object defined in package \pkg{stars}.
}
\usage{
as_stars(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}.
}
}
\details{
Simple coercion to \code{stars} object of package \pkg{stars}. Currently, color tables and attribution tables are not supported.
%%~ 
%%~ \code{as_stars} and \code{as.stars} are synonyms.
}
\value{
Object of class \code{stars} for argument of class \code{ursaRaster}. Otherwise, \code{NULL}.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}

\examples{
session_grid(NULL)
a <- ursa_dummy(3)
x <- as_stars(a)
class(x)
if (requireNamespace("stars")) {
   print(x)
   b <- as_ursa(x)
   print(a)
   print(b)
}
}
\keyword{spatial}
