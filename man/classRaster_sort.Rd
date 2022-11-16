\name{sort}
\alias{sort}
\alias{sort.ursaRaster}
\title{
Sort multiband raster by band names.
}
\description{
Changing order of bands based on sorting of band names.
}
\usage{
\method{sort}{ursaRaster}(x, decreasing = FALSE, ...)
}
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{decreasing}{
Logical. Should the sort be increasing or decreasing? Not available for partial sorting. Default is \code{FALSE}.
}
  \item{\dots}{
Other arguments, which are passed to S3 method for sorting characters.
}
}
\details{
Function \code{sort()} for \code{ursaRaster} assumes bands reordering based on character band names.
}
\value{
Object of class \code{ursaRaster}
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\examples{
a <- ursa_dummy(nband=7L)
a
sort(a)
sort(a,decreasing=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
\keyword{methods}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
