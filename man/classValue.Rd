\name{ursaValue}
\alias{class-ursaValue}
\alias{class-ursaNumeric}
\alias{class-ursaCategory}
\alias{print.ursaCategory}
\alias{print.ursaNumeric}
\alias{ursa_value}
\alias{ursa_value<-}
\title{
Values of raster images.
}
\description{
Class \code{ursaValue} is a part of class \code{ursaRaster}. It contains values of image. In the case of numeric values, the exterior class is \code{ursaNumeric}. In the case of categorical values, the exterior class is \code{ursaCategory}.
}
\usage{
\method{print}{ursaCategory}(x, ...)

\method{print}{ursaNumeric}(x, ...)

ursa_value(obj, band)
ursa_value(obj, band) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of the one of classes \code{ursaNumeric} or \code{ursaCategory}
}
  \item{\dots}{
Further arguments passed to generic functions \code{\link[base]{print}} and \code{\link[utils]{str}}.
}
  \item{obj}{
Object of class \code{ursaRaster}
}
  \item{band}{
Optional. Vector of band numbers (positive integer) or band names (character).
}
  \item{value}{
Numeric or integer scalar, vector, or matrix. Coerced to dimenstion of \code{ursaValue}.
}
}
\details{
%%~ It is allowed to operate with \code{$value} item of \code{ursaRaster} object. However item \code{$value} should have class \code{ursaValue}.
%%~ \cr
%%~ The tips are 1) \code{obj$value[] <- val}, or 2) \code{obj$value <- val; class(obj$value) <- "ursaNumeric"}

Try to use high-level assignment using replacement \code{\link[ursa:classRaster.Replace]{[<-}} operator for class \code{ursaRaster}. However, if you don't get desired result, you can downgrade the level of your code.
}
\value{
Object of class \code{ursaNumeric} is a numerical matrix. Object of class \code{ursaCategory} is an integer matrix. Dimensions of this matrix:

\item{dim(\dots)[1]}{Spatial domain; the length is muliplications of lines (rows) and samples (columns)}
\item{dim(\dots)[2]}{Band or temporal domain; the length is number of bands}

It is allowed to use scalar value \code{NA} in the case when values are not in memory. In this case the class is \code{ursaValue}.
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
Extract \code{\link[ursa:classRaster.Extract]{[}} and replacement \code{\link[ursa:classRaster.Replace]{[<-}} methods for class \code{ursaRaster}
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/4))
a1 <- create_envi("exam1.envi",bandname=c("today","tomorrow"))
str(ursa_value(a1))
close(a1)
envi_remove("exam1")
a2 <- ursa_dummy(nband=4,min=1,max=99)
str(ursa_value(a2),digits=3)
a3 <- as.integer(a2)
str(ursa_value(a3))
str(ursa_value(a3,2))
print(ursa_value(a3))
print(a3)
ursa_value(a3,"Band 2") <- 199
ursa_value(a3)[,3] <- 299
a3[4] <- 399
print(a3)
ursa_value(a3[1:3]) <- ursa_value(a3[4])
print(a3)
ursa_value(a3[1:3]) <- -c(1:3)
print(a3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{print}
