\name{ursaStack}
\alias{class-ursaStack}
\alias{ursa_stack}
\alias{ursa_brick}
\alias{ursa_apply}
\alias{ursaStack}
\alias{as.list.ursaRaster}
\alias{unlist.ursaStack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List of raster images.
}
\description{
Functions to create list (layers) of multiband raster images (\emph{stack} in the notation of \pkg{raster} package) and to coerce list of images to single multiband image (\emph{brick} in the notation of \pkg{raster} package).
}
\usage{

ursa_stack(...)

ursa_brick(obj)

ursa_apply(obj, FUN, ...)

\method{as.list}{ursaRaster}(x, ...)

\method{unlist}{ursaStack}(x, recursive, use.names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj, x}{
Object of class \code{ursaRaster} or list of \code{ursaRaster} objects. In function \code{ursa_apply} argument "\code{X}", which is passed to function \code{\link[base:lapply]{lapply}}.
}
  \item{FUN}{
Argument "\code{FUN}", which is passed to function \code{\link[base:lapply]{lapply}}.
}
  \item{recursive}{
Not used. For consistency with generic function \code{\link[base:unlist]{unlist}}.
}
  \item{use.names}{
Not used. For consistency with generic function \code{\link[base:unlist]{unlist}}.
}
  \item{\dots}{Denending of functions:\cr
      \tabular{ll}{
   \code{ursa_stack}, \code{as.list} \tab\cr\tab List of \code{ursaRaster} objects\cr
   \code{ursa_apply} \tab\cr\tab Arguments "\code{\dots}", which are passed to function \code{\link[base:lapply]{lapply}}.
   }
}
}
\details{

\code{as.list} (of \code{ursaRaster} object(s)), \code{ursa_stack} create list of \code{ursaRaster} objects, where items of list are sinle-band images. If \code{x} is \code{ursaRaster} object, then \code{list(x)} create a list of length one, which item is multiband image.

\code{unlist} (for list of \code{ursaRaster} objects), \code{ursa_brick} create single multiband \code{ursaRaster} object. There is an alternative way for unlisting the list of \code{ursaRaster}: \code{\link[ursa:ursa_as]{as.ursa}}.

Raster \emph{stack} is a way to group bands, for example, by units (degree Celsium, meters).\cr
Raster \emph{brick} is a way to combine versalite images to the single multiband image, for example, for saving in file.
}
\value{
\code{ursa_stack}, \code{as.list} return object of class \code{ursaStack}. It is a list, with class "ursaStack" attribute.

\code{unlist} (for list of \code{ursaRaster} objects), \code{ursa_brick} return object of class \code{ursaRaster}.

\code{ursa_apply} returns object of class \code{ursaStack}, if result is list of \code{ursaRaster} objects, otherwise returns general \code{\link[base:list]{list}}.

}
\references{
\url{https://CRAN.R-project.org/package=raster}
}
\author{
Nikita Platonov
}
\note{
Generic \code{\link[base:unlist]{unlist}(x)} deals only with class of \code{x}, but doesn't take into account class of objects in list (e. g., \code{x[[1]]}). So, there is no effective way to use only \code{\link[base:list]{list}}/\code{\link[base:unlist]{unlist}} for \code{ursaRaster} objects to do a conversion between raster \emph{brick} and \emph{stack}.
Generic \code{\link[base:unlist]{unlist}(x)} deals only with class of \code{x}, but doesn't take into account class of objects in list (e. g., \code{x[[1]]}). So, there is no effective way to use only \code{\link[base:list]{list}}/\code{\link[base:unlist]{unlist}} for \code{ursaRaster} objects to do a conversion between raster \emph{brick} and \emph{stack}.
}

\section{Warning }{
There is no any verifications, that \link[ursa:classGrid]{grids} of \code{ursaRaster} objects are the same.
}

\seealso{
\code{\link[base:lapply]{lapply}} \code{\link[base:list]{list}} \code{\link[base:unlist]{unlist}}

\code{\link[ursa:classRaster_c]{c}} for \code{ursaRaster} objects.
}
\examples{
session_grid(NULL)
a <- ursa_dummy(3)
print(a)
b1 <- ursa_stack(a[1:2],colorize(a[3],ramp=FALSE))
print(b1)
b2 <- as.list(a)
print(b2)
b3 <- list(a[1],a[2:3])
print(b3)
b31 <- lapply(b3,colorize,ramp=FALSE)
print(b31)
b32 <- ursa_apply(b3,colorize,ramp=FALSE,rev=TRUE)
print(b32)
s311 <- ursa_apply(b31,ursa_colortable)
print(s311)
s21 <- lapply(b2,global_mean)
print(s21)
s22 <- sapply(b2,global_mean)
print(s22)
s31 <- lapply(b3,global_mean)
print(s31)
s32 <- sapply(b3,global_mean)
print(s32)
c1 <- unlist(b1)
print(c1)
c2 <- unlist(b2)
print(c2)
c3 <- unlist(b3)
print(if (is.ursa(c3)) c3 else "broken object")
d3 <- as.ursa(b3)
print(if (is.ursa(d3)) d3 else "broken object")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
