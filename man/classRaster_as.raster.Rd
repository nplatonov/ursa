\name{as.raster}
\alias{as.raster.ursaRaster}
\title{
Export raster object to a colored representation.
}
\description{
\code{as.raster} transforms object of class \code{ursaRaster} to the object of class \code{\link[grDevices:as.raster]{raster}} (package \pkg{grDevices})
}
\usage{
\method{as.raster}{ursaRaster}(x, ...)
%%~ .as.raster(x, max = 255)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object
}
  \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
  \describe{
     \item{\code{max}}{number giving the maximum of the color values range. Passed to function \code{\link[grDevices]{as.raster}} for S3 class 'array'. Default is 255.}
  }
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
A \code{\link[grDevices:as.raster]{raster}} object. It is a matrix. The values of matrix are colors.
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
\code{\link[ursa:classRaster_as.array]{as.array}}
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/2))
a <- ursa_dummy(4,min=0,max=255)
a[a<70] <- NA
compose_open(layout=c(1,4),legend=NULL)
for (i in seq(4)) {
   panel_new()
   panel_plot(as.raster(a[seq(i)]),interpolate=FALSE)
   panel_annotation(paste("Number of channels:",i))
}
compose_close()

op <- par(mfrow=c(2,2),mar=rep(0.5,4))
plot(as.raster(a[1:1]),interpolate=FALSE)
plot(as.raster(a[1:2]),interpolate=FALSE)
plot(as.raster(a[1:3]),interpolate=FALSE)
plot(as.raster(a[1:4]),interpolate=FALSE)
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
