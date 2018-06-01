\name{bandname}
\alias{bandname}
\alias{bandname<-}
\alias{names.ursaRaster}
\alias{names<-.ursaRaster}
\title{
Band names for raster image.
}
\description{
\code{bandname} (\code{names}) returns names of bands for object of class \code{ursaRaster} or existing ENVI labelled *.hdr file.
\code{bandname<-} (\code{names<-}) sets names of bands for object of class \code{ursaRaster}.
}
\usage{
bandname(x)
bandname(x) <- value

\method{names}{ursaRaster}(x)

\method{names}{ursaRaster}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}. In the \code{bandname} function it is allowed to specify character \file{ENVI labelled *.hdr} file name.
}
  \item{value}{
Character of length the same length of number of bands of \code{x}
}
}
\details{
\code{names} is a synonym for \code{bandname}. \code{names<-} is a synonym for \code{bandname<-}
}
\value{
For \code{bandname} and \code{names}, character vector.

For \code{bandname<-} and \code{names<-}, updated object of class \code{ursaRaster}.
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
\code{\link[ursa:classRaster_length]{nband}}
}
\examples{
session_grid(NULL)
a1 <- pixelsize()
a2 <- c("Band 1"=a1,Band2=a1/2,sqrt=sqrt(a1),NA)
print(a2)
print(bandname(a2))
bandname(a2)[1:2] <- c("Original","Half")
print(a2)
print(bandname(a2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
