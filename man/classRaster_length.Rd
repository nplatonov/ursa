\name{nband}
\alias{nband}
\alias{length.ursaRaster}
\title{
Get number of bands of raster image.
}
\description{
\code{nband} (\code{length}) returns number of bands (\emph{layers}, if appropriate in terminology) of \code{ursaRaster} object.
}
\usage{
nband(x)

\method{length}{ursaRaster}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
}
\details{
\code{length} for \code{ursaRaster} object is a synonym for \code{nband}.
}
\value{
Positive integer of length 1.
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
\code{\link[ursa:classRaster_names]{bandname}} (\code{\link[ursa:classRaster_names]{names}} for \code{ursaRaster} object).
}
\examples{
session_grid(NULL)
a1 <- pixelsize()
print(a1)
print(nband(a1))
a2 <- c("Band 1"=a1,Band2=a1/2,sqrt=sqrt(a1),NA)
print(a2)
print(nband(a2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
