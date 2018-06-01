\name{rep}
\alias{rep}
\alias{rep.ursaRaster}
\title{
Replicate bands of raster image.
}
\description{
\code{rep} for object of class \code{ursaRaster} creates new \code{ursaRaster} objects with repitition of orininal band sequence.
}
\usage{
\method{rep}{ursaRaster}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{\dots}{
Further arguments to be passed to or from other methods. Keywords:
\describe{
\item{\code{times}}{Positive integer. Number of times to repeat each band.}
}
If argument has no name, then \code{times} is assumpted.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
Object of class \code{ursaRaster}.
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
\code{\link[ursa:classRaster_c]{c}} for \code{ursaRaster}.
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/4))
a <- ursa_dummy(nband=3)
print(a)
b1 <- rep(a,by=2)
print(b1)
b2 <- rep(a,length=5)
print(b2)
b3 <- rep(a[3],3)
print(b3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
