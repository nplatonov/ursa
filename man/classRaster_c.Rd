\name{c}
\alias{c.ursaRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine bands into raster brick.
}
\description{
This function is an instrument for appending bands or for reorganizing bands.
}
\usage{
\method{c}{ursaRaster}(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Objects of class \code{ursaRaster} or coerced to class \code{ursaRaster}. First argument should be the object of class \code{ursaRaster}. The objects in the sequence can be named.
}
}
\details{
You may use this function to assign new bandname for single-band raster: \code{objDst <- c('Relative density'=objSrc)}

Use also 'Extract' operator \code{\link[ursa:classRaster.Extract]{[ ]}} to reorganize band sequence.

The returned object can be interpreted as a \strong{\emph{brick}} in the notation of package \pkg{raster}. To produce \strong{\emph{stack}} just call \code{\link[base:list]{list}} or \code{\link[ursa:classStack]{ursa_stack}}.
}
\value{
\code{ursaRaster} object.
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
\code{\link[ursa:classStack]{ursa_brick}} converts list of \code{ursaRaster} objects (\strong{\emph{stack}}) to a singe multiband \code{ursaRaster} object (\strong{\emph{brick}}).
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/16))
a1 <- ursa_dummy(nband=2)
names(a1) <-  weekdays(Sys.Date()+seq(length(a1))-1)
a2 <- ursa_dummy(nband=2)
names(a2) <- names(a1)
print(a1)
print(a2)
a3 <- a1[1]
print(names(a3))
a4 <- c(today=a3)
print(names(a4))
print(b1 <- c(a1,a2))
print(b2 <- c(a1=a1))
print(b3 <- c(a1=a1,a2=a2))
print(b5 <- c(a1=a1,a2=a2[1]))
print(b4 <- c(a1,'(tomorrow)'=a1[2])) ## raster append
print(b6 <- c(a1,50))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
