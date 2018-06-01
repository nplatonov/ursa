\name{na.omit}
\alias{na.omit.ursaRaster}
\title{
Drop bands which don't have data.
}
\description{
The bands with \link[ursa:ursa_blank]{band_blank} images, are omitted.
}
\usage{
\method{na.omit}{ursaRaster}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{ursaRaster}.
}
  \item{\dots}{
Ignored. For consistence with definition of generic function.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
Object of class \code{ursaRaster}, which has no bands without any data.
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
\code{\link[ursa:ursa_blank]{band_blank}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/4))
a <- ursa_new(value=1:3)
print(a)
a[2] <- NA
print(a)
a2 <- na.omit(a)
print(a2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
