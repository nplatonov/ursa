\name{ursa_crs}
\alias{ursa_crs}
\alias{ursa_proj}
\alias{ursa_proj4}
\alias{ursa_crs<-}
\alias{ursa_proj<-}
\alias{ursa_proj4<-}
\title{
Extract and assign projection of raster images.
%%  ~~function to do ... ~~
}
\description{
Functions manipulate with \code{$crs} item of the \code{\link[ursa:classGrid]{ursaGrid}} object, which is embedded in the \code{\link[ursa:classRaster]{ursaRaster}} object (\code{obj$grid$crs}). Projection is specified in PROJ.4 notation.
}
\usage{
ursa_crs(obj)
ursa_crs(obj, keepGrid = FALSE) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
\code{ursaRaster} object. It is allowed \code{ursaGrid} object for \code{ursa_proj} \emph{(Extract)} function.
}
  \item{keepGrid}{
Logical. Should sessional grid be changed after assignment. Default is \code{FALSE}.
}
  \item{value}{
Character sting in PROJ.4 format.
}
}
\details{
Boath \emph{Extract} and \emph{Replace} functions \code{ursa_proj()} and \code{ursa_proj4()} are synonyms for \code{ursa_crs}.
}
\value{
\emph{Extract} function \code{ursa_crs} returns character value of \code{$grid$crs} item of \code{ursaRaster} object.

\emph{Replace} function \code{ursa_crs<-} returns \code{ursaRaster} with modified \code{$grid$crs} item.
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

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=1)
print(ursa_crs(a))
p4s <- "+init=epsg:3576"
ursa_crs(a) <- p4s
print(ursa_crs(a))
fname <- tempfile()
write_envi(a,fname)
a2 <- read_envi(fname,resetGrid=TRUE)
print(ursa_crs(a2))
try(print(rgdal::CRSargs(sp::CRS(p4s))))
envi_remove(fname)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
