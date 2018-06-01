\name{ursa_proj}
\alias{ursa_proj}
\alias{ursa_proj4}
\alias{ursa_proj<-}
\alias{ursa_proj4<-}
\title{
Extract and assign projection of raster images.
%%  ~~function to do ... ~~
}
\description{
Functions manipulate with \code{$proj4} item of the \code{\link[ursa:classGrid]{ursaGrid}} object, which is embedded in the \code{\link[ursa:classRaster]{ursaRaster}} object (obj$grid$proj4). Projection is specified in PROJ.4 notation.
}
\usage{
ursa_proj(obj)
ursa_proj(obj, keepGrid = FALSE) <- value
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
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
\emph{Extract} function \code{ursa_proj} returns character value of \code{$grid$proj4} item of \code{ursaRaster} object.

\emph{Replace} function \code{ursa_proj<-} returns \code{ursaRaster} with modified \code{$grid$proj4} item.
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
print(ursa_proj(a))
p4s <- "+init=epsg:3576"
ursa_proj(a) <- p4s
print(ursa_proj(a))
fname <- tempfile()
write_envi(a,fname)
a2 <- read_envi(fname,resetGrid=TRUE)
print(ursa_proj(a2))
try(print(rgdal::CRSargs(sp::CRS(p4s))))
envi_remove(fname)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
