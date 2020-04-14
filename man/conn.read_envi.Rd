\name{read_envi}
\alias{read_envi}
\title{
Read ENVI .hdr Labelled Raster file to memory
}
\description{
Reads all or several bands of ENVI .hdr Labelled Raster file from disk to memory.
}
\usage{
read_envi(fname, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
Character. Filename for ENVI .hdr Labelled Raster file.
}
   \item{\dots}{
%%~    For \code{readENVI}: arguments, which are passed to \code{read_envi} function.
   
   For \code{read_envi}: Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes. In the case of grids mismatch some arguments (e.g., \code{resample}) are passed to \code{\link[ursa:regrid]{regrid}} function.
      \describe{
         \item{\code{(subset)*}}{\emph{Name can be omitted.} Integer or character. If integer, then indices of bands, either positive or negative. Positive indices are for included bands, negative indices for omitted bands. If character, then either sequence of band names or regex string. By default (\code{subset=NULL}), function reads all bands.}
         \item{\code{(ref)*}}{\emph{Name can be omitted.} \code{ursaRaster} or \code{ursaGrid} object. Reference grid for raster image resizing. By default (\code{ref=NULL}) there is no resizing.}
         \item{\code{(nodata|ignorevalue)}}{Numeric. Value, which is ignored. By default (\code{nodata=NaN}) igrore value is taken from ENVI metadata (*.hdr or *.aux.xml).}
         \item{\code{reset(Grid)*}}{Logical. If \code{TRUE}, then \link[ursa:session]{session grid} is ignored, and new session grid is assigned from input file. If \code{FALSE}, then input file is nested in the session grid.}
         \item{\code{cache}}{Integer. Using cache for compressed files. \code{0L} - do not use cache, \code{1L} - use cache; any other value resets cache. Default is \code{FALSE}.}
         \item{\code{verb(ose)*}}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
      }
   }
}
\details{
Function \code{read_envi} is designed to one-time reading (from disk to memory) ENVI .hdr Labelled Raster file. For multiple access to disk (by chunks), use followed construction: \preformatted{
a <- open_envi(fname)
d1 <- a[condition_1]
d2 <- a[condition_2]
...
close(a)
}
In this case, the connection keeps open. The gain is more effective for compressed binary files.
}
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
\code{\link[ursa:conn.open_envi]{open_envi}}, \emph{\link[ursa:classRaster.Extract]{Extract}} method \code{\link[ursa:classRaster.Extract]{[}} for \code{ursaRaster} object, \code{\link[ursa:classRaster_close]{close_envi}}.

\code{\link[ursa:conn.read_gdal]{read_gdal}} uses GDAL (\pkg{rgdal}) to read ENVI .hdr Labelled Raster file.
}
\examples{
session_grid(NULL)
fname <- tempfile()
a <- ursa_dummy()
bandname(a) <- c("first","second","third")
write_envi(a,fname,compress=FALSE)

print(read_envi(fname))
print(read_envi(fname,c(1,3)))
print(read_envi(fname,-c(1,3)))
print(read_envi(fname,c("first","third")))
print(read_envi(fname,"iR"))

print(session_grid())
g <- regrid(session_grid(),mul=1/2.3)
b <- read_envi(fname,ref=g)
print(session_grid())
print(b)

envi_remove(fname)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
