\name{spatial_write}
\alias{spatial_write}
\title{
Wrapper functions for writing spatial objects.
}
\description{
Write spatial object to disk. If spatial object is Simple Features, then appropriate functions from package \pkg{sf} are used.
If spatial objest are abstract of class Spatial (package \pkg{sp}) then preliminarly transformation to Simple Features is performed.

}
\usage{
spatial_write(obj, fname, layer, driver = NA, compress = "", verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Spatial object:
Either
Simple Features (\pkg{sf})
or Spatial Abstract (\pkg{sp}).
\link[base:list]{List} of spatial objects can be used.
}
  \item{fname}{
Character. File name with or without extension. If extension is missed, then argument \code{driver} must be specified.
}
  \item{layer}{
Character. Layer name. If missed, then basename of \code{fname} is used.
}
  \item{driver}{
Character. Driver for specification of output file format. Default is \code{NA}; value is determined from extension of \code{fname}.
}
  \item{compress}{
Character or logical. Will output file or list of files be packed after writing and what archive format will be used. Available character values are \code{""} (default; no compression), \code{"gz"}, \code{"gzip"}, \code{"bz2"}, \code{"bzip2"}, \code{"zip"}, \code{"xz"}. If logical and \code{TRUE}, then "zip" is used for \code{driver} "ESRI Shapefile" and "gzip" otherwise. If logical and \code{FALSE}, then no compression.
}
%%~   \item{ogr2ogr}{
%%~ DEPRECATED: ogr2ogr = nchar(Sys.which("ogr2ogr"))>0
%%~ Logical. If \code{"ogr2ogr"} is found in system path, then GDAL utils are used for speed up and layer merge (if \code{obj} is list of spatial objects)
%%~ }
  \item{verbose}{
Logical. Value \code{TRUE} provides information on console. Default is \code{FALSE}.
}
}
\details{
Based on \code{\link[sf:st_write]{sf::st_write}}
%~ and \code{\link[rgdal:writeOGR]{rgdal::writeOGR}}
function with additonal options: compressing of output file(s), coordinates trasforming (to longitudes and latitudes for \code{driver="GeoJSON"}), creating multi-layer destination (for \code{driver="SQLite"}).
}
\value{
invisible \code{NULL}.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}

\seealso{
\code{\link[sf:st_write]{write_sf}}
%%~ (valid if package \pkg{sf} is installed)

%%~ \code{\link[rgdal:writeOGR]{writeOGR}} (package \pkg{rgdal})

\code{\link[ursa:spatial_read]{spatial_read}}
}
\examples{
session_grid(NULL)
n <- 1e2
x <- runif(n,min=25,max=65)
y <- runif(n,min=55,max=65)
z <- runif(n,min=1,max=10)
da <- data.frame(x=x,y=y,z=z)
if (requireNamespace("sf",quietly=TRUE)) {
   obj1 <- sf::st_as_sf(da,coords=c("x","y"),crs=4326)
   print(series(obj1))
   fname1 <- file.path(tempdir(),"res1.shp")
   print(fname1)
   spatial_write(obj1,fname1)
   res1 <- spatial_read(fname1,engine="sf")
   print(series(res1))
}
print(spatial_dir(tempdir()))
}
\keyword{attribute}
