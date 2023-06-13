\name{spatial_read}
\alias{spatial_read}
\title{
Wrapper functions for reading spatial objects.
}
\description{
Read either simple features (package \pkg{sf}) and abstract of class Spatial (package \pkg{sp}) from disk using appropriate functionality (\emph{\dQuote{engine}}) of respective packages is used. 
}
\usage{
spatial_read(dsn, engine = c("native", "sp", "sf", "geojsonsf"))
}
\arguments{
  \item{dsn}{
Character. File name of spatial object (vector GIS).
}
  \item{engine}{
Character. Functionality of which package is used for reading data. If value is \code{"sf"}, then package \pkg{sf} is used and simple features are returned. If value is \code{"geojsonsf"}, GDAL driver is GeoJSON and package \pkg{geojsonsf} can be loaded, then package \pkg{geojsonsf} is used and simple features are returned. If value is \code{"sp"}, then package \pkg{rgdal} is used and Spatial abstracts (package \pkg{sp}) are returned. If value is \code{"native"} then engine selection depends on has suggested package \pkg{sf} been installed and is there possibility to use \pkg{geosonjf} for GeoJSON driver. If these checks are failed then package \pkg{rgdal} is used and Spatial abstracts (package \pkg{sp}) are returned.
}
}
\details{
Currently, list of arguments of this funtion is simplified and can be expanded.
}
\value{
Depending of used engine, either simple features (package \pkg{sf}) or Spatial abstracts (\pkg{sp}). 
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
   For GeoJSON files in the case \code{engine="geojsonsf"} reading is faster and the order of fields can be rearranged.
}

\seealso{
\code{\link[sf:st_read]{read_sf}} (valid if package \pkg{sf} is installed)

\code{\link[rgdal:readOGR]{readOGR}} (package \pkg{rgdal})

\code{\link[ursa:spatial_write]{spatial_write}}
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
if (requireNamespace("sp")) {
   obj2 <- da
   sp::coordinates(obj2) <- c("x","y")
   sp::proj4string(obj2) <- sp::CRS("+init=epsg:4326")
   print(series(obj2))
   print(spatial_crs(obj2))
   fname2 <- file.path(tempdir(),"res2.shp")
   print(fname2)
   spatial_write(obj2,fname2)
   res2 <- spatial_read(fname2,engine="sp")
   print(series(obj2))
}
print(spatial_dir(tempdir()))
}
\keyword{attribute}
