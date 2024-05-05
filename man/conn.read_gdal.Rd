\name{read_gdal}
\alias{read_gdal}
\alias{ursa_read}
\title{
Read GDAL supported raster files.
}
\description{
\code{read_gdal} creates \code{ursaRaster} object from GDAL supported raster files using functions from packages with low-level raster reading.
}
\usage{
read_gdal(fname, resetGrid = TRUE, band = NULL,
          engine = c("native", "sf"),
          verbose = FALSE, ...)

ursa_read(fname, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
Character. GDAL supported raster file name.
}
  \item{resetGrid}{
Logical. If \code{TRUE} then new sessional grid is based on opened raster image. Default is \code{TRUE}
}
  \item{band}{
Character (\link[base:regex]{regular expression}) or integer.
}
  \item{engine}{
Character. Functionality of which package is used for reading data. This is experimental list, which future depends on evolution of reviewed packages and their availability for partial reading of multiband rasters.
%%~ Character. Functionality of which package is used for reading data. If partial data (\code{band} is not \code{NULL}), then \code{engine} is \code{"rgdal"}. If value is \code{"sf"} and no partial data reading then \code{sf::gdal_read()} is used before importing. If value is \code{"rgdal"} or partial data reading, then interaction with \pkg{rgdal} is used. Currently, \code{"native"} is similar to \code{"rgdal"}, but in next versions behaviour can be changed to engine selection depends on which namespace has already loaded or has suggested package \pkg{sf} been installed.
}
  \item{verbose}{
Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.
}
  \item{\dots}{
Ignored.
}
}
\details{
\code{ursa_read} is simplified implementation of \code{gdal_read}.

The composite GDAL formats (e.g., \href{https://gdal.org/drivers/raster/netcdf.html}{NetCDF: Network Common Data Format}, \href{https://gdal.org/drivers/raster/hdf5.html}{HDF5: Hierarchical Data Format Release 5}) are likely unsupported.

\code{read_gdal} uses functions from other other packages. It's a wrapper.
%%~ \itemize{
%%~    \item\code{\link[rgdal:readGDAL]{GDALinfo}} - to get raster metadata.
%%~    \item\code{\link[rgdal:GDALRasterBand-class]{getRasterData}} - to get raster data.
%%~ }

Category names and color tables are supported.
}
\value{
Object of class \code{\link[ursa:classRaster]{ursaRaster}}.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%~ \code{\link[rgdal:readGDAL]{rgdal::GDALinfo()}} for supported GDAL raster formats

\code{\link[ursa:ursa_as]{as.ursa}} is an alternative call for GDAL raster files import.
}
\examples{
session_grid(NULL)

# rgdal::gdalDrivers()
if (requireNamespace("sf"))
   print(sf::st_drivers())
if (file.exists(Fin1 <- system.file("gdal/gdalicon.png",package="sf"))) {
   a1 <- read_gdal(Fin1)
   print(a1)
   display(a1)
}
Fin2 <- tempfile(fileext=".")
a <- ursa_dummy(1,resetGrid=TRUE)
b <- colorize(a[a>91],stretch="equal",name=format(Sys.Date()+seq(0,6),"\%A \%d"))
write_envi(b,Fin2)
b1 <- read_gdal(Fin2)
b2 <- read_envi(Fin2,resetGrid=TRUE)
envi_remove(Fin2)
print(c('same colortable?'=identical(ursa_colortable(b1),ursa_colortable(b2))))
print(ursa_colortable(b1))
print(as.table(b1))
print(c('same values?'=identical(ursa_value(b1),ursa_value(b2))))
print(c('same grid?'=identical(ursa_grid(b1),ursa_grid(b2))))
if (requireNamespace("sf")) {
   p1 <- sf::st_crs(ursa_crs(b1))
   p2 <- sf::st_crs(ursa_crs(b2))
   print(c('same proj4string for CRS?'=identical(p1$proj4string,p2$proj4string)))
   print(c('same WKT for CRS?'=identical(p1$Wkt,p2$Wkt)))
   ursa_crs(b1) <- ursa_crs(b2)
   print(c('after same CRS, same grid?'=identical(ursa_grid(b1),ursa_grid(b2))))
}
display(b1,detail="l")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
