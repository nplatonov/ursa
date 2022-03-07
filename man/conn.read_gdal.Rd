\name{read_gdal}
\alias{read_gdal}
\alias{ursa_read}
\title{
Read GDAL supported raster files.
}
\description{
\code{read_gdal} creates \code{ursaRaster} object from GDAL supported raster files using functions from \pkg{rgdal} packages.
}
\usage{
read_gdal(fname, resetGrid = TRUE, band = NULL, engine = c("native", "rgdal", "sf"),
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
Character. Functionality of which package is used for reading data. If partial data (\code{band} is not \code{NULL}), then \code{engine} is \code{"rgdal"}. If value is \code{"sf"} and no partial data reading then \code{sf::gdal_read()} is used before importing. If value is \code{"rgdal"} or partial data reading, then interaction with \pkg{rgdal} is used. Currently, \code{"native"} is similar to \code{"rgdal"}, but in next versions behaviour can be changed to engine selection depends on which namespace has already loaded or has suggested package \pkg{sf} been installed.
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

\code{read_gdal} uses functions from \pkg{rgdal}:
\itemize{
   \item\code{\link[rgdal:readGDAL]{GDALinfo}} - to get raster metadata.
   \item\code{\link[rgdal:GDALRasterBand-class]{getRasterData}} - to get raster data.
}

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
\code{\link[rgdal:readGDAL]{rgdal::GDALinfo()}} for supported GDAL raster formats

\code{\link[ursa:ursa_as]{as.ursa}} is an alternative call for GDAL raster files import.
}
\examples{
session_grid(NULL)

rgdal::gdalDrivers()
Fin1 <- system.file("pictures/Rlogo.jpg",package="rgdal")
a1 <- read_gdal(Fin1)
print(a1)
display(a1)

Fin2 <- system.file("pictures/test_envi_class.envi",package="rgdal")
b1 <- read_gdal(Fin2)
b2 <- read_envi(Fin2,resetGrid=TRUE)
print(identical(ursa_grid(b1),ursa_grid(b2)))
print(identical(ursa_value(b1),ursa_value(b2)))
print(identical(ursa_colortable(b1),ursa_colortable(b2)))
print(ursa_colortable(b1))
print(as.table(b1))
display(b1,detail="l")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
