\name{get_earthdata}
\alias{get_earthdata}
\title{
Retrive data from Global Imagery Browse Services (GIBS) using API for Developers
}
\description{
\code{get_earthdata} allows retrieving georeferences optical satellite images of low and moderate spatial resolution (up to 250m per cell) using GIBS API for Developers.
}
\usage{
get_earthdata(bbox = NA, res = c("2km", "1km", "500m", "250m"),
              date = NA, product = "", geocode = "",
              expand = 1.05, border = 0, display = FALSE,
              cache = NA, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbox}{
Numeric of length 4 or character. Spatial extent in the notation \code{c(minx,miny,maxx,maxy)}. Can be in utits of meters or degrees. If all absolute values are less than 360, then units are in degrees and projection is EPSG:3857, else units are in meters and projection is EPSG:3413. If \code{bbox=NULL}, then function return list of available products. If \code{bbox=NA} then boundary box is attempted for taking from session grid. If character, then boundary box is taken from geocoding. Default is region of Vaigach Island.
}
  \item{res}{
Character or numeric. Parameter, which is responsible for dimension of output raster image. If character, then zoom is selected using keyword list \code{c("2km", "1km", "500m", "250m")} for EPSG:3413. If \code{res<10} then it is interpreted as zoom for Web Map Tile Service (WMTS). If \code{res>100} then \code{res} is interpreted as preffered image dimension. If \code{res=NA} then \code{res=480L}.
}
  \item{date}{
Character or \code{"Date"} object. Date for image retrieving. Default is \code{Sys.Date()-1L}.
}
  \item{product}{
Character of integer. Data product form GIBS. Currently only MODIS-oriented (corrected reflectance) products are available:
\enumerate{
   \item "MODIS_Aqua_CorrectedReflectance_Bands721"
   \item "MODIS_Terra_CorrectedReflectance_Bands721"
   \item "MODIS_Aqua_CorrectedReflectance_TrueColor"
   \item "MODIS_Terra_CorrectedReflectance_TrueColor"
   \item "VIIRS_SNPP_CorrectedReflectance_TrueColor"
   \item "Coastlines"
   }
Please check actual list by calling \code{get_earthdata(bbox=NULL)}.

If numeric, then index of item among available products. \link[base:regex]{Regular expressions} can be used to simplify value of product, e.g., case-insensitive "aqua 721", "terra truecolor", "suomi", "SNNP".
}
   \item{geocode}{
Character. Keyword for geocode service. Valid values are \code{"google"}, \code{"nominatim"}. Default is \code{""}; several services are considered in the case of failure.
}
  \item{expand}{
Numeric. Multiplier for plotting panel zoom in relation to extent of plotting geometry. Ignored if geocoding is not applied. Default is \code{1.05}.
}
  \item{border}{
Integer. Value in pixels of fixed margins around plotting geometry. Ignored if geocoding is not applied. Default is \code{0L}.
}
  \item{display}{
Logical. Value \code{TRUE} forces to display image instead of return it. Default is \code{FALSE}.
}
  \item{cache}{
Logical. Is cache used? Default is \code{NA}, which is interpreted as \code{TRUE} for any requested date excepting not late time of today (approximately 17:00 UTC).
}
  \item{verbose}{
Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.
}
}
\details{
Argument \code{method="libcurl"} is used in function \code{\link[utils:download.file]{download.file}} for tile downloading. Please check \code{\link[base:capabilities]{capabilities}("libcurl")}.

Valid zoom values (e. g., specified via \code{res} argument) are 3:6 for EPSG:3413 and 0:8 for EPSG:3587.

Longitude 180 degrees has a seam in EPSG:3857 (e.g., see \code{bbox=c(170,68,-170,73)} and \code{bbox=c(-1600000,1308000,-1370000,1570000)} for Wrangel Island. If region crosses longitude 180 degrees in EPSG:3857, then the prior day is taken for Western Hemisphere.

}
\value{
If \code{bbox=NULL}, then character vector of available products.

If \code{display=FALSE} then object of class \code{ursaRaster} with RGBA image.

If \code{display=TRUE} then returned value of \code{\link[ursa:display_brick]{display_brick}}.
}

\references{
\href{https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+API+for+Developers}{GIBS API for Developers}
}
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
pr <- get_earthdata()
print(pr,quote=FALSE)

\donttest{ ## internet connection is required -- begin
a1 <- get_earthdata(bbox=c(2000000,400000,2300000,700000))
display(a1)
}
\donttest{ ## internet connection is required -- end
a2 <- get_earthdata(product=2
                  # ,date=Sys.Date()-7L ## empty tiles for winter polar regions
                   ,date=as.Date(format(Sys.Date()-365L,"\%Y-06-15"))
                   ,res=7,bbox=c(57.8,69.4,62.3,70.8))
display(a2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}% use one of  RShowDoc("KEYWORDS")
