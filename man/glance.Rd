\name{glance}
\alias{glance}
\alias{.glance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Command line utility for spatial view of raster or vector GIS file.
}
\description{
\code{glance} is a parser of command line arguments for non-public function \code{.glance}, which creates multi-panel plots for each attribute of vector file or for each band of raster file.
}
\usage{
glance(...)

# non-public
.glance(dsn, layer = ".*", grid = NULL, field = ".+", size = NA, expand = 1,
        border = 27, lat0 = NA, lon0 = NA, resetProj = FALSE, style = "auto",
        feature = c("auto", "field", "geometry"), alpha = NA,
        basemap.order = c("after", "before"), basemap.alpha = NA,
        engine = c("native", "sp", "sf"), geocode = "",place="",
        area = c("bounding", "point"), zoom = NA, gdal_rasterize = FALSE,
        verbose = FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{
Character or object of either \code{ursaRaster}, \code{Spatial}, or \code{sf} classes. If character, then data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database).
%%~ If \code{Spatial} then package \pkg{methods} should be loaded before.
}
  \item{layer}{
Character or integer. If integer, then layer index. If character, then pattern (\link[base:regex]{regular expressions}) to recognize layer by name. Only one layer selection is allowed. If selected more then one layer, the error message contains indices and names of layers. Usually, datasets (e. g., "ESRI Shapefile") have only one layer. Default is \code{".*"}; interpreted as all layers.
}
  \item{grid}{
Object of class \code{ursaGrid} or NULL. Reference CRS and boundary box for visualization. If \code{NULL}, then CRS and boundary box are zoomed to layer. Default is \code{NULL}.
}
  \item{field}{
Character. Pattern for field (attribute, column,...) selection by name using \link[base:regex]{regular expressions}. Multiple selection is allowed. Default is \code{".+"}; all fields.
}
  \item{size}{
Integer of length 1 or 2 or character of length 1. Size of plotting panel in pixels. If character, then parsed to integer of length 1 or 2. Length 2 is used only for web cartography. If length 1, then \code{size} defines width of panel, and height is defined automatically. If integer, then width of panel for plotting in pixels. Default is \code{NA}; for web cartogrpaphy value of maximal size of static maps, and code{640} for other cases.
}
  \item{expand}{
Numeric. Multiplier for plotting panel zoom in relation to extent of plotting geometry. Default is \code{1.0}.
}
  \item{border}{
Integer. Value in pixels of fixed margins around plotting geometry. Default is \code{27L}.
}
  \item{lat0}{
Numeric. Parallel os zero distortion. If \code{NA}, then parallel os zero distortion is determed from object geometry. Actual for \code{"+proj=stere"} projections. Default is \code{NA}.
}
  \item{lon0}{
Numeric. Central meridian, which have vertical direction on the plot. If \code{NA}, then central meridian is determed from object geometry. Default is \code{NA}.
}
  \item{resetProj}{
Logical. Value \code{TRUE} overwrites projection of vector file. Default is \code{FALSE}.
}
  \item{style}{
Character. Either projection class or source of web-catrogrpaphy for basemep. Specified by a sentence of words separated by spaces.
\itemize{
   \item Projection class \cr
Valid values are \code{"stere"}, \code{"laea"}, \code{"merc"}, \code{"longlat"}. Default is keyword \code{"auto"}; use object projection, if this projection differs from projection class \code{"+longlat"}, otherwise, projection (\code{"stere"} or \code{"merc"}) is determed internally.
   \item Web cartography.
      \itemize{
         \item Static map\cr
         Valid values are \code{"google"}, \code{"openstreetmap"}, \code{"sputnikmap"}. Static maps have priority over tile services. however additional word \code{"static"} can be specified in the sentence, e.g., \code{"openstreetmap static"} or \code{"static google"}. Additional parameters for request to web-script can be added in the sentence in the form \code{"argument1=value1 [argument2=value2]"}, e.g., \code{style="google static maptype=terrain language=ru-RU scale=2"}.
         \item Tile service\cr
         Supported tile services can be returned by calling of non-public function \code{ursa:::.tileService()} without arguments. Valid values are \code{"mapnik"}, \code{"cycle"}, \code{"transport"}, \code{"mapsurfer"}, \code{"sputnik"}, \code{"thunderforest"}, \code{"carto"}, \code{"kosmosnimki"}, etc. 
%%~          Adding word \code{"tile"} forces tile services using, e.g. \code{style="tile mapnik"}.
      }
      By default, if data has no data fields (e. g., geometry only), then basemap is drawn in color, else in grayscale. Adding word \code{"color"} (or \code{"colour"}) to the sentence forces to use colored basemap. Adding word \code{"gray"} (or \code{"grey"}, \code{"greyscale"}, \code{"grayscale"}) to the sentence forces to use colored basemap.
      
      The order of words in the sentence is any.
   }
   Keywords \code{"google"}, \code{"openstreetmap"} force to use "Google Static Map" or "OpenStreetMap static map images" for basemap; the resulted projection has class \code{"+proj=merc"}.
}
%%~ }
  \item{feature}{
Character. Appearance of visualization. If \code{"field"} then data of each field is plotted on separate panel (number of panels is equal to number of columns in attribute table). If \code{"geometry"} then each feature is plotted on separate panel (number of panels is equal to number of rows in attribute table). Default is \code{"auto"}; if intersects of features are found, then \code{"geometry"} is used, else \code{"field"}.
}
   \item{basemap.order}{
Character. The order of basemap layer rendering in the case of web-cartography basemap.If \code{"before"}, then basemap is plotted before object plot. If \code{"after"}, then basemap is plotted over object. 
}
   \item{basemap.alpha}{
Character. The saturation of basemap in the case of web-cartography basemap. Default is \code{NA}; \code{basemap.alpha=0.5} for \code{basemap.order="before"} and \code{basemap.alpha=0.35} for \code{basemap.order="after"}.
}
   \item{alpha}{
Character. The opacity of plotted object. Default is \code{NA}; 0.75 for \code{basemap.order="before"} in web-cartogrpaphy style, \code{1.00} - in all other cases.
}
   \item{engine}{
Character keyword. Forcing to vector files processing by functions from package \pkg{sp} (\code{engine="sp"}) or package \pkg{sf} (\code{engine="sf"}, if \pkg{sf} is installed). Default is \code{"native"}; if \code{dsn} is \code{Spatial} object or if \pkg{sf} is not installed, then \code{"sp"} is used.
}
   \item{geocode}{
Character. Keyword for geocode service. Valid values are \code{"google"}, \code{"nominatim"}. If \code{dsn} is character and file \code{dsn} not found, then trying to interpret \code{dsn} as a request to geocode service. The output is only basemap of web cartogrpaphy. Default is \code{""}; several services are considered in the case of failure. Is \code{style} is not specified, then "Google Static Map" is used for \code{geocode="google"}, and "OpenStreetMap static map images" for \code{geocode="nominatim"}.
}
   \item{place}{
Character. Type of geographical object (river, island) in the geocoding request. If \code{geocode} service is \code{"nominatim"}, then \code{place} is searched among attributes "class" and "type". Default is \code{""}; any object is acceptable.
}
   \item{area}{
Character. Keyword of spatial class of geocoded object. \code{"bounding"} is used for boundary box; \code{"point"} is used for point. Default value is extracted by \code{match.arg(area)}.
}
   \item{zoom}{
Positive integer or character. Zooming if web-cartography is applied for basemap. If integer, then value of zoom for tile services and staticmap. If character, then \code{"0"} means zoom by default (defined internally), \code{"+1"} means increment on 1 of default zoom, \code{"+2"} means zoom increment on 2, \code{"-1"} means zoom decrement on 1, \code{"-2"} means zoom decrement on 2, \emph{etc}. Default is \code{NA}; zoom is defined internally.
}
   \item{gdal_rasterize}{
Logical. If \code{TRUE} and GDAL utilities are in the system search path, then overlay for panels is formed via rastetization of vector file. GDAL utility "gdal_rasterize" is used. Note, that GDAL (system level) is optional for this package. Default is \code{FALSE}.
}
  \item{verbose}{
Logical. Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.
}
  \item{\dots}{
\code{glance}: Arguments, which are passed to \code{.glance} or to \code{\link[ursa:display]{display}}.
\cr\code{.glance}: Arguments, which are passed to static maps API, \code{\link[ursa:colorize]{colorize}}, \code{\link[ursa:display]{display}}, \emph{etc}.

%%~ Keywords for \code{proj="google"} or \code{proj="osm"} to form request and post-process:
%%~    \tabular{lll}{
%%~       \strong{Argument} \tab \strong{Default value} \tab \strong{Comment}
%%~       \cr \code{"maptype"} \tab \code{"terrain"} \tab Allowed values in \code{c("terrain", "satellite")}
%%~       \cr \code{"language"} \tab \code{"en-EN"} \tab
%%~       \cr \code{"color"} \tab \code{"bw"} \tab Allowed values in \code{c("bw", "color"), not in API request.}
%%~       \cr \code{"scale"} \tab \code{1} \tab valid values are in \code{c(1,2)}.
%%~       \cr \code{"zoom"} \tab defined from object geometry \tab
%%~    }
}
}
\details{
Command line usage implies set of arguments using pair: argument \emph{\verb{name}} and argument \emph{\verb{value}}. and values in the format "\emph{\verb{[name1=]value1 [name2]=value2}}". No spaces around \emph{\verb{=}} (equal symbol). Argument \emph{\verb{name}} can be omitted, symbol \emph{\verb{=}} is omitted too. If argument \emph{\verb{value}} has spaces, then argumetn value should be surrounded by double quotes (\code{fname="my test.shp"}). If argument \emph{\verb{value}} is matched to \R function, then such value should be surrounded by single quotes (\code{layer='density'}).

Command line usage example: \code{Rscript -e ursa::glance() 'final_more_than_032.sqlite' attr="select" resetProj=TRUE expand=1.5}

For OS Windows, bat-file can be created for raster and vector file association:
\code{Rscript -e ursa::glance() \%*}

Command line usage implies external software for PNG view \code{\link[ursa:session]{session_pngviewer}(TRUE)}.
}
\value{
\code{glance} returns integer: \code{0L} - successful, \code{10L} - call without arguments.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Package \pkg{sf} is 'Suggested' for package \pkg{ursa}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
f <- system.file("vectors","scot_BNG.shp",package="rgdal")
glance(f,style="merc",field="(NAME|COUNT)")
cmd <- paste("Rscript --vanilla -e ursa::glance()",paste0("\"",f,"\"")
            ,"proj=\"merc\"","field=\"(lon|lat)\"")
cat(" --------- Try in command line: -----------\n")
message(cmd)
cat(" ----------- end of quoting ---------------\n")
\donttest{ ## windows: figure will be opened using *.png file association 
system(cmd)
}

require(sp)
a <- data.frame(lat=c(70.734,71.657),lon=c(178.577,-177.38),place="Wrangel Island")
coordinates(a) <- ~lon+lat
proj4string(a) <- "+init=epsg:4326"

\donttest{ ## internet connection is required
glance(a,style="google color maptype=terrain")
}
\donttest{ ## internet connection is required
glance(a,style="openstreetmap color")
}
\donttest{ ## internet connection is required
glance(a,style="mapnik color tile")
}
\donttest{ ## internet connection is required
glance("Svalbard")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
