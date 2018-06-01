\name{session}
\alias{session_grid}
\alias{session_proj4}
\alias{session_crs}
\alias{session_cellsize}
\alias{session_bbox}
\alias{session_pngviewer}
\alias{session_tempdir}
\alias{session_use_experimental_functions}
\title{
Get and set sessional parameters for grid, CRS, external software for open PNG files.
}
\description{

\code{session_grid} without arguments returns current grid properties. \code{session_grid} with arguments specifies grid, which is used by functions of this package, e.g., for plotting, for opened and created raster images during current session.

\code{session_pngviwer} is used to permit external software to open PNG files.

\code{session_tempdir} spefies directory for temporal files in some cases.

\code{session_use_experimental_functions} allows to use undocumented (experimental) functions.

Group of functions \code{session_proj4}, \code{session_crs}, \code{session_cellsize}, \code{session_bbox} extracts certain properties of sessional grid.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
session_grid(obj)
session_proj4()
session_crs()
session_cellsize()
session_bbox()

session_pngviewer(allow = NA)
session_tempdir(dst = character())
session_use_experimental_functions()

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Either missing, or \code{NULL}, or file name, or object of class \code{ursaRaster}, or object of class \code{\link[ursa:classGrid]{ursaGrid}}.}
  \item{allow}{Logical. If \code{TRUE} then it is allowed to use external software for viewing PNG files. \code{NA} is interpreted as \code{TRUE} in the case of "Rscript" usage, and intepreted as \code{FALSE} in the case of interactive session or "R CMD BATCH" usage. Default is \code{NA}.}
  \item{dst}{Character. Directory name for temporal files. Empty character or non-character is interpreted as \code{\link[base:getwd]{getwd()}} in the case of "Rscript" usage, and intepreted as \code{\link[base:tempfile]{tempdir()}} in the case of interactive session or "R CMD BATCH" usage. Default is \code{character()} (empty character).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{session_grid} deals with option \dQuote{ursaSessionGrid}: \code{options(ursaSessionGrid=\dots)} or \code{getOption("ursaSessionGrid")}.

Usage \code{session_grid()} without arguments return value of \dQuote{ursaSessionGrid} option via calling \code{getOption("ursaSessionGrid")}. If \code{is.null(getOption("ursaSessionGrid"))} then \code{session_grid()} returns default CRS.

Usage \code{session_grid(NULL)} resets \dQuote{ursaSessionGrid} option via calling \code{options(ursaSessionGrid=NULL)}.

The sequential calling \preformatted{
session_grid(NULL)
session_grid()
} returns default CRS. For checking that the option has been resetted successfully, use \code{getOption("ursaSessionGrid")} after \code{session_grid(NULL)}

\code{session_proj4} and \code{session_crs} are synonyms.

}

\value{
Object of class \code{\link[ursa:classGrid]{ursaGrid}}. It is a list. Default values are grid parameters of NSIDC polar stereo gridded data of Northern hemispere with nominal gridded resolution 25 km (\url{http://nsidc.org/data/polar_stereo/ps_grids.html}).\preformatted{
List of 9
 $ columns: int 304
 $ rows   : int 448
 $ resx   : num 25000
 $ resy   : num 25000
 $ minx   : num -3850000
 $ maxx   : num 3750000
 $ miny   : num -5350000
 $ maxy   : num 5850000
 $ proj4  : chr "+proj=stere +lat_0=90 +lat_ts=70.0 +lon_0=-45.0 +k=1
                 +x_0=0.0 +y_0=0.0 +a=6378273.000 +b=6356889.449 +units=m +no_defs"
 - attr(*, "class")= chr "ursaGrid"
NULL
}
%~ Items: \item{columns}{Number of columns (samples). Default is \code{304L}}
%~ \item{rows}{Number of rows (lines). Default is \code{448L}}
%~ \item{resx}{Grid cell size by horizontal axis. Default is \code{25000}}
%~ \item{resy}{Grid cell size by vertical axis. Default is \code{25000}}
%~ \item{minx}{Left margin of boundary box. Default is \code{-3850000}}
%~ \item{maxx}{Right margin of boundary box. Default is \code{3750000}}
%~ \item{miny}{Bottom margin of boundary box. Default is \code{-5350000}}
%~ \item{maxy}{Top margin of boundary box. Default is \code{5850000}}
%~ \item{proj4}{PROJ.4 string. Default is \code{"+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"} (NSIDC Sea Ice Polar Stereographic North, "+init=epsg:3411")}

\code{session_proj4} and \code{session_crs} return item \code{proj4}.

\code{session_cellsize} returns squared root from multiplication of cell dimension: \code{sqrt(resx*resy)}.

\code{sessiong_pngviewer} returns value of \code{getOption("ursaAllowPngViewer")}.

\code{sessiong_bbox} returns named numeric of lenght 4: minimal x-coodrinate (\code{xmin}), minimal y-coordinate (\code{ymin}), maximal x-coordinate  (\code{xmax}), maximal y-coordinate  (\code{ymax}).

\code{session_use_experimental_functions} added some non-public functions to current namespaces and returns invisile list of function names.

}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{Nikita Platonov \email{platonov@sevin.ru}}
%%~ \note{
%%~ %~ There is a limitai
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Class \code{\link[ursa:classGrid]{ursaGrid}}. Use \code{\link[ursa]{regrid}} to partial grid changing.

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
getOption("ursaSessionGrid") ## NULL
(g1 <- session_grid()) ## default
g1$resx <- g1$resy <- 12500
g1$columns <- as.integer(with(g1,(maxx-minx)/resx))
g1$rows <- as.integer(with(g1,(maxy-miny)/resy))
session_grid(g1)
session_grid(NULL)
a <- ursa_new(value=3)
session_grid(a)

print(session_pngviewer())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{environment}
