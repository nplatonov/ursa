\name{panel_scalebar}
\alias{panel_scalebar}
\alias{.panel_scalebar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add scale bar to the image panel
}
\description{
\code{panel_scalebar} puts a scale bar ('box' style) on the panel with raster image.
}
\usage{
panel_scalebar(...)

# non-public
.panel_scalebar(position = "bottomleft", w = NA, cex = 0.85,
                col = "#0000002F", bg = "transparent", fill = "#FFFFFF2F",
                language=NA, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes. Passed to non-public \code{.panel_scalebar}, excepting argument \code{scalebar}:
%%~       \describe{
%%~          \item{\code{scalebar}}{Logical or integer. Responsible for should scale bar be displayed or not. If logical and \code{TRUE} then scale bars are plotted on all panel of layout. If logical and \code{FALSE} then scale bars are not displayed. If argument is a vector of positive integers, then scale bars are plotted only in the specified panels, which sequence is defined in \code{\link[ursa]{compose_design}} function and returned from \code{getOption("ursaPngLayout")$layout}. Default is \code{TRUE}.
         \tabular{lll}{
            \strong{Pattern} (\code{panel_scalebar}) \tab \strong{Argument} (\code{.panel_scalebar}) \tab \strong{Description}
            \cr \code{(scalebar|ruler|decor)} \tab \code{   } \tab Logical or integer. Responsible for should scale bar be displayed or not. If logical and \code{TRUE} then scale bars are plotted on all panel of layout. If logical and \code{FALSE} then scale bars are not displayed. If argument is a vector of positive integers, then scale bars are plotted only in the specified panels, which sequence is defined in \code{\link[ursa]{compose_design}} function and returned from \code{getOption("ursaPngLayout")$layout}. Default is \code{TRUE}.
            \cr \code{(scalebar\\\\.)*pos(ition)*} \tab \code{position} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*w} \tab \code{w} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*cex} \tab \code{cex} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*col} \tab \code{col} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*fill} \tab \code{fill} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*bg} \tab \code{bg} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*language} \tab \code{language} \tab \emph{See below}.
            \cr \code{(scalebar\\\\.)*verb(ose)*} \tab \code{verbose} \tab \emph{See below}.
         }
      }
   \item{position}{Character keyword or numeric of length 2 in the interval [0,1]. Defines the location of scale bar. If character, then one of the "bottomleft", "bottomright", "topleft", "topright", "left", "right", "bottom", "top", or "center". If numeric then relative position on panel is defined using shift on horizontal and vertical axes from origin in the bottom-left corner. Default is \code{"bottomleft"}.}
%%~          \item{\code{x}}{Numeric betweein 0 and 1. Horizontal coordinate of scale bar locations. It has a sence in the case of empty \code{""} value of the \code{position} argument. Default is \code{0}.}
%%~          \item{\code{y}}{Numeric betweein 0 and 1. Vertical coordinate of scale bar locations. It has a sence in the case of empty \code{""} value of the \code{position} argument. Default is \code{0}.}
   \item{w}{Positive numeric. The length \emph{in km} of scalebar's right segment. If \code{w=NA} then length of segment is defined automatically. Default is \code{NA}.}
   \item{cex}{Positive numeric. The relative font size for scalebar's labels. Default is \code{0.85}.}
   \item{col}{Character. Primary fill color for scalebar box and scalebar labels. Default is \code{"#0000002F"}.}
   \item{fill}{Character. Secondary fill color for scalebar box. Default is \verb{"#FFFFFF2F"}.}
   \item{bg}{Character. Background color for the area of scalebar box and labels. Default is \code{"transparent"}.}
   \item{language}{Character. Language for longitude and latitude captions. If \code{"ru"} then captions are in Russian else in English. Default is \code{NA}.}
   \item{verbose}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
}
\details{
The scalebar has 2 left segments and 2 right segments. Left and right segments are separated by 0. The length of left segments is a half of length of right segments.

Argument \code{scalebar} (or, synonym, \code{ruler}) is introduced for unconditional calling of \code{panel_scalebar} inside of high-level functions.

Default \code{x=0} and \code{y=0} define the \verb{"bottomleft"} position of scale bar.

If argument \code{scale} in the function \code{\link[ursa]{compose_open}} is character, then the length of one segment is exactly 1 cm, and the total length of scalebar is 3 cm.

If not \code{language="ru"} but environmental variable \code{LANGUAGE=ru} then labels are in Russian (cyrillics).

The length distortions is taken into account for transverse Mercator (\code{"+proj=tmerc"}) projection regarding to location of scalebar.

Scalebar (single occurence) can be controlled in high-level plot functions (\emph{e.g.}, \code{\link[ursa]{display}}, \code{\link[ursa]{compose_plot}}, \code{\link[ursa]{display_stack}}, \code{\link[ursa]{display_brick}}, \code{\link[ursa]{display_rgb}}, \emph{etc}.). 

To plot scalebar, use argument \code{scalebar=TRUE} and prefix \code{(ruler|scalebar)} (\code{scalebar.*} or \code{ruler.*}) for scalebar's parameters, \emph{e.g.}, \code{scalebar.pos="bottomright"}, \code{scalebar.cex=0.9}.

Scalebar is not displayed for longlat projection (\code{"+proj=longlat"}), where units are degrees.
}
\value{
This function returns \code{NULL} value.
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
# example no.1 -- direct usage
   a <- colorize(pixelsize())
   compose_open(a)
   panel_new()
   panel_raster(a)
   \donttest{panel_graticule()}
   \donttest{panel_coastline()}
   panel_scalebar()
   compose_close()

# example no.2 -- indirect usage
   display_rgb(ursa_dummy(nband=3,min=0,max=255),coastline=FALSE
              ,scalebar=TRUE,scalebar.col="white",scalebar.fill="black")

# example no.3 -- for paper copy
   a <- colorize(pixelsize(),breakvalue=seq(400,650,by=50),pal="Greys",inv=FALSE)
   compose_open(scale="1:95000000",dpi=150,device="cairo",family="Times")
   compose_plot(a,graticule=TRUE,coastline=FALSE,scalebar=TRUE,scalebar.x=1,units=expression(km^2))
   compose_close(bpp=8)

# example no.4 -- length distortion in the Transverse Mercator projection
   a1 <- regrid(setbound=c(10,65,71,83),dim=c(100,100),crs=4326)
   a2 <- polygonize(ursa_bbox(a1))
   a3 <- spatial_transform(a2,3857)
   a4 <- regrid(setbound=spatial_bbox(a3),res=20000,crs=spatial_crs(a3))
   compose_open(legend=NULL)
   panel_new("white")
   panel_coastline(fill="#00000010",detail="l")
  # panel_graticule()
   for (p in c("bottom","center","top"))
      panel_scalebar(pos=p,w=200)
   compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
