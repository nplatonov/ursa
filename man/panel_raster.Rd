\name{panel_raster}
\alias{panel_raster}
\alias{.panel_raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add raster to the image panel
}
\description{
If specified image has 3 or 4 bands, then color composite is plotted on image panel, else the image is plotted regarding to its color table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
panel_raster(...)
%%~ ## non-public
%%~ .panel_raster(obj, useRaster = NA, interpolate = FALSE, alpha = NA , verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes.
   \cr 1. Passed to \code{\link[ursa:colorize]{colorize}}.
   \cr 2. Interpeted in this function:
%%~    Passed to non-public \code{.panel_raster}.
      \describe{
         \item{\code{"(^$|obj)"} \var{as} \code{obj}}{Object of class \code{ursaRaster}. Raster band for plotting. Multiple bands are allowed if then can be interpreted as RGB or RGBA.}
         \item{\code{"useRaster"}  \var{as} \code{useRaster}}{Logical. If \code{TRUE} then a bitmap raster is used to plot the image instead of polygons. See argument \code{useRaster} in function \code{\link[graphics]{image}}. Default depends on PNG device (\code{getOption("ursaPngDevice")}, which is set up in \code{\link[ursa]{compose_open}}); it is \code{TRUE} for \code{"cairo"} device, and \code{FALSE} for \code{"windows"} device.}
         \item{\code{"interp(olate)*"} \var{as} \code{interpolate}}{Logical. Passed as argument \code{interpolate} to function \code{\link[graphics:rasterImage]{rasterImage}}.}
         \item{\code{"(alpha|transp(aren(cy)*)*)"} \var{as} \code{alpha}}{Numeric or character. Level of transparency. If numeric, the either \verb{0 <= alpha <= 1} or \code{0 <= alpha <= 255}. If character, then one byte of hexadecimal value \verb{"00" <= alpha <= "FF"}. If \code{NA}, then transparency is used from colortable, else transparency of colortable is overwritten by \code{alpha}. Default is \code{NA}.}
         \item{\code{"verb(ose)*"} \var{as} \code{verbose}}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
      }
%%~       \tabular{lll}{
%%~          \emph{Pattern} (\code{panel_raster}) \tab \emph{Argument} (\code{.panel_raster}) \tab \emph{Description}
%%~          \cr \code{(^$|obj)} \tab \code{obj} \tab \emph{See below}.
%%~          \cr \code{useRaster} \tab \code{useRaster} \tab \emph{See below}.
%%~          \cr \code{interp(olate)*} \tab \code{interpolate} \tab \emph{See below}.
%%~          \cr \code{(alpha|transp(aren(cy)*)*)} \tab \code{alpha} \tab \emph{See below}.
%%~          \cr \code{verb(ose)*} \tab \code{verbose} \tab \emph{See below}.
%%~          \cr \code{\dots} \tab  \tab Passed to \code{\link[ursa:colorize]{colorize}}.
%%~       }
   }
%%~    \item{obj}{Object of class \code{ursaRaster}. Raster band for plotting. Multiple bands are allowed if then can be interpreted as RGB or RGBA.}
%%~    \item{useRaster}{Logical. If \code{TRUE} then a bitmap raster is used to plot the image instead of polygons. See argument \code{useRaster} in function \code{\link[graphics]{image}}. Default depends on PNG device (\code{getOption("ursaPngDevice")}, which is set up in \code{\link[ursa]{compose_open}}); it is \code{TRUE} for \code{"cairo"} device, and \code{FALSE} for \code{"windows"} device.}
%%~    \item{interpolate}{Logical. Passed as argument \code{interpolate} to function \code{\link[graphics:rasterImage]{rasterImage}}.}
%%~    \item{alpha}{Numeric or character. Level of transparency. If numeric, the either \verb{0 <= alpha <= 1} or \code{0 <= alpha <= 255}. If character, then one byte of hexadecimal value \verb{"00" <= alpha <= "FF"}. If \code{NA}, then transparency is used from colortable, else transparency of colortable is overwritten by \code{alpha}. Default is \code{NA}.}
%%~    \item{verbose}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
}
\details{
If \code{obj} is list of raster images, then \code{panel_raster} is applied to each item of list, and colortable of last item is returned.

If \code{obj} has 3 or 4 bands then \code{obj} is intepreted as RGB(A) image.

Function attempts to speed up plotting by reduce image matrix for big rasters.
}
\value{
If argument \code{obj} has strictly one band, then function returns \link[ursa:classColorTable]{color table} - object of class \code{ursaColorTable}, which can be used as an input argument for the colorbar legend (function \code{\link[ursa:legend_colorbar]{legend_colorbar}}). Otherwise function returns \code{NULL} value.
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
# example no.1 -- direct use
session_grid(regrid(mul=1/32))
dima <- with(session_grid(),c(columns,rows,3))
a <- ursa_new(value=array(runif(prod(dima),min=127,max=255),dim=dima))
p <- colorize(a,pal=c("black","white"),ramp=TRUE,value=0:256)
compose_open(layout=c(2,3),skip=4,legend=list(list("top","full"),list("bottom",2:3)))
for (i in seq(6)) {
   panel_new()
   if (i<4)
      panel_raster(p[i])
   else
      panel_raster(a,interpolate=i==5)
   panel_decor(col="black",coast=FALSE)
   panel_annotation(c("red","green","blue"
                    ,"interpolate=FALSE","interpolate=TRUE"))
}
legend_colorbar(p,label=seq(0,256,by=16),units="channels")
legend_mtext("color composite")
compose_close()

# example no.2 -- indirect use
display(pixelsize(NULL),raster.verb=TRUE)

# example no.3 -- color table for legend
session_grid(NULL)
compose_open()
panel_new()
ct <- panel_raster(pixelsize(),palname="Greens")
panel_decor()
compose_legend(ct)
compose_close()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
