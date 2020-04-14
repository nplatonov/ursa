\name{panel_contour}
\alias{panel_contour}
\title{
Add colored contour to the image panel
}
\description{
An instrument to overlay multiple rasters on the same image panel. Contour is derived from one band of raster image. The colors (and respective colorbar in legend) is an alternative to contour labeling.

}
\usage{
panel_contour(obj, type = "", ...)

%%~ # non-public
%%~ .panel_contour(obj, bg = "black", lwd = 1, lwd.bg = lwd*1.5, detail = 1, short = 0L,
%%~                    verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{obj}{Object of class \code{ursaRaster} or \code{NULL}. Raster band for contouring. If \code{NULL} then contour is not displayed. Default is \code{NULL}.}
   \item{type}{Character. Keyword list to descibe characteristics of contour, which is interpreted using \link[base:regex]{regular expressions}.\cr\code{"label"} specifies displaying labels on contour lines. \cr\code{"line"} specifies displaying contour lines.\cr\code{"colo(u)*r"} specifies displaying colored contour lines. \cr\code{"fill"} specifies displaying filled contour lines.\cr Keywords can be combined in a single character, e.g., \code{"fill label"} specifies displaying filled contours with labels.}
   \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
      \describe{
         \item{\code{bg}}{Character. Color name or code for contour border. Used for contrast increasing. Semi-transparency and transparency (\code{"transparent"}) are allowed. Default is \code{"black"}.}
         \item{\code{lwd(\\\\.fg)*}}{Positive numeric. Width of contour line. Default is \code{1}.}
         \item{\code{lwd\\\\.bg}}{Poistive numeric. Width of the back of contour line. For  bordering shoould exceed foreground width (argument \code{lwd}). Default is \code{lwd*1.5}.}
         \item{\code{lty}}{Numeric of character. Line type for contour. Default is \code{1}.}
         \item{\code{(lab)*cex}}{Numeric. Character expansion factor for labels. Default is \code{0.85}.}
         \item{\code{method}}{Character. Argument, which is passed to \code{\link[graphics]{contour}}. Default is \code{"flattest"}.}
%%~          \item{\code{add}{Logical. Patch for displaying contour lines without starting new plot layout. Default is \code{TRUE}.}
         \item{\code{expand}}{Numeric. Scale factor (\code{>=1}) to artificial increasing contour details by means of smoothing of increased image size. Not applicable for images with \link[ursa:classColorTable]{color tables}. Default is \code{NA}; smoothing is determined intuitively.}
         \item{\code{before}}{Logical. Should image reclassification be done before smoothing? Default is \code{FALSE} for categorical images and \code{TRUE} for numerical images.}
         \item{\code{cover}}{Numeric. Argument, which is passed to \code{\link[ursa:regrid]{regrid}} to control \code{NA} values during smoothing. Default is \code{NA}; connected to default value of argument \code{cover} in function \code{\link[ursa:regrid]{regrid}}.}
         \item{\code{short}}{Positive integer. Minimal number of points in segments for displaying. Prevents displaying very short segments in the case of high-detailed image. Default is \code{0}: all segments are displayed.}
         \item{\code{verb(ose)*}}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
         Other arguments are used in the functuion \code{\link[ursa]{colorize}} to produce color tables.
      }
   }
}
\details{
Function \code{\link[grDevices]{contourLines}} is used for contouring.

The color table of input raster image is kept. The output panel have one element left, because contours are borders between areas of the same color. It is recommended to use only gradient palettes.

The color table is forced not to be ramp (argument \code{ramp=FALSE} in the function \code{\link[ursa]{colorize}}) to prevent extra density of contour lines.

The color table is forced to be interval (argument \code{interval=1L} in the function \code{\link[ursa]{colorize}}) to prevent lost of elements in the palette.
}
\value{
Object of class \code{ursaColorTable}, which then should be used as an input argument for the colorbar legend (function \code{\link[ursa:legend_colorbar]{legend_colorbar}}). If there is no argument of class \code{ursaRaster} then function returns \code{NULL} value.
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
\code{\link[grDevices]{contourLines}}

\code{\link[graphics]{contour}}
}
\examples{
session_grid(NULL)
a <- pixelsize()
refval <- seq(450,650,by=25)
val <- refval[seq(refval) \%\% 2 == 1]
ref <- colorize(a,breakvalue=refval,pal.rich=45,pal.rotate=0)
p1 <- colorize(a,breakvalue=val,pal.rich=135,pal.rotate=0)
p2 <- colorize(a,breakvalue=val,pal.rich=-15,pal.rotate=0)
p3 <- colorize(a,value=refval)
if (exam1 <- FALSE) { ## to avoid 'Examples with CPU or elapsed time > 10s' 
   compose_open(legend=list(list(1,"left"),list(1,"right")),scale=2)
   panel_new()
  # ct1 <- panel_raster(ref)
  # ct2 <- panel_contour(p2,"colored line",palname="Greens",lwd=15,lwd.bg=0)
   ct2 <- panel_contour(p2,"colored line",pal.rich=240,pal.rotate=0,lwd.fg=15,lwd.bg=0)
  # panel_contour(ref,lwd=0)
  # mysource("contour.R")
  # mycontour(.panel_contour(a),lwd=0)
   if (exists("ct1"))
      compose_legend(ct1,units="raster")
   if (exists("ct2"))
      compose_legend(ct2,units="contour")
   compose_close(bpp=8)
}
if (exam2 <- TRUE) {
   compose_open(layout=c(2,2),byrow=FALSE
               ,legend=list(list(1,"left"),list("bottom",1)
                           ,list(1,"right"),list("top",2)
                           ,list(2,"right"),list("bottom",2)))
   panel_new()
   panel_raster(ref)
   panel_contour(a)
   panel_new()
   ct0 <- panel_contour(a,"color",value=val,pal.rich=240,pal.rotate=0,lwd=11,lwd.bg=12)
   panel_contour(a)
   panel_annotation(text="no colortable")
   panel_new()
   panel_raster(p1)
   ct1 <- panel_contour(p1,"color",lwd=11,lwd.bg=2)
   panel_contour(a)
   panel_annotation(text="colortable:category")
   panel_new()
   panel_raster(p2)
   ct2 <- panel_contour(p2,"color",lwd=11,lwd.bg=2)
   panel_contour(a)#,cex=0.5)
   panel_annotation(text="colortable:interval")
   compose_legend(ref,units="reference")
   compose_legend(ct0,units="contour")
   compose_legend(p1,units="raster")
   compose_legend(ct1,units="contour")
   compose_legend(p2,units="raster")
   compose_legend(ct2,units="contour")
   compose_close()
}
if (exam3 <- TRUE) {
   s <- 29
   session_grid(NULL)
   a <- as.ursa(volcano)
   if (FALSE) {
      display(a)
      a2 <- regrid(a,mul=s,cascade=TRUE,verbose=TRUE)
      display(a2)
      session_grid(a)
   }
   compose_open() ## device="windows")
   panel_new()
   ct1 <- panel_raster(a,ramp=FALSE,interval=TRUE)
   ct2 <- panel_contour(a,"label")
   rm(ct2)
   panel_decor()
   if (exists("ct2"))
      legend_colorbar(ct2)
   else if (exists("ct1"))
      legend_colorbar(ct1)
   compose_close()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
