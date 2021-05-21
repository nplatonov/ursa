\name{panel_plot}
\alias{panel_plot}
\alias{panel_box}
\alias{panel_lines}
\alias{panel_points}
\alias{panel_text}
\alias{panel_abline}
\alias{panel_polygon}
\alias{panel_segments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add graphical elements to the image panel
}
\description{
Standard fuctions for plotting from package \pkg{graphics} are used for manual adding elements to current plot. Theses series of functions used that standard instruments with additional controling the acceptability of plotting.
}
\usage{
panel_plot(obj,...)

panel_box(...)
panel_lines(...)
panel_points(...)
panel_text(...)
panel_abline(...)
panel_polygon(...)
panel_segments(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{\R object.}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
   In \code{panel_plot} arguments are passed to function \code{\link[graphics:plot.default]{plot}}.\cr
   In \code{panel_box} arguments are passed to function \code{\link[graphics]{box}}.\cr
   In \code{panel_lines} arguments are passed to function \code{\link[graphics]{lines}}.\cr
   In \code{panel_points} arguments are passed to function \code{\link[graphics]{points}}.\cr
   In \code{panel_text} arguments are passed to function \code{\link[graphics]{text}}.\cr
   In \code{panel_abline} arguments are passed to function \code{\link[graphics]{abline}}.\cr
   In \code{panel_polygon} arguments are passed to function \code{\link[graphics]{polygon}}.\cr
   In \code{panel_segments} arguments are passed to function \code{\link[graphics]{segments}}.\cr
}
}
\details{
If unable to get value \code{TRUE} from \code{getOption("ursaPngPlot")} then plotting is disable, and any function from this series returns \code{NULL}.

Generally, for spatial objects argument \code{add=TRUE} is used in \code{panel_plot}.

%%~ If object of class \code{ursaRaster} is found among arguments of function \code{panel_contour}, then this object is converted to a suitable list \code{\link[ursa:classRaster_as.matrix]{as.matrix(obj,coords=TRUE)}}, and argument \code{add} for function \code{\link[graphics]{contour}} is forced to value \code{TRUE}. 
}
\value{
For spatial objects (simple features from \pkg{sf} or spatial abstract classes from \pkg{sp}) function \code{panel_plot} returns object of class \code{ursaLegend}. It is a list with items, which can be used to as arguments of \code{\link[graphics:legend]{legend()}}. This is intermediate step for experimental feature (not ready) to display colorbars on plot panel. For other objects function \code{panel_plot} returns value of function \code{\link[graphics:plot.default]{plot}}.

Function \code{panel_box} returns value of function \code{\link[graphics]{box}}.\cr
Function \code{panel_lines} returns value of function \code{\link[graphics]{lines}}.\cr
Function \code{panel_points} returns value of function \code{\link[graphics]{points}}.\cr
Function \code{panel_text} returns value of function \code{\link[graphics]{text}}.\cr
Function \code{panel_abline} returns value of function \code{\link[graphics]{abline}}.\cr
Function \code{panel_polygon} returns value of function \code{\link[graphics]{polygon}}.\cr
Function \code{panel_segments} returns value of function \code{\link[graphics]{segments}}.\cr
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
For plotted elements it is possible to create legend for colors using color bars. No shapes kind and size, no line widths.

To convert object \code{x} of class \code{ursaLegend} to object of class \code{ursaColorTable} please use \code{ursa_colortable(x)}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[ursa:panel_contour]{panel_contour}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
Package \pkg{graphics} (\code{help(package="graphics")}) and functions \code{\link[graphics:plot.default]{plot}}, \code{\link[graphics]{box}}, \code{\link[graphics]{lines}}, \code{\link[graphics]{points}}, \code{\link[graphics]{text}}, \code{\link[graphics]{abline}}, \code{\link[graphics]{polygon}}, \code{\link[graphics]{segments}}.
}
\examples{
session_grid(NULL)
require(rgdal)
a <- pixelsize()
g1 <- session_grid()
n <- 12L
k <- 5L
x <- with(g1,runif(n,min=minx,max=maxx))
y <- with(g1,runif(n,min=miny,max=maxy))
panel_plot(x,y) ## plots nothing, because 'compose_open(...,dev=F)' is not called yet
sl <- lapply(seq(k),function(id){
   x <- sort(with(g1,runif(n,min=minx,max=maxx)))
   y <- sort(with(g1,runif(n,min=miny,max=maxy)))
   sp::Lines(sp::Line(cbind(x,y)),ID=id)
})
sl <- sp::SpatialLines(sl,proj4string=sp::CRS(ursa_proj(g1)))#,id=length(sl))
lab <- t(sapply(sp::coordinates(sl),function(xy) xy[[1]][round(n/2),]))
lab <- as.data.frame(cbind(lab,z=seq(k)))
sl <- sp::SpatialLinesDataFrame(sl
              ,data=data.frame(ID=runif(k,min=5,max=9),desc=LETTERS[seq(k)]))
print(sl@data)
ct <- colorize(sl@data$ID)#,name=sldf@data$desc)
shpname <- tempfile(pattern = "___tmp",tmpdir=".",fileext=".shp")
layername <- gsub("\\\\.shp$","",basename(shpname))
try(writeOGR(sl,dirname(shpname),layername,driver="ESRI Shapefile"))
compose_open(layout=c(1,2),legend=list(list("bottom",2)))
panel_new()
panel_decor()
panel_lines(x,y,col="orange")
panel_points(x,y,cex=5,pch=21,col="transparent",bg="#00FF005F")
panel_points(0,0,pch=3)
panel_text(0,0,"North\nPole",pos=4,cex=1.5,family="Courier New",font=3)
panel_new()
panel_decor()
panel_plot(sl,lwd=4,col="grey20")
if (file.exists(shpname))
   panel_plot(shpname,lwd=3,col=ct$colortable[ct$index])
panel_points(lab$x,lab$y,pch=as.character(lab$z),cex=2)
compose_legend(ct$colortable)
compose_close()
file.remove(dir(path=dirname(shpname)
               ,pattern=paste0(layername,"\\\\.(cpg|dbf|prj|shp|shx)")
               ,full.names=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
