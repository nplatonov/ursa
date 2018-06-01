\name{panel_coastline}
\alias{panel_coastline}
\alias{compose_coastline}
\alias{update_coastline}
\alias{.panel_coastline}
\alias{.compose_coastline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add coastline to the image panel.
}
\description{
\code{panel_coastline} puts a coastline to the active panel of layout with optional land shadowing. The package provides data for coastline.
}
\usage{
compose_coastline(...)

panel_coastline(...)

update_coastline(merge = TRUE)

# not public
.compose_coastline(obj = NULL, panel = 0, col = NA, fill = "transparent",
                    detail = NA, density = NA, angle = NA, land = FALSE,
                    lwd = 0.5, lty = 1, fail180 = NA, verbose = FALSE)

# not public
.panel_coastline(obj, verbose = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
      \tabular{lll}{
          \emph{Matched pattern}\code{   } \tab \emph{Used name} \tab \emph{Description}
          \cr\code{coast(line)*} \tab \code{   } \tab
          Logical or integer. Responsible for should coastline be displayed or not. If integer, the indicates panel index for coastline displaying. If \code{TRUE} then coastline is plotted. If \code{FALSE} then coastline is not displayed. Default is \code{TRUE}.
          \cr\code{(obj)*} \tab \code{obj} \tab \emph{See below.}
          \cr\code{panel} \tab \code{panel} \tab \emph{See below.}
          \cr\code{fill} \tab \code{fill} \tab \emph{See below.}
          \cr\code{detail} \tab \code{detail} \tab \emph{See below.}
          \cr\code{density} \tab \code{density} \tab \emph{See below.}
          \cr\code{angle} \tab \code{angle} \tab \emph{See below.}
          \cr\code{land} \tab \code{land} \tab \emph{See below.}
          \cr\code{lwd} \tab \code{lwd} \tab \emph{See below.}
          \cr\code{lty} \tab \code{lty} \tab \emph{See below.}
          \cr\code{fail180} \tab \code{fail180} \tab \emph{See below.}
          \cr\code{verb(ose)*} \tab \code{verbose} \tab \emph{See below.}
      }
   }
   \item{obj}{Objects of the one of the classes \code{\link[sp:Spatial-class]{Spatial}}, \code{\link[sf:sfc]{sfc}}, \code{ursaCoastLine}. The last one is internal structure, which is returned by function \code{.compose_coastline}.}
   \item{panel}{Integer vector. Panel for which coastline will be displayed. \code{0L} means that coastline will bw displayed for all panels. Default is \code{0L}.}
   \item{col}{Character. Color code/name for coasline. Default is \code{"grey60"}.}
   \item{fill}{Character. Color code/name for land masking/shadowing. Default is \code{"transapent"}.}
   \item{detail}{Character keyword. The categorical spatial resolution for coastline. Valid values are \code{"l"} (low), \code{"m"} (medium), \code{"h"} (high), \code{"f"} (full). If value is \code{NA}, then coasline resolution is selected internally. Default is \code{NA}.}
   \item{density}{Numeric. The density of shading lines for land masking/shadowing. If \code{NA} then no shading lines are drawn. Default is \code{NA}. See \code{density} in \code{\link[graphics]{polygon}}.}
   \item{angle}{Numeric. The slope of shading lines, given as an angle in degrees (counter-clockwise). If \code{NA} then no shading lines are drawn. Default is \code{NA}. See \code{angle} in \code{\link[graphics]{polygon}}.}
   \item{land}{Logical. If \code{TRUE} then map's accent is to land, and ocean is masked/shadowed. If \code{FALSE} then map's accent is to ocean, and land is masked/shadowed. Default is \code{FALSE}.}
   \item{lwd}{Positive numeric. Width of coastline. Default is \code{1}. See \code{lwd} in \code{\link[graphics]{par}}.}
   \item{lty}{Positive integer. Type (pattern) of coasltline. Default is \code{1L} (solid). See \code{lty} in \code{\link[graphics]{par}}.}
   \item{fail180}{Logical. Patch for correct plotof poligons crossing 180 degree longitude. \code{NA} means than decision is taken intuitively. \code{TRUE} forces to implement crossing og 180 degree longitude. \code{FALSE} forces to not implement crossing og 180 degree longitude. Default is \code{NA}.}
   \item{verbose}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
   \item{merge}{Logical. Ingored.}
}
\details{

\code{compose_coastline} forms an obect of class \code{ursaCoastLine}. \code{panel_coastline} displays object of class \code{ursaCoastLine}. It is expected higher performance for multi-panel plotting.

If \code{obj} is \code{NULL}, then internal data is used. This data is based on simplified polygons of \href{http://openstreetmapdata.com/data/land-polygons}{OpenStreetMap-derived data}. Source data is \href{http://openstreetmapdata.com/info/license}{licensed} under the Open Data Commons Open Database License (ODbL). The crossing longitude 180 degrees polygons are merged. Removing of small polygons and simplifying of polygons geometry is applyed for three levels of detalizations (\code{"l"} - low, \code{"i"} - interim, \code{"h"} - high). For the full (\code{"f"}) level of detalization data simplification is not applied.

Coastline data are taken from directory, which is specidied by \code{getOption("ursaRequisite")} with default value \code{system.file("requisite",package="ursa")}. Package contains data of \code{"l"} (low) detalization level in the file \code{system.file("requisite/coast-l.rds",package="ursa")}. Data of higher levels can be added using \code{update_coastline()} function. It is required to specify user's requisite path using \code{options(ursaRequisite=path/to/user/files)} before loading \pkg{ursa}, e.g. in the user's \code{~/.Rprofile} file. Otherwise, there is a chance that data can not be updated due to 'permission deny' of the system directories. Package \pkg{sf} and some it's suggestions are required for data update.



If \code{detail=NA} then te spatial resolution is selected using CRS boundary and resolution using intuitive approach. If package's database cannot supply required detalization, then lower resolution is used.

Source coastline data in EPSG:4326 are transformed to CRS projection, extracted using \code{\link[ursa:session]{session_grid}} function. Coastlines with optional filling of either land or ocean area is interpreted as polygins. If filling is solid (there is no transparency or shading lines (numerical values of arguments \code{density} and \code{angle}), then coastline plotting is imlemented via \code{\link[graphics]{polypath}} function, otherwise  \code{\link[graphics]{polygon}} function.
}
\value{
\code{panel_coastline} returns \code{NULL}

\code{compose_coastline} returns of object of class \code{ursaCoastLine}. It is a list:
\item{coast_xy}{Two-column matix of coordinates in the sessional projection. The polygons are separated by \code{c(NA,NA)} rows.}
\item{panel}{Integer. Panel for coastline displaying. If \code{0L}, then coastline is displayed on each panel.}
\item{col}{See description of argument \code{col}.}
\item{fill}{See description of argument \code{fill}.}
\item{shadow}{If filling is semi-transparent, then it is "alpha" of filling color (argument \code{fill}).}
\item{land}{See description of argument \code{land}.}
\item{density}{See description of argument \code{density}.}
\item{angle}{See description of argument \code{angle}.}
\item{lwd}{See description of argument \code{lwd}.}
\item{lty}{See description of argument \code{lty}.}
}
%%~ \references{
%%~ Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical, High-resolution Shoreline Database, \emph{J. Geophys. Res.}, \emph{101}, 8741-8743, 1996.
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
In the versions <=3.7-19 package \pkg{ursa} contained land polygons based on union of "GSHHS_l_L1.shp" and "GSHHS_l_L5.shp" data from Self-consistent Hierarchical High-resolution Geography Database (\href{https://www.soest.hawaii.edu/pwessel/gshhg}{GSHHG}), Version 2.3.3 (01 November 2014), distributed under the Lesser GNU Public License, Version 3 (29 June 2007). 

}

\section{License}{
   Coastal data (land polygons) is distributed under ODbL.
} 

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
a <- pixelsize()
p1 <- colorize(a[a>500],ramp=FALSE,interval=TRUE)
p2 <- colorize(a,ramp=FALSE,pal=colorRampPalette(c("grey40","grey100"))
              ,verbose=!TRUE,interval=TRUE)
compose_open(layout=c(2,2),legend=list(list(1,"right"),list(2,"left")))
for (i in 1:4) {
   panel_new(col=ifelse(i==2,"white",NA))
   if (i \%in\% c(3,4))
      panel_raster(p1)
   else if (i \%in\% 2)
      panel_raster(p2)
   if (i==1)
      panel_coastline()
   panel_coastline(coast=4,col="#007F00",fill="lightgreen",land=TRUE)
   panel_coastline(coast=3,col="#0000003F",fill="#0000003F")
   panel_coastline(coast=2,col="black",fill="black",density=20
                   ,angle=c(-45,45),lwd=0.25,detail="l")
  # panel_graticule(decor=4)
   panel_annotation(text=as.character(i))
   if (i==1)
      panel_annotation(pos=c(1,1),text="default")
   else if (i==2)
      panel_annotation(pos=c(0,1),text="greyscale")
   else if (i==3)
      panel_annotation(pos=c(1,1),text="land is shadowed")
   else if (i==4)
      panel_annotation(pos=c(0,1),text="ocean is masked")
}
compose_legend(p2,p1)
compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
