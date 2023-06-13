\name{segmentize}
\alias{segmentize}
\title{
Create segmented line from points' sequence
}
\description{
Connect sequence of points (locations) by direct lines (tracks)
}
\usage{
segmentize(obj, by=NULL, connect=c("united", "consequent"))
}
\arguments{
  \item{obj}{
   Simple feature (package \pkg{sf}) or Spatial abstract class (package \pkg{sp}) with POINTS spatial geometry.
}
  \item{by}{
   Either field name or \code{NULL}. If specified, then value \code{"united"} is applied for argument \code{connect}, returned spatial object is splitted regarding values of \code{by} with linked single-column attribute table with specified field. Default is \code{NULL}.
}
  \item{connect}{
   Structure of output segments; either sequence of single segments (\code{"consequent"}) or single multi-segment (\code{"united"}).
}
}
\details{
   Function generates \code{n-1} segments from \code{n} input points. Data (attribute table) is trasfered to output object with excluding of first row.
}
\value{
   Simple feature (package \pkg{sf}) or Spatial abstract class (package \pkg{sp}) with LINESTRING spatial geometry.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\examples{
session_grid(NULL)
n <- 15
lon <- rev(sort(runif(n,min=40,max=60)))
lat <- sort(runif(n,min=30,max=50))
pt <- data.frame(lon=lon,lat=lat,value=seq(n))
sp::coordinates(pt) <- c("lon","lat")
sp::proj4string(pt) <- "+proj=longlat"
ct <- ursa_colortable(colorize(pt$value))
tr <- segmentize(pt)
session_grid(pt,expand=1.1)
compose_open(2)
panel_new()
panel_plot(pt,col=ct)
panel_decor()
panel_new()
panel_plot(tr,col=ct,lwd=3)
panel_decor()
compose_legend(ct)
compose_close()
}
\keyword{attribute}
\keyword{spatial}
