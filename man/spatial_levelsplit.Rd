\name{spatial_levelsplit}
\alias{spatial_levelsplit}
\title{
   Drops spatial object with overlapped geometry to spatial object with non-overlapped geometry.
}
\description{
Contour (levels after kernel utilization distribution, isochrones and other isolines) polygonizations produces set of polygon layers, which geometry is overlapped. Plot of such polygons may cause the invisibility the less polygon behind the larger polygon. This function makes consequent geometries. Simplifuied, concentrated circles are dropped to non-overlapped rings.
}
\usage{
spatial_levelsplit(obj, sep = " - ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
  Spatial object, either simple features (package \pkg{sf}) or abstract class Spatial (package \pkg{sp})
}
  \item{sep}{
  Separator between concatenation of two values
}
}
\value{
Spatial object, which class is the same as class of \code{obj}.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
palette("Set3")
radius <- seq(1,length.out=5,by=1)*200
ct <- ursa_colortable(colorize(radius,alpha=0.5,pal=sample(palette(),length(radius))))
origin <- data.frame(lon=139.2,lat=36.6)
sp::coordinates(origin) <- ~lon+lat
sp::proj4string(origin) <- "EPSG:4326"
origin <- spatial_transform(origin,"EPSG:6671")
isopoly <- do.call(spatial_bind,lapply(radius*1e3,function(r) spatial_buffer(origin,r)))
spatial_data(isopoly) <- data.frame(radius=radius)
isointerval <- spatial_levelsplit(isopoly)
isointerval$radius
ct2 <- ursa_colortable(colorize(isointerval$radius,pal=unname(ct)))
session_grid(isopoly,border=20)
compose_open(2,legend=list("left","right"))
compose_panel(isopoly,col=ct
             ,annotation.text="Semi-transparent colors are overlapped")
compose_panel(isointerval,col=ct2
             ,annotation.text="Not overlapped rings")
compose_legend(list(ct,ct2))
compose_close()
}
\keyword{attribute}
