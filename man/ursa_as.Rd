\name{as.ursa}
\alias{as.ursa}
\alias{as_ursa}
\title{
Create raster image from R objects or GDAL raster files.
}
\description{
\code{as.ursa} converts \R \pkg{base} objects \code{\link[base]{matrix}}, \code{\link[base]{array}}, \code{\link[base]{numeric}}, \code{\link[base]{data.frame}} \code{\link[base]{list}}, \pkg{sp} objects \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}}, \code{\link[sp:SpatialGridDataFrame]{SpatialPixelsDataFrame}} and \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}, \pkg{raster} objects \code{\link[raster]{raster}}, \code{\link[raster]{stack}} and \code{\link[raster]{brick}}, and GDAL raster files (using functions from \pkg{rgdal} package) to \code{ursaRaster} object.
}
\usage{
as.ursa(obj, ...)
as_ursa(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
\R object for coercion
}
  \item{\dots}{
Depending on class of \code{obj}, arguments are passed to repsective functions.
}
}
\details{
\code{as_ursa} is a synonym to \code{as.ursa}.

This is a high-level function to create \code{ursaRaster} objects. The follwed classes of \R objects are implemented:
 \tabular{lll}{
   \sQuote{Data Class} \tab \sQuote{Appropriate method}\cr
   \code{\link[base]{array}} \tab \code{\link[ursa]{ursa_new}}\cr
   \code{\link[base]{matrix}} \tab \code{\link[ursa]{ursa_new}}\cr
   \code{\link[base]{numeric}} \tab \code{\link[ursa]{ursa_new}}\cr
   \code{\link[base]{data.frame}} \tab \code{\link[ursa]{allocate}}\cr
   \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} (\pkg{sp})\tab \code{\link[ursa]{allocate}}\cr
   \code{\link[sp:SpatialGridDataFrame]{SpatialPixelsDataFrame}} (\pkg{sp})\tab \code{\link[ursa]{allocate}}\cr
   \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} (\pkg{sp})\tab \code{\link[ursa]{ursa_new}}\cr
   \code{\link[ursa:classStack]{list}} of \code{ursaRaster} objects \tab \code{\link[ursa:classStack]{unlist}}\cr
   \code{\link[sf:gdal]{list}} returned by \code{\link[sf:gdal]{sf::gdal_read}} \tab \code{\link[ursa]{ursa_new}}\cr
   \code{\link[base]{list}} \emph{(general)} \tab Items \code{$x} and \code{$y} are required, If lengths of \code{$x} and \code{$y} are equal to dim of data, then \code{\link[ursa]{allocate}}, else: 1) \link[ursa:classGrid]{raster grid} is defined from \code{$x} and \code{$y}, 2) \code{\link[ursa]{ursa_new}} is called.\cr
   \code{ggmap} (\pkg{ggmap}) \tab \code{\link[ursa]{ursa_new}}.\cr
   \code{\link[raster]{raster}} (\pkg{raster})\tab \code{\link[ursa]{ursa_new}}.\cr
   \code{\link[raster]{brick}} (\pkg{raster})\tab \code{\link[ursa]{ursa_new}}.\cr
   \code{\link[raster]{stack}} (\pkg{raster})\tab \code{\link[ursa]{ursa_new}}.\cr
   \code{\link[magick:editing]{bitmap}} (\pkg{magick})\tab \code{\link[ursa]{ursa_new}}.\cr
   \code{\link[base]{character}} (GDAL supported file name) \tab \code{\link[ursa:conn.read_gdal]{read_gdal}}.
}
Generally, \code{\link[ursa]{allocate}} is used for objects with non-regular grid, and \code{\link[ursa]{ursa_new}} is used for regular grids. The \link[ursa:classGrid]{raster grid} is defined from object properties or from \link[ursa:session]{sessional grid}.

Color tables are supported for GDAL file names and \pkg{raster} objects (raster, brick, stack).

For ENVI *.hdr Labelled Raster Files there are alternatives:
\enumerate{
   \item Read object with GDAL (\code{\link[ursa:conn.read_gdal]{read_gdal}});
   \item Read object without GDAL (\code{\link[ursa:conn.read_envi]{read_envi}}).
}
}
\value{
Object of class \code{ursaRaster}
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
a1 <- as.ursa(volcano)
print(a1)
display(a1)

session_grid(NULL)
b <- ursa_dummy(mul=1/16,bandname=format(Sys.Date()+seq(3)-1,"\%A"))
print(b)

c1 <- b[[1]] ## equal to 'c1 <- as.matrix(b[1],coords=TRUE)'
str(c1)
b1a <- as.ursa(c1)
print(c(original=b[1],imported=b1a))
print(c(projection.b1a=ursa_proj(b1a)))
session_grid(NULL)
b1b <- as.ursa(c1$z)
print(b1b)
print(c(projection.b1b=ursa_proj(b1b)))

c2 <- as.data.frame(b)
str(c2)
session_grid(NULL)
b2a <- as.ursa(c2)
print(b2a)

session_grid(NULL)
attr(c2,"proj4") <- NULL
b2b <- as.ursa(c2)
print(b2b)
print(ursa_grid(b2b))

c3 <- unclass(as.matrix(b,coords=TRUE))
str(c3)
session_grid(b)
b3a <- as.ursa(c3)
print(b3a)
print(ursa_grid(b3a))
session_grid(NULL)
b3b <- as.ursa(c3)
print(b3b)
print(ursa_grid(b3b))

c4 <- as.array(b)
str(c4)
session_grid(b)
b4a <- as.ursa(c4)
print(b4a)
print(ursa_grid(b4a))
session_grid(NULL)
b4b <- as.ursa(c4)
print(b4b)
print(ursa_grid(b4b))

n <- 20
c5 <- data.frame(y=runif(n,min=1000000,max=5000000)
                ,x=runif(n,min=-3000000,max=1000000)
                ,value=runif(n,min=0,max=10))
print(head(c5))
session_grid(b)
b5a <- as.ursa(c5)
print(b5a)
display(b5a)
session_grid(NULL)
b5b <- as.ursa(c5)
print(b5b)
display(b5b)

b6 <- as.ursa(system.file("pictures/erdas_spnad83.tif",package="rgdal"))
print(b6)
display(b6,coast=FALSE,col="orange")

\donttest{ ## package 'raster' is required -- begin
if (requireNamespace("raster")) {
   r <- raster::brick(system.file("external/rlogo.gri",package="raster"))
   print(r)
   b7 <- as.ursa(r)
   ursa_proj(b7) <- ""
   print(b7)
   display_rgb(b7)}
} ## package 'raster' is required -- end
} 
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
