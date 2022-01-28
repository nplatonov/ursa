\name{identify}
\alias{value_xy}
\alias{value_cr}
\alias{value_ll}
\alias{coord_xy}
\alias{coord_cr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get value and coordinates from location
}
\description{
Functions to extract values of raster image from given location, specified by coordinates in raster projection, by cell position or by geogpaphical coordinates. Additional utils to convert cell position and planar coordinates mutually.
}
\usage{
value_xy(obj, ...)

value_ll(obj, ...)

value_cr(obj, ...)

coord_xy(obj, ...)

coord_cr(obj, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}.
}
   \item{\dots}{the set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
      \tabular{llll}{
          \emph{Matched pattern}\code{   } \tab \emph{Function}\code{   } \tab \emph{Used name}
          
          \cr\code{ind} \tab \code{*_*} \tab \code{ind} \tab
          Index (positive \code{integer}) in internal value storage.
          \cr\code{^c} \tab \code{*_cr} \tab \code{col} \tab
          Integer of non-zero length. Index of column/sample Length of column and row indices should be the same for creating set of two-dimension coordinates.\cr
          \cr\code{^r} \tab \code{*_cr} \tab \code{row} \tab
          Integer of non-zero length. Index of row/line. Length of column and row indices should be the same for creating set of two-dimension coordinates.
          \cr\code{^x} \tab \code{*_xy} \tab \code{x} \tab
          Numeric of non-zero length. X-axis coordinate in grid of \code{obj}. The length of X-axis and Y-axis coordinates should be the same for creating set of two-dimension coordinates.
          \cr\code{^y} \tab \code{*_xy} \tab \code{y} \tab
          Numeric of non-zero length. Y-axis coordinate in grid of \code{obj}. The length of X-axis and Y-axis coordinates should be the same for creating set of two-dimension coordinates.
          \cr\code{^lon} \tab \code{value_ll} \tab \code{lon}  \tab
          Longitude. The length of longitudes and latitudes should be the same for creating set of two-dimension coordinates.
          \cr\code{^lat} \tab \code{value_ll} \tab \code{lat} \tab
          Latitude. The length of longitudes and latitudes should be the same for creating set of two-dimension coordinates.
      }
   }
}
\details{
\code{value_xy} returns values for location, which is specified by planar coordinates (x, y).\cr
\code{value_cr} returns values for location, which is specified by cell posisition (column, row) relative to upper-left corner of image .\cr
\code{value_ll} returns values for location, which is specified by longitude and latitude (long, lat).

\code{coord_xy} transforms planar coordinates (x, y) to cell position (column, row).\cr
\code{coord_cr} transforms cell position (column, row) to planar coordinates (x, y).

It is required to use a couple of coordinate vectors: \code{(x, y)}, \code{(c, r)} or \code{(lon, lat)} of the same length. The unary argument is interpreted as index in internal value storage.

Position in column/row coordinates starts from upper-lever corner. The cell of upper-level corner has (1, 1) coordinates (in \R indices starts from \code{1L}), whereas in some GIS the same corner cell has (0, 0) coordinates.

The column names of returned matrix are character format of index in internal value storage. This index can be specify in any function as argument \code{ind} instead of coordinates (planar, geographical, cell position).

}
\value{
For \code{value.*} numeric matrix of raster values. Band values for specific coordinates are by column. Set of specific coordinates are by row. \code{\link[base:colnames]{rownames}} are band names, and \code{\link[base:colnames]{colnames}} are index in internal value storage.

For \code{coord.*} numeric matrix of coordinates with a vector of couple coordinates, one coordinate per one row. \code{\link[base:colnames]{rownames}} are returned coordinates, and \code{\link[base:colnames]{colnames}} are index in internal value storage.
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
set.seed(352)
a <- as.integer(ursa_dummy(3,min=0,max=999))
ind <- which(ursa_value(a[1])==890)
print(ind)
msk <- a[1]==890
am <- a[msk]
b <- as.data.frame(am)
b$jx <- b$x+runif(nrow(b),min=-1000,max=1000)
b$jy <- b$y+runif(nrow(b),min=-1000,max=1000)
print(b)
cr1 <- coord_xy(a,x=b$jx,y=b$jy)
cr2 <- coord_xy(a,y=b$y,x=b$x)
cr3 <- coord_xy(a,ind=ind)
print(cr1)
print(list('cr1 and cr2'=all.equal(cr1,cr2)
          ,'cr2 and cr3'=all.equal(cr2,cr3)
          ,'cr3 and cr1'=all.equal(cr3,cr1)))
xy1 <- coord_cr(a,c=cr1["c",],r=cr1["r",])
print(xy1)
print(list('in x'=identical(unname(xy1["x",]),b[,"x",drop=TRUE])
          ,'in y'=identical(unname(xy1["y",]),b[,"y",drop=TRUE])))
val1 <- value_xy(a,x=b$jx,y=b$jy)
val2 <- value_xy(a,x=b$x,y=b$y)
val3 <- value_cr(a,ind=ind)
val4 <- value_cr(a,c=cr1["c",],r=cr1["r",])
print(val1)
print(list('val1 and val2'=all.equal(val1,val2)
       ,'val2 and val3'=all.equal(val2,val3)
       ,'val3 and val4'=all.equal(val3,val4)
       ,'val4 and val1'=all.equal(val4,val1)))
ps <- pixelsize()
v <- value_ll(ps,lon=180,lat=70)
print(c('True scale'=v/with(ursa_grid(ps),1e-6*resx*resy)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
