\name{regrid}
\alias{regrid}
\alias{.regrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change raster image resolution and extent
}
\description{
General function to change parameters of cells under the same geographical projection. It is implemented via raster resampling to the new grid.
}
\usage{
regrid(x, ...)

## non-public
.regrid(grid = NULL, mul = NA, res = NA, resx = NA, resy = NA, setbound = NA,
        columns = NA, rows = NA, dim = NA, bbox = NA, expand = NA,
        minx = NA, miny = NA, maxx = NA, maxy = NA, cut = NA, proj4 = NA, crs = NA,
        border = 0, zero = c("keep", "node", "center"), raster = FALSE, tolerance = NA,
        verbose = FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Object of class \code{ursaRaster}.
}
   \item{\dots}{\enumerate{
      \item Arguments, which are passed to non-public \code{.regrid} to define parameters of new grid.
      \item Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
         \describe{
            \item{\code{^reset(Grid)*}}{Logical. Whether new grid will be defined as a sessional parameter? If \code{TRUE} then returned raster defines new sessional grid. If \code{FALSE} then session grid is not changed. Default is \code{TRUE}.}
            \item{\code{resample}}{Logical or positive numeric. The range of aggregation in the units of cell area. If \code{0} or \code{FALSE} then "nearest neighbor" value is used. The \code{resample>0} defines the side of rectangular area in proportion to cell size; and aggregation of adjacent cells is weighted in proportion to overlapping parts of cells. Default is \code{1} (or, equally, \code{TRUE}); it means that value of output cell is weighted mean of values of overlapped input cells in proportion of overlapping of output cell by input cells.}
            \item{\code{cover}}{Positive numeric in the range \code{[0, 1]}. The maximal fraction of \code{NA} values in adjusted input cells for the rule to write \code{NA} value to the output cell. Default is \code{0.499}.}
            \item{\code{cascade}}{Logical. Option to get more smooth results. If \code{TRUE} and \code{resample>2} then \code{resize} function is applied sequentially with argument \code{resample<=2}.}
            \item{\code{verb(ose)*}}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
         }
      }
   }
  \item{grid}{
Reference \code{\link[ursa:classGrid]{ursaGrid}} or \code{ursaRaster} object. If missing then reference grid is obtained from sessional grid \code{session_grid()}
}
  \item{mul}{
\code{numeric} of length 1. Multiplication for changing image size by means of changing of cell size (\code{1/mul}). \code{mul>1} decreases cell size, \code{mul<1} increases cell size
}
  \item{res}{
\code{numeric} of length 1 or 2. New grid size by horizontal and vertical axes. If length is 1 then the same grid size for both axes.
}
  \item{resx}{
Positive \code{numeric} of length 1. New grid size by horizontal axis.
}
  \item{resy}{
Positive \code{numeric} of length 1. New grid size by vertical axis.
}
  \item{setbound}{
\code{numeric} of length 4. Change units of spatial extension and define new spatial extension (boundary box) in the notation \code{c(minx,miny,maxx,maxy)}. 
}
  \item{columns}{
Positive \code{integer} of length 1. Number of columns/samples in the case of definition of new spatial extension (\code{setbound} is non-\code{NA}).
}
  \item{rows}{
Positive \code{integer} of length 1. Number of rows/lines in the case of definition of new spatial extension (\code{setbound} is non-\code{NA}).
}
  \item{dim}{
Positive \code{integer} of length 2. Dimenstion of raster image in the notation \code{c(rows, columns)} (or, \code{c(lines, samples)}) in the case of definition of new spatial extension (\code{setbound} is non-\code{NA}).
}
  \item{bbox}{
\code{numeric} of length 4. New spatial extension (boundary box) in the notation \code{c(minx,miny,maxx,maxy)} in the same units of existing spatial extension.
%~ \code{c(left=minx,bottom=miny,right=maxx,top=maxy)}
}
  \item{minx}{
\code{numeric} of length 1. New value for left boundary.
}
  \item{miny}{
\code{numeric} of length 1. New value for bottom boundary.
}
  \item{maxx}{
\code{numeric} of length 1. New value for right boundary.
}
  \item{maxy}{
\code{numeric} of length 1. New value for top boundary.
}
  \item{cut}{
\code{numeric} of length 4. Vector (left, bottom, right, top) in CRS units for extent expand.
}
  \item{border}{
\code{numeric} of length 1 or 4. If length 4, then vector (bottom, left, top, right) in cells for extent expand. If length <4, then value is repeated for length 4.
}
  \item{proj4}{
\code{character} of length 1. New projection string in the PROJ.4 notation
}
  \item{crs}{
\code{character} of length 1. The synonym to \code{proj4}.
}
  \item{expand}{
\code{numeric} of length 1. Multiplier of boundary box.
}
  \item{raster}{
\code{logical}. Should return blank \code{ursaRaster} object instead of \code{\link[ursa:classGrid]{ursaGrid}} object? See \sQuote{Value} section
}
  \item{zero}{
\code{character}. Define central cell position relative to zero coordinates. If value is "keep", then central cell position is without changes. If value is "node", then zero coordinates are on the crossing of cell borders. If value is "center", then zero coordinates are in the center of central cell. \emph{Currently is not implemented. If grid is consistent, then value "keep" is used, else "node"}.
}
  \item{tolerance}{
\code{numeric}. Threshold for comparison float point numerics. Required for internal check of grid consistence. Default is \code{NA}; value \code{.Machine$double.eps} multiplied on maximal value of coordinates is used.
}
  \item{verbose}{
Reporting via \code{message} about violation and restoration of coordinate grid regilarity after non-consistence usage of parameters.
%%     ~~Describe \code{verbose} here~~
}
}
\details{
Generally, argument \code{resample} sets a rectangular region. The area of this region is in proportion to area of output cell. Argument \code{resample} is the value of this proportion. Each cell is interpreted as a set of adjoining rectangular figures. The value of output cells is a weighted mean of that input cells, which fall into rectangular region. The weights are defined as an partial area inside of rectangular region.

Function implements "nearest neighbor" resampling method if argument \code{resample=0} (or, \code{resample=FALSE}). If \code{resample=1} (or, \code{resample=TRUE}) and both input and output rasters have the same cell size, then resampling method is "bilinear interpolation".

Expand raster \code{x} to 3 times with cell repeating: \code{regrid(x,mul=3,resample=FALSE) ## nearest neighbor};\cr
Expand raster \code{x} to 3 times with cell aggregation: \code{regrid(x,mul=3,resample=TRUE) ## bilinear interpolation};\cr
Contract raster \code{x} to 3 times without cell aggregation: \code{regrid(x,mul=1/3,resample=FALSE) ## nearest neighbor};\cr
Contract raster \code{x} to 3 times with cell aggregation: \code{regrid(x,mul=1/3,resample=TRUE) ## weighted mean};\cr
Low-pass filtering by \code{3 x 3} window size: \code{regrid(x,resample=3*3) ##} see \code{\link[ursa:Ops.focal_mean]{focal_mean}}

However, simple contraction \code{regrid(x,mul=1/2,resample=FALSE)} is implemented as contration with aggregation (\code{regrid(x,mul=1/2,resample=FALSE)}), because centers or output cells are located in the nodes (crossing of boundaries of input cells).

It seems that for categorical rasters parameter \code{resample=0} is more suitable, because nearest neigboring does not introduce new values to output raster, excepting coincidence of input cells' nodes and output cell centers.

Usage of \code{proj4} argument specifies only desirable PROJ.4 string and does not do reprojection.

The violation of grid regualarity is due to columns and rows of image should be integer. The restoration of grid regularity is realized by spatial extension (boundary box) expansion.

}
\value{
\code{regrid} returns object of class \code{ursaRaster}.

Return value of non-public function \code{.regrid} depends on logical value of \code{raster} argument. If \code{raster=FALSE} then \code{.regrid} returns new grid without any change of sessional grid. If \code{raster=TRUE} then \code{.regrid} returns blank image and changes sessional grid.


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
\code{\link[ursa]{regrid}}, \code{\link[ursa:Ops.focal_mean]{focal_mean}}
}
\examples{
session_grid(NULL)
print(g1 <- session_grid())
print(g2 <- regrid(g1,mul=2))
print(g3 <- regrid(g1,res=50000,lim=c(-1200000,-1400000,1600000,1800000)))
print(g4 <- regrid(g1,res=50000,lim=c(-1200100,-1400900,1600900,1800100),verbose=TRUE))
print(g5 <- regrid(g1,mul=1/4))
print(g6 <- regrid(g1,mul=1/4,cut=c(-1,-2,3,4)*25000))
print(g7 <- regrid(g1,mul=1/4,expand=1.05))
print(session_grid()) ## equal to 'g1'
print(a <- regrid(g1,mul=1/4,border=3,raster=TRUE))
print(session_grid()) ## not equal to 'g1'

session_grid(NULL)
'.makeRaster' <- function(nc=6,nr=8) {
   as.ursa(t(matrix(runif(nc*nr,min=0,max=255),ncol=nc,nrow=nr)))
}
session_grid(NULL)
a <- .makeRaster(12,18)
expand <- 1/3
a1 <- regrid(regrid(a,mul=expand,resample=FALSE),a,resample=FALSE)
a2 <- regrid(regrid(a,mul=expand,resample=TRUE),a,resample=FALSE)
b <- c('source'=a,'contract'=a1,'aggregation'=a2)
print(b)
display_brick(b,grid=TRUE
             ,grid.lon=(seq(ncol(a)*expand+1)-1)/expand
             ,grid.lat=(seq(nrow(a)*expand+1)-1)/expand)
session_grid(NULL)
a <- .makeRaster(6,8)
expand <- 3
b <- c("source"=regrid(a,mul=expand,resample=FALSE,resetGrid=FALSE)
      ,"simple"=regrid(a,mul=expand,cascade=TRUE,resetGrid=FALSE)
      ,"cascaded"=regrid(a,mul=expand,cascade=FALSE,resetGrid=FALSE))
print(b)
display_brick(b)
session_grid(a)
eps <- 1e-4
r <- c(0,expand^(-2)-eps,expand^(-2)+eps,1,expand^0.5
      ,(expand+2/3)^2-eps,(expand+2/3)^2+eps,99)
g2 <- regrid(mul=expand)
session_grid(g2)
b <- ursa_new(bandname=sprintf("Resample=\%.4f",r))
for (i in seq(b))
   b[i] <- regrid(a,g2,resample=r[i])
print(b)
display_brick(b,layout=c(2,NA)
             ,grid=TRUE,grid.lon=seq(ncol(a)+1)-1,grid.lat=seq(nrow(a)+1)-1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
