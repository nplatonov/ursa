\name{allocate}
\alias{allocate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rasterization of point data into grid cells
}
\description{
\code{allocate} takes \code{x} and \code{y} coordinates and values from data frame, which is describing point spatial data, and puts them into cells of raster. The certain function (either mean value, sum of values, number of points) is applied for >0 points inside of the exact cell borders.
}
\usage{
allocate(vec, coords = c("x", "y"), nodata = NA, attr = ".+", fun = c("mean", "sum", "n"),
         cellsize = NA, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
\code{data.frame}. At least \code{x} and \code{y} should be in \code{colnames(vec)}. Is is allowed to use \code{\link[sp:SpatialPoints]{"SpatialPointsDataFrame"}} from package \bold{sp}. The "on the fly" reprojection is not supported.
}
  \item{coords}{
Character of length 2. Colums names, which contain coordinates of data points. Raster bands are not produced for specified columns. For misreference of coordinate columns, the attempt to find more appropriate coordinate columns is taken.
}
  \item{fun}{
Character keyword of function, which is applied to value of points, which are dropped into the same cell. Valid values are \code{"mean"} (mean value), \code{"sum"} (sum of values), \code{"n"} (number of points)
}
  \item{nodata}{
Numeric of length 1. This value used to mark \code{NA} values in the writing to file.
}
  \item{attr}{
Pattern in the format of \link[base:regex]{regular expressions}, which is used to select required columns in data frame. By default (\code{".*"}) all columns are used.
}
  \item{cellsize}{
Numeric. Desired size of cell in the raster grid. Used only when source data are not in regular grid. Default is \code{NA}; cell size is determined automatically to exclude case of points overlapping.
}
  \item{verbose}{
Logical. Some output in console. Primarily for debug purposes.
}
}
\details{
Here \code{fun} differs from \R-styled \code{fun} in such functions as \code{\link[base:lapply]{*apply}}, \code{\link[stats:aggregate]{aggregate}}.

It was refused \dQuote{rasterize} for function name to distinguish with \code{\link[raster:rasterize]{rasterize}} in the package \pkg{raster}
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
g1 <- session_grid(regrid(session_grid(),mul=1/10))
n <- 1000
x <- with(g1,runif(n,min=minx,max=maxx))
y <- with(g1,runif(n,min=miny,max=maxy))
z <- with(g1,runif(n,min=0,max=10))
da <- data.frame(x=x,y=y,value=z)
res <- c(mean=allocate(da,fun="mean")
        ,mean_=NA
        ,sum=allocate(da,fun="sum")
        ,count=allocate(da,fun="n"))
res["mean_"]=res["sum"]/res["count"]
print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
