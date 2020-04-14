\name{focal_median}
\alias{focal_median}
\title{
Median spatial filter for image
}
\description{
For each band and for each cell, function finds median value inside of square window. \emph{Focal} operation of map algebra.
}
\usage{
focal_median(x, size = 3, cover = 1e-06, fillNA = FALSE, saveMargin = TRUE, verbose = 0L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}.
}
  \item{size}{
Positive numeric. Odd values (3, 5, 7, \dots) are allowed, but if other value is specified, then it expanded to the next odd value not less than original value. Default is \code{3L}.
}
  \item{cover}{
Numeric. \code{0<=cover<=1}. Quota for \code{NA} values in the focal window in relation to the squared size of the focal window. Quota exceeding leads to recording \code{NA} value in the cell. Default is \code{cover=1e-6}.
}
  \item{fillNA}{
Logical. If \code{TRUE} then only \code{NA} values of source image can be changed, and non-\code{NA} values of source image are kept without changes. It may provide less reducing of spatial resolution in the task of spatial interpolation. Default is \code{FALSE}.
}
  \item{saveMargin}{
Logical. If \code{TRUE} then adaptive window size is used for cells, where original window goes over image boundary. If \code{FALSE} then image is expanded to the half size of focal window by \code{NA} values and argument \code{cover} is applied to this expanded part. Default is \code{TRUE}.
}
  \item{verbose}{
Integer of \code{0L}, \code{1L}, or \code{2L}, or logical, which is coerced to integer. The level of verbosity. Values \code{>0} provide some additional information on console, \code{verbose=1L} is less detailed, \code{verbose=2L} is more detailed. Default is \code{0L}.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
Object of class \code{ursaRaster} with the same number of bands as in input raster.
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
Other focal operations: \code{\link[ursa:Ops.focal_mean]{focal_mean}}, \code{\link[ursa:Ops.focal_extrem]{focal_extrem}}.
}
\examples{
session_grid(NULL)
a <- ursa_dummy(1,mul=1/8,elements=0,bandname="src")
a[a<80] <- NA
bF <- c(fillNA.F=focal_median(a[1],size=5,cover=0.5,fillNA=FALSE))
bT <- c(fillNA.T=focal_median(a[1],size=5,cover=0.5,fillNA=TRUE))
print(c(diff=bT-bF))
d <- c(a,bF,bT)
print(d)
display(d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
