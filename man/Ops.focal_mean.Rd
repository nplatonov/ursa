\name{focal_mean}
\alias{focal_mean}
\title{
Low-pass spatial filter for image.
}
\description{
Low-pass filtering by a square window in the image processing. \emph{Focal} operation of map algebra. Weight of pixels is proportional to cell area inside of focal window.
}
\usage{
focal_mean(x, size = 3.0, cover = 1e-06, fillNA = FALSE, saveMargin = TRUE
          ,noNA = TRUE, verbose = 0L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}.
}
  \item{size}{
Positive numeric. Size of square focal window. Fractional values are allowed. If \code{size} is not odd (3, 5, 7, \dots), then window size is expanded to the nearest odd value not less than original value, and pixels on border are taken with the weight, which is proportional to the cell area inside of orininal size. Default \code{size=3}.
}
  \item{cover}{
Numeric. \code{0<=cover<=1}. Quota for \code{NA} values in the focal window in relation to all values. Values are taken with the weight proportional of cell areas inside of focal window. Quota exceeding leads to recording \code{NA} value in the cell. Default is \code{cover=1e-6}.
}
  \item{fillNA}{
Logical. If \code{TRUE} then only \code{NA} values of source image can be changed, and non-\code{NA} values of source image are kept without changes. It may provide less reducing of spatial resolution in the task of spatial interpolation. Default is \code{FALSE}.
}
  \item{saveMargin}{
Logical. If \code{TRUE} then adaptive window size is used for cells, where original window goes over image boundary. If \code{FALSE} then image is epxaned to the half size of focal window by \code{NA} values and argument \code{cover} is applied to this expanded part. Default is \code{TRUE}.
}
  \item{noNA}{
Logical. If \code{TRUE} then \code{NA} values are transformed to numerical constant, which is interpeted as "no data" value. Filter without \code{NA} values has more perfomance, and generally filter with pre- and post-trandormations of \code{NA} values have more perfomance too. Default is \code{TRUE}.
}
  \item{verbose}{
Integer of \code{0L}, \code{1L}, or \code{2L}, or logical, which is coerced to integer. The level of verbosity. Values \code{>0} provide some additional information on console, \code{verbose=1L} is less detailed, \code{verbose=2L} is more detailed. Default is \code{0L}.
}
}
\details{
The reference is always central pixel, even if window size is even.

If \code{size=3} then multiplicator is \code{3^(-2)} and elements have equal weights:
\preformatted{
     [,1] [,2] [,3]
[1,]    1    1    1
[2,]    1    1    1
[3,]    1    1    1
}
If \code{size=2} then multiplicator is \code{2^(-2)} and weights of elements are:
\preformatted{
     [,1] [,2] [,3]
[1,] 0.25 0.50 0.25
[2,] 0.50 1.00 0.50
[3,] 0.25 0.50 0.25
}
If \code{size=3.4} then multiplicator is \code{3.4^(-2)} and weights of elements are:
\preformatted{
     [,1] [,2] [,3] [,4] [,5]
[1,] 0.04 0.20 0.20 0.20 0.04
[2,] 0.20 1.00 1.00 1.00 0.20
[3,] 0.20 1.00 1.00 1.00 0.20
[4,] 0.20 1.00 1.00 1.00 0.20
[5,] 0.04 0.20 0.20 0.20 0.04
}
}
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

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=1,mul=1/8,elements=0)
a[a<80] <- NA
print(a)
b1 <- focal_mean(a,size=6,cover=0.5,saveMargin=FALSE)
b2 <- focal_mean(a,size=6,cover=0.5,saveMargin=TRUE)
b3 <- focal_mean(a,size=6,cover=0.5,saveMargin=TRUE,fillNA=TRUE)
print(b3-a)
display(c(a,b1,b2,b3),blank.angle=c(-45,45),blank.density=20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
