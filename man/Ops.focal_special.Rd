\name{focal_special}
\alias{focal_special}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Custom spatial filtering for image
}
\description{
For each band and for each cell, function calculates value using specific matrix of square window. \emph{Focal} operation of map algebra.
}
\usage{
focal_special(x, type = c("custom", "gaussian", "laplacian", "osisaf",
                          "hires", "correl", "LoG", "sobel", "sobelG"),
             fmask = NULL, size = 3, alpha = 0.5, sigma = (size-1)/4,
             cover = 1 - 1e-06, fillNA = FALSE, saveMargin = FALSE, verbose = 0L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}.
}
  \item{type}{
Character, which is checked by \code{\link[base:match.arg]{match.arg}}.
}
  \item{fmask}{
Numeric square matrix. Filter mask. If \code{NULL} then \code{matrix(1,ncol=1)} is used. Default is \code{NULL}.
}
  \item{size}{
Numeric. Diameter of circuled filter mask. Coerced to the nearest odd value not less than original value.
}
  \item{alpha}{
Nimeric. Parameter \code{alpha} for "Laplacian", "LoG", "hires", "correl" filters. Ignored for others. Default is \code{0.5}.
}
  \item{sigma}{
Numeric. Parameter \code{sigma} for "Gaussian", "LoG" filters. Ignored for others. Default is \code{0.5}.
}
  \item{cover}{
Numeric. \code{0<=cover<=1}. Quota for \code{NA} values in the focal window in relation to the squared size of the focal window. Quota exceeding leads to recording \code{NA} value in the cell. Default is \code{cover=1-1e-6}.
}
  \item{fillNA}{
Logical. If \code{TRUE} then only \code{NA} values of source image can be changed, and non-\code{NA} values of source image are kept without changes. It may provide less reducing of spatial resolution in the task of spatial interpolation. Default is \code{FALSE}.
}
  \item{saveMargin}{
Logical. If \code{TRUE} then adaptive window size is used for cells, where original window goes over image boundary. If \code{FALSE} then image is epxaned to the half size of focal window by \code{NA} values and argument \code{cover} is applied to this expanded part. Default is \code{FALSE}.
}
  \item{verbose}{
Integer of \code{0L}, \code{1L}, or \code{2L}, or logical, which is coerced to integer. The level of verbosity. Values \code{>0} provide some additional information on console, \code{verbose=1L} is less detailed, \code{verbose=2L} is more detailed. Default is \code{0L}.
}
}
\details{
Developed under impression from Matlab's "fspecial".



\itemize{ 
   \item\code{type="custom"}\cr
   Filter mask (argument \code{fmask}) should be specified manually.
   \item\code{type="gaussian"}\cr
      Gaussian filter. For cascade filtering (sequence of increasing or decreasing window \code{size}) \code{sigma=(size-1)/4} produces the same distribution density relative to window size. If \code{sigma} is high but not \code{Inf} then it is low-pass filter with diameter=\code{size} of circular focal window.
   \item\code{type="laplacian"}\cr
   Laplacian filter. Only \code{size=3} makes sence. Any \code{size} is coerced to \code{size=3}.
   \item\code{type="osisaf"}\cr
   Filter for edge detection. Only \code{size=5} makes sence. Any \code{size} is coerced to \code{size=5}. \emph{TODO (pl): reference} \preformatted{
       -0.0625 -0.0625 -0.0625 -0.0625 -0.0625
       -0.0625  0.1250  0.1250  0.1250 -0.0625
       -0.0625  0.1250  0.0000  0.1250 -0.0625
       -0.0625  0.1250  0.1250  0.1250 -0.0625
       -0.0625 -0.0625 -0.0625 -0.0625 -0.0625}
   \item\code{type="hires"}\cr
   Filter for unsharping. Only \code{size=3} makes sence. Any \code{size} is coerced to \code{size=3}.\preformatted{
      -alpha   alpha-1 -alpha
       alpha-1 alpha+5  alpha-1
      -alpha   alpha-1 -alpha}
   \item\code{type="correl"}\cr
   Filter for contrast increasing. Only \code{size=3} makes sence. Any \code{size} is coerced to \code{size=3}.\preformatted{
                 alpha^2    -alpha*(1+alpha^2)               alpha^2
      -alpha*(1+alpha^2)         (1+alpha^2)^2    -alpha*(1+alpha^2)
                 alpha^2    -alpha*(1+alpha^2)               alpha^2}
   \item\code{type="LoG"}\cr
   Laplacian of Gaussian. Filter for edge detection. \code{sigma} is used, \code{alpha} is ignored.
   \item\code{type="sobel"}\cr
   Two-directional Sobel filtering. Only \code{size=3} makes sence. Any \code{size} is coerced to \code{size=3}.
   \item\code{type="sobelG"}\cr
   Sobel gradient. Only \code{size=3} makes sence. Any \code{size} is coerced to \code{size=3}.
   }
}
\value{
Object of class \code{ursaRaster} with the same number of bands as in input raster.
}
\references{
\emph{TODO(pl): at least reference to 'osisaf'.}
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ %%  ~~further notes~~
%%~ }
\section{Warning}{
Laplacian of Gaussian filter (\code{type="LoG"}) is not implemented clearly due to applying continuous-valued formula to discrete matrix.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Other focal operations: \code{\link[ursa:Ops.focal_mean]{focal_mean}}, \code{\link[ursa:Ops.focal_median]{focal_median}}, \code{\link[ursa:Ops.focal_extrem]{focal_extrem}}.
}
\examples{
session_grid(NULL)
v <- round(runif(8,min=-1,max=1),3)
customFilter <- matrix(c(v[1:4],-sum(v),v[5:8]),ncol=3)
a <- ursa_dummy(1,mul=4/8,elements=32)
tpList <- eval(formals("focal_special")$type)
res <- c(src=a,as.ursa(bandname=tpList))
for (tp in tpList) {
   message(tp)
   res[tp] <- focal_special(a,tp,fmask=customFilter,size=11,sigma=1,alpha=0.8
                           ,saveMargin=0,verbose=2L)
}
print(res)
display(res,decor=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
