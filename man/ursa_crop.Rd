\name{ursa_crop}
\alias{ursa_crop}
%%~ \alias{subset.ursaRaster}
\title{
Crop 'no data' margins.
}
\description{
Function \code{ursa_crop}
%%~ (\code{subset} for \code{ursaRaster} object is alias)
makes such spatial subset of source raster image, where margins of 'no data' values are absent or have specified width.
}
\usage{
%%~ \method{subset}{ursaRaster}(x, ...)

ursa_crop(obj, condition, border = 0, resetGrid = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}
}
  \item{condition}{
Object of class \code{ursaRaster} or \code{\link[base]{missing}}. The condition for cutting. If \sQuote{missing} then condition is defined from \code{obj}.
}
  \item{border}{
Integer of length 1 or 4. Desired margins for geographical subset. Units are cells (pixels).
}
  \item{resetGrid}{
Logical. If \code{resetGrid=TRUE} then sessional grid parameters is established from grid parameters of created raster. If \code{resetGrid=FALSE} then sessional grid parameters keep without change. Defailu is \code{TRUE}.
}
  \item{verbose}{
Logical. \code{TRUE} provides some additional information on console.
}
%%~   \item{x}{
%%~ Passed to function \code{ursa_crop} as argument \code{obj}.
%%~ }
%%~   \item{\dots}{
%%~ Other arguments, which are passed to function \code{ursa_crop}.
%%~ }
}
\details{
%%~ \code{subset} for \code{ursaRaster} object is a wrapper to \code{ursa_crop}.

This function calls \code{\link[ursa:regrid]{regrid}} with passing values of arguments \code{resetGrid} and \code{verbose} without changes.

This function is an instrument for data compression for spatial matrices with wide margins of 'no data' value. It keeps spatial structure (pixel's neighborhood) in the internal data storage. Otherwise, \code{\link[ursa:codec]{compress}} reduces object size using spatial indexing with dropping of spatial structure.
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
'printCR' <- function(obj) print(with(ursa_grid(obj),c(c=columns,r=rows)))
g0 <- session_grid()
a <- pixelsize()
th <- with(ursa_grid(a),resx*resy*1e-6)
a0 <- a[a>th*0.9]
print(session_grid())
printCR(a0)
print(a0)
a1 <- ursa_crop(a0,resetGrid=TRUE)
print(session_grid())
printCR(a1)
print(a1)
a2 <- ursa_crop(a0,resetGrid=FALSE)
print(session_grid())
printCR(a2)
print(a2)
a3 <- a[a>=th*0.85 & a<=th*1.01]
b1 <- ursa_dummy(nband=3,min=0,max=255)
print(b1)
b2 <- ursa_crop(b1[a3>0],border=10)
print(b2)
printCR(b2)
b2[is.na(b2)] <- 255
display_rgb(b2)
b3 <- ursa_crop(b1,a3,border=0)
print(b3)
printCR(b3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
