\name{as.matrix}
\alias{as.matrix.ursaRaster}
\alias{[[.ursaRaster}
\title{
Convert raster image to a matrix
}
\description{
\code{as.matrix(coords=TRUE)} prepares a list from the first band of \code{ursaRaster}, which is suitable as input parameter for functions \code{\link[graphics]{image}}, \code{\link[graphics]{contour}} and \code{\link[graphics]{filled.contour}}.
}
\usage{
\method{as.matrix}{ursaRaster}(x, ...)
\method{[[}{ursaRaster}(x, i)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes.
  \describe{
  \item{\code{(coord(s)*|crd|^$)}}{
      Logical If \code{TRUE} then \code{\link[base:list]{list}} is created with \code{x}, \code{y}, \code{z} components, where component \code{$z} contains matrix, components \code{$x} and \code{$y} are coordinates for elements if matrix. 
    }
  }
}
  \item{i}{
Positive integer or character of lentg. If integer, then band index. If character, then band name. If missing, then first band (value \code{1L}) is used.
}
}
\details{
Item \code{colortable} is mainly for internal usage, e. g., for mapping. Item \code{proj} is useful for convertion back to \code{ursaRaster} object by calling \code{\link[ursa:ursa_as]{as.ursa}} function.

Extract operator \code{x[[i]]} is a wrapper for \code{as.matrix(x[i],coords=TRUE)}
}
\value{
Depending of argument \code{coords}.

If \code{coords=FALSE}, then it is a two-dimensional matrix \code{c(samples*lines, bands)}, \link[base:class]{unclassed} from \code{ursaValue} class.

If \code{coords=TRUE}, then it is a list:
\item{x}{Numeric. Midpoints of cells on horizontal axis}
\item{y}{Numeric. Midpoints of cells on vertical axis}
\item{z}{Numeric. Matrix of values}
\item{attr(*,"proj")}{PROJ.4 string for grid, defined by \code{x} and \code{y}}
\item{attr(*,"colortable")}{Optional. Object of class \code{\link[ursa:classColorTable]{ursaColorTable}}. Missing if raster has no color table.}
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
a <- ursa_dummy(nband=3,min=0,max=100)
a <- a[a>=20 & a<=80]
ignorevalue(a) <- 121
str(ursa_value(a[2]))
str(as.matrix(a[2]))
b1 <- a[[2]]
str(b1)
image(b1,asp=1)
b2 <- as.matrix(a[2:3],coords=TRUE)
print(c('theSame?'=identical(b1,b2)))
a2 <- as.ursa(b2)
res <- c(src=a[2],exported_then_imported=a2,diff=a[2]-a2)
print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
