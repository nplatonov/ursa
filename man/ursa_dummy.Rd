\name{ursa_dummy}
\alias{ursa_dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate raster image for examples.
}
\description{
\code{ursa_dummy} returns georeferenced raster image with required number of bands. The value of such image has no sence in reality, but are suitable for \R's examples.
}
\usage{
ursa_dummy(nband = 3L, minvalue = 0, maxvalue = 255, mul = 1, elements = 8L,
           bandname = NULL, nodata = TRUE, resetGrid=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nband}{
Positive integer. Number of bands. Default is 3L.
}
  \item{minvalue}{
Numeric of length 1. Minimal value for raster image. Default is \code{0}.
}
  \item{maxvalue}{
Numeric of length 1. Maximal value for raster image. Default is \code{255}.
}
  \item{mul}{
Positive numeric. The scaling of the existing \link[ursa:session]{session grid}. Value \code{1} means the actual pixel size. Value \code{<1} decreases image size by increasing cell size. Value \code{>1} decreases image size by increasing cell size. Default is \code{1}.
}
  \item{elements}{
Positive integer. Maximal dimension of matrix, which is proportional to \link[ursa:session]{session grid}. If \code{elements} has small value then the resulting image is smooth, like low-resolution image. The \code{elements} has big value, then the resulting image is like white noise.
}
  \item{bandname}{
Character vector or \code{NULL}. Band names for created raster image. If \code{NULL}, then band names are generated autimatically. Default is \code{NULL}.
}
  \item{nodata}{
Numerical or logical. Set value, which is interpreted as 'no-data' flag. If logical and \code{FALSE} then no no-data flag is assigned. If logical and \code{TRUE} then value of no-data flag is generated automatically. If numeric, then no-data is assigned to value of this argument. Default is \code{TRUE}.
}
\item{resetGrid}{Logical. Whether the grid will be resetted to default before raster generation? If \code{FALSE} then raster is generated in the sessional grid. If \code{TRUE} then default parameters are used for raster and session. Default is \code{FALSE}.}

}
\details{
Currently, the values are generated using \code{\link[stats:Uniform]{runif}}.

The value \code{mul<1} speeds up raster generation.
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
a1 <- as.integer(ursa_dummy(nband=1,elements=1e3))  ## white noise
display(a1,legend=NULL)
a2 <- ursa_dummy()
print(a2)
display_brick(a2)
display_stack(a2)
display_rgb(a2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
