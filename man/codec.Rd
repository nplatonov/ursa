\name{codec}
\alias{compress}
\alias{decompress}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reduce and restore dimenstions for sparse data matrix
}
\description{
\code{compress} reduces dimension of source image matrix and assigns indices. \code{decompress} uses indices for expansion of reduced image matrix.
}
\usage{
decompress(obj)
compress(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
After masking, vectorization of lines, points and small polygons image matrix is often sparse.
Compressing (\code{compress}) is an option to reduce object size in memory. Decompressing (\code{decompress}) restore original data matrix.
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
\note{
Currently, usage of compressed image matrix is limited. Spatial filtering (e.g. \code{\link[ursa:Ops.focal_mean]{focal_mean}}) does not operate with compressed data.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
b <- as.data.frame(pixelsize())
b <- subset(b,x>1000000 & x<2000000 & y>3000000 & y<4000000)
a1 <- as.ursa(b)
print(a1)
print(object.size(a1))
a2 <- compress(a1)
print(a2)
print(object.size(a2))
a3 <- decompress(a2)
print(a3)
print(object.size(a3))
print(identical(a1,a3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
