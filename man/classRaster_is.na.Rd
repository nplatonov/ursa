\name{is.na}
\alias{is.na.ursaRaster}
\alias{is.infinite.ursaRaster}
\alias{is.nan.ursaRaster}
\alias{is.na<-.ursaRaster}
\title{
\sQuote{No data} values for raster images.
}
\description{
The "Extract" function \code{is.na} creates mask for each band. In this mask value \code{1L} corresponds to \code{NA} value in the source image, and value \code{NA} corresponds non-missing values in the source image. The "Replacement" function \code{is.na<-} assings numerical value for cells with \sQuote{no data} value.
}
\usage{
\method{is.na}{ursaRaster}(x)

\method{is.infinite}{ursaRaster}(x)

\method{is.nan}{ursaRaster}(x)

\method{is.na}{ursaRaster}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}
}
  \item{value}{
Numeric.
}
}
\details{
These functions are corresponded to \emph{\strong{local}} operators of map algebra.
}
\value{
"Extract" functions \code{is.na}, \code{is.infinite}, \code{is.nan} return object of class \code{ursaRaster}.

"Replacement" function \code{is.na<-} modifies object of class \code{ursaRaster}.
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
session_grid(regrid(mul=1/4))
a <- ursa_dummy(nband=2,min=0,max=100)
print(a)
print(is.na(a))
a2 <- ursa_new(nband=2)
print(a2)
print(is.na(a2))
a3 <- a
a3[a3<30 | a3>70] <- NA
print(a3)
print(is.na(a3))
is.na(a3) <- 200
print(a3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
