\name{temporal_mean}
\alias{temporal_mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smooth value across bands using moving mean window
}
\description{
\code{temporal_mean} is applicable for multiband raster image, where bands are regular timestamps or period. For each cell (\emph{local} operation of map algebra), the values are averaged using moving window.
}
\usage{
temporal_mean(obj, win = 7, cover = 0, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster} or matrix, where spatial locations are by rows and temporal observations are by columns.
}
  \item{win}{
Positive integer. Size of moving window. Required odd value; otherwise is coerced to the closest odd integer.
}
  \item{cover}{
Numeric in the interval \code{0 <= cover <= 1} or positive numeric >1. The required amount of non-\code{NA} elements in window to do a filtering. Otherwise, \code{NA} value is in output cell. If \code{cover<=1} then amount is relative to window size. Default is \code{0}: \code{NA} values are produced only if all elements in window have \code{NA} value.
}
  \item{verbose}{
Logical. \code{TRUE} provides some additional information on console. Default is FALSE.
}
}
\details{
\code{temporal_mean} is similar to function \code{runmean(x=obj, k=win, endrule="mean")} from package \pkg{caTools}.
}
\value{
\code{ursaRaster} object, if \code{obj} is object of class \code{ursaRaster}.

\code{matrix} object, if \code{obj} is a matrix.
}
\references{
Package \pkg{caTools} \url{https://CRAN.R-project.org/package=caTools }
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

\section{Advanced}{
\code{temporal_mean} is only smoothing of time-series. For time-series analysis and processing it is suggested to apply lower-level approach.

\code{\link[ursa:classRaster_as.matrix]{as.matrix}} (for \code{ursaRaster} object with argument \code{coords=FALSE}) or \code{\link[ursa:classValue]{ursa_value}} return matrix with spatial component by rows and temporal component by columns. It is possible to use \code{\link[base:apply]{apply}} with argument \code{MARGIN=1} to this matrix. If \code{apply} returns matrix \code{Y}, then this matrix can be coerced to \code{ursaRaster} object by calling \code{\link[ursa:ursa_as]{as.ursa}} with argument \code{t(Y)}. \preformatted{
      X <- as.matrix(obj)
      Y <- apply(X, 1, function(x) {y <- do_something_return_matrix(x); y})
      res <- as.ursa(t(Y))
   }
   For example, package \pkg{caTools} provides some functions for manipulation with moving window.
}

\seealso{
\code{\link[caTools:runmean]{caTools::runmean}} (click if package \pkg{caTools} is installed)
}
\examples{
session_grid(NULL)
set.seed(352)
n <- 45 # bands
m <- 3 # sample size
k <- median(seq(n))+seq(m)-(m \%/\% 2)-1 ## sample subset
s <- 5 # window size
a <- round(ursa_dummy(n,min=-60,max=60,elements=15,mul=1/8))

\dontrun{
if (requireNamespace("caTools")) {
   b1 <- as.ursa(t(apply(as.matrix(a),1,caTools::runmean,k=s,endrule="mean")))
   b2 <- temporal_mean(a,s)
   print(b1[k])
   print(b2[k])
   print(c('identical?'=all.equal(ursa_value(b1),ursa_value(b2))))
}}

a[a<(-40)] <- NA
va <- as.matrix(a) # or 'ursa_value(a)'
b3 <- temporal_mean(a,s,cover=3/4,verbose=TRUE)
b4 <- as.ursa(temporal_mean(as.matrix(va),s,cover=3/4,verbose=TRUE))
p <- list('Before moving window'=a[k]
         ,'After moving window'=b3[k]
         ,'\'temporal_mean\' to matrix'=b4[k])
print(p)
print(c('identical?'=all.equal(ursa_value(b3),ursa_value(b4))))
display(p[1:2],legend=list(list(1,"right"),list(2,"right")),decor=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
