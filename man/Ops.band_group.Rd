%%~ is it permitted to use '\name' which is not in a function list?
\name{band_group}
%%~ \name{band_mean}

\alias{band_mean}
\alias{band_sd}
\alias{band_sum}
\alias{band_min}
\alias{band_max}
\alias{band_n}
\alias{band_nNA}
\alias{band_quantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract certain statistics of each band.
}
\description{
Function from this \code{band.*} list returns required statistics for each band.
}
\usage{
band_mean(obj)
band_sd(obj)
band_sum(obj)
band_min(obj)
band_max(obj)
band_n(obj)
band_nNA(obj)
band_quantile(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}.
}
  \item{\dots}{
Arguments, which are passed to generic \code{\link[stats:quantile]{quantile}} function, e.g. \code{probs}, \code{type}.
}
}
\details{
\itemize{
   \item \code{band_mean} returns mean value.
   \item \code{band_sd} returns value of standard deviation with \code{n-1} denominator.
   \item \code{band_sum} returns sum of values.
   \item \code{band_min} returns minimal value.
   \item \code{band_max} returns maximal value.
   \item \code{band_n} returns number of non-\code{NA} pixels.
   \item \code{band_nNA} returns number of \code{NA} pixels.
   \item \code{band_quantile} returns matrix of quantiles.
}
}
\value{
Named vector of numerical or integer values. Band names are used for naming.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Currently, implementation is not optimal, because firstly bundle of statistics is computed using {\link[ursa:Ops.band_stat]{band_stat}} function, and then required statistics is extracted.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[ursa:Ops.band_stat]{band_stat}}

}
\examples{
session_grid(NULL)
a <- ursa_dummy()
print(a)
print(a<80)
print(class(a))
a[a<80]
a[a<80] <- NA
b1 <- band_stat(a)
print(b1)
b2.n <- band_n(a)
str(b2.n)
b2.mean <- band_mean(a)
print(b1$mean)
print(b2.mean)
print(b1$mean-b2.mean)
print(band_quantile(a))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
