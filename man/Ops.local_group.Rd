\name{local_group}
\alias{local_mean}
\alias{local_sum}
\alias{local_median}
\alias{local_min}
\alias{local_max}
\alias{local_sd}
\alias{local_var}
\alias{local_all}
\alias{local_any}
\alias{local_length}
\alias{mean.ursaRaster}
\alias{median.ursaRaster}
\alias{.average}
\title{
Create single-band raster using statistics of multi-bands raster.
}
\description{
Local operations (mean value, sum of values, median, minimum, maximum) of map algebra for multi-bands \code{ursaRaster} object.
}
\usage{
local_mean(x, cover = 0.5 - 1e-3, weight = NULL, verbose = FALSE, bandname = "mean")
local_sum(x, cover = 0.5 - 1e-3, weight = NULL, verbose = FALSE, bandname = "sum")
local_median(x, cover = 0.5 - 1e-3, verbose = FALSE)
local_min(x, cover = 0.5 - 1e-3, verbose = FALSE)
local_max(x, cover = 0.5 - 1e-3, verbose = FALSE)
local_sd(x, cover = 0.5 - 1e-3, verbose = FALSE)
local_var(x, cover = 0.5 - 1e-3, verbose = FALSE)

\method{mean}{ursaRaster}(x, ...)

\method{median}{ursaRaster}(x, ...)

# non public
.average(x, cover = 0.5 - 1e-3, weight = NULL, sum = FALSE, verbose = FALSE)

}
\arguments{
  \item{x}{
\code{ursaRaster} object. In function \code{local_mean} and \code{local_sum} it is allowed to specify array with 3 dimensions (col, row, band) or (row, col, band)
}
  \item{cover}{
Numeric. \code{0<=cover<=1} or \code{>1}. Quota for \code{NA} values in the location for all bands. Quota exceeding leads to recording \code{NA} value in the created map. If \code{code>1} then number of bands. If \code{0<=cover<=1} then proportion \code{cover} to number of bands.
}
  \item{weight}{
Positive numeric of length equal to number of bands. For \code{local_mean} and \code{local_sum} only. If specified, then weighted mean or sum are applied. The prior normalization is not required.
}
  \item{sum}{
Logical. For \code{.average} only. If \code{sum=TRUE} then fuction returns sum of values else mean value.
}
  \item{verbose}{
Logical. If \code{verbose=TRUE} then some output appears in console. Used for debug and benchark.
}
  \item{bandname}{
Character. Band name for created single-band image.
}
  \item{...}{
Function \code{mean} - arguments, which are passed to \code{local_mean}.
\cr
Function \code{median} - arguments, which are passed to \code{local_median}.
}
}
\details{
If for valid output cell value it is requred to have at least \code{m} values not marked as \code{NA}, specify quota as \code{cover=m/nband(x)}.

\code{local_mean} and \code{local_sum} are wrapper to non-public function \code{.average}.

Generic functions \code{mean}, \code{median}, \code{sd} for \code{ursaRaster} class are implemented via \code{local_mean}, \code{local_median}, \code{local_sd}, respectively. 
}
\value{
Single-band \code{ursaRaster} object.
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
If bands are interpreted as time series, apply \code{\link[ursa:Ops.local_stat]{local_stat}}

Mean value for image brick \code{\link[ursa:Ops.global_group]{global_mean}}

Mean value for each band \code{\link[ursa:Ops.band_group]{band_mean}}
}
\examples{
session_grid(NULL)
b <- ursa_dummy(nband=7,min=0,max=100)
b[b<40] <- NA
print(b)
res <- c('mean'=mean(b),'local_mean'=local_mean(b)
        ,'sum0'=local_sum(b,cover=0),'sum1'=local_sum(b,cover=1))
print(res)

\dontrun{
display(b)
display(res)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
