\name{local_stat}
\alias{local_stat}
\title{
Bundle of statistics, which is applied to each cell of multi-band image.
}
\description{
If bands of \code{ursaRaster} object are interpreted as observations in time, then \code{local_stat} returns some parameters for time-series analysis. This is a \bold{local} operation of map algebra.
}
\usage{
local_stat(obj, time = NULL, cover = 1e-06, smooth = FALSE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}
}
  \item{time}{
Numeric or \code{NULL}. If \code{NULL} then regression parameters are for regular time-series using position of band in the brick (or, \code{time=seq(obj)}). If numeric, then length of \code{time} should be equal to number of bands of \code{obj}, and \code{time} is used to set irregularity for time-series.
}
  \item{cover}{
Numeric. \code{0<=cover<=1} or \code{>1}. Quota for \code{NA} values in the location for all bands. Quota exceeding leads to recording \code{NA} value in the cell of created band. If \code{code>1} then number of bands. If \code{0<=cover<=1} then proportion \code{cover} to number of bands. Default is \code{1e-6}.
}
  \item{smooth}{
Logical. If \code{TRUE} then \link[ursa:Ops.focal_median]{median} focal smoothing is applying to created 'slope' band; it is more suitable for visualization. Default is \code{FALSE}.
}
  \item{verbose}{
Logical. Value \code{TRUE} provides some additional information on console. Default is \code{FALSE}.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
Object of class \code{ursaRaster} with bands:
\item{mean}{Mean value in each cell across all bands of source raster.}
\item{sd}{Standard deviation in each cell across all bands of source raster.  Denominator is \code{n}. }
\item{sum}{Sum value in each cell across all bands of source raster.}
\item{min}{Minimal value in each cell across all bands of source raster.}
\item{max}{Maximal value in each cell across all bands of source raster.}
\item{n}{Number of non-\code{NA} values in each cell across all bands of source raster (number of observations).}
\item{slope}{Slope value in each cell across all bands of source raster.}
\item{slopeS}{Significance of slope value taken with a sign of slope.}
\item{RSS}{Resisual sum of squares.}
\item{ESS}{Explained sum of squares.}
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
\emph{\link[ursa:Ops.local_group]{Local}} statistics of map algebra, 
\link[ursa:classRaster_GroupGeneric]{Group} generics for objects of class \code{ursaRaster}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
set.seed(353)
session_grid(regrid(mul=1/8))
a <- ursa_dummy(nband=15)
a[a<60] <- NA
cvr <- 12
b <- local_stat(a,cover=cvr)
print(b)
c.mean <- c('<bundle> mean'=b["mean"]
           ,'local_mean'=local_mean(a,cover=cvr)
           ,'<generic> mean'=mean(a,cover=cvr))
c.max <- c('<bundle> max'=b["max"]
          ,'local_max'=local_max(a,cover=cvr)
          ,'<generic> max'=max(a,cover=cvr))
print(c.mean)
print(c.max)
cmp <- c(mean=b["mean"]-local_mean(a,cover=cvr)
        ,sd=b["sd"]-local_sd(a,cover=cvr))
print(round(cmp,12))
d <- as.list(b)
d[["slopeS"]] <- colorize(d[["slopeS"]],stretch="signif")
display(d,blank.density=20,blank.angle=c(-45,45))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
