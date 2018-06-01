\name{band_stat}
\alias{band_stat}
\title{
Computes statistics for each band of raster.
}
\description{
For each band of \code{ursaRaster} object, \code{band_stat} returns certain statistics (mean, sd, sum, min, max, number of non-\code{NA} pixels, number of \code{NA} pixels). Regarding to each band, it is \emph{global} operations of map algebra.
}
\usage{
band_stat(x, grid = FALSE, raw = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}.
}
  \item{grid}{
Logical. If \code{TRUE} then metadata are returned instead of statistics. Default is \code{FALSE}
}
  \item{raw}{
Logical. For the case of raster values are categories, if \code{raw=TRUE}, then function returns statistics of categories; if \code{raw=FALSE} and names of categories can be tranformed to numerical values, then function returns statistics for un-categirized values. Default is \code{FALSE}.
}
}
\details{
If raster values are not in memory or \code{grid=TRUE} then \code{\link[ursa:ursa_info]{ursa_info}} is returned.

Generic function \code{\link[ursa:classRaster]{print}} for object of class \code{ursaRaster} uses returned value of \code{band_stat} function with formatted columns.

Statistics is computed for omitted \code{NA} values.
}
\value{
\code{\link[base:data.frame]{data.frame}}. Row names are indices of bands. Column names are:
\item{name}{Band name.}
\item{mean}{Mean value.}
\item{sd}{Value of standard deviation with \code{n-1} denomination.}
\item{sum}{Sum of values.}
\item{min}{Minimal value.}
\item{min}{Maximal value.}
\item{n}{Number of non-\code{NA} pixels.}
\item{nNA}{Number of \code{NA} pixels.}
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
Columns extraction from returned data frame is in the group of \code{\link[ursa:Ops.band_group]{band.*}} functions.
}
\examples{
session_grid(NULL)
s <- substr(as.character(sessionInfo()),1,48)
a <- reclass(ursa_dummy(bandname=s),ramp=FALSE)
band_stat(a,grid=TRUE)
b2 <- band_stat(a)
b3 <- band_stat(a,raw=TRUE)
str(b2)
str(b3)
print(b2)
print(a) ## 'print.ursaRaster' uses 'band_stat'
print(a,raw=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
