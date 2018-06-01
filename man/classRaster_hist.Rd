\name{hist}
\alias{ursa_hist}
\alias{hist.ursaRaster}
\alias{histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Histogram of raster image
}
\description{
Two functions for manipulation with histograms. In function \code{hist} values of \code{ursaRaster} objects are passed to generic function \code{\link[graphics]{hist}}, which allows compute and optionally plot histograms. Other function, \code{histogram}, plots histogram in the graphical device \code{\link[grDevices]{png}} directly.
}
\usage{
\method{hist}{ursaRaster}(x, ...)

ursa_hist(obj, width = 800, height = 600, ...)
histogram(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj, x}{
Object of class \code{ursaRaster}
}
  \item{width}{
Positive integer. Width of histogram's panel.
}
  \item{height}{
Positive integer. Height of histogram's panel.
}
  \item{\dots}{
Other arguments, which are passed to \code{\link[ursa]{colorize}} and \code{\link[ursa]{compose_open}} functions.
}
}
\details{
\code{histogram} is synonym of \code{ursa_hist}.

Function \code{hist} for \code{ursaRaster} object is defined as \code{hist(ursa_value(obj),...)}.
\cr
In the function \code{histogram} each bin corresponds to category. The image splitting to categories is realized via \code{\link[ursa]{colorize}} function. The panel of plotting is constucted using artificial coordinate system without geographical projection. The purpose of \code{\link[ursa]{compose_open}} function is prepare layout for plotting raster images; in the case of histogram, the purpose of this function is prepare layout for plotting histogram
}
\value{
Function \code{histogram} returns \code{0L}.
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
\code{\link[ursa]{colorize}} is used to define histogram bins.
\cr
\code{\link[ursa]{compose_open}} prepares panel for histogram plotting.
\cr
\code{\link[graphics]{hist}} computes and plots histograms.
}

\examples{
session_grid(NULL)
a <- pixelsize()
hist(a)
histogram(a,breaks=21)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
