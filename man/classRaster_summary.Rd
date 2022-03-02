\name{summary}
\alias{summary.ursaNumeric}
\alias{summary.ursaCategory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of raster image.
}
\description{
Function \code{summary} for \code{ursaRaster} object produces summaries for each band.
\cr
Function \code{summary} for \code{ursaValue} object produces summaries for all values of raster image regardless of bands. 
}
\usage{
\method{summary}{ursaRaster}(object, ...)

\method{summary}{ursaNumeric}(object, ...)

\method{summary}{ursaCategory}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of classes \code{ursaRaster}, \code{ursaNumeric}, or \code{ursaCategory}
}
  \item{\dots}{
Additional arguments affecting the summary produced.
}
}
\details{
\code{summary} for \code{ursaRaster} object applies \code{\link[base]{summary}} to each column of two-dimensions value matrix and collating the results.
\cr
\code{summary} for \code{ursaValue} object drops dimensions and applies \code{\link[base]{summary}} to a vector.
}
\value{
\code{summary} for \code{ursaRaster} object returns value of function \code{\link[base:summary]{summary.matrix}}.
\cr
\code{summary} for \code{ursaValue} object returns object of class \verb{\dQuote{summaryDefault}}.
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
\code{\link[base]{summary}} in package \pkg{base}.
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/4))
a <- ursa_dummy(nband=3)
print(summary(a))
print(summary(ursa_value(a)))
print(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
