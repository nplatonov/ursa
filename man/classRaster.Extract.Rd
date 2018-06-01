\name{Extract}
\alias{Extract}
\alias{Subset}
\alias{[.ursaRaster}
\title{
Extract portion of raster images
%%  ~~function to do ... ~~
}
\description{
This operator is used to get single band or subset of bands from multi-band \code{ursaRaster} object. Another purpose is get portions of data including data reading from files.
}
\usage{
\method{[}{ursaRaster}(x, i, j, ..., drop = FALSE)
%~ x[i, j, ..., drop = FALSE]
}
\arguments{
  \item{x}{
\code{ursaRaster} object
}
  \item{i}{
Integer or character. If integer, then band index, specifying bands to extract. If character, either list of band names or charater sting for \code{\link[base:grep]{regular expression}} to match band index. In the \emph{(spatial, temporal)} iterpretation of \code{ursaRaster} object \code{j} points to \emph{temporal} component.
}
  \item{j}{
Integer. Line index, specifying lines to extract.
}
  \item{\dots}{
Mentioned for consistence with internal generic function \code{\link[base:Extract]{[}}.
\cr
Use \code{regexp=FALSE} for matching by \code{\link[base]{match}}, and \code{regexp=TRUE} for matching by Perl-compatible regexps case insensitive \code{\link[base:grep]{grep}}. Default is \code{FALSE}.
}
  \item{drop}{
Not used. For consistence with generic function.
%%~ If \code{TRUE} than \code{$value} item of \code{ursaRaster} object is coerced to the lowest possible dimension.
}
}
\details{
Operator \verb{\sQuote{[}} is high-level implementation for data reading. If \code{x$value} item is not applicable, then value of \code{ursaRaster} is not in memory. In this case the controlled by \code{i} and \code{j} portion is read to memory. If both \code{i} and \code{j} are missing, then \code{x[]} reads all values from file.

\code{x[,j]} is appropriate for time series analysis and processing in the case bands have relation to \emph{temporal} observation. Use \code{\link[ursa:regrid]{regrid}} for geographical subset and cropping.

}
\value{
   \code{ursaRaster} object with extracter bands. Values (\code{$value} item) are in memory.
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

\section{Warning}{
   It is not allowed to read simultaneously portion of bands and portion of lines from file, \emph{e.g.},
\preformatted{
   x <- open_envi(fname)
   y <- x[2:3,10:20]
   close(x)
}
Such brunch is not implemented in code. You use one of the followed tricks:
\preformatted{
   x <- open_envi(fname)
   y <- x[2:3][,20:30]
   close(x)
}
or
\preformatted{
   x <- open_envi(fname)
   y <- x[,20:30][2:3]
   close(x)
}
}

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
## Prepare
session_grid(regrid(mul=1/8))
a <- pixelsize()
w <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"
      ,"MondayAgain")
b <- rep(a/mean(a),length(w))+seq(length(w))-1
bandname(b) <- w
nr <- ursa_rows(b)
bottom <- (as.integer(nr/2)):nr
write_envi(b,"tmp1",compress=FALSE,interleave="bil")

## Extract
print(b["Monday",regexp=TRUE])
print(b["Monday",regexp=FALSE])
print(b["s"])
print(b["^s"])
d1 <- b[6,bottom]
rm(b)

## Read from file
b <- open_envi("tmp1")
print(b[])
print(b[-c(6:8)])
d2 <- b[,bottom][6] ## don't use b[6,bottom]
close(b)
envi_remove("tmp1")

## Compare
print(d1)
print(d2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
