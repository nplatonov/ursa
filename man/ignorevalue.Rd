\name{ignorevalue}
\alias{ignorevalue}
\alias{ignorevalue<-}
\alias{ursa_nodata}
\alias{ursa_nodata<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract and assign \sQuote{nodata} value of raster images.
}
\description{
Ignored values (\emph{aka} 'nodata') are implemented via \code{NA} values, and are optional for raster images in memory. However, to avoid ambiguity for data storage, it is desirable to specify ignored value. \dQuote{ENVI .hdr Labelled Raster} supports \sQuote{nodata} by means of \dQuote{data ignore value} field in the header file.
}
\usage{
ignorevalue(obj)
ursa_nodata(obj)

ignorevalue(obj) <- value
ursa_nodata(obj) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
     \code{ursaRaster} object.
   }
  \item{value}{
     Integer of numeric of length one. Ignored (\sQuote{nodata}) value.
   }
}
\details{
\code{ursa_nodata} is synonim to \code{ignorevalue} for both \emph{Extract} and \emph{Replace} methods.

The \sQuote{nodata} value of raster image \code{obj} is specified in the item \code{obj$con$nodata}.

If values of raster image are in memory then \emph{replace} function \code{ignorevalue<-} also changes \sQuote{nodata} values to \code{NA} values.
}
\value{
\emph{Extract} function \code{ignorevalue} returns value of \code{$con$nodata} item of \code{ursaRaster} object.

\emph{Replace} function \code{ignorevalue<-} returns \code{ursaRaster} with modified \code{$con$nodata} item.
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
a <- round(ursa_dummy(nband=1,min=0.500001,max=4.499999))
print(a)
print(as.table(a))
print(ignorevalue(a))
ignorevalue(a) <- NA
print(as.table(a))
print(ignorevalue(a))
ignorevalue(a) <- 4
print(as.table(a))
print(ignorevalue(a))
print(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
