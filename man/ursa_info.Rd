\name{ursa_info}
\alias{ursa_info}
\title{
Print metadata for raster image.
}
\description{
Function shows information about raster CRS, data type, storage mode, nodata value, structure of band names.
}
\usage{
ursa_info(obj, detail = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
  \code{ursaRaster} object
}
  \item{detail}{
\emph{Not used. Reserved for potential detalization levels}
}
%%~   \item{digits}{
%%~   Non-negative integer. Minimal number of \emph{significant} digits, see \code{\link[base]{print.default}}. Default is \code{3}.
%%~ }
  \item{\dots}{
  Arguments, which are passed to \code{\link[utils:str]{str}}.
}
}
\details{
\code{ursa_info} generates a list and then shows structure of this list via function \code{\link[utils:str]{str}}.
}
\value{
Object of \emph{temporal} class \code{ursaMetadata} is a list with items:
\item{columns}{Number of columns (samples)}
\item{rows}{Number of rows (lines)}
\item{resx}{Grid cell size by horizontal axis}
\item{resy}{Grid cell size by vertical axis}
\item{minx}{Left margin of boundary box}
\item{maxx}{Right margin of boundary box}
\item{miny}{Bottom margin of boundary box}
\item{maxy}{Top margin of boundary box}
\item{proj4}{PROJ.4 string}
\item{nodata}{\emph{Optional}. Value, which is interpreted as \code{NA}}
\item{datatype}{\emph{Optional}. If data are on disk, then integer code of data type.}
\item{interleave}{\emph{Optional}. If data are on disk, then abbreviation of bands interleave.}
\item{mode}{Character of length 2: \code{\link[base:mode]{storage mode}} and \code{\link[base:class]{class}} of value. If data has not been read, then class is \code{"logical"}. If data is not in memory, then storage mode is \code{"raw"}.}
\item{bandname}{Band names.}
\item{colortable}{\emph{Optional}. Structure of \link[ursa:classColorTable]{color table}.}

Function returs \code{NULL}.
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
\code{\link[utils:str]{str}}\cr
\code{\link[ursa:classRaster]{print.ursaRaster}}
}
\examples{
session_grid(NULL)
a <- as.integer(round(ursa_dummy(nband=3)))

print(a) ## print data
ursa_info(a,digits=1) ## print metadata

fname <- tempfile()
write_envi(a,fname,compress=FALSE)
b1 <- open_envi(fname)
ursa_info(b1)
close(b1)
b2 <- read_envi(fname)
ursa_info(b2)

# print ENVI header
sapply(c(" -------------- begin --------------",readLines(paste0(fname,".hdr"))
        ," --------------- end ---------------"),message)

envi_remove(fname)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
