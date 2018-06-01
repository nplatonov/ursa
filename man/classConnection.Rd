\name{ursaConnection}
\alias{class-ursaConnection}
\alias{print.ursaConnection}
\alias{seek.ursaConnection}
\title{
   Connection of raster objects.
}
\description{
Class \code{ursaConnection} is a part of class \code{ursaRaster}. It defines storage of raster images and manipulations with reading and writing.
}
\usage{
\method{print}{ursaConnection}(x, ...)

\method{seek}{ursaConnection}(con, where = NA, origin = "start", rw = "", ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaConnection} object in function \code{print}.
%%~ Any in function \code{is.con}
}
  \item{con}{
\code{ursaConnection} object in function \code{seek}.
}
  \item{where}{
Passed to \code{\link[base:seek]{seek}} for class \code{connection}.
}
  \item{origin}{
Passed to \code{\link[base:seek]{seek}} for class \code{connection}.
}
  \item{rw}{
Passed to \code{\link[base:seek]{seek}} for class \code{connection}.
}
  \item{\dots}{
\code{print}: Further arguments passed to generic functions \code{\link[base:print]{print}} and \code{\link[utils:str]{str}}.\cr
\code{seek}: Further arguments passed to function \code{\link[base:seek]{seek}} for class \code{connection}.}
}
\details{
\code{ursaConnection} get item \code{$con} from \code{ursaRaster} object.

Functions \code{print} and \code{is.con} are for developers rather than users.

Non-public function \code{.con.skeleton()} is used to generate the blank \code{ursaConnection} object. This approach provides unified sequence of list's items:
%~ \preformatted{
%~ List of 26
%~  $ driver    : chr NA
%~  $ samples   : int NA
%~  $ lines     : int NA
%~  $ bands     : int NA
%~  $ datatype  : int NA
%~  $ interleave: chr NA
%~  $ byteorder : int NA
%~  $ endian    : chr NA
%~  $ swap      : int NA
%~  $ signed    : logi NA
%~  $ offset    : int NA
%~  $ wkt       : logi FALSE
%~  $ nodata    : num NA
%~  $ mode      : chr "raw"
%~  $ sizeof    : int NA
%~  $ indexC    : int NA
%~  $ indexR    : int NA
%~  $ indexZ    : int NA
%~  $ posC      : int NA
%~  $ posR      : int NA
%~  $ posZ      : int NA
%~  $ fname     : chr NA
%~  $ connection: chr NA
%~  $ compress  : int 0
%~  $ seek      : logi NA
%~  $ handle    : logi NA
%~  - attr(*, "class")= chr "ursaConnection"
%~ NULL
%~ }
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{ursaConnection} is a list. The most of names have a relation to specification of \href{http://www.harrisgeospatial.com/docs/ENVIHeaderFiles.html}{ENVI Header Files}. Items:
\item{driver}{Character. Keyword of supported image formats. Allowed "ENVI" or "GDAL".}
\item{samples}{Integer. Number of image columns (samples)}
\item{lines}{Integer. Number of image rows (lines)}
\item{bands}{Integer. Number of image bands (channels, layers)}
\item{datatype}{Integer. Keyword for data type (4 - 32-bit floating point, 2 - 16-bit signed integer, \emph{etc})}
\item{interleave}{Character \code{"bsq"}, \code{"bil"}, \code{"bip"}. Data interleave - streams of bytes in storage.}
\item{byteorder}{Integer, 0 or 1. The order of bytes. \code{byteorder=0} less significant byte first, \code{byteorder=1} most significant byte first}
\item{endian}{Character. See \verb{\dQuote{endian}} argument in \code{\link[base:readBin]{readBin}} and \code{\link[base:readBin]{writeBin}}}
\item{swap}{Integer 0 or 1. Passed to C-funcions \code{fread} and \code{fwrite}}
\item{signed}{Logical. Derived from \code{$datatype}}
\item{offset}{Integer. Header offset in binary file. Default is 0.}
\item{wkt}{Logical. In ENVI Header files \code{wkt=TRUE} forced to use \dQuote{coordinate system string} field instead of \dQuote{projection info} field}
\item{nodata}{Numeric. Replacement \code{NA} values in the storage.}
\item{mode}{Character. \code{\link[base:mode]{storage.mode}} of data}
\item{sizeof}{Integer, positive. Size in bytes for stored values. Extracted from \code{$datatype}}
\item{indexC}{Integer vector. Sample indices in spatial cropping.}
\item{indexR}{Integer vector. Line indices in spatial cropping.}
\item{indexZ}{Integer vector. Band indices}
\item{posC}{Integer vector. Sample indices in partial reading.}
\item{posR}{Integer vector. Line indices in partial reading.}
\item{posZ}{Integer vector. Band indices in spatial crottping or partial reading.}
\item{fname}{Character. File name. If \code{driver=ENVI}, then full path for ENVI binary file.}
\item{connection}{Character. See \code{\link[base]{connections}}}
\item{compress}{Signed integer. \code{0L} no compressing, \code{-1L} comressed file plus decompressed file,\code{-2L} decompressed file, \code{1L} - file will be compressed. }
\item{seek}{Logical. Does connection support \code{\link[base]{seek}}?}
\item{handle}{\code{\link[base]{connections}} in fact}
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
\code{\link[ursa:ursa_000]{ursa}(obj,"con")}
}
\examples{
session_grid(NULL)
\dontrun{print(methods(class="ursaConnection"))}

\dontrun{print(ursa:::.con.skeleton())}

a <- pixelsize()
write_envi(rep(a,5),"tmp1",compress=FALSE)
## change spatial domain for cropping example
g <- session_grid(regrid(lim=c(-1200000,-1400000,1600000,1800000)))
print(g)
b <- open_envi("tmp1")
d <- b[,30:70]
print(ursa(d[2:3],"con"))
close(b)
envi_remove("tmp1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{print}
