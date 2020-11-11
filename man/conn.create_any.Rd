\name{create_envi}
\alias{create_envi}
\alias{create_gdal}
\title{
Create ENVI or GDAL files on disk
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{create_envi} creates ENVI binary and header files on disk. ENVI binary file is filled by blank (zero) values.
\cr
\code{create_gdal} is a wrapper for creating new object of class \code{\link[rgdal:GDALTransientDataset-class]{GDALTransientDataset}}.
}
\usage{
create_gdal(x, ...)

create_envi(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Filename, or any refenerce object to help assign properties of new ENVI file. Can be missed.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
Use \code{name = value} sequence. Properties of new ENVI file are extracted from keywords in 'name' and data types of 'value'.
}
}
\details{
   \code{create_envi} and \code{create_gdal} use parameters of grid (boundary box, cell size, projection) from reference object of class \code{ursaRaster} in argument \code{x} or calls \code{\link[ursa:session]{session_grid}}. You may specify values of GDAL or ENVI binary file later using \code{\link[ursa:classRaster.Replace]{[<-}}. If \code{x} is object of class \code{ursaRaster} then metadata parameters (interleave, data type, ignore value, etc) are inherited. 

Keywords:
\itemize{
\item{\code{fname} - character. File name for created GDAL or ENVI file.
\cr\cr \emph{For} \code{create_envi} \emph{only}: If \code{compress} of \code{connections} is not specified then example for \dQuote{fileout} file name:
\itemize{
\item{"fileout" - If external 'gzip' is found then "fileout.envigz" is created else "fileout.envi"}
\item{"fileout.envi" - "fileout.envi" is created without any compression.}
\item{"fileout." - "fileout" is created without any compression.}
\item{"fileout.bin" - "fileout.bin" is created without any compression.}
\item{"fileout.img" - "fileout.img" is created without any compression.}
\item{"fileout.dat" - "fileout.dat" is created without any compression.}
}
}
\item{\code{driver} - character. \emph{For} \code{create_gdal} \emph{only}. Which GDAL driver is used.}
\item{\code{layername} - character of length>=1. Layernames (\sQuote{Band name} in ENVI header file)}
\item{\code{bandname} - character of length>=1. Layernames (\sQuote{Band name} in ENVI header file)}
\item{\code{name} - character of length>=1. Layernames (\sQuote{Band name} in ENVI header file)}
\item{\code{nodata} - integer or numeric. Value in GDAL or ENVI binary file, which is interpretted as \code{NA} in \R}
\item{\code{ignore} - integer or numeric. Value in GDAL or ENVI binary file, which is interpretted as \code{NA} in \R}
\item{\code{ignorevalue} - integer or numeric. Value in GDAL or ENVI binary file, which is interpretted as \code{NA} in \R}
\item{\code{bg} - integer or numeric. Value in GDAL or ENVI binary file, which is interpretted as \code{NA} in \R}
\item{\code{connection} - character. \emph{For} \code{create_envi} \emph{only}. \code{\link[base]{connections}} for ENVI binary file.}
\cr\cr Valid values are:
\itemize{
\item{"gz" - connection is "gzfile"}
\item{"bz" - connection is "bzfile"}
\item{"xz" - connection is "xzfile"}
\item{"file" - connection is "file"}
}
\item{\code{interleave} - character. Interleave. Valid values are "bsq", "bil", "bip". For \code{create_gdal} and \code{driver="GTiff"} valid values are "bsq" and "bil".}
\item{\code{datatype} - character or integer (numeric). Data type.
\cr\cr Valid values are:
\itemize{
\item{1, "byte", "Byte", "UInt8" = Byte: 8-bit unsigned integer}
\item{2, "integer", "Int16" = Integer: 16-bit signed integer}
\item{3, "Int32" = Long: 32-bit signed integer}
\item{4, "real", "float", "Float32" = Floating-point: 32-bit single-precision}
\item{5, "Float64" = Double-precision: 64-bit double-precision floating-point}
\item{11, "UInt8" = Byte: 8-bit signed integer. \bold{Not in specification.} Only for use with this package.}
\item{12, "UInt16" = Integer: 16-bit unsigned integer}
\item{13, "UInt32" = Long: 32-bit unsigned integer}
}
Specification \url{https://www.l3harrisgeospatial.com/docs/ENVIHeaderFiles.html} is used.
}
\item{\code{byteorder} - numeric (integer). Byte order.}
\item{\code{bands} - numeric( integer). Number of bands/layers}
\item{\code{nband} - numeric( integer). Number of bands/layers}
\item{\code{nlayer} - numeric( integer). Number of bands/layers}
\item{\code{layers} - numeric( integer). Number of bands/layers}
\item{\code{compress} - integer (numeric) or logical. \emph{For} \code{create_envi} \emph{only}. Should ENVI binary file be compressed after closing connection.}
\item{\code{wkt} - integer (numeric) or logical. Forced adding \sQuote{coordinate system string} to ENVI header file}
\item{\code{ext} - character. \emph{For} \code{create_envi} \emph{only}. Extension of ENVI binary file. For extensions not in \code{c("envi","bin","dat","img")} list}
}
If file name is unknown, then random file name is used with informing via \code{message()}. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
   Object of class \code{ursaRaster} with opened connection of GDAL or ENVI binary file.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{Nikita Platonov \email{platonov@sevin.ru}}
%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link[ursa:ursa_new]{ursa_new}} creates object of class \code{ursaRaster} in memory and allows to assign values at once.

Use \code{\link[ursa:session]{session_grid}} to check or specify parameters of grid before calling \code{create_envi}.

Use \code{\link[ursa:classRaster.Replace]{[<-}} to assign values to ENVI binary file after calling \code{create_envi}.

Use \code{\link[ursa:classRaster_close]{close}} (or \code{\link[ursa:classRaster_close]{close_envi}}) to close \code{\link[base]{connections}}.
}
\examples{
session_grid(NULL)
a <- create_envi()
fname <- a$con$fname
dir(pattern=basename(envi_list(fname)))
close(a)
invisible(envi_remove(fname))

a <- create_envi("exam1",layername=paste("Band",1:5)
                ,ignorevalue=99,datatype="Int16",interleave="bil")
ursa_info(a)
print(a[])
close(a)
invisible(envi_remove("exam1"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
