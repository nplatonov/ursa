\name{write_gdal}
\alias{write_gdal}
\alias{ursa_write}
\title{
Write raster image to GDAL file(s)
}
\description{
\code{write_gdal} writes in-memory object of class \code{ursaRaster} to disk using GDAL from \pkg{rgdal} package.
}
\usage{
write_gdal(obj, ...)
ursa_write(obj, fname, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}.
}
  \item{\dots}{
Arguments, which are passed to \code{\link[ursa:conn.create_any]{create_gdal}}. Usually, only file name with extension (character) is required. If extension is ".envi", then GDAL driver "ENVI" is used. If extension is ".tif", then GDAL driver "GTiff" is used. If extension is ".img", then GDAL driver "HFA" is used. If extension is ".jpg" or "*.jpeg", then GDAL driver "JPEG" is used. If extension is ".bmp", then GDAL driver "BMP" is used. If extension is ".png", then GDAL driver "PNG" is used. Additionally, argument \code{driver} should be specified. If argument \code{\dots} is \link[base]{missing}, then occasional name is assigned.

For GDAL formats it is creation options \code{"-co"}, e. g., \code{compress="LZW",tiled="NO"} are interpeted as \code{-co "COMPRESS=LZW" -co "TILED=NO"}.

For GDAL formats \code{options=} (named list \code{list(foo="bar1", foo2="bar2")}, named characters \code{c(foo="bar1", foo2="bar2")}, characters in format \code{"foo1=bar1 foo2=bar2"}) is interpeted as creation options (\code{-co}) explicitly. 

For GDAL formats \code{driver=} is interpeted as driver short name (\code{-fo}) explicitly.

}
  \item{fname}{
Character. File name with extension.
}
}
\details{
\code{ursa_write} is simplified call of \code{write_gdal}.


\code{write_gdal} implements writing the whole \code{ursaRaster} object to disk. For multiple access to disk (by chunks), use followed \emph{\link[ursa:classRaster.Replace]{Replace}} construction: \preformatted{
a <- create_gdal(fname)
a[condition_1] <- value1
a[condition_2] <- value2
...
close(a)
}
}
\value{
Integer code of ENVI data type. See values of the \dQuote{data type} field in description of the \href{https://www.nv5geospatialsoftware.com/docs/ENVIHeaderFiles.html}{ENVI Header Format}.
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
\code{\link[ursa:conn.create_any]{create_gdal}}, \emph{\link[ursa:classRaster.Replace]{Replace}} method \code{\link[ursa:classRaster.Replace]{[<-}} for \code{ursaRaster} object, \code{\link[ursa:classRaster_close]{close}} method for \code{ursaRaster} object.

\code{\link[ursa:conn.write_envi]{write_envi}}
}
\examples{
session_grid(NULL)
ftemp <- tempfile(pattern="",fileext="")
fpath <- dirname(ftemp)
fname <- basename(ftemp)
a <- round(ursa_dummy(1,min=0,max=255,nodata=NA))
write_envi(a,file.path(fpath,paste0(fname,"_1",".envi")))
write_gdal(a,file.path(fpath,paste0(fname,"_2")))
write_gdal(a,file.path(fpath,paste0(fname,"_3",".tif")))
write_gdal(a,file.path(fpath,paste0(fname,"_4")),driver="EHdr")
flist <- dir(path=fpath,pattern=fname,full.names=TRUE)
file.remove(flist)
blist <- basename(flist)
res <- NULL
for (i in seq(4))
   res <- c(res,paste(grep(paste0("_",i),blist,value=TRUE),collapse=" "))
print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
