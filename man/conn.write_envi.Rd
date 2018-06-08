\name{write_envi}
\alias{write_envi}
\title{
Write raster image to ENVI .hdr Labelled Raster file.
}
\description{
\code{write_envi} writes in-memory object of class \code{ursaRaster} to disk in the ENVI .hdr Labelled Raster file format.
}
\usage{
write_envi(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}.
}
  \item{\dots}{
Arguments, which are passed to \code{\link[ursa:conn.create_any]{create_envi}}. Usually, only file name (character) is required. If \link[base]{missing}, then occasional name is assigned.
}
}
\details{
\code{write_envi} implements writting the whole \code{ursaRaster} object to disk. For multiple access to disk (by chunks), use followed construction: \preformatted{
a <- create_envi(fname)
a[condition_1] <- value1
a[condition_2] <- value2
...
close(a)
}
}
\value{
Integer code of ENVI data type. See values of the \dQuote{data type} field in description of the \href{http://www.harrisgeospatial.com/docs/ENVIHeaderFiles.html}{ENVI Header Format}.
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
\code{\link[ursa:conn.create_any]{create_envi}}, \emph{\link[ursa:classRaster.Replace]{Replace}} method \code{\link[ursa:classRaster.Replace]{[<-}} for \code{ursaRaster} object, \code{\link[ursa:classRaster_close]{close_envi}} (\code{\link[ursa:classRaster_close]{close}} for \code{ursaRaster} object).

\code{\link[ursa:conn.write_gdal]{write_gdal}(...,driver="ENVI")} uses GDAL (\pkg{rgdal}) for writting \code{ursaRaster} object to the ENVI .hdr Labelled Raster file.

}
\examples{
session_grid(NULL)
dir.create(tmpWD <- file.path(tempdir(),"certain"))
wd <- setwd(tmpWD)
print(c('temp dir'=session_tempdir(),'working dir'=getwd()))
list1a <- envi_list(session_tempdir())
list1b <- envi_list()
fname <- tempfile(tmpdir=".")
a <- ursa_dummy()
bandname(a) <- c("first","second","third")
write_envi(a)
write_envi(a,fname)
list2a <- envi_list(session_tempdir())
list2b <- envi_list()
fname1 <- list2a[!(list2a \%in\% list1a)]
fname2 <- list2b[!(list2b \%in\% list1b)]
print(c('in temp dir'=fname1,'in working dir'=fname2))
a2 <- open_envi(fname1)
print(a2)
close(a2)
envi_remove(c(fname1,fname2))
setwd(wd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
