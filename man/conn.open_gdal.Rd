\name{open_gdal}
\alias{open_gdal}
\alias{ursa_open}
\title{
Open GDAL file
}
\description{
\code{open_gdal} creates object of \code{ursaRaster} class, and prepares \code{\link[base]{connections}} for data reading.
}
\usage{
open_gdal(fname, verbose = FALSE)

ursa_open(fname, verbose = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
Character. Filename; full-name or short-name.
}
  \item{verbose}{
Logical. \code{verbose=TRUE} provides some additional information on console. Default is \code{FALSE}.
}
}
\details{
\code{ursa_open} is a synonym to \code{open_gdal}. \emph{Generally, both function names are abridged version of \verb{ursa_open_dgal}}.

\code{open_gdal} doesn't read data. Data can be read later using \emph{\link[ursa:classRaster.Extract]{Extract}} operator \code{\link[ursa:classRaster.Extract]{[}}.

If argument \code{fname} is \href{http://www.gdal.org/frmt_various.html#ENVI}{ENVI .hdr Labelled Raster} then either \code{open_gdal} or \code{\link[ursa:conn.open_envi]{open_envi}} can be used. The former provides external implementation for data reading via GDAL in \pkg{rgdal} package.
}
\value{
   Returns object of class \code{ursaRaster}. Values are not in memory.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link[ursa:classRaster_close]{close}}, \code{\link[ursa:conn.open_envi]{open_envi}}, \code{\link[ursa:conn.read_gdal]{read_gdal}}.
}

\examples{
session_grid(NULL)
fname1 <- system.file("pictures/cea.tif",package="rgdal")
message(fname1)
a1 <- open_gdal(fname1)
print(a1)
print(a1[])
close(a1)
fname2 <- system.file("pictures/test_envi_class.envi",package="rgdal")
message(fname2)
b1 <- open_gdal(fname2)
b2 <- open_envi(fname2)
print(b1)
print(b2)
print(c('The same grid?'=identical(ursa_grid(b1),ursa_grid(b2))
       ,'The same data?'=identical(ursa_value(b1[]),ursa_value(b2[]))))
close(b1,b2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
