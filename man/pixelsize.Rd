\name{pixelsize}
\alias{pixelsize}
\title{
The actual size of each grid cell with considerable distortion in area of map projection.
}
\description{
This function helps to calculate size of pixels in the unit of area (squared km) for zonal statistics with taking into account distortion in area for classes of projections.
}
\usage{
pixelsize(obj, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Either \code{ursaRaster} object or \code{ursaGrid} object or \code{NULL} or \code{\link[base]{missing}}.
}
  \item{verbose}{
Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.
}
}
\details{
\code{pixelsize()} is applied to coordinate reference system (grid) of \code{ursaRaster} object or to \link[ursa:classGrid]{raster grid} directly. If argument \code{obj} is \link[base:missing]{missed}, then \link[ursa:session]{session grid} is used.

Currently, only \emph{Stereographic} (\code{"+stere"} in PROJ.4 notation), \emph{Mercator} (\code{"+merc"}), and \emph{Lambert Azimuthal Equal Area} (\code{"+laea"}) classes of map projections are implemented, though the last one (LAEA) has no distortion in area.
}
\value{
Object of class \code{ursaRaster}, single-band. If size of cell is more than \code{10e5} square meters, then the unit is squared kilometers (band name is "Pixel Size (sq.km)") else squared meters (band name is "Pixel Size (sq.m)").

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
pixelsize()

\dontrun{
dpath <- file.path("ftp://sidads.colorado.edu/pub/DATASETS"
                  ,"nsidc0081_nrt_nasateam_seaice/F18_uncalibrated/north")
dst <- tempfile(fileext=".bin")
isOK <- FALSE
d3 <- Sys.Date()
for (i in seq(5)) {
   src <- file.path(dpath,format(d3,"nt_\%Y\%m\%d_f18_nrt_n.bin"))
   a <- try(download.file(src,dst,mode="wb"))
   if ((is.integer(a))&&(a==0)) {
      isOK <- TRUE
      break
   }
   d3 <- d3-1
}
if (isOK) {
   g1 <- regrid(bbox=c(-385,-535,375,585)*1e4,res=25*1e3
               ,proj4=paste("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45"
                           ,"+k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449"
                           ,"+units=m +no_defs"))
   session_grid(g1)
   b <- readBin(dst,integer(),size=1L,n=136492L,signed=FALSE)
   ice <- ursa_new(value=tail(b,-300))
   ice[ice>251] <- NA ## keep Pole
   ice[ice==251] <- 250 ## consider 100\% ice at Pole
   ice <- ice/2.5 ## uncategorize
   ice[ice<15] <- 0 ## not ice, if less 15\%
   extent1 <- band_sum(ice*1e-2*with(g1,resx*resy*1e-6))*1e-6
   extent2 <- band_sum(ice*1e-2*pixelsize(ice))*1e-6
   message(paste("Near real-time Arctic sea ice extent(NASA Team algorithm, NSIDC)"))
   message(sprintf("   Direct area calculation:         \%5.2f*1e6 km^2.",extent1))
   message(sprintf("   Distortion in area is corrected: \%5.2f*1e6 km^2.",extent2))
}
else
   message("It is failed to get sea ice concentration data.")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
