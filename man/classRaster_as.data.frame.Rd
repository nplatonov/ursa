\name{as.data.frame}
\alias{as.data.frame.ursaRaster}
\alias{.as.data.frame}
%%~ \alias{as_data_frame.ursaRaster}
\title{
Convert raster image to a data frame
}
\description{
\code{as.data.frame} reorganizes \code{ursaRaster} object into data frame, where first two columns (\code{x} and \code{y}) are coordinates of cells, and the rest columns are cell values.
}
\usage{
\method{as.data.frame}{ursaRaster}(x, ...)
%%~ \method{as_data_frame}{ursaRaster}(obj, band = FALSE, id = FALSE, na.rm = TRUE, all.na = FALSE)

# non-public
.as.data.frame(obj, band = FALSE, id = FALSE, na.rm = TRUE, all.na = FALSE,
               col.names = NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, obj}{
Object of class \code{ursaRaster}
}
  \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes. Passed to non-public \code{.as.data.frame}.
      \tabular{lll}{
         \strong{Pattern} (\code{as.data.frame}) \tab \strong{Argument} (\code{.as.data.frame}) \tab \strong{Description}
         \cr \code{band} \tab \code{band} \tab \emph{See below}.
         \cr \code{id} \tab \code{id} \tab \emph{See below}.
         \cr \code{na\\\\.rm} \tab \code{na.rm} \tab \emph{See below}.
         \cr \code{all\\\\.na} \tab \code{all.na} \tab \emph{See below}.
         \cr \code{col(\\\\.)*name(s)*} \tab \code{col.names} \tab \emph{See below}.
      }
}
  \item{band}{
Logical. If \code{band=FALSE} then each band is presented by separate column in the data frame. If \code{band=TRUE} then band name is presented as a \code{\link[base]{factor}} in the column \code{$band}, and values are written in the column \code{$z}. If \code{band=TRUE} then number of rows is 
}
  \item{id}{
Logical. If \code{band=FALSE} then is ignored. If \code{id=TRUE} then addiditional columns \code{$id} will contain unique cell number in the source raster.
}
  \item{na.rm}{
Logical. If \code{na.rm=FALSE} then number of rows for data frame is equal to number of cells of spatial grid of raster. If \code{na.rm=TRUE} then cells with 'no data' values for all (\code{all.na=FALSE}) or any (\code{all.na=TRUE}) bands are omitted.
}
  \item{all.na}{
Logical. If \code{na.rm=FALSE} then ignored. If number of rows for data frame is equal to number of cells of spatial grid of raster. If \code{na.rm=TRUE} then cells with 'no data' values for all bands are omitted.
}
  \item{col.names}{
Character vector or \code{NULL}. Names for columns of data frame. If \code{NULL}, then column names are generated from band names. Default is \code{NULL}.
}
}
\details{
The structure of voxel is kept. The number of rows for \code{band=TRUE} is equal to the number of rows for \code{band=FALSE} multiplied to number of bands. To extract all numeric data with destroying of voxel, you may use followed code:\cr\command{subset(as.data.frame(obj,band=TRUE),!is.na(z))}.

%%~ If package \pkg{tibble} is installed then \code{as_data_frame()} converts \code{ursaRaster} object to the \code{\link[tibble:as_tibble]{as_data_frame}} object.
}
\value{
Data frame.

If \code{band=TRUE} then
\item{x}{Horizontal coordinate of cell's midpoint}
\item{y}{Vertical coordinate of cell's midpoint}
\item{z}{Value}
\item{band}{Band as a \code{\link[base]{factor}}}
\item{id}{\emph{Optional}. Unique number for \code{(x,y)} coordinate.}
If \code{band=FALSE} then
\item{x}{Horizontal coordinate of cell's midpoint}
\item{y}{Vertical coordinate of cell's midpoint}
\item{...}{Additional columns. Names of columns are names of bands. Values of columns are values of corresponded bands.}

If \code{ursaRaster} is projected, then data frame has additional attribute \code{attr(...,"proj")} with value of PROJ.4 string.
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
session_grid(regrid(res=50000,lim=c(-1200100,-1400800,1600900,1800200)))
a0 <- ursa_dummy(nband=3,min=0,max=100)
a0[a0<30 | a0>70] <- NA
names(a0) <- c("x","y","z")
print(a0)
b0 <- as.data.frame(a0)
session_grid(NULL)
a1 <- as.ursa(b0)
print(a1-a0)
session_grid(NULL)
session_grid(regrid(res=5800000))
set.seed(352)
a2 <- as.integer(ursa_dummy(nband=2,min=0,max=100))
a2[a2>50] <- NA
print(a2)
print(b1 <- as.data.frame(a2,na.rm=FALSE))
print(b2 <- as.data.frame(a2,na.rm=TRUE))
print(b3 <- as.data.frame(a2,all.na=TRUE))
print(b4 <- as.data.frame(a2,band=TRUE,na.rm=FALSE))
print(b5 <- as.data.frame(a2,band=TRUE,all.na=FALSE))
print(b6 <- as.data.frame(a2,band=TRUE,all.na=TRUE))
print(b7 <- as.data.frame(a2,band=TRUE,all.na=TRUE,id=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
