\name{ursa_new}
\alias{ursa_new}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create raster image in memory
}
\description{
\code{ursa_new} creates object of class \code{ursaRaster} in memory using \link[ursa:session]{session grid parameters} or properties of input object (\code{\link[base]{matrix}} or \code{\link[base]{array}}). By option, \link[ursa:classRaster_names]{band names} and \link[ursa:ignorevalue]{ignore values} are specified.
}
\usage{
ursa_new(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
      \describe{
         \item{\code{value}}{Pattern is \code{"(|^value)"}. Admissible classes are (\code{\link[base]{matrix}}, \code{\link[base]{array}},\code{\link[base]{numeric}},\code{\link[base]{logical}}. Values to fill image. Array or matrix defines raster grid. If \code{value=FALSE} (logical), then created raster image has no values. By default, \code{value=NA}, the created raster image is filled by blank values (\code{NA}).}
         \item{\code{nband}}{Positive integer. Number of bands. Default is \code{1L}.}
         \item{\code{bandname}}{Character. Band names. Default is \code{NULL}. If specified, then \code{nband} is ignored, and the number of bands is equal to length of \code{bandname} character vector.}
         \item{\code{ignorevalue}}{Integer or numeric. Value in ENVI binary file, which is iterpetted as \code{NA} in \R.}
         \item{\code{datatype}}{Positive integer \code{c(1L, 2L, 3L, 4L, 5L, 11L, 12L, 13L)} or character. Data type (integer, floating-point) and byte length. See details for argument \code{datatype} of function \code{\link[ursa:conn.create_any]{create_envi}}. Required for writting raster to ENVI binart file. Optional for rasters in memory. Default is \code{NA}: data type is defined internally.}
         \item{\code{colortable}}{Object of class \code{\link[ursa:classColorTable]{ursaColorTable}}. Color table for raster. Default is \code{NULL}: color table is absent}
         \item{\code{permute}}{Logical. Should dimensions of input matrix be changed. Default is \code{FALSE}.}
         \item{\code{flip}}{Logical. Vertical flip for input matrix. Default is \code{FALSE}: no flip.}
         \item{\code{crs}}{Character or object of class \code{ursaGrid}. The reference grid for raster's cells. Default is \code{NULL}: the grid is defined ether from matrix/array structure or from \link[ursa:session]{sessional parameters}.}
         \item{\code{verb(ose)*}}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
      }
   }
}
\details{
\code{ursa_new} creates \code{ursaRaster} object in memory. To manipulate with raster chunks use the followed construction: \preformatted{
a <- create_envi(fname,...)
a[condition_1] <- value
print(a[condition_2]
...
close(a)
}

\code{ursa_new} is designed to create blank raster images. Use \code{\link[ursa:ursa_as]{as.ursa}} for conversion \R objects to \code{ursaRaster}.
}
\value{
Object of class \code{ursaRaster}.
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
\code{\link[ursa:ursa_as]{as.ursa}}, \code{\link[ursa:conn.create_any]{create_envi}}.
}
\examples{
session_grid(NULL)
a1 <- ursa_new(volcano)
print(a1)
display(a1)

a2 <- ursa_new(volcano,flip=TRUE)
print(a2)
display(a2)

a3 <- ursa_new(volcano,permute=TRUE)
print(a3)
display(a3)

dima <- c(200,300,4)
b1 <- as.ursa(array(runif(prod(dima)),dim=dima))
print(b1)
display_brick(b1,scale=1,palname="Greys",decor=FALSE)

session_grid(NULL)

c1 <- ursa_new(seq(3))
print(c1)
c2 <- ursa_new(seq(3),bands=3)
print(c2)

c3 <- ursa_new(value=FALSE)
str(ursa_value(c3))

c4 <- ursa_new(bands=2,nodata=-99L)
print(c4)
print(ignorevalue(c4))

c5 <- ursa_new(bandname=format(Sys.Date()+seq(7)-1,"\%A"))
ursa_value(c5) <- rev(seq(nband(c5)))
c5 <- colorize(c5)
ct <- ursa_colortable(c5)
print(c5)

v <- ursa_value(c5[3:5])
str(v)
v <- c(v)
str(v)
c6 <- ursa_new(v,colortable=ct)
print(c6)
print(ursa_colortable(c6))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
