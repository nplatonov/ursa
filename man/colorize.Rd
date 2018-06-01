\name{colorize}
\alias{colorize}
\title{
Create color table
}
\description{
\code{colorize} assigns color table to raster image.
}
\usage{
colorize(obj, value = NULL, breakvalue = NULL, name = NULL, pal = NULL, inv = NA,
         stretch = c("default", "linear", "equal", "mean", "positive",
                     "negative", "diff", "category", "julian", "date", "time",
                     "slope", "conc", "sd", "significance", "bathy",
                     "grayscale", "greyscale", ".onetoone"),
         minvalue = NA, maxvalue = NA, byvalue = NA, ltail = NA, rtail = NA, tail = NA,
         ncolor = NA, nbreak = NA, interval = 0L, ramp = TRUE, byte = FALSE,
         lazyload = FALSE, reset = FALSE, origin = "1970-01-01" ,format = "",
         alpha = "", colortable = NULL, verbose = FALSE, ...)
                          

}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{obj}{
      \code{ursaRaster} object or one-dimension numeric vector.
   }
   \item{value}{
      Numeric. Values to be assigned to categories.
   }
   \item{breakvalue}{
      Numeric. Values to be assigned to intervals.
   }
   \item{name}{
      Character. Names of categories.
   }
   %%~   \item{col}{
   %%~ Character. Colors of categories in the color table.
   %%~ }
   \item{pal}{
      Function or character. If function then value should corresponded to function, which creates a vector of colors. If character then values should correponded to \R color names or hexadecimal string of the form "#RRGGBB" or "#RRGGBBAA".
   }
   %%~   \item{palname}{
   %%~ \strong{Deprecated}. Character. A palette name. Keywords are from \bold{\code{\link[RColorBrewer:ColorBrewer]{RColorBrewer}}} package.
   %%~ }
   \item{inv}{
      Logical. Invert sequence of colors.
   }
   \item{stretch}{
      Character. Either kind of value transformatin (\code{"linear", "equal"}) or pre-defined options with palette specification (\code{"positive", "data", "significance"}, etc)
   }
   \item{minvalue}{
      Numeric. Lower range limit.
   }
   \item{maxvalue}{
      Numeric. Upper range limit.
   }
   \item{byvalue}{
      Numeric. Increment of the sequence from \code{minvalue} to \code{maxvalue}.
   }
   \item{ltail}{
      Numeric. Partition of omitted values at left tail.
   }
   \item{rtail}{
      Numeric. Partition of omitted values at right tail.
   }
   \item{tail}{
      Numeric. Partition of omitted values at both tail. If length of \code{tail} is 2 then left and right tails may differ.
   }
   \item{ncolor}{
      Numeric or interer. Number of desired colors (or categories)
   }
   \item{nbreak}{
      Numeric or interer. Number of desired separators between colors.
   }
   %~   \item{breaks}{
   %~ Numeric or interer. Number of desired color breaks for intervals.
   %~ }
   \item{interval}{
      Integer. How to underwrite categories? Use direct
   }
   \item{ramp}{
      Logical. Is color ramp required?
   }
   \item{byte}{
      Logical. Forcing to produce color table for storage in byte format (not more than 255 colors). Default is \code{FALSE}.
   }
   \item{lazyload}{
      Logical. If \code{FALSE} then raster is reclassified to categories. If \code{TRUE} then color table is created without any change to source raster. Default is \code{FALSE}.
   }
   \item{reset}{
      Logical. If \code{TRUE} and source raster has color table, then this color table is destroyed, and new one is created. Default is \code{FALSE}.
   }
   \item{origin}{
      Character. Origin for \code{stretch="date"} (passed to function \code{\link[base:as.Date]{as.Date}}) and \code{stretch="time"} (passed to function \code{\link[base:as.POSIXlt]{as.POSIXct}}). See desription of \code{orogin} in respective functions. Default is "1970-01-01".
   }
   \item{format}{
      Character. Format date/time objects for arguments \code{stretch} with values "date", "time", or "julian". Default is "" (character of length 0).
   }
   \item{alpha}{
      Character or numeric. The characteristics of transparency. If character, then hexadecimal values between "00" and "FF" are allowed, and then coerced to numeric value between 0 and 255. If numeric, and \verb{0 <= alpha <= 1}, then \code{alpha} is multiplied to 255. \code{alpha=0} means full transparency,  \code{alpha=255} means full opacity. Default is ""; if palette has no alpha channel, then \code{alpha} is assing to \code{"FF"}.
   }
   \item{colortable}{
      Object of class \code{ursaColorTable} or object of class \code{ursaRaster} with color table. Reference color table. Is specified, then all other arguments are ignored, expexted \code{lazyload}. Default is \code{NULL} (unspecified).
   }
   \item{verbose}{
      Logical. Some output in console. Primarily for debug purposes.
   }
   \item{\dots}{
      If \code{pal} is a function, and argrument names are in the format "\code{pal.*}" then prefix "\code{pal.}" is omitted, and the rest part is used for argument names, which are passed to \code{pal} function.
   }
}
\details{
\code{colortable} is designed to prepare pretty thematic maps.

%%~ Currently \code{categorize} is synonym of \code{colorize}. It is a mock-up to make categories without color table.

Color rampimg (\code{ramp=TRUE}) is not quick in computatons and has no effective labelling. It is intoduced to visualize non-thematic maps, and it is assumed that labeling can be omitted for such maps.

The labelling implementation is based on some improvements of \code{\link[base]{pretty}} function. The notation of intervals is mixed by brackets and comparative symbols, for example: \code{"<=1.5","(1.5,2.5]","(2.5,3.5]",">3.5"}

Reserved values for \code{interval}:
\itemize{
\item{\code{0L} or \code{FALSE} - no interlavs. Values are interpreted as category, even if they are in non-nominal scale}
\item{\code{1L} or \code{TRUE} - each category corresponds to interval. The low limit of lowest category is \code{-Inf}. The high limit of highest category is \code{+Inf}}
\item{\code{2L} - different implementation of \code{interval=1}. In some cases may relult more pretty labeling.
}

If \code{breaks} is numerical vector and \code{colors} has zero length, then it is assumed interal scaling, and \code{interval=1L} is assigned to unspecified \code{interval}


Finite values of extreme intervals are neccessary sometimes, however this option is not implemented currently
}

Keywords for \code{stretch} to create pre-defined color tables:
\itemize{
\item{\code{"positive"} - lower limit is 0. Palette is "Oranges"}
\item{\code{"negative"} - higher limit is 0. Palette is "Purples"}
\item{\code{"grayscale", "greyscale"} - palette is "Greys". Usually used for raw satellite images.}
\item{\code{"mean"} - designed for common thematic maps and for averaged map across set of maps. Palette is "Spectral"}
\item{\code{"sd"} - designed for spatial mapping of standard deviation across set of maps. Palette is "YlGnBu"}
\item{\code{"diff"} - diverge palette "RdBu". Absolute values of lower and upper limits are equal, zero is in the middle of palette. Designed for anomaly maps.}
\item{\code{"slope"} - is similar to \code{diff} but without extreme colors, which are reserved for contouring of statistically significant areas.}
\item{\code{"significance"} - desiged to illustrate statistically significant areas of slope.
The realisation is \code{colortable(obj, value=c(-0.999, -0.99, -0.95, -0.9, -0.5, +0.5, +0.9, +0.95, +0.99, +0.999), interval=1L, palname="RdBu")}}
\item{\code{"category"} - Values are interpreted in nominal scale. Palette is based on random colors from "Pairs" palette.}
\item{\code{"conc"} - designed for visualization of sea ice concentration data, which have lower limit 0 and higher limit 100. Palette is "Blues"}
\item{\code{"bathy"} - designed for ocean depth (bathymetry) maps. Internally \code{colorize(obj, stretch="equal",interval=1L, palname="Blues", inv=TRUE)} is used to detect the crossing from shelf waters to deep water basin. Better practice is to do second step with manual specification of \code{value} argument.}
\item{\code{"internal"} - continuous colors, designed for conversion to greyscale with keeping of intensities.}
\item{\code{"default"} - allowing to detect stretch by intuition, without any strong mathematical criteria}
}
It is allowed manual correction of labels using followed code example:
\code{names(ursa_colortable(x)) <- c("a<=0","0<a<=1","a>1")}
}
\value{
Object of class \code{ursaRaster} with named character vector of item \code{$colortable}
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
\code{\link[ursa:classColorTable]{ursa_colortable}}, 
\code{\link[ursa:classColorTable]{ursa_colortable<-}}
}
\examples{
session_grid(NULL)
a <- pixelsize()-350
print(a)
b1 <- colorize(a,ramp=FALSE)
print(ursa_colortable(b1))
b2 <- colorize(a,interval=1,stretch="positive",ramp=FALSE)
print(ursa_colortable(b2))
b3 <- colorize(a,interval=2,stretch="positive",ramp=FALSE)
print(ursa_colortable(b3))
b4 <- colorize(a,value=c(150,250),interval=1)
print(ursa_colortable(b4))
names(ursa_colortable(b4)) <- c("x<=150","150<x<=250","x>250")
print(ursa_colortable(b4))
display(b4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{color}
