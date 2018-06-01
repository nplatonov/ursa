\name{display_rgb}
\alias{display_rgb}
\title{
Plot RGB (RGBA) color composition in the PNG format.
}
\description{
Raster images are forced to be interpeted as color composition with 3 (RGB) or 4 (RGBA) channels. Values should be in the range between 0 and 255.
}
\usage{
display_rgb(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster} or \code{\link[base]{list}} of \code{ursaRaster} objects.
}
  \item{\dots}{
Passed to hierarchy of plotting functions:
\itemize{
   \item \code{\link[ursa]{compose_open}}
      \itemize{
         \item \code{\link[ursa]{compose_design}}
      }
   \item \code{\link[ursa]{compose_plot}}
      \itemize{
         \item \code{\link[ursa]{panel_new}}
         \item \code{\link[ursa]{panel_raster}}
         \item \code{\link[ursa]{panel_decor}}
            \itemize{
               \item \code{\link[ursa]{panel_graticule}}
               \item \code{\link[ursa]{panel_coastline}}
               \item \code{\link[ursa]{panel_scalebar}}
            }
      }
   \item \code{\link[ursa]{compose_close}}
}
}
}
\details{
If argument \code{obj} is \code{\link[base]{list}} of \code{ursaRaster} objects (or object of class \code{ursaStack}) then \code{obj} is coerced to class \code{ursaRaster} ('stack' is coerced to 'brick').

Colorbar is not plotted.

By default, the created PNG has 24 bits per pixel. This is equal to parameter \code{bpp=24} (\code{\link[ursa]{compose_close}}). It is allow to specify other value, e.g., \code{display_rgb(a,bpp=8)}.

By default, labels of gridlines are located in bottom and left sides of the panel with raster. This is equal to parameter \code{margin=c(TRUE,TRUE,FALSE,FALSE)} (\code{\link[ursa]{panel_graticule}}). It is allow to specify other value, e.g., \code{display_rgb(a,margin=T)}.

Currently, for color compositions the argument \code{useRaster} (\code{\link[ursa]{panel_raster}}) is introduced to fix possible coordinate mismatch for Cairo-devices, but have never used.
}
\value{
Function returns \code{NULL} value.
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
\code{\link[ursa]{display}}, \code{\link[ursa]{display_brick}}, \code{\link[ursa]{display_stack}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=3)
display_rgb(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
