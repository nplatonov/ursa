\name{display}
\alias{display}
\title{
Plot raster image(s) in the PNG format.
}
\description{
High-level function to create multi-panel layout of images and to display with decoration (gridlines, coastlines, scalebar, colorbars) in the PNG format. It is an aggregator of low-level functions for typical plotting.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
display(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster} or \code{\link[base]{list}} of \code{ursaRaster} objects.
}
  \item{\dots}{
Passed to either \code{\link[ursa]{display_brick}} or \code{\link[ursa]{display_stack}} or \code{\link[ursa]{display_rgb}} functions and further to hierarchy of plotting functions:
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
               \item \code{\link[ursa]{panel_annotation}}
            }
         \item \code{\link[ursa]{compose_legend}}
      }
   \item \code{\link[ursa]{compose_close}}
}
}
}
\details{
If argument \code{obj} is missing (e.g, calling \code{display()} without parameters) then plotting the sessional CRS with blank image.

If argument \code{obj} is \code{\link[base]{list}} of \code{ursaRaster} objects (or object of class \code{ursaStack}) then \code{\link[ursa]{display_stack}} is called.

If argument \code{obj} is object of class \code{ursaRaster} and has 3 or 4 bands and values in each band are \code{\link[base]{integer}} and in interval between 0 and 255, then \code{\link[ursa]{display_rgb}} is called.

If argument \code{obj} is object of class \code{ursaRaster} then firstly internal test is applied to detect either image's bands contains homogeneous information (raster brick) or heterogeneous information (raster stack). Then either \code{\link[ursa]{display_brick}} or \code{\link[ursa]{display_stack}} is called. This test is rough due to unknown data origin. It is supposed to adjust kind of plotting by means of direct specification of \code{\link[ursa]{display_brick}} or \code{\link[ursa]{display_stack}}.
}
\value{
%%~ Function returns \code{NULL} value.

Returned value from either \code{\link[ursa]{display_brick}} or \code{\link[ursa]{display_stack}} or \code{\link[ursa]{display_rgb}} functions.
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
\code{\link[ursa]{display_brick}}, \code{\link[ursa]{display_stack}}, \code{\link[ursa]{display_rgb}}

\R-styled plotting:  \code{\link[ursa:classRaster_plot]{plot}}, \code{\link[ursa:classRaster_plot]{image}}
}
\examples{
session_grid(NULL)
set.seed(500)
a.brick <- a.stack <- ursa_dummy(nband=3,min=0,max=255,mul=1/16)
a.stack[2] <- a.stack[2]/10
a.stack[3] <- sqrt(a.stack[3])
a.rgb <- as.integer(round(a.brick))
print(a.brick)
print(a.stack)
print(a.rgb)
display(a.brick,decor=FALSE)
display(a.stack,decor=FALSE)
display(a.rgb)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
