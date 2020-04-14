\name{display_stack}
\alias{display_stack}
\alias{display_hetero}
\title{
Plot multi-band heterogenous raster images in the PNG format.
}
\description{
Raster images are forced to be interpreted as heterogenuous (having the different units). It implies creating multi-panel layout with multiple colorbars.
}
\usage{
display_stack(obj, ...)
display_hetero(obj, ...)
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
         \item \code{\link[ursa]{compose_panel}}
         \itemize{
            \item \code{\link[ursa]{panel_new}}
            \item \code{\link[ursa]{panel_raster}}
            \item \code{\link[ursa]{panel_decor}}
            \itemize{
               \item \code{\link[ursa]{panel_graticule}}
               \item \code{\link[ursa]{panel_coastline}}
               \item \code{\link[ursa]{panel_scalebar}}
            }
            \item \code{\link[ursa]{panel_annotation}}
         }
         \item \code{\link[ursa]{compose_legend}}
         \itemize{
            \item \code{\link[ursa]{legend_colorbar}}
         }
      }
   \item \code{\link[ursa]{compose_close}}
}
}
}
\details{
If argument \code{obj} is object of class \code{ursaRaster} then \code{obj} is coerced to \code{\link[base]{list}} of \code{ursaRaster} objects ('brick' is coerced to 'stack').

The plot layout is either two-columns or two-rows. Extent of coordinate grid has a form of rectangle. The layout selection depends on ratio of rectangle's sides. For single-column design use parameter \code{layout=c(NA,1L)}. e.g., \code{display_brick(a,layout=c(NA,1))}, for single-row design use parameter \code{layout=c(1,NA)}. The same is for forcing of two-columns (\code{layout=c(NA,2L)}) and two-rows layouts (\code{layout=c(2L,NA)}). Other layouts are not applicable for multiple colorbars.

\code{display_hetero} is a synonym to \code{display_stack}. It is introduced to emphasize the plotting of complex object with heterogeneous elements, for example, having different units.

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
\code{\link[ursa]{display}}, \code{\link[ursa]{display_brick}}, \code{\link[ursa]{display_rgb}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=3)
display_stack(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
