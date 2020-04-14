\name{display_brick}
\alias{display_brick}
\alias{display_homo}
\title{
Plot multi-band homogenous raster image in the PNG format.
}
\description{
Raster image is forced to be interpreted as homogenuous (having the same units). It implies creating multi-panel layout with multiple colorbars. 
}
\usage{
display_brick(obj, ...)
display_homo(obj, ...)
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
               \item \code{\link[ursa]{panel_annotation}}
            }
         \item \code{\link[ursa]{compose_legend}}
      }
   \item \code{\link[ursa]{compose_close}}
}
}
}
\details{
If argument \code{obj} is \code{\link[base]{list}} of \code{ursaRaster} objects (or object of class \code{ursaStack}) then \code{obj} is coerced to class \code{ursaRaster} ('stack' is coerced to 'brick').

\code{display_homo} is a synonym to \code{display_brick}. It is introduced to emphasize the plotting of homogenous object.
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
\code{\link[ursa]{display}}, \code{\link[ursa]{display_stack}}, \code{\link[ursa]{display_rgb}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=3,min=0,max=250)
a[2] <- -a[1]
a[3] <- sqrt(a[1])
a2 <- ursa_stack(a) 
print(a2)
display(a2) # likely 'display_stack' will be called
display_brick(a2,stretch="eq",labels=c(-150,-100,0,10,12,20,100,150))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
