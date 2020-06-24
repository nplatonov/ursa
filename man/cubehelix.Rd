\name{cubehelix}
\alias{cubehelix}
\title{
Generate "cubehelix" palette.
}
\description{
\code{cubehelix} returns set of RGB colours, which are  screen display of intensity images
}
\usage{
cubehelix(n, value = numeric(), weak = NA, rich = NA, rotate = NA, hue = NA, gamma = 1,
          dark = NA, light = NA, inv = NA, verbose = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Positive integer. Length of returned color vector. If \code{n} is \link[base:missing]{missing} and length of \code{value} is positive, then length of \code{value}. If missing \code{n} and empty \code{value}, then \code{n=256}.
}
  \item{value}{
Numeric vector of values, which are associated with a palette. If both positive and negative values are in this vector, then divergence color palette is returned. Default in numeric of length zero (unspecified).
}
  \item{weak}{
Numeric. The angle (in degrees) of the helix for color with \code{light} intensity. If both \code{rich} and \code{weak} are specified, the \code{rotate} is defined as difference between \code{rich} and \code{weak}. If all \code{weak}, \code{rich} and \code{rotate} are unspecified, then random values are used. Default is \code{NA} (unspecified).
}
  \item{rich}{
Numeric. The angle (in degrees) of the helix for color with \code{dark} intensity. If both \code{rich} and \code{weak} are specified, the \code{rotate} is defined as difference between \code{rich} and \code{weak}. If all \code{weak}, \code{rich} and \code{rotate} are unspecified, then random values are used. Default is \code{NA} (unspecified).
}
  \item{rotate}{
Numeric. The angle of rotation (in degrees) of the helix over the scale; can be negative. If \code{rotate} and \code{weak} are specified, then \code{rich} is defined as sum of \code{weak} and \code{rotate}. If \code{rotate} and \code{rich} are specified, then \code{weak} is defined as difference between \code{rotate} and \code{weak}. If all \code{weak}, \code{rich} and \code{rotate} are unspecified, then random values are used. Default is \code{NA} (unspecified).
}
  \item{hue}{
Non-negative numeric. Saturation of color. \code{hue=0} gives pure greyscale. If unspecified, then random value in interval [0.9, 1.5] is used. Default is \code{NA} (unspecified).
}
  \item{gamma}{
Numeric. Power of intensity. Intensity is between \code{dark} and \code{light}, which are normalized to interval [0, 1]. \code{gamma} changes normalized intensity to intensity\code{^gamma}. Default is 1.
}
  \item{dark}{
Positive numeric in interval between 0 and 255. The intensity of the darkest color in the palette. For light backgrounds default is 63. For dark backgrounds default is 14 (inverse order with \code{light}).
}
  \item{light}{
Positive numeric in interval between 0 and 255. The intensity of the lightest color in the palette. For light backgrounds default is 241, for dark backgrounds default is 192 (inverse order with \code{dark}).
}
  \item{inv}{
Logical. Inversion of color intensity. If \code{TRUE} then color vector is \link[base:rev]{reversed} before return. Default is \code{FALSE}.
}
  \item{verbose}{
Logical. Value \code{TRUE} provides information about cube helix on console. Default is \code{NA}, which is interpreted as \code{FALSE}.
}
}
\details{
This is modified source code of function \code{cubeHelix} from package \pkg{rje} under GPL>=2 license.

The palette design is oriented that figures can be printed on white paper. Under this assumption, light color is for small values, and dark color is for big values. In some computer vision and GIS software black background is used, and in this case light color for big values, and dark color of small values looks more naturally. For some thematic maps big values are light, and small values are small (for example, sea ice concentration: open water is blue, close ice is white). RGB and Grayscale remote sensing and photo imagery use light colors for strong signal, and dark colors for weak signal.

Light background is default for figure (specified by argument \code{background} in function \code{\link[ursa:compose_open]{compose_open}}).
%%~ and for image panels (specified by argument \code{fill} in function \code{\link[ursa:panel_new]{panel_new}}).

The palette divergency can be defined only if \code{value} is specified. If all values are positive, or all values are negative, then returned palette is not drivergent. For divergent palettes the helix sequence is continuous.

If \code{dark} and \code{lihgt} are unspecified, the color contrast bewteen \code{dark} and \code{light} drops on reducing number of colors in returned vector.
}
\value{
Vector of RGB color specification.
}
\references{
\href{http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/}{Dave Green's `cubehelix' colour scheme.}

Green, D. A., 2011, `A colour scheme for the display of astronomical intensity images', Bulletin of the Astronomical Society of India, 39, 289. http://astron-soc.in/bulletin/11June/289392011.pdf \href{https://arxiv.org/pdf/1108.5083.pdf}{(pre-print at 'arxiv.org')}

\pkg{rje} at CRAN: \url{https://CRAN.R-project.org/package=rje}
}
\author{
Dave Green

Robin Evans 

Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

\section{Acknowledgements}{
Dave Green, Robin Evans
}

\seealso{
Original source code \code{\link[rje:cubeHelix]{rje::cubeHelix}} (valid link if package \pkg{rje} is installed), or see CRAN reference.
}
\examples{
session_grid(NULL)
set.seed(352)
session_grid(regrid(mul=1/16))
a <- ursa_dummy(3,min=0,max=255)
b4 <- b3 <- b2 <- b1 <- vector("list",length(a))
for (i in seq_along(b1)) {
   b1[[i]] <- colorize(a[i],pal=cubehelix(11,weak=45*i,rotate=+270),ncolor=11)
   b2[[i]] <- colorize(a[i],pal=cubehelix(11,weak=45*i,rotate=-270),ncolor=11)
   b3[[i]] <- colorize(a[i]-127,pal=cubehelix)
   hue <- sample(seq(2)-1,1)
   s <- ifelse(hue==0,NA,runif(1,min=91,max=223))
   b4[[i]] <- colorize(a[i]-127,pal=cubehelix,pal.hue=hue,pal.dark=s,pal.light=s)
}
display(c(b1,b2),layout=c(2,NA),decor=FALSE)
display(c(b3,b4),layout=c(2,NA),decor=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{color}
