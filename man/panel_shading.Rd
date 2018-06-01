\name{panel_shading}
\alias{panel_shading}
\title{
Shaded overlay by image mask
}
\description{
This specific function is designed to illustrate linear slope and areas of statistically significant slope on the same panel, however can be used commonly for shading by raster mask.
}
\usage{
panel_shading(obj, level = NA, col = NULL, density = 25, angle = c( -45, 45),
                  lwd = 1, lty = 1, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}. 
}
  \item{level}{
Positive numeric. Threshold for \code{obj} reclassification \code{\{ obj<(-level) | obj>(+level) \}}. If \code{NULL} then mask is created from non-\code{NA} values of \code{obj}. Default is \code{NULL}.
}
  \item{col}{
\code{ursaColorTable} (\code{ursaRaster} with color table) or \code{character}. Color for shading lines (grid). If object of class \code{ursaColorTable}.\cr
Two colors on the limits of color vector are extracted to separate source values \code{<=(-level)} and \code{>=(+level)}.
}
  \item{density}{
Numeric. The density of shading lines, in lines per inch. Default is \code{25}. See description of argument \code{density} in function \code{\link[graphics:polygon]{polygon}}.
}
  \item{angle}{
Numeric. The slope of shading lines, given as an angle in degrees (counter-clockwise). Default is vector of length two \code{c(-45,45)}. See description of argument \code{angle} in \code{\link[graphics:polygon]{polygon}} function.
}
  \item{lwd}{
Numeric. Line width for shading. Default is \code{1}. See description of \code{lwd} in \code{\link[graphics:par]{par}} function
}
  \item{lty}{
Numeric or character. Line type for shading. Default is \code{1}. See description of \code{lty} in \code{\link[graphics:par]{par}} function.
}
  \item{verbose}{
Logical. If \code{TRUE} then progresss bar is appeared. Default is \code{TRUE}.
}
}
\details{
Values of input \code{obj} is reclassified to raster mask: \{ values\code{<=(-level)} OR values\code{>=(+level)} \}. For common use, select appropriate \code{level} and, if necessary, reclassify \code{obj} prior.

Color limits are extracted using \code{\link[base:range]{range}} function.

Raster images can be used for colored shading using \code{alpha} argument of \code{\link[ursa:panel_raster]{panel_raster}} function, e.g. \code{panel_raster(a,alpha=3/4)}
}
\value{
\code{NULL}
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
if (first.example <- TRUE) {
   session_grid(NULL)
   session_grid(regrid(mul=1/8))
   ps <- pixelsize()
   compose_open()
   ct <- compose_panel()
   panel_shading(ps>1.1*global_mean(ps),angle=90)
   compose_legend(ct)
   compose_close()
}
if (second.example <- TRUE) {
   session_grid(NULL)
   a <- ursa_dummy(nband=15,mul=1/8)
   b <- local_stat(a)
   compose_open()
   lev <- 0.90
   d <- as.matrix(b["slopeS"],coords=TRUE)
   e <- contourLines(d,levels=c(-lev,lev))
   p <- list(significance.raw=colorize(b["slopeS"])
            ,significance.formatted=colorize(b["slopeS"],stretch="significance")
            ,slope=colorize(b["slope"]))
   p <- c(p,rep(p[3],3))
   names(p)[c(3,4,5)] <- c("Slope and shaded significance"
                          ,"Slope and contoured significance"
                          ,"Slope and 'contourLines'")
   compose_open(p,layout=c(2,NA),byrow=FALSE)
   compose_panel(p[1])
   compose_panel(p[2])
   compose_panel(p[3])
   panel_shading(b["slopeS"],level=lev)
   compose_panel(p[4])
   panel_contour(b["slopeS"],value=c(-lev,lev))
   compose_panel(p[5])
   lapply(e,panel_polygon)
   compose_panel(p[6])
   ct <- panel_contour(b["slopeS"],"color"
                      ,value=c(-0.99,-0.95,-0.9,-0.5,0.5,0.9,0.95,0.99))
   compose_legend(c(head(p,-1),'(Colorbar for contours)'=list(ct)),las=3)
   compose_close()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
