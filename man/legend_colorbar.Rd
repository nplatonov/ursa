\name{legend_colorbar}
\alias{legend_colorbar}
\alias{.legend_colorbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot colorbar
}
\description{
Functions draw single color bar outside of maps panels. \code{legend_colorbar} (without prefix dot) is a wrapper for non-public \code{.legend_colorbar} (with prefix dot)
}
\usage{
legend_colorbar(...)

## non-public
.legend_colorbar(ct, units = "", labels = NA, align = NULL, shift = 1, cex = 1,
                 adj = NA, las = 1, forceLabel = FALSE, lomar = 0, himar = 0,
                 turn = FALSE, useRaster = NA, trim = 0L, abbrev = 24L,
                 opacity = NA, verbose = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes.  Passed to non-public \code{.legend_colorbar}, excepting argument \code{colorbar}:
   \tabular{lll}{
      \emph{Matched pattern} (\code{legend_colorbar}) \tab \emph{Argument} (\code{.legend_colorbar}) \tab \emph{Description}
      \cr \code{colorbar} \tab \tab Prefix for indirect use (e.g., in \code{\link[ursa:display]{display}}). Separated by a dot \code{"."}, e.g., \code{colorbar.units="mbar"}.
      \cr \code{(ct)*} \tab \code{ct} \tab \emph{See below}.
      \cr \code{unit(s)*} \tab \code{units} \tab \emph{See below}.
      \cr \code{labels} \tab \code{labels} \tab \emph{See below}.
      \cr \code{align} \tab \code{align} \tab \emph{See below}.
      \cr \code{shift} \tab \code{shift} \tab \emph{See below}.
      \cr \code{cex} \tab \code{cex} \tab \emph{See below}.
      \cr \code{adj} \tab \code{adj} \tab \emph{See below}.
      \cr \code{las} \tab \code{las} \tab \emph{See below}.
      \cr \code{forceLabel} \tab \code{forceLabel} \tab \emph{See below}.
      \cr \code{lomar} \tab \code{lomar} \tab \emph{See below}.
      \cr \code{himar} \tab \code{himar} \tab \emph{See below}.
      \cr \code{turn} \tab \code{turn} \tab \emph{See below}.
      \cr \code{useRaster} \tab \code{useRaster} \tab \emph{See below}.
      \cr \code{trim} \tab \code{trim} \tab \emph{See below}.
      \cr \code{abbrev} \tab \code{abbrev} \tab \emph{See below}.
      \cr \code{opacity} \tab \code{opacity} \tab \emph{See below}.
      \cr \code{verb(ose)*} \tab \code{verbose} \tab \emph{See below}.
   }
   }
  \item{ct}{
  \code{ursaRaster} object with color table or object of class \code{ursaColorTable}. First argument in \code{legend_colorbar}; name can be omitted.
  }
  \item{units}{
  Argument of class \code{character} or \code{expression} with matching name \code{"unit(s)*"} in \code{legend_colorbar}. Text, which is used as a caption for color bars. If \code{character} then caption is displayed in bold. Default is \code{""}: no caption.
  }
  \item{labels}{
  Argument of class \code{integer} or \code{character} with matching name \code{"labels"} in \code{legend_colorbar}. If \code{labels} is vector of length 1, then it is number of labels at the color bar, else vector of specified values. Default is \code{NA}: it means 11 labels for numerical values and 31 labels for categorical values, but this number can be reduced for perpendicular orientation to the axes to prevent label overlapping.}
  \item{align}{
  Argument of class \code{numeric} with matching name \code{"align"} in \code{legend_colorbar}. The indent for right alignment of labels. May be useful, if two or more color bars are located on the same side, but with different units and order of values. Can be specified by the string of maximal length or via \code{\link[ursa:legend_align]{legend_align}}. Default is \code{NULL}: right alignment of each color bar is independent.}
  \item{shift}{
  Argument of class \code{numeric} with matching name \code{"shift"} in \code{legend_colorbar}. Multiplier for manual correction of labels alignment in the case when automatical alignment is poor. Default is \code{1}: no changes. If \code{shift<1} then labels are shifted to the left. If \code{shift>1} then labels are shifted to the right.}
  \item{cex}{
  Argument of class \code{numeric} with matching name \code{"cex"} in \code{legend_colorbar}. A numerical value giving the amount by which labels should be magnified relative to the default. Default is \code{1}.}
  \item{adj}{
  Argument of class \code{numeric} with matching name \code{"adj"} in \code{legend_colorbar}. Adjustment for labels. For labels parallel to the axes, \code{adj=0} means left or bottom alignment, and \code{adj=1} means right or top alignment. Default is \code{NA}: for labels parallel to the axes \code{adj=0.5}, for labels perpendicular to the axis \code{adj=1} for numeric and \code{adj=0} for character.}
  \item{las}{
  Argument of values {0, 1, 2, 3} with matching name \code{"adj"} in \code{legend_colorbar}. The correspondence between directions of axis and labels. The same definition as for \code{\link[graphics:par]{par(las=)}}. Default is \code{1L}.}
  \item{forceLabel}{
  Argument of class \code{logical} with matching name \code{"forceLabel"} in \code{legend_colorbar}. If \code{TRUE} then all labels are plotted regardless their possible overlapping.}
  \item{lomar}{
  Argument of class \code{numeric}, non-negative, with matching name \code{"lomar"} in \code{legend_colorbar}.Relative shifting of the lower (left or bottom) position of colorbar. Default is \code{0}: the lower position is corresponded to the limit of panel(s). Positive value decreases length of colorbar.}
  \item{himar}{
  Argument of class \code{numeric}, non-negative, with matching name \code{"himar"} in \code{legend_colorbar}. Relative shifting of the higher (right or bottom) position of colorbar. Default is \code{0}: the higher position is corresponded to the limit of panel(s). Positive value decreases length of colorbar.}
  \item{turn}{
  Argument of class \code{logical} with matching name \code{"turn"} in \code{legend_colorbar}. Default is \code{FALSE}: lower value is on left or bottom, higher value is on right or top. If \code{turn=TRUE}, then opposite order.}
  \item{useRaster}{
  Argument of class \code{logical} with matching name \code{"useRaster"} in \code{legend_colorbar}. Passed as argument \code{useRaster} to function \code{\link[graphics:image]{image}}. Default is \code{NA}, which is interpreted as \code{TRUE} for "cairo" graphical device and as \code{FALSE} for "windows" graphical device (see description of argument \code{type} in \code{\link[grDevices:png]{png}}).}
  \item{trim}{
Argument of values {0L, 1L, 2L} with matching name \code{"trim"} in \code{legend_colorbar}. Determines behaviour for plotting marginal labels. If \code{0L}, then marginal labels are displayed as is. If \code{1L}, then marginal labels are shifted inside of color bar to prevent their outcrop to the panel(s) limits. If \code{2L} then outctopped labels are not displayed.}
  \item{abbrev}{
  Argument of class \code{integer} or \code{logical} with matching name \code{"abbrev"} in \code{legend_colorbar}. \code{TRUE} is interpreted as default value. \code{FALSE} is interpreted as \code{0L}. If positive, then labels are abbreviated, and this argument is passed as argument \code{minlength} to function \code{\link[base:abbreviate]{abbreviate}}: \code{abbreviate(label, minlength=abbrev, strict=TRUE)}. If abbreviation is failed (e.g., non-ASCII symbols), the \code{\link[base:subset]{subset}} is applied. 
  }
  \item{opacity}{
  Argument of class \code{integer} or \code{logical} with matching name \code{"abbrev"} in \code{legend_colorbar}. Responses for shading of color bar. If \code{FALSE} or \code{0}, then no shading. If \code{TRUE} or \code{1}, then shading is forced. Default is \code{NA}; if semi-transparence is detected, then shading is applied.
  }
  \item{verbose}{Argument of class \code{logical} with matching name \code{"verb(ose)*"} in \code{legend_colorbar}. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.
}
}
\details{
If \code{units} are \code{expression}, then possible way for formatting is:\preformatted{
units=as.expression(substitute(bold(degree*C)))
}
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
display(ursa_dummy(1),units="Required 99 labels; displayed less"
        ,colorbar.labels=99,las=3,gridline.trim=FALSE,colorbar.trim=1L)
cname <- c("Apple","Orange","Cherry","Blueberry","Strawberry","Currant")
a <- ursa_dummy(4)
b <- list(colorize(a[1],value=seq(50,200,length=length(cname))
                  ,name=cname)#,stretch="category")
         ,colorize(a[2]*10,ramp=FALSE),colorize(a[3]*100),colorize(a[4]/10))
la <- legend_align(b[3:4])
leg <- vector("list",10)
leg[[1]] <- list(1,"left")
leg[[2]] <- list(1,"right")
for (i in seq(4)) {
   leg[[i+2]] <- list("top",i)
   leg[[i+6]] <- list("bottom",i)
}
compose_open(layout=c(1,4),legend=leg,scale=NA,dev=FALSE) # use 'dev=TRUE' to check layout
compose_panel(b)
legend_colorbar(b[[1]],lomar=20,himar=0) ## "left"
legend_colorbar(b[[4]],labels=c(6,7.5,12,15,20)
               ,units="Manual set of labels") ## "right"
legend_colorbar(b[[1]],las=2,adj=0.5,turn=TRUE,lomar=6,himar=6
               ,units="Central adjustment; inverse order") ## ("top",1)
legend_colorbar(b[[2]],cex=0.9
               ,units="Horizontal labels can be overlapped") ## ("top",2)
legend_colorbar(b[[3]],las=3,align=la
               ,units="Increased width, but aligned -->") ## ("top",3)
legend_colorbar(b[[4]],las=3,align=la,labels=3
               ,units="<-- Reduced width, but aligned") ## ("top",4)
legend_colorbar(b[[1]],las=2,adj=0,shift=0.9,turn=FALSE,lomar=2,himar=10
               ,units="Left adjustement. Non-optimal; shifted") ## ("bottom",1)
legend_colorbar(b[[2]],las=3,adj=0
               ,units="But right adj. is default for numeric") ## ("bottom",2)
legend_colorbar(b[[3]],labels=99,las=3,trim=2L
               ,units="Required 99 labels, but displayed less") ## ("bottom",3)
legend_colorbar('Caption from named item'=b[[4]],labels=99) ## ("bottom",4)
compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
