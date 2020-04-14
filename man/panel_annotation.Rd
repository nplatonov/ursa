\name{panel_annotation}
\alias{panel_annotation}
\alias{.panel_annotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add label or annotation to the image panel.
}
\description{
\code{panel_annotation} puts an annotation (text label) on the panel with raster image without anchors to any layer. Can be used as captions to image panels.
}
\usage{
panel_annotation(...)

# non-public
.panel_annotation(label = expression(), position = "bottomright",
                  lon = NA, lat = NA, x = NA, y = NA, cex = 1.0, adjust = 0.5,
                  fg = "#000000", bg = "#FFFFFF1F", buffer = 1, fill = "#FFFFFF7F",
                  font = par("family"), vertical = FALSE, alpha = 1,
                  interpolate = FALSE, resample = FALSE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
   \tabular{lll}{
      \emph{Matched pattern} (\code{panel_annotation}) \tab \emph{Argument} (\code{.panel_annotation}) \tab \emph{Description}
      \cr \code{((caption|ann(otation)*)} \tab \tab Logical or integer. Responsible for should annotation be displayed or not. If logical and \code{TRUE} then annotations are plotted on all panel of layout. If logical and \code{FALSE} then annotation is not displayed. If argument is a vector of positive integers, then annotations are plotted only in the specified panels, which sequence is defined in \code{\link[ursa]{compose_design}} function and returned from \code{getOption("ursaPngLayout")$layout}. Default is \code{TRUE}.
      \cr \code{(label|text)} \tab \code{label} \tab \emph{See below}.
      \cr \code{pos(ition)*} \tab \code{pos} \tab \emph{See below}.
      \cr \code{lon(gitude)*} \tab \code{lon} \tab \emph{See below}.
      \cr \code{lat(itude)*} \tab \code{lat} \tab \emph{See below}.
      \cr \code{x$} \tab \code{x} \tab \emph{See below}.
      \cr \code{y$} \tab \code{y} \tab \emph{See below}.
      \cr \code{cex} \tab \code{cex} \tab \emph{See below}.
      \cr \code{adj(ust)*} \tab \code{adjust} \tab \emph{See below}.
      \cr \code{fg} \tab \code{fg} \tab \emph{See below}.
      \cr \code{bg} \tab \code{bg} \tab \emph{See below}.
      \cr \code{buf(fer)*} \tab \code{buffer} \tab \emph{See below}.
      \cr \code{fill} \tab \code{fill} \tab \emph{See below}.
      \cr \code{font} \tab \code{font} \tab \emph{See below}.
      \cr \code{vert(ical)*} \tab \code{vertical} \tab \emph{See below}.
      \cr \code{(alpha|transp(aren(cy)*)*)} \tab \code{alpha} \tab \emph{See below}.
      \cr \code{interp(olate)*} \tab \code{interpolate} \tab \emph{See below}.
      \cr \code{resample} \tab \code{resample} \tab \emph{See below}.
      \cr \code{verb(ose)*} \tab \code{verbose} \tab \emph{See below}.
      }
   }
   \item{label}{Character, \link[grDevices:plotmath]{expression}, or objects of classes \code{array} or \code{matrix}. Text, symbols or logo for displaying on image panel. Multi-row characters are allowed with delimiter \code{"\\n"}. Default is \code{expression()}.}
   \item{position}{Character keyword or numeric of length 2 in the interval [0,1]. Defines the location of scale bar. If character, then one of the "bottomleft", "bottomright", "topleft", "topright", "left", "right", "bottom", "top", or "center". If numeric then relative position on panel is defined using shift on horizontal and vertical axes from origin in the bottom-left corner. Default is \code{"bottomright"}.}
   \item{lon}{Numeric. Longitude for center of annotation's position. Default is \code{NA}.}
   \item{lat}{Numeric. Latitude for center of annotation's position. Default is \code{NA}.}
   \item{x}{Numeric. The horizontal coordinate of the annotation's position in the units of image grid. Default is \code{NA}.}
   \item{y}{Numeric. The vertical coordinate of the annotation's position in the units of image grid. Default is \code{NA}.}
   \item{cex}{Positive numeric. The relative font size for annotation's label. Default is \code{1}. See description of argument \code{cex} in \link[graphics]{text} function.}
   \item{adjust}{One or two values in [0, 1]. Specifies the horizontal (and optionally vertical) adjustment of the labels. See description of argument \code{adj} in \link[graphics]{text} function.}
   \item{fg}{Character. Color name or code for label (texts and symbols). Default is \code{"#000000"} (black). }
   \item{bg}{Character. Color name or code for thin buffer around label's elements. Default is \code{NA}, which is interpreted as \code{"transparent"} for \emph{captions} and as \code{"#FFFFFF1F"} for \emph{annotations}.}
   \item{buffer}{Numeric. The relative width of buffer around label's elements. Default is \code{1}.}
   \item{fill}{Character. Color name or code for circumscribed rectangle around labels. Default is \code{NA}, which is interpreted as \code{"#FFFFFF7F"} for \emph{captions} and as \code{"transparent"} for \emph{annotations}.}
   \item{font}{Character. Font family. Default is \code{par("family")}.}
   \item{vertical}{Logical. Vertical orientation of label. If \code{FALSE}, then horizontal labeling. Default is \code{FALSE}.}
   \item{interpolate}{Logical. Passed as argument \code{interpolate} to function \code{\link[graphics:rasterImage]{rasterImage}} for logo annotation.}
   \item{resample}{Logical or numeric. Passed as argument \code{resample} to function \code{\link[ursa]{regrid}} for logo annotation. Default is \code{FALSE}. If \code{TRUE} , then resized logo is drawn smoothly.}
   \item{alpha}{Numeric or character. Level of transparency for logos. If numeric, the either \verb{0 <= alpha <= 1} or \code{0 <= alpha <= 255}. If character, then one byte of hexadecimal value \verb{"00" <= alpha <= "FF"}. Default is \code{1}.}
   \item{verbose}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
}
\details{
This function is based on function \link[graphics]{text} with adding some decoration elements. For low-level plotting use \link[ursa:panel_plot]{layout.text} function, which is equal to function \link[graphics]{text} with additional control of image panels.

Since the most of character keywords of \code{position} have relation to the boundary of image panel, such annotation is assigned as a \emph{caption} for image panel. Default decoration is shadowed background rectangle, which is implemented by function \link[graphics]{rect}.

If location is defined by two-dimensional vector (either relative position inside of image boundaries (\code{pos} is numeric of length two), or pair \code{lon}, \code{lat}, or pair \code{x}, \code{y}), then such labeling is assigned as an \emph{annotation}. Default decoration is thin buffer around symbols. The implementaion is via application of function function \link[graphics]{text} for small diplacements around original position.

The priority of arguments (from higher to lower): 1) pair \code{lon}, \code{lat}, 2) pair \code{x}, \code{y}, 3) two-dimensional numeric of \code{pos}, 4) character keyword of \code{pos}. However, the default annotation is interpreted as a caption.
}
\value{
This function returns \code{NULL} value.
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
## exam no.1 -- direct use
compose_open(layout=c(2,3),legend=NULL,device="cairo")
for (i in seq(6)) {
   panel_new()
   panel_annotation(label=LETTERS,cex=1.5)
   panel_annotation(pos=c(0.7,0.2)
                    ,label=paste("panel",paste("no.",i),sep="\n"))
   if (i==1)
      panel_annotation(pos="center")
}
compose_close()

## exam no.2 -- indirect use
display(pixelsize(),scale=2
       ,ann.label="FJL",ann.lon=52,ann.lat=80,ann.buffer=1
       ,ann.bg="#8F6FFF2F",ann.fill="#FFFF7F9F",ann.font="courier")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
