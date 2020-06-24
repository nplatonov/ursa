\name{panel_new}
\alias{panel_new}
\alias{.panel_new}
\title{
Start plotting on the new image panel
}
\description{
\code{panel_new} finishes plotting on previuos image panel and starts plotting on next image panel.
}
\usage{
panel_new(...)

# non-public
.panel_new(col = "chessboard", density = NA, angle = NA, lwd = 1, lty = 1,
           asp = NA, mar = rep(0, 4), verbose = FALSE)

}
%%~ ## coerced to 'panel_new(fill = "grey80", density = NA, angle = NA, lwd = 1, lty = 1L, asp = 1, mar = rep(0, 4))'
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of arguments, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes. Passed to non-public \code{.panel_new}. Optional prefix \code{"blank"} is used for indirect use. Separated by a dot \code{"."}, e.g., \code{blank.fill="transparent"}.
      \tabular{lll}{
         \strong{Pattern} (\code{panel_new}) \tab \strong{Argument} (\code{.panel_new}) \tab \strong{Description}
%%~          \cr \code{blank} \tab \code{} \tab \var{See below.}
         \cr \code{(blank\\\\.)*(^$|bg|fill)} \tab \code{col} \tab \var{See below.} Keyword \code{"chessboard"} is used by default to produce original background texture. However argument \code{col} has other default value.
         \cr \code{(blank\\\\.)*density} \tab \code{density} \tab \var{See below.}
         \cr \code{(blank\\\\.)*angle} \tab \code{angle} \tab \var{See below.}
         \cr \code{(blank\\\\.)*lwd} \tab \code{lwd} \tab \var{See below.}
         \cr \code{(blank\\\\.)*lty} \tab \code{lty} \tab \var{See below.}
         \cr \code{(blank\\\\.)*asp} \tab \code{asp} \tab \var{See below.}
         \cr \code{(blank\\\\.)*mar} \tab \code{mar} \tab \var{See below.}
         \cr \code{(blank\\\\.)*verb(ose)*} \tab \code{verbose} \tab \var{See below.}
         
      }
   }
%%~    \item{\code{blank}}{Prefix for indirect use. Separated by a dot \code{"."}, e.g., \code{blank.col="transparent"}.}
   \item{col}{Character. Color code/name for panel filling/shadowing. Default is \code{"grey80"} for georeferenced images, and \code{"grey90"} for non-projected images.}
   \item{density}{Numeric. The density of shading lines for fill/shadowing. If \code{NA} then no shading lines are drawn. Default is \code{NA}. See \code{density} in \code{\link[graphics]{rect}}.}
   \item{angle}{Numeric. The slope of shading lines, given as an angle in degrees (counter-clockwise). If \code{NA} then no shading lines are drawn. Default is \code{NA}. See \code{angle} in \code{\link[graphics]{rect}}.}
   \item{lwd}{Positive numeric. Width of coastline. Default is \code{1}. See \code{lwd} in \code{\link[graphics]{rect}}.}
   \item{lty}{Character or positive integer. Type (pattern) of coastline. Default is \code{1L} (solid). See \code{lty} in \code{\link[graphics]{rect}}.}
   \item{asp}{Positive numeric. The \emph{y/x} aspect ration. Default is \code{1}. See \code{asp} in \code{\link[graphics]{plot.window}}.}
   \item{mar}{Positive numeric of length 4. Plot margins. Default is \code{rep(0,4L)}. See \code{mar} in \code{\link[graphics]{par}}.}
   \item{verbose}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
}
\details{
Prefix \code{blank} is introduced for manipulations with \code{panel_new} inside of high-level functions (e.g., \code{\link[ursa]{display}}). Prefix skipping is the subject for confict with functions, which use the same name of arguments.

It is required to call \code{panel_new} for every image panel. First calling starts plotting on the first panel. Second and next callings change image panels.

The panel sequence is set in function \code{\link[ursa]{compose_design}}, which is called directly or indirectly from \code{\link[ursa]{compose_open}}, and keeps in the options (access via \code{getOption("ursaPngLayout")$layout}).

Image background is formed via consecutive call of functions \code{\link[graphics:plot.default]{plot}(...,type="n")}, and \code{\link[graphics]{rect}(...)}.
%%  ~~ If necessary, more details than the description above ~~
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

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
# example no.1 -- direct use
compose_open(layout=c(1,2),legend=NULL)
panel_new()
panel_annotation(label="Default + Empty")
panel_new(col="#0000FF3F",density=15,angle=45,lwd=3)
panel_decor()
panel_annotation(label="Settings + Grid")
compose_close()

# example no.2 -- indirect use
a <- pixelsize()
a <- a[a>560]
display(a,blank.col="#0000FF3F",blank.density=15,blank.angle=45,blank.lwd=3
       ,coast.fill="#007F005F",coast.density=20,coast.angle=c(-30,60))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
