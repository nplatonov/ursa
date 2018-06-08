\name{panel_graticule}
\alias{panel_graticule}
\alias{compose_graticule}
\alias{.panel_graticule}
\alias{.compose_graticule}
\title{
Add latitude/longitude or metric grid to the image panel.
}
\description{
\code{panel_graticule} puts a grid on the panel with raster image. If CRS is georeferenced then grid is generated from longitudes and latitudes.
}
\usage{
panel_graticule(...)

compose_graticule(...)

# non-public
.compose_graticule(panel = 0L, col = "grey70", border = "grey70", lon = NA, lat = NA,
                lwd = 0.5, lty = 2, marginalia = rep(FALSE, 4), trim = FALSE,
                language = NA_character_, cex = 0.75, verbose = FALSE)

# non-public
.panel_graticule(obj, marginalia = rep(TRUE, 4), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes:
      \tabular{lll}{
          \emph{Matched pattern} \tab \emph{Used argument} \tab \emph{Description}
          \cr\code{(graticule|grid(line)*)} \tab \code{   } \tab Logical. Responsible for should grid lines be displayed or not. If \code{TRUE} then grid lines are plotted. If \code{FALSE} then grid lines are not displayed. Default is \code{TRUE}.
          \cr \code{^(graticule|grid(line)*)$} \tab \code{panel} \tab \emph{See below}.
          \cr \code{(graticule|grid(line)\\.)*col} \tab \code{col} \tab \emph{See below}. Default value in \code{compose_graticule} depends on panel background color, \code{"#0000002F"} for ligth panel background, and \code{"#FFFFFF4F"} for dark panel background.
          \cr \code{(graticule|grid(line)\\.)*border} \tab \code{border} \tab \emph{See below}. Default value in \code{compose_graticule} depends on figure background color, \code{"#0000002F"} for ligth figure background, and \code{"#FFFFFF4F"} for dark figure background.
          \cr \code{(graticule|grid(line)\\.)*lon} \tab \code{lon} \tab \emph{See below}.
          \cr \code{(graticule|grid(line)\\.)*lat} \tab \code{lat} \tab \emph{See below}.
          \cr \code{(graticule|grid(line)\\.)*lwd} \tab \code{lwd} \tab \emph{See below}.
          \cr \code{(graticule|grid(line)\\.)*lty} \tab \code{lty} \tab \emph{See below}.
          \cr \code{(decor|margin(alia)*)} \tab \code{marginalia} \tab \emph{See below}. \code{.compose_graticule} and \code{.panel_graticule} don't validate correctness of this parameter.
          \cr \code{((graticule|grid(line)\\.)*trim} \tab \code{trim} \tab \emph{See below}.
          \cr \code{(graticule|grid(line)\\.)*language} \tab \code{language} \tab \emph{See below}.
          \cr \code{(graticule|grid(line)\\.)*verb(ose)*} \tab \code{verbose} \tab \emph{See below}.
      }
   }
%%~    \item{\code{grid(line)*}}{Logical. Responsible for should grid lines be displayed or not. If \code{TRUE} then grid lines are plotted. If \code{FALSE} then grid lines are not displayed. Default is \code{TRUE}.}
   \item{obj}{Objects of the class \code{ursaGridLine}. It is internal structure, which is returned by function \code{.compose_graticule}.}
   \item{panel}{Integer vector. Panel for which coastline will be displayed. \code{0L} means that coastline will bw displayed for all panels. Default is \code{0L}.}
   \item{col}{Character. Color code/name for grid lines. Default is \verb{"grey70"}.}
   \item{border}{Character. Color code/name for marginal labels and ticks. Default is \verb{"grey70"}. }
   \item{lon}{Numeric vector. Set of longitudes for grid. If \code{NA} then set of logitudes is formed internally. Default is \code{NA}.}
   \item{lat}{Numeric vector. Set of latitudes for grid. If \code{NA} then set of latidudes is formed internally. Default is \code{NA}.}
   \item{lwd}{Positive numeric. Width of grid line. Default is \code{0.5}. See \code{lwd} in \code{\link[graphics]{par}}.}
   \item{lty}{Positive integer. Type (pattern) of grid line. Default is \code{2} (dashed). See \code{lty} in \code{\link[graphics]{par}}.}
   \item{marginalia}{Logical or integer vectors. Responsible for whether longitudes and latitudes (or metric coordinates) be labelled on the frame of panel with raster image. If logical and \code{TRUE}, then labels will be displatyed on each open side of panel. If logical and \code{FALSE} then labels will not be displayed. If logical of length 4, then labels will be displayed on specific side, where side index is \code{c(bottom, left, top, right)} (see description for marginal parameters in \code{\link[graphics]{par}}). If argument is a vector of positive integers, then labels for grid lines are plotted only for the specified panels, which sequence is defined in \code{\link[ursa]{compose_design}} function and returend from \code{getOption("ursaPngLayout")$layout}. Default is \code{c(TRUE,TRUE,TRUE,TRUE)}.}
   \item{language}{Character. Language for longitude and latitude captions. If \code{"ru"} then captions are in Russian else in English. Default is \code{NA}.}
   \item{trim}{Logical. If grid lines are labelled then \code{trim=TRUE} prevents crossing the labels on neighbor perpendicular sides.}
   \item{cex}{Positive numeric. The relative font size for grid lines' labels. Make sence in the case of labels plotting. Default is \code{0.75}.}
   \item{verbose}{Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.}
}
\details{
If not \code{language="ru"} but environmental variable \code{LANGUAGE=ru} then labels are in Russian (cyrillics).

Argument \code{gridline} (or, \code{grid}) is introduced for unconditional calling of \code{panel_graticule} inside of high-level functions.

Grid lines can be controlled in high-level plot functions (\emph{e.g.}, \code{\link[ursa]{display}}, \code{\link[ursa]{compose_plot}}, \code{\link[ursa]{display_stack}}, \code{\link[ursa]{display_brick}}, \code{\link[ursa]{display_rgb}}, \emph{etc}.). To prevent displaying grid lines, use argument \code{gridline=FALSE} (or \code{grid=FALSE}). To display grid lines, use argument \code{gridline=TRUE} (or \code{grid=TRUE}) and prefix \code{grid(line)*} (\code{gridline.*} or \code{grid.*}) for grid lines' parametes, \emph{e.g.}, \code{gridline.verb=TRUE}, \code{grid.col="black"}. If prefix is omitted then agruments with the same names affect in other functions in the part of high-level function.

If grid lines are formed internally, then desirable number of lines for each direction is 3. The design of line density is based on intuition, providing pretty labelling.

If CRS is georeferenced then grid lines are corresponded to longitudes and latitudes. Integer minutes are used to illustate fractional values of degrees. If precision of minutes is insufficient, then integer values of seconds are introduced. The fractional values of seconds are not used.

Labels are located at the points, where grid lines cross plot margin. Labels are not overlapped along the same side. To prevent overlapping along the same side, labels are shifted or omitted. Argument \code{trim=TRUE} prevents overlapping labels from neighbor sides via hidding.
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
\donttest{ ## Changing of environmental variables is out of CRAN Policy 
\dontrun{Sys.setenv(LANGUAGE="ru")}
}

# example no.1
cl <- compose_design(layout=c(2,2),legend=NULL)
session_grid(regrid(lim=3.2*1e6*c(-1,-1,1,1)))
compose_open(cl)
for (i in 1:4) {
   panel_new()
   panel_coastline()
   panel_graticule(decor=TRUE,trim=i \%in\% c(2:4))
   panel_annotation(text=as.character(i))
   panel_scalebar(scalebar=i==3)
}
compose_close()

# example no.2
session_grid(regrid(lim=1e6*c(-0.5,0.5,1.5,2.5)))
compose_open(layout=c(2,2),legend=NULL,skip=4)
for (i in seq(getOption("ursaPngLayout")$image)) {
   panel_new()
   panel_coastline()
   if (i==1)
      panel_graticule()
   else if (i==2)
      panel_graticule(decor=TRUE,lon=seq(0,360,by=40)[-1],lat=seq(-90,90,by=10))
   else if (i==3)
      panel_graticule(decor=TRUE,lon=seq(0,360,by=20)[-1],lat=seq(-90,90,by=5)
                     ,trim=TRUE)
   else if (i==4)
      panel_graticule(gridline=FALSE)
   panel_scalebar(scalebar=1)
   panel_annotation(text=as.character(i))
}
compose_close()

# example no.3 -- indirect usage
session_grid(NULL)
display(pixelsize(),decor=TRUE,grid.col="green3",coast.col="darkgreen",side=2)

\donttest{ ## Changing of environmental variables is out of CRAN Policy 
\dontrun{Sys.setenv(LANGUAGE="") # reset environmental variable}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
