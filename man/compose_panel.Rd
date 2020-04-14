\name{compose_panel}
\alias{compose_panel}
\title{
Plot raster images and decorations on the multipanel layout.
}
\description{
\code{compose_panel} divides the multi-band raster image (\emph{brick}) or layers of raster images (\emph{stack}) on the sequence of single-band images and plots each image on the separate panel of layout. Panel plotting is finalized by adding of decoration (gridlines, coastline, annotation, scalebar).
}
\usage{
compose_panel(..., silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of arguments, which are passed to \code{\link[ursa:panel_new]{panel_new}}, \code{\link[ursa:panel_raster]{panel_raster}}, \code{\link[ursa:panel_coastline]{panel_coastline}}, \code{\link[ursa:panel_graticule]{panel_graticule}}, \code{\link[ursa:panel_annotation]{panel_annotation}}, \code{\link[ursa:panel_scalebar]{panel_scalebar}}.
   }
     \item{silent}{Logical. Value \code{TRUE} cancels progress bar. Default is \code{FALSE}.
   }
}
\details{
For each panel of layout the sequence of called functions is permanent:\cr\code{\link[ursa:panel_new]{panel_new}} - - > \code{\link[ursa:panel_raster]{panel_raster}} - - > \code{\link[ursa:panel_coastline]{panel_coastline}} - - > \code{\link[ursa:panel_graticule]{panel_graticule}} - - > \code{\link[ursa:panel_annotation]{panel_annotation}} - -> \code{\link[ursa:panel_scalebar]{panel_scalebar}}.

If this order is undesirable, then call these functions in the required sequence.
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

\seealso{
\code{\link[ursa:compose_plot]{compose_plot}}, \code{\link[ursa:compose_legend]{compose_legend}}

\code{\link[ursa:panel_new]{panel_new}}, \code{\link[ursa:panel_raster]{panel_raster}}, \code{\link[ursa:panel_coastline]{panel_coastline}}, \code{\link[ursa:panel_graticule]{panel_graticule}}, \code{\link[ursa:panel_annotation]{panel_annotation}}, \code{\link[ursa:panel_scalebar]{panel_scalebar}}
}
\examples{
session_grid(NULL)
a <- ursa_dummy(6)
b1 <- list(maxi=a[1:4]*1e2,mini=a[5:6]/1e2)
print(b1)
b2 <- lapply(b1,function(x) colorize(x,nbreak=ifelse(global_mean(x)<100,5,NA)))
compose_open(b2,byrow=FALSE
            ,legend=list(list("bottom",1:2),list("bottom",3),list("left")))
ct <- compose_panel(b2,scalebar=2,coastline=3:4,gridline=5:6,gridline.margin=5
                   ,annotation.text=as.character(seq(6)))
compose_legend(ct)
legend_mtext(as.expression(substitute(italic("Colorbars are on the bottom"))))
compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
