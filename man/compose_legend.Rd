\name{compose_legend}
\alias{compose_legend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot colorbars or marginal texts.
}
\description{
\code{compose_legend} recognizes color tables and characters among arguments and passes them to suitable functions for plotting on margins outside of panel area.
}
\usage{
compose_legend(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
If first argument is a list, then either \code{ursaColorTable} or \code{\link[base:character]{character}} objects are detected in this list. \code{ursaColorTable} can be extracted from \code{ursaRaster} (if presents). Other objects are coerced to character.

If first argument is \code{ursaColorTable} or \code{ursaRaster} with color tables, then other arguments are interpreted as color tables. If coercion to color table is impossible, the coersion is to \code{character}.

\code{\link[ursa:legend_colorbar]{legend_colorbar}} is called for objects of class \code{ursaColorTable}.
\code{\link[ursa:legend_mtext]{legend_mtext}} is called for objects of class \code{ursaColorTable}. If first argument is a list, then other arguments are passed to respective function calls.
}
}
\details{
Named list in the first argument is allowed or named vectors are allowed if first argument is not a list. For \code{\link[ursa:legend_colorbar]{legend_colorbar}} name of object can be used as an argument \code{units}.

This function is designed to make plot on moderate level of usage with the followed construction:\preformatted{
compose_open(...)
compose_panel(...)
compose_legend(...)
compose_close(...)
}
Function \code{\link[ursa:compose_panel]{compose_panel}} returns list of color tables of plotted rasters, and followed sequence is available:\preformatted{
ct <- compose_panel(a)
compose_legend(ct) # or, if 'a' has color tables, then 'compose_legend(a)'
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

\seealso{\code{\link[ursa:legend_colorbar]{legend_colorbar}}

\code{\link[ursa:legend_mtext]{legend_mtext}}

}
\examples{
session_grid(NULL)
b <- lapply(as.list(ursa_dummy(2)),colorize)
cd <- compose_design(layout=c(1,2),legend=list(list(1,"left"),list(1,"right")
                                              ,list("top","full"),list("bottom",1)))
for (i in 1:4) {
   compose_open(cd,dev=i==1)
   ct <- compose_panel(b,decor=FALSE)
   if (i==2)
      compose_legend(ct)
   else if (i==3)
      compose_legend(ct[[1]],'Tomorrow'=b[[2]]
                    ,top="This is example of legend composition"
                    ,format(Sys.Date(),"(c) \%Y"))
   else if (i==4)
      compose_legend(c(ct,"top","bottom"),units=c("left","right"))
   compose_close()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
