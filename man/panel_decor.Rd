\name{panel_decor}
\alias{panel_decor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add auxiliary elements to the plotting panel.
}
\description{
\code{panel_decor} adds over plot sequentially the followed elements: coastline, gridline, scalebar. Unlike \code{panel_decor}, function \code{layout.grid} does not add scalebar.
}
\usage{
panel_decor(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Passed to sequence of plotting functions:
\itemize{
   \item \code{\link[ursa]{panel_graticule}}. To distinguish the same argument names in different functions it is provided to use prefix \dQuote{\code{grid.*}}, e.g., \code{grid.col="grey40"}.
   \item \code{\link[ursa]{panel_coastline}}. To distinguish the same argument names in different functions it is provided to use prefix \dQuote{\code{coast.*}}, e.g., \code{coast.col="grey60"}.
   \item \emph{(not applicable for \code{layout.grid})} \code{\link[ursa]{panel_scalebar}}. To distinguish the same argument names in different functions it is provided to use prefix \dQuote{\code{scalebar.*}}, e.g., \code{scalebar.col="black"}.
   }
}
}
\details{
The sequence of elements is constant. To change order, use direct calling of \code{\link[ursa]{panel_graticule}}, \code{\link[ursa]{panel_coastline}}, \code{\link[ursa]{panel_scalebar}} in any sequence.

Sometimes, for rasters with \code{NA} values the followed sequence may be used: \preformatted{
panel_coastline(col="transparent",fill="grey80")
panel_raster(a)
panel_coastline(col="grey40")}
}
\value{
\code{panel_decor} returns \code{NULL} value.\cr
\code{layout.grid} returns \code{NULL} value.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sebin.ru}
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
a <- ursa_dummy(nband=1,min=0,max=100)
a[a<30] <- NA
compose_open()
panel_new()
ct <- panel_raster(a)
panel_decor(graticule.col="green4",graticule.lwd=2,scalebar.col="brown")
compose_legend(ct)
compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
