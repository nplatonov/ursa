\name{compose_plot}
\alias{compose_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot layout of images and color bars.
}
\description{
\code{compose_plot} plots images (raster brick or raster stack) and corresponding color bars according to given rectangular layout.
}
\usage{
compose_plot(\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of arguments, which are passed to \code{\link[ursa:compose_panel]{compose_panel}} and \code{\link[ursa:compose_panel]{compose_legend}}
   }
}
\details{
Function merges to functions. The first one plots image layout and returns list of color tables. The second one plots legend (colorbars) based on returned color tables. Simplified description is:\preformatted{
ct <- compose_panel(...)
compose_legend(ct,...)
}
These two functions are separated to allow use additional plotting on image panel after primary plot of raster and decorations before panel change or legend plot.
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

\seealso{
\code{\link[ursa:compose_panel]{compose_panel}}\cr
\code{\link[ursa:compose_panel]{compose_legend}}
}
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=6,min=0,max=255,mul=1/4)
if (example1 <- TRUE) {
   b1 <- ursa_brick(a)
  # b1 <- colorize(b1,stretch="positive",ramp=FALSE)
   compose_open(b1)
   compose_plot(b1,grid=FALSE,coast=FALSE,scale=FALSE,trim=1
               ,stretch="positive",ramp=!FALSE)
   compose_close()
}
if (example2 <- TRUE) {
   b2 <- ursa_stack(a)
   compose_open(b2)
   compose_plot(b2,grid=FALSE,coast=FALSE,labels=5,trim=2,las=0)
   compose_close()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
