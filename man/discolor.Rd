\name{discolor}
\alias{discolor}
\title{
Destroy color table for raster images.
}
\description{
If raster's categories are integer or numeric, then raster values are restored from names of categories. Otherwise only category names are dropped.
}
\usage{
discolor(obj, nodata = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}
}
  \item{nodata}{
Numeric. Flag value for "no-data". If \code{NA}, then no-data values are missed. Default is \code{NA}.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
Object of class \code{ursaRaster} without color table.
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
a <- colorize(pixelsize(),ncolor=7)
print(ursa_colortable(a))
print(a)
b <- discolor(a)
print(ursa_colortable(b))
print(b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{color}
