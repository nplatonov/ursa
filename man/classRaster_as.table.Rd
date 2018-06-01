\name{as.table}
\alias{as.table}
\alias{as.table.ursaRaster}
\alias{ursa_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequency of unique values
}
\description{
\code{as.table} is an implementation of function \code{\link[base:table]{base::table}} for values of raster image.
}
\usage{
\method{as.table}{ursaRaster}(x, ...)

ursa_table(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object.
}
  \item{\dots}{
Other arguments which passed to function \code{\link[base]{table}} of package \pkg{base}.
}
}
\details{
If \code{ursaRaster} has a \code{\link[ursa:classColorTable]{colortable}}, then values are replaced by names of categories.

\code{ursa_table} is synonym to method \code{as.table} for class \code{`ursaRaster`}.
}
\value{
Object of class \code{\link[base]{table}}.
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
a <- colorize(pixelsize(),nbreak=4)
t1 <- as.table(a)
print(t1)
str(t1)
ursa_colortable(a) <- NULL
t2 <- as.table(a)
print(t2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
