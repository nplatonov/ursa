\name{as.integer}
\alias{as.integer}
\alias{as.integer.ursaRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform values to type integer 
}
\description{
\code{as.integer} for object of class \code{ursaRaster} truncates decimal part of image values and then converts to type \code{\link[base]{integer}}.
}
\usage{
\method{as.integer}{ursaRaster}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object
}
  \item{\dots}{
Other arguments which passed to function \code{\link[base:integer]{as.integer}} of package \pkg{base}.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
Object of class \code{ursaRaster} where \code{\link[base:mode]{storage.mode}} of values is \code{integer}.
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
a <- pixelsize()
a <- a-min(a)+0.5
str(ursa_value(a))
print(storage.mode(a$value))
b <- as.integer(a)
str(ursa_value(b))
print(storage.mode(b$value))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
