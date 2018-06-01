\name{blank}
\alias{band_blank}
%%~ \alias{which_blank}
\alias{ursa_blank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Does any band contain no information?
}
\description{
Set of functions for checking is any or all bands have no data, and for retrieving indices for non-data bands.
}
\usage{
band_blank(obj, verbose = FALSE)
%%~ which_blank(obj)
ursa_blank(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}
}
  \item{verbose}{
Logical. Value \code{TRUE} provides progress bar. Default is \code{FALSE}.
}
}
\details{
It is defined locally that if all values of band are \code{NA} or 0, then such band is blank. The fact is \code{\link[ursa]{ursa_new}} create new object in memory with default values \code{NA}, but \code{\link[ursa:conn.create_any]{create_envi}} writes zeros to disk quick. It is decided to consider both these cases as blank. Function \code{band_blank} checks blanks for each band of image. If all bands are blank then function \code{ursa_blank} returns \code{TRUE}.
%%~ Function \code{which.blank} gives band numbers of blank bands.
}
\value{
Function \code{ursa_blank} returns logical value of length 1.

Function \code{band_blank} returns logical value of length \code{\link[ursa:classRaster_length]{nband(obj)}}.

%%~ Function \code{which.blank} returns integer value of any length from 1 to \code{\link[ursa:classRaster_length]{nband(obj)}}.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link[ursa:classRaster_is.na]{is.na}} returns object of class \code{ursaRaster}; it is mask of cells, which have \code{NA} value.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
session_grid(NULL)
a <- ursa_new(bandname=c("first","second","third","fourth"))
ursa_value(a,"first") <- 0 ## 'a[1] <- 1' works, but it is slow
print(ursa_blank(a))
a[3] <- pixelsize()
a[4] <- a[3]>625
print(a)
print(band_blank(a))
print(which(band_blank(a)))
print(ursa_blank(a))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
