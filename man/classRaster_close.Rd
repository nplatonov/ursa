\name{close}
\alias{close.ursaRaster}
\alias{close_envi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Close connections for files with data
}
\description{
\code{close()} for \code{ursaRaster} object closes connection for opened file using inheritted function \code{\link[base:connections]{base::close}}. Function \code{close_envi()} closes opened connection for ENVI binary file. 
%~ \code{close_envi()} (or quite \code{close()} for \code{imageRaster} object) closes connection for opened ENVI binary file using inheritted function.
}
\usage{
\method{close}{ursaRaster}(...)

close_envi(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Object or sequence of objects of class \code{ursaRaster}.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
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
\code{\link[base:connections]{close}} of \pkg{base} package
}
\examples{
session_grid(NULL)
a <- create_envi()
fname <- a$con$fname
message(paste("Created file",dQuote(basename(fname)),"will be deleted."))
print(dir(pattern=basename(envi_list(fname))))
close(a)
invisible(envi_remove(fname))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
