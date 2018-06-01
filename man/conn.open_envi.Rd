\name{open_envi}
\alias{open_envi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
open_envi file
%%  ~~function to do ... ~~
}
\description{
\code{open_envi} creates object of \code{ursaRaster} class, reads ENVI header file and prepares \code{\link[base]{connections}} for ENVI binary file
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
open_envi(fname, resetGrid = FALSE, headerOnly = FALSE, decompress = !headerOnly,
          cache = 0L, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
Filename; full-name or short-name
}
  \item{resetGrid}{
Logical. If \code{TRUE} then existing base grid (from \code{\link[ursa:session]{session_grid}}) will be overwritten. Otherwise the spatial subsetting will be attempted.
%%     ~~Describe \code{resetGrid} here~~
}
  \item{headerOnly}{
Logical. If \code{TRUE} then only reading of ENVI header file without creating connection to binary data; there is no necessary to decompress packed binary in this case. Default is \code{FALSE}.
}
  \item{decompress}{
If ENVI binary file is compressed and you have not to use ENVI values then put \code{decompress=FALSE} to avoid useless operation
%%     ~~Describe \code{decompress} here~~
}
  \item{cache}{
Integer. Using cache for compressed files. If \code{0L} then cache is not used. If \code{1L}, then cache is used. Any value, which is differed from \code{0L} and \code{1L}, resets cache. Default is \code{0L}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
If input file does not exists then these additional arguments will be passed to \code{\link[ursa:conn.create_any]{create_envi}} function.
}
}
\details{
\code{open_envi} try to find ENVI files (binary and header) and open them. If unsuccessful then function passes \code{\dots}-arguments to \code{\link[ursa:conn.create_any]{create_envi}} function

%%  ~~ If necessary, more details than the description above ~~
}
\value{
   Returns object of class \code{ursaRaster}. Values from ENVI binary are not in memory yet.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{Nikita Platonov \email{platonov@sevin.ru}}

%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[ursa:classRaster_close]{close}}, \code{\link[ursa:conn.create_any]{create_envi}}
}

\examples{
session_grid(NULL)
a <- pixelsize()
write_envi(a,"example")
a <- open_envi("example")
dir(pattern="^example.*")
ursa_info(a)
close(a)
rm(a)
envi_remove("example")

## additional arguments are enough to create new ENVI file
dir(pattern="^example.*")
a <- open_envi("example",layername=paste0("test",1:3))
ursa_info(a)
dir(pattern="^example.*")
close(a)
envi_remove("example")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
