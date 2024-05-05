\name{Replace}
\alias{Replace}
\alias{Assign}
\alias{[<-}
\alias{[<-.ursaRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
assign values to the portion of raster images
}
\description{
This operator is used to set or replace values in portion of bands or lines in \code{ursaRaster} object in memory or data writing to file.
}
\usage{
\method{[}{ursaRaster}(x, i, j, ...) <- value
%~ x[j, i, ...] <- value
%~ [<-.ursaRaster(x, j, i, ..., value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object
}
  \item{i}{
Integer or character. If integer, then band index, specifying bands to replace. If character, either list of band names or character sting for \code{\link[base:grep]{regular expression}} to match band index. In the \emph{(spatial, temporal)} interpretation of \code{ursaRaster} object \code{j} points to \emph{temporal} component.
}
  \item{j}{
Mentioned for consistence with internal generic function \code{\link[base:Extract]{[<-}}.
}
  \item{\dots}{
Mentioned for consistence with internal generic function \code{\link[base:Extract]{[<-}}.
\cr
Use \code{regexp=FALSE} for matching by \code{\link[base]{match}}, and \code{regexp=TRUE} for matching by Perl-compatible regexps case insensitive \code{\link[base:grep]{grep}}. Default is \code{FALSE}.
}
  \item{value}{
\code{ursaRaster} object or numeric (scalar, \code{\link[base]{matrix}}, \code{\link[base]{array}}). The latter is coerced to internal matrix of \code{$value} item of \code{ursaRaster} object.
}
}
\details{
Operator \verb{\sQuote{[<-}} is high-level implementation for data writing. If \code{x$value} item is not applicable, then value of \code{ursaRaster} is not in memory. In this case the controlled by \code{i} and \code{j} portion is written to file. If both \code{i} and \code{j} are missing, then \code{x[] <- value} writes values to file wholly.

It is not implemented the simultaneously writing to file portion of bands and portion of lines.

Files (currently, ENVI Binary) are opened for reading and writing.
}
\value{
If values of \code{ursaRaster} object are in memory, then modified \code{ursaRaster} object with replaced bands or lines.
\cr
If values of \code{ursaRaster} object are not applicable, then \code{ursaRaster} object \emph{as is}.
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
\code{\link[ursa:classRaster.Extract]{Extract}}
}
\examples{
session_grid(NULL)
## Prepare
session_grid(regrid(mul=1/4))
a <- pixelsize()
w <- c("first","second","third","fourth","fifth","sixth")
b1 <- rep(a/mean(a),length(w))+seq(length(w))-1
bandname(b1) <- w
nr <- ursa_rows(b1)
bottom <- (as.integer(nr/2)):nr
write_envi(b1,"tmp1",compress=FALSE,interleave="bil")
b2 <- b1
print(b1)

## Replace
b2[1] <- 10+b1["second"]
b2[2] <- 20
try({
   data(volcano)
   b2[3] <- 30+volcano
}) ## error: unable to coerce
b2["fourth"] <- 40+as.matrix(b1[3])
b2[5] <- 50+as.array(b1[4])
set.seed(352)
try(b2["six"] <- 60+6+runif(5,min=-1,max=1)) ## Data structures mismatching
print(b2)
print(object.size(b2))

## Write
b3 <- create_envi(b2,"tmp2")
print(object.size(b3))
for (i in chunk_line(b3,0.04))
{
   b3[,i] <- b2[,i]+100
   if (5 \%in\% i)
      print(object.size(b3))
}
close(b3)
print(object.size(b3))
b4 <- read_envi("tmp2")
print(b4)
envi_remove("tmp[12]")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
