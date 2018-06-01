\name{groupGeneric}
\alias{groupGeneric}
\alias{groupGeneric.ursaRaster}
\alias{Math.ursaRaster}
\alias{Ops.ursaRaster}
\alias{Summary.ursaRaster}
\alias{Complex.ursaRaster}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group Generic Functions for raster image
}
\description{
These functions implement arithmetical and logical operations, mathematical functions for objects of class \code{ursaRaster} as well as group generic functions from package \pkg{base} do similar for S3 class. These are \strong{\emph{local}} operations in the raster algebra (map algebra).
}
\usage{
\method{Ops}{ursaRaster}(e1, e2 = NULL)

\method{Math}{ursaRaster}(x, \dots)

\method{Complex}{ursaRaster}(z)

\method{Summary}{ursaRaster}(..., na.rm = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object
}
  \item{e1}{
\code{ursaRaster} object
}
  \item{e2}{
Numeric of length 1, matrix, array, or \code{ursaRaster} object.
}
  \item{na.rm}{
Logical. If \code{na.rm=TRUE} then no-data values are omitted.
}
  \item{z}{
Any.
}
  \item{\dots}{
For group \emph{\link[base:groupGeneric]{Math}} - further arguments passed to methods. See description for generic.

For group \emph{\link[base:groupGeneric]{Summary}} - set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}), position and classes.
   \describe{
     \item{.*}{
     Position 1. Object of class \code{ursaRaster}.
   }
     \item{\code{cov|cvr}}{
   Position >1. Numeric between 0 and 1. If proportion of bands with no data for given location exceeds \code{cover} then output value is \code{NA} (no data). Default is \code{0.5-1e-3}.
   }
     \item{\code{w}}{
   Position >1. Numeric of length number of bands or \code{NULL}. Band weights for weighted mean. Default is \code{NULL}; all bands have equal weights.
   }
     \item{\code{name}}{
   Position >1. Character of length 1. Band name for output raster. Default is \code{""}; band name is assigned automatically.
   }
     \item{\code{verb(ose)*}}{
   Position >1. Logical. \code{verbose=TRUE} provides some additional information on console. Default is \code{FALSE}.
   }
   }
}
}
\details{
The groups are \verb{'Summary'}, \verb{'Ops'}, \verb{'Math'}, and \verb{'Complex}'. See \dQuote{Details} section in the \link[base:groupGeneric]{S3 Generic Functions} help page.

The group \verb{'Complex'} is unsupported.

The groups \verb{'Math'} and \verb{'Summary'} are implemented completely.


%~ The exceptions for group \code{Summary} are operators
%~ \verb{\dQuote{min}}, \verb{\dQuote{min}}, \verb{\dQuote{sum}}, which are implemented together with \code{\link{[ursa:classRaster_mean]{mean}}} function

The group \verb{'Ops'} has some features.
\itemize{
\item{Logical operators \verb{"<"}, \verb{">"}, \verb{"<="}, \verb{">="}, \verb{"=="}, \verb{\"!="}} return \code{'NA'} for value \code{FALSE} and \code{'1'} for value \code{TRUE} to organize cells' masking.
\item{Unary operator \verb{"!"}} is equal to binary operator operators \verb{"!="}, where the second argument is scalar value \code{0} (zero).
}

The operators of groups \verb{'Math'} and \verb{'Ops'} destroy \link[ursa:classColorTable]{color tables}.

For group \verb{'Summary'} the realization of local operators of map algebra is possible via \code{\link[base]{apply}} function:\cr
\code{apply(ursa_value(obj),1,function(x) {y <- sd(x)+1;y})}\cr
or\cr
\code{as.ursa(apply(obj,1:2,function(x) {y <- sd(x)+1;y}))}
}

\value{
Operators of groups \verb{'Complex'} return \code{\link[base]{stop}}
\cr
Operators of groups \verb{'Math'}, \verb{'Ops'}, \verb{'Summary'} return object of class \code{ursaRaster}
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
Other S3 generic function for local operations of map algebra are \code{\link[ursa:Ops.local_stat]{mean}}, \code{\link[ursa:Ops.local_stat]{median}}.
\cr
Standard deviation (local) and certain local operations can be extracted using \code{\link[ursa:Ops.local_stat]{local_stat}}.
}
\examples{
session_grid(NULL)
session_grid(regrid(mul=1/4))
a1 <- ursa_dummy(nband=3,min=-5*pi,max=5*pi)
print(a1)

try(print(complex1 <- Re(a1)))

print(math1 <- a2 <- round(a1))
print(math1 <- sin(a1))
print(math2 <- floor(a1))
print(math3 <- ceiling(a1))
print(math4 <- cumsum(a1)) ## does this have a sense for rasters?

print(ops1 <- a1-2*rev(a1)+mean(a1))
print(mean(ops1)) ## vanishing
a2 <- ursa_new(value=c(1,2,4),bandname=c("single","double","quadruple"))
print(a2)
print(ops2 <- a2[1]==a2[2])
print(ops3 <- a2[1]==a2[2]/2)
print(ops4 <- a1>0)
print(a1[a1>0])

print(sum1 <- sum(a1))
print(sum2 <- range(a1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
