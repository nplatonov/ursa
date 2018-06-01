\name{global operator}
\alias{global_mean}
\alias{global_median}
\alias{global_sd}
\alias{global_sum}
\alias{global_min}
\alias{global_max}
\alias{global_n}
\alias{global_nNA}
\alias{global_range}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract certains statistics for whole image
}
\description{
Function from this \verb{global.\emph{FUN}} list returns requred statistics \emph{FUN} for the whole image.
}
\usage{
global_mean(x, ursa = FALSE, ...)
global_median(x, ursa = FALSE, ...)
global_sd(x, ursa = FALSE, ...)
global_sum(x, ursa = FALSE, ...)
global_min(x, ursa = FALSE, ...)
global_max(x, ursa = FALSE, ...)
global_n(x, ursa = FALSE, ...)
global_nNA(x, ursa = FALSE, ...)
global_range(x, ursa = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{ursaRaster}.
}
  \item{ursa}{
Logical. The class of returned value. If \code{FALSE} then numeric vector of length one is returned (for \code{global_range} vector has length two). If \code{TRUE} then returned value is single-band raster image (two-bands image for \code{global_range}) with constant value for all cells (blank image). Default is \code{FALSE}.
}
  \item{\dots}{
Arguments in function \verb{global.\emph{FUN}} which are passed to function \verb{\emph{FUN}}. 
}
}
\details{
For any function \verb{global.\emph{FUN}}, if argument \code{na.rm} is not in \code{\dots}, then \verb{\emph{FUN}} is called with forced \code{na.rm=TRUE}.

\verb{global_range\emph{list of arguments}} is implemented as \verb{c(global_min(\emph{list of arguments}), global_max(\emph{list of arguments}))} with the same list of arguments.

Alternative method to get global statistics is function applying directly to the raster value. For example, \code{sd(ursa_value(x,na.rm=TRUE))}. This way is also appropriate for missing global functions: for example, \code{var(ursa_value(x,na.rm=TRUE))}.
}
\value{
If \code{ursa=FALSE} then \code{\link[base:numeric]{numeric}}.
\cr
If \code{ursa=TRUE} then object of class \code{\link[ursa:classRaster]{ursaRaster}}.
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
a <- ursa_dummy(2,min=-40,max=80)
a[a<0] <- NA
print(a)
a.mean <- global_mean(a)
a.sd <- global_sd(a)
a.sum <- global_sum(a)
a.min <- global_min(a)
a.max <- global_max(a)
a.median <- global_median(a)
print(c(mean=a.mean,sd=a.sd,sum=a.sum,min=a.min,max=a.max,median=a.median))
v.max <- max(ursa_value(a),na.rm=TRUE)
print(c('global_max()'=a.max,'max(ursa_value())'=v.max,dif=a.max-v.max))
r.max <- global_max(a,ursa=TRUE)
print(r.max)
b <- c(a,'appended scalar value'=a.max)
print(b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
