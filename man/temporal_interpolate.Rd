\name{temporal_interpolate}
\alias{temporal_interpolate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill gaps across bands using moving mean window
}
\description{
\code{temporal_interpolate} is applicable for multiband raster image, where bands are regular timestamps or period. For each cell (\emph{local} operation of map algebra), \code{NA} value is replaced by averaging of two closest values (one value before, one value later) inside of moving window.
}
\usage{
temporal_interpolate(obj, win = 7, cover = 0, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster} or \code{\link[base:matrix]{matrix}}, where spatial locations are by rows and temporal observations are by columns.
}
  \item{win}{
Positive integer. Size of moving window. Required odd value; otherwise is coerced to the closest odd integer.
}
  \item{cover}{
\emph{\bold{Not applicable}}. For consistence call with \code{\link[ursa:temporal_mean]{temporal_mean}}.
}
  \item{verbose}{
Logical. \code{TRUE} provides some additional information on console. Default is FALSE.
}
}
\details{
Function uses weighted averaging depending of proximity of found non-\code{NA} values. For example, if \code{ind} is temporal index of \code{NA} value in the center of movind window, \code{indL=ind-2} is temporal index of the closest early value \code{valL}, and \code{indR=ind+1} is temporal index of the closest late value \code{valR}, then result is \verb{val <- (1/3) * valL + (2/3) * valR}.
}
\value{
\code{ursaRaster} object, if \code{obj} is object of class \code{ursaRaster}.

\code{matrix} object, if \code{obj} is a matrix.
}
%%~ \references{
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

\seealso{
\code{\link[ursa:temporal_mean]{temporal_mean}}
}
\examples{
session_grid(NULL)
n <- 45 # bands
m <- 3 # sample size
k <- median(seq(n))+seq(m)-(m \%/\% 2)-1 ## sample subset
s <- 5 # window size
a <- round(ursa_dummy(n,min=-60,max=60,elements=15,mul=1/8))
a[a<(-40)] <- NA
b <- temporal_interpolate(a,7)
p1 <- colorize(a,lazy=TRUE)
p2 <- colorize(b,lazy=TRUE,colortable=p1)
display(list('Source'=p1[k],'Gaps are filled'=p2[k]),layout=c(2,NA)
       ,legend=list(list(1,"right"),list(2,"right")),decor=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
