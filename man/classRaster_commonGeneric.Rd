\name{commonGeneric}
\alias{commonGeneric}
\alias{duplicated.ursaRaster}
\alias{diff.ursaRaster}
\title{
Some generic functions for \code{ursaRaster} class.
}
\description{
Set of generic functions, implemented for objects of \code{ursaRaster} class.
}
\usage{
\method{duplicated}{ursaRaster}(x, incomparables = FALSE, MARGIN = 2, fromLast = FALSE, ...)

\method{diff}{ursaRaster}(x, lag = 1, differences = 1, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of \code{ursaRaster} class
}
  \item{incomparables}{
Passed to S3 method \code{duplicated} for class \code{matrix}.
}
  \item{MARGIN}{
Overwitten to value \code{2}. Passed to S3 method \code{duplicated} for class \code{matrix}.
}
  \item{fromLast}{
Passed to S3 method \code{duplicated} for class \code{matrix}.
}
  \item{lag}{
Passed to default S3 method \code{diff}.
}
  \item{differences}{
Passed to default S3 method \code{diff}.
}
  \item{\dots}{
Other arguments, which are passed to the respective S3 method.
}
}
%%~ \details{
%%~ %%  ~~ If necessary, more details than the description above ~~
%%~ }
\value{
\code{duplicated()}: logical of length equal to number of bands.

\code{diff()}: \code{ursaRaster} object.

}
%%~ \references{
%%~ put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%  ~~further notes~~
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base:diff]{duplicated}}, \code{\link[base:diff]{diff}},
}
\examples{
a <- ursa_dummy(5)
a[3] <- a[2]
a
duplicated(a)
diff(a)
}
