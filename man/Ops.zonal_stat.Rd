\name{zonal_stat}
\alias{zonal_stat}
\alias{aggregate.ursaRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Zonal statistics for raster maps
}
\description{
'Zonal' operator of map algebra. Applied to raster images.
}
\usage{
zonal_stat(x, by, FUN, table = FALSE)

\method{aggregate}{ursaRaster}(x, by, FUN, table = FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object. Image for analysis.
}
  \item{by}{
\code{ursaRaster} object. Image of grouping elements.
}
  \item{FUN}{
a function to compute the summary statistics which can be applied to all data subsets.
}
  \item{table}{
Logical. If \code{table=TRUE} then summary statistics for each group is returned. The statistics is defined by \code{FUN}. If \code{stat=FALSE} then result is presented as \code{ursaRaster} object.
}
  \item{\dots}{
Other arguments which passed to function \code{\link[stats]{aggregate}} of package \pkg{stats}
}
}
\details{
%%~ \code{zonal_stat} is simplification of \code{aggregate} with \code{FUN=mean} argument.

\code{zonal_stat} is a wrapper of \code{aggregate(x,by,FUN,table=FALSE,na.rm=TRUE)}

%%~ \code{choropleth} is a synonym of \code{zonal_stat} and probably will be removed. It is known the same function name in the the package \pkg{GISTools}. See \code{\link[GISTools:choropleth]{GISTools::choropleth}}.

You can use multichannel image (argument \code{x}) for analysis.

You can use multichannel raster image for group elements (argument \code{by})

}
\value{
If \code{table=FALSE} then \code{ursaRaster} object of summarized statistics.
\cr
If \code{table=TRUE} then \code{data.frame}.
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
session_grid(regrid(mul=1/2))
a <- pixelsize()
val <- c(normal=a,half=a/2)
gr <- c(group=colorize(a,nbreak=1))#+0
print(as.table(gr))
##~ display(gr)
ra <- round(aggregate(val,gr,mean),4)
print(ra)
print(as.table(ra[1]))
print(as.table(ra[2]))
da <- aggregate(val,gr,table=TRUE,mean)
n <- aggregate(a,gr,table=TRUE,length)[,2,drop=FALSE]
da <- cbind(da,n=unname(n))
gr2 <- c(group2=colorize(a,nbreak=6))#+0
mgr <- list(gr,gr2)
da2 <- aggregate(val[1],mgr,table=TRUE,mean)
print(da2)
da3 <- aggregate(val,mgr,table=TRUE,mean)
print(da3)
ra3 <- aggregate(val,mgr,table=FALSE,mean) ## not implemented for rasters
print(ra3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
