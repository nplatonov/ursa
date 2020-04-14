\name{reclass}
\alias{reclass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reclassify specific values of image
}
\description{
This is look-up table reclassification: the destination value is found for each source value.
}
\usage{
reclass(obj, dst = NULL, src = NULL, sparse = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster} or \code{ursaColorTable}.
}
  \item{dst}{
Object of class \code{ursaRaster}, or object of class \code{ursaColorTable}, or numeric vector. If numeric, then the desired destination set of values, else reference object for reclassification; this object should have numerical values of categories.
}
  \item{src}{
Numerical vector, but allowed using with numerical vector of \code{dst} and \code{length(src)==length(dst)}. Source set of values.
}
  \item{sparse}{
Logical. If image has a lot of \code{NA} values then \code{sparse=TRUE} may speed up transformation. Default is \code{FALSE}.
}
  \item{\dots}{
Other arguments are used for classification in the function \code{\link[ursa]{colorize}}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If \code{dst} is numeric vector, then the source value have to be specific, without any ranges. It is required the equality lengths of \code{src} and \code{dst}. If image has \link[ursa:classColorTable]{color table} then function tries reconstruct \code{dst} from names of \link[ursa:classColorTable]{categories}.

This function can be used for data compression for storage, e.g. for distribution or interchange.

}
\value{
If \code{obj} is object of class \code{ursaColorTable} then numeric vector of categories' centers.

If \code{dst} is numeric, then object of class \code{ursaRaster} without \link[ursa:classColorTable]{color table}.

If \code{dst} is \code{ursaColorTable} then object of class \code{ursaRaster} (\code{NA} values) in \link[ursa:classColorTable]{color table}.

If \code{dst} is \code{NULL} then object of class \code{ursaRaster} with empty color names (\code{NA} values) in \link[ursa:classColorTable]{color table}.
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
\note{
There were no a lot of tests how GIS software reads "ENVI .hdr Labelled Raster" files with color tables without color values (only \emph{categories}). At least, \href{http://www.gdal.org/frmt_various.html#ENVI}{GDAL} recognizes categories (\href{http://www.gdal.org/gdalinfo.html}{\code{gdalinfo}} utility).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The reclassification from interval source values to specific destination values is used in \code{\link[ursa]{colorize}}.
}
\examples{
session_grid(NULL)
# example no.1 manual classification
a <- as.ursa(round(matrix(runif(100,min=0.5,max=3.5),ncol=10)))
print(as.table(a))
b <- reclass(a,src=c(3,1,2),dst=round(runif(3),2))
print(as.table(b))
print(c(src=a,dst=b))

# example no.2 -- similarity to other object
session_grid(NULL)
a <- ursa_dummy(nband=2,min=-1,max=1)
print(a)
b1 <- colorize(a[1],value=seq(-1,1,length=21),pal.rich=240,pal.rotate=0)
b2 <- reclass(a[2],b1)
b3 <- reclass(a[2],ursa_colortable(b2))
b <- c(b1,b2,b3)
print(reclass(b))

# example no.3 -- compression with data lost
a <- pixelsize(NULL)
b <- reclass(a,byte=TRUE,tail=0) ## try 'byte=FALSE'
a2 <- reclass(b)
res <- c(source=a,as_category=a2,difference=a-a2)
print(res)
message(paste("RMS error: ",format(sqrt(band_sum(res[3]^2)/band_n(res[3])))))
prefix <- names(res)[1:2]
fname <- file.path(tempdir(),paste0(prefix,".envi"))
s <- data.frame(object.size=sapply(list(a,b),object.size))
rownames(s) <- prefix
print(s)
write_envi(a,fname[1])
write_envi(b,fname[2])
f <- file.info(dir(path=tempdir()
                  ,pattern=paste0("(",prefix,")\\\\.(envi|hdr)",sep="|")
                  ,full.names=TRUE))[,"size",drop=FALSE]
colnames(f) <- "file.size"
print(f)
envi_remove(fname)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{category}
