\name{chunk}
\alias{chunk_band}
\alias{chunk_line}
\alias{chunk_expand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get indices for partial image reading/writing
}
\description{
In the case of 'Cannot allocate vector of size ...' error message \code{chunk_band} returns list of bands indices, which are suitable for allocation in memory at once, \code{chunk_line} returns list of lines (rows) indices, which are suitable for allocation in memory at once. \code{chunk_expand} is used to expand lines indices and can by applied in focal functions.
}
\usage{
chunk_band(obj, mem = 100, mul = 1)
chunk_line(obj, mem = 100, mul = 1)
chunk_expand(ind, size = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}
}
  \item{mem}{
Numeric. Memory size in GB, which is suitable for allocation.
}
  \item{mul}{
Numeric. Expansion or reduction factor (multiplier) of default value of memory allocation.
}
  \item{ind}{
Integer. Line indices.
}
  \item{size}{
Integer. Size of focal window.
}
}
%%~ \details{
%%~    
%%~ }
\value{
\code{chunk_band} returns list with sequences of bands

\code{chunk_line} returns list with sequences of lines

\code{chunk_expand} returns list:
\item{src}{expanded set if line indices}
\item{dst}{matching of source indices in the expanded set}
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
 ## 1. Prepare data
 session_grid(NULL)
 fname <- ursa:::.maketmp(2)
 a <- create_envi(fname[1],nband=3,ignorevalue=-99)
 for (i in seq(nband(a)))
   a[i] <- pixelsize()^(1/i)
 close(a)
 rm(a)
 
 ## 2. Read 
 a <- open_envi(fname[1])
 chB <- chunk_band(a,2)
 str(chB)
 for (i in chB)
    print(a[i])
 chL <- chunk_line(a,2.5)
 str(chL)
 for (j in chL)
    print(a[,j])
 
 ## 3. Filtering with partial reading
 b <- create_envi(a,fname[2])
 fsize <- 15
 for (j in chL) {
    k <- chunk_expand(j,fsize)
    b[,j] <- focal_mean(a[,k$src],size=fsize)[,k$dst]
 }
 d1 <- b[]
 
 ##  4. Filtering in memory
 d2 <- focal_mean(a[],size=fsize)
 close(a,b)
 envi_remove(fname)
 print(d1-d2)
 print(round(d1-d2,4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
