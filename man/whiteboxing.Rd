\name{whiteboxing}
\alias{whiteboxing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper to WhiteboxTools
}
\description{
Wrapper to tools from \pkg{"whitebox"} package to manipulate with \code{ursaRaster} objects

}
\usage{
whiteboxing(tool_name, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tool_name}{
Either tool name of Whitebox or function name from \pkg{\link[whitebox]{whitebox}}.
}
  \item{\dots}{
List of parameters.
}
}
\details{
Wrapper to function \code{\link[whitebox]{wbt_run_tool}}.

\code{ursaRaster} object \code{foo} can be passed via parameter \code{input=foo} instead of GeoTIFF file name.
}
\value{
If argument \code{output} is missed or \code{output=FALSE}, then object of class \code{ursaRaster}. Otherwise, output GeoTIFF file name.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Internally, for piping support, first character argument without \code{*.tif} suffix is interpreted as \code{tool_name}. First unnamed character argument with \code{*.tif} suffix or \code{ursaRaster} object is interpreted as input.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\href{https://github.com/jblindsay/whitebox-tools/releases/}{WhiteboxTools}
\pkg{\link[whitebox]{whitebox}}  

}
\examples{
if ((requireNamespace("whitebox"))&&(isTRUE(whitebox::wbt_init()))&&
    (requireNamespace("stars"))) {
   dem <- whitebox::sample_dem_data()
   a1 <- c(DEM=read_gdal(dem))
   a2 <- whiteboxing("BreachDepressions",input=a1)
   b <- list(value=c(a1,a2),difference=c(diff=a1-a2))
   print(b)
   display(b,layout=c(2,NA),legend=list(list("first","left"),list("last","left")))
   if (getRversion()>="4.1.0") {
      a5 <- dem |>
         whiteboxing("feature_preserving_smoothing",filter=9) |>
         whiteboxing("breach_depressions") |>
         print()
   }
}
}
\keyword{manip}
