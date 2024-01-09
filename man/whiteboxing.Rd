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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\href{https://github.com/jblindsay/whitebox-tools/releases/}{WhiteboxTools}
\pkg{\link[whitebox]{whitebox}}

}
\examples{
if ((requireNamespace("whitebox"))&&(requireNamespace("stars"))) {
   a1 <- read_gdal(c(DEM=whitebox::sample_dem_data()))
   a2 <- whiteboxing("BreachDepressions",input=a1)
   print(list(value=c(a1,a2),diff=a1-a2))
}
}
\keyword{manip}
