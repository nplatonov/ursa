\name{colortable}
\alias{class-ursaColorTable}
\alias{ursa_colortable}
\alias{ursa_colortable<-}
\alias{print.ursaColorTable}
\alias{[.ursaColorTable}
\alias{names.ursaColorTable}
\alias{names<-.ursaColorTable}
\alias{ursa_colorindex}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Color Tables of raster images.
}
\description{
   Manipulation with color tables of raster images.
}
\usage{
\method{print}{ursaColorTable}(x, ...)

\method{[}{ursaColorTable}(x, i)


ursa_colortable(x)

ursa_colortable(x) <- value

ursa_colorindex(ct)

\method{names}{ursaColorTable}(x)

\method{names}{ursaColorTable}(x) <- value

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{ursaRaster} object. 
}
  \item{ct}{
\code{ursaColorTable} object with or witout indexing.
}
  \item{value}{
Named character vector. In Replacement functions:

For \code{ursa_colortable()}: values are colors in \dQuote{#RRGGBB} notation or \R color names (\code{\link[grDevices]{colors}}). \code{names(value)} are names of categories.

For \code{names()}: values are names of categories. If length of names is \code{n-1}, where \code{n} is length of colors, then intervaling is assumed, and code{value} are assing to interval breaks.
}
  \item{i}{
Integer vector. Indices specifying elements to extract part (subset) of color table.
}
  \item{\dots}{
Passing to generic \code{\link[base]{print}}. Currently not used.
}
}
\details{
The example of the class structure\preformatted{
Class 'ursaColorTable'  Named chr [1:4] "#313695" "#BCE1EE" "#FDBE70" "#A50026"
  ..- attr(*, "names")= chr [1:4] "<= 450" "(450;550]" "(550;650]" "> 650"
}

It is recommended to use \code{ursa_colortable} and \code{ursa_colortable<-} instead of \code{colortable} and \code{colortable<-}. \code{ursa_colortable} and \code{colortable} are synonyms. \code{ursa_colortable<-} and \code{colortable<-} are synonyms too. Package \pkg{raster} contains \code{\link[raster:colortable]{colortable}} and \code{\link[raster:colortable]{colortable<-}} functions. \code{colortable} and \code{colortable<-} will be remove from this package if the case of frequent joint use of both packages.

If color tables describe continuous and non-intersecting intervals, then \code{print} gives additional line of extracted breaks.
}
\value{
\code{ursa_colortable} returns value of \code{$colortable} element if \code{ursaRaster} object.

\code{ursa_colortable<-} returns \code{ursaRaster} object with modified \code{$colortable} element.

Class of \code{$colortable} element is \dQuote{ursaColorTable}. This is named character vector, where names are categories, and values are \dQuote{#RRGGBB} or \R color names. 

\emph{Extract} function \code{[]} for \code{ursaColorTable} object returns object of class \code{ursaColorTable}.

\emph{Extract} function \code{names} for \code{ursaColorTable} object returns character vector (names of categories).

\emph{Replace} function \code{names<-} for \code{ursaColorTable} object returns \code{ursaColorTable} with changed names of categories.

\code{ursa_colorindex} returns index (if presents) for \code{ursaColorTable} object.

Color tables are written to ENVI header file.
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

\section{Warning}{
If colors are specified as \R color names, then slow down may appear.
}

\seealso{
\code{\link[ursa]{colorize}}
}
\examples{
session_grid(NULL)
print(methods(class="ursaColorTable"))

a <- pixelsize()
print(a)
b1 <- colorize(a,value=c(400,500,600,700),interval=FALSE)
b2 <- colorize(a,value=c(450,550,650)    ,interval=TRUE)
display(list(b1,b2))
print(is.ursa(a,"colortable"))
print(is.ursa(b1,"colortable"))
print(is.ursa(b2,"colortable"))
print(ursa_colortable(a))
print(ursa_colortable(b1))
print(ursa_colortable(b2))
ursa_colortable(b2) <- c("Low"="darkolivegreen1"
                        ,"Moderate"="darkolivegreen2"
                        ,"High"="darkolivegreen3"
                        ,"errata"="darkolivegreen4")
print(ursa_colortable(b2))
names(ursa_colortable(b2))[4] <- "Polar"
print(ursa_colortable(b2))
display(b2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{print}
