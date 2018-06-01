\name{ursa}
\alias{ursa}
\alias{ursa<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get and set properties of raster image.
}
\description{
For package description see.

\code{ursa} is a wrapper to initialize object of class \code{ursaRaster}, to get and to set properties for this object.
}
\usage{
ursa(obj, attr, ...)
ursa(obj, attr, ...) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Any numeric structure (scalar, matrix, array) for initializing. Object of class \code{ursaRaster} for extracting and assigning properties.
}
  \item{attr}{
Character. Name of property.
}
  \item{\dots}{
Arguments, which are passed for properties specification. 
}
  \item{value}{
Value for property assignment.
}
}
\details{
Initializing function \code{ursa} with missing argument \code{attr} is a wrapper for function \code{as.ursa}.

\tabular{llll}{
\emph{Matched pattern} \tab \emph{Replace method?} \tab \emph{Description of property} \tab \emph{Implementation}
\cr\code{"grid"} \tab Yes \tab Raster grid (extent, projection, cellsize) \tab \code{\link[ursa:ursa_grid]{ursa_grid}}
\cr\code{"(proj|crs)"} \tab Yes \tab Coordinate reference system \tab \code{\link[ursa:ursa_proj]{ursa_proj}}
\cr\code{"val"} \tab Yes \tab Raster value in the internal storage format \tab \code{\link[ursa:classValue]{ursa_value}}
\cr\code{"(colort|ct)"} \tab Yes \tab Color table \tab \code{\link[ursa:classColorTable]{ursa_colortable}}
\cr\code{"(categ|class)"} \tab Yes \tab Names of categories \tab \code{names(ursa_colortable(obj))}
\cr\code{"name"} \tab Yes \tab Band names \tab \code{\link[ursa:classRaster_names]{names}}
\cr\code{"(nodata|ignorevalue|bg)"} \tab Yes \tab Value, which is interpreted as \code{NA}. \tab \code{\link[ursa:ignorevalue]{ignorevalue}}
\cr\code{"^table$"} \tab No \tab Frequency of unique values \tab \code{\link[ursa:classRaster_as.table]{as.table}}
\cr\code{"cell"} \tab No \tab \emph{Squared} cell size \tab \code{with(ursa_grid(obj),sqrt(resx*resy))}
\cr\code{"^dim$"} \tab No \tab Dimension of raster image \tab \code{\link[ursa:classRaster_dim]{dim}}
\cr\code{"(extent|bbox)"} \tab No \tab Spatial extent of raster image \tab \code{with(ursa_grid(obj),c(xmin=minx,ymin=miny,xmax=maxx,ymax=maxy))}
\cr\code{"(nrow|rows|lines)"} \tab No \tab Number of rows of raster image \tab \code{ursa_grid(obj)$rows}
\cr\code{"(ncol|columns|samples)"} \tab No \tab Number of columns of raster image \tab \code{ursa_grid(obj)$columns}
\cr\code{"con"} \tab No \tab \strong{str}ucture of connection \tab \code{obj$con}
\cr\code{"(info|meta(data)*)"} \tab No \tab Metadata, brief info \tab \code{\link[ursa:ursa_info]{ursa_info}}
\cr\code{"^file(name)*"} \tab No \tab Connection name (filename) \tab \code{obj$con$fname}
}
Argument \dots is used to specify band index or band pattern in \code{ursa(obj,"value",...)}

}
\value{
Inititalizing function \code{ursa} (missing \code{attr}) returns object of class \code{ursaRaster}.

\emph{Extract} function \code{ursa} returns object of respective property.

\emph{Replace} function \code{ursa<-} returns object 
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
\code{\link[ursa:ursa_as]{as.ursa}}
}
\examples{
a1 <- ursa(volcano)
print(a1)
display(a1)

a2 <- ursa(volcano,flip=TRUE)
print(a2)
display(a2)

a3 <- ursa(volcano,permute=TRUE)
print(a3)
display(a3)

a4 <- ursa(volcano,flip=TRUE,permute=TRUE)
print(a4)
display(a4)

dima <- c(200,300,4)
b1 <- ursa(array(runif(prod(dima)),dim=dima))
print(b1)
display_brick(b1,scale=1,pal.rotate=0,pal.hue=0,decor=FALSE)

session_grid(NULL)

c1 <- ursa(seq(3))
print(c1)
c2 <- ursa(seq(3),bands=3)
print(c2)

c3 <- ursa(value=FALSE)
str(ursa(c3,"value"))

c4 <- ursa(bands=2,nodata=-99L)
print(c4)
print(ursa(c4,"nodata"))

c5 <- ursa(bandname=format(Sys.Date()+seq(7)-1,"\%A"))
ursa(c5,"value") <- rev(seq(nband(c5)))
c5 <- colorize(c5)
ct <- ursa(c5,"colortable")
print(c5)

v <- ursa(c5[3:5],"value")
str(v)
v <- c(v)
str(v)
c6 <- ursa(v,colortable=ct)
print(c6)
print(ursa(c6,"colortable"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
