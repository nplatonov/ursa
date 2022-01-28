\name{ursa_grid}
\alias{ursa_grid}
\alias{ursa_grid<-}
\alias{ursa_grid<-}
\alias{ursa_ncol}
\alias{ursa_nrow}
\alias{ursa_columns}
\alias{ursa_rows}
\alias{ursa_samples}
\alias{ursa_lines}
\alias{ursa_extent}
\alias{ursa_bbox}
\alias{consistent_grid}
\title{
   Extract and assign spatial parameters of raster images.
}
\description{
Raster image (\code{ursaRaster}) contains embedded spatial parameters (\code{\link[ursa:classGrid]{ursaGrid}}) in item \code{$grid}. These functions manipulate with item \code{$grid}.
}
\usage{
ursa_grid(obj)
ursa_grid(obj) <- value

ursa_ncol(obj)
ursa_nrow(obj)
ursa_columns(obj)
ursa_rows(obj)
ursa_samples(obj)
ursa_lines(obj)

ursa_extent(obj)
ursa_bbox(obj)

consistent_grid(obj, ref, expand = 1, border = rep(0, 4), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
\code{ursaRaster} object. For \code{ursa_grid} function \code{\link[base]{list}} of \code{ursaRaster} objects is allowed.
}
  \item{value}{
\code{\link[ursa:classGrid]{ursaGrid}} object.
}
  \item{ref}{
\code{\link[ursa:classGrid]{ursaGrid}} reference object.
}
  \item{expand}{
Podsitive numeric. Multiplier of boundary box.
}
  \item{border}{
\code{integer} of length 1 or 4. If length 4, then vector (bottom, left, top, right) in cells for extent expand. If length <4, then value is repeated for length 4. Passed to \code{\link[ursa:regrid]{regrid()}}.
}
  \item{verbose}{
Logical. Some output in console. Primarily for debug purposes.
}
}
\details{
\code{ursa_grid<-} may used to minor corrections of spatial parameters. Howevert, it seems that this function is not claimed in practice.

\code{ursa_ncol}, \code{ursa_columns}, \code{ursa_samples} are synonyms for extracting number of columns/samples.

\code{ursa_nrow}, \code{ursa_rows}, \code{ursa_lines} are synonyms for extracting number of rows/lines.

\code{ursa_extent}, \code{ursa_bbox}, are synonyms for extracting boundary box (spatial extent).

\code{consistent_grid} trasforms dimension (\code{ursa_nrow()} by \code{ursa_ncol()}) \code{obj}-grid to dimension of \code{ref}-grid. This helpful for multipanel plotting if objects have different boundary boxes.
}
\value{
\code{ursa_grid} return value of \code{$grid} item of \code{ursaRaster} object.

\code{ursa_grid<-} return \code{ursaRaster} with modified \code{$grid} item.

\code{ursa_ncol}, \code{ursa_columns}, \code{ursa_samples} return integer of length 1.

\code{ursa_nrow}, \code{ursa_rows}, \code{ursa_lines} return integer of length 1.

\code{ursa_extent}, \code{ursa_bbox} return numeric of length 4 (xmin, ymin, xmax, ymax).

\code{ursa_consistent} returns \code{\link[ursa:classGrid]{ursaGrid}} object.

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
a <- pixelsize()
print(ursa_grid(a))
ursa_grid(a)$crs <- gsub("\\\\.0+","",ursa_grid(a)$crs)
print(ursa_grid(a))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{attribute}
