\name{spatial_engine}
\alias{spatial_engine}
\alias{spatial_crs}
\alias{spatial_crs<-}
\alias{spatial_proj}
\alias{spatial_proj<-}
\alias{spatial_proj4}
\alias{spatial_proj4<-}
\alias{spatial_bbox}
\alias{spatial_bbox<-}
\alias{spatial_data}
\alias{spatial_data<-}
\alias{spatial_geometry}
\alias{spatial_geometry<-}
\alias{spatial_geotype}
\alias{spatial_shape}
\alias{spatial_transform}
\alias{spatial_coordinates}
\alias{spatial_fields}
\alias{spatial_fields<-}
\alias{spatial_colnames}
\alias{spatial_colnames<-}
\alias{spatial_area}
\alias{spatial_length}
\alias{spatial_dim}
\alias{spatial_count}
\alias{spatial_nrow}
\alias{spatial_ncol}
\alias{spatial_filelist}
\alias{spatial_dir}
\alias{spatial_basename}
\alias{spatial_pattern}
\alias{is_spatial}
\alias{is_spatial_points}
\alias{is_spatial_lines}
\alias{is_spatial_polygons}
\alias{spatial_intersection}
\alias{spatial_symdifference}
\alias{spatial_union}
\alias{spatial_buffer}
\alias{spatial_trim}
\alias{spatial_valid}
\alias{spatial_grid}
\title{
Wrapper functions for manipulation with non-raster spatial objects
}
\description{
These wrappers return iniform properties or do consimilar manipulations for spatial objects of different types: simple features (package \pkg{sf}) and abstract class Spatial (package \pkg{sp}). Appropriate functionality (\emph{\dQuote{engine}}) of respective packages is used. 
 
}
\usage{
spatial_engine(obj, verbose = FALSE)

spatial_crs(obj, verbose = FALSE)
spatial_proj(obj, verbose = FALSE)
spatial_proj4(obj, verbose = FALSE)

spatial_crs(obj, verbose = FALSE) <- value
spatial_proj(obj, verbose = FALSE) <- value
spatial_proj4(obj, verbose = FALSE) <- value

spatial_bbox(obj, verbose = FALSE)
spatial_bbox(obj, verbose = FALSE) <- value

spatial_data(obj, subset= ".+", drop = NA, verbose = FALSE)
spatial_data(obj, verbose = FALSE) <- value

spatial_geometry(obj, verbose = FALSE)
spatial_geometry(obj, verbose = FALSE) <- value

spatial_geotype(obj, each = FALSE, verbose = FALSE)
spatial_shape(obj, each = FALSE, verbose = FALSE)

spatial_transform(obj, crs, verbose = FALSE, ...)

spatial_coordinates(obj, verbose = FALSE)

spatial_fields(obj, verbose = FALSE)
spatial_colnames(obj, verbose = FALSE)

spatial_fields(obj, verbose = FALSE) <- value
spatial_colnames(obj, verbose = FALSE) <- value

spatial_area(obj, verbose = FALSE)

spatial_dim(obj, verbose = FALSE)

spatial_count(obj, verbose = FALSE)

spatial_nrow(obj, verbose = FALSE)
spatial_ncol(obj, verbose = FALSE)

spatial_filelist(path = ".", pattern = NA, full.names = TRUE, recursive = FALSE,
                 ignore.case = TRUE)
spatial_dir(path = ".", pattern = NA, full.names = TRUE, recursive = FALSE,
            ignore.case = TRUE)

spatial_basename(fname)
spatial_pattern(fname)

is_spatial(obj, verbose = FALSE)

is_spatial_points(obj, verbose = FALSE)
is_spatial_lines(obj, verbose = FALSE)
is_spatial_polygons(obj, verbose = FALSE)

spatial_intersection(x, y,
                     geometry=c("default", "polygons", "lines", "points", "all"),
                     verbose = FALSE)
spatial_symdifference(x, y, verbose = FALSE)
spatial_union(x, y, byid=NA, verbose = FALSE)

spatial_buffer(obj, dist = 0, quadsegs = 30L, verbose = FALSE)

spatial_trim(obj)

spatial_valid(obj, each = FALSE, reason = FALSE, verbose = FALSE)

spatial_grid(obj)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Simple feature (package \pkg{sf}) or Spatial abstract class (package \pkg{sp}) for all functions, excepting \code{spatial_geometry<-}. Data frame for \emph{Replace} function \code{spatial_geometry<-}.
}
  \item{x, y}{
Objects of simple feature (package \pkg{sf}) class or Spatial abstract class (package \pkg{sp}).
}
  \item{crs}{
Projection EPSG code or projection PROJ.4 string.
}
  \item{subset}{
Pattern to field names (colnames) of attribute table (data frame) for subbsetting using \code{\link[base:grep]{regular expressions}}. By default, all fields are secected.
}
  \item{drop}{
Logical. Dropping column of data frame. If \code{TRUE}, then vector of data is returned. If \code{FALSE}, then structure of data is kept. Default is \code{NA}, which is interpreted as \code{TRUE} for single column and as \code{FALSE} for multiple columns.
}
  \item{value}{
Value for property assignment in \emph{replacement} functions. Either numeric EPSG code or character PROJ.4 string for \code{spatial_crs<-} and \code{spatial_proj4<-}. Spatial object or geometry of spatial object for \code{spatial_geometry<-}.
}
  \item{path}{
See description of argument \code{path} in function \code{\link[base:list.files]{dir}}.
}
  \item{pattern}{
See description of argument \code{pattern} in function \code{\link[base:list.files]{dir}}.
}
  \item{full.names}{
See description of argument \code{full.names} in function \code{\link[base:list.files]{dir}}.
}
  \item{recursive}{
See description of argument \code{recirsive} in function \code{\link[base:list.files]{dir}}.
}
  \item{ignore.case}{
See description of argument \code{ignore.case} in function \code{\link[base:list.files]{dir}}.
}
  \item{quadsegs}{
Integer. Number of segments per quadrant (fourth of a circle), for all or per-feature. See description for \code{quadsegs} argument of \code{\link[rgeos:misc-gBuffer]{gBuffer}} and for \code{nQuadSegs} argument of \code{\link[sf:geos_unary]{st_buffer}}.
}
  \item{dist}{
Numeric. Buffer distance for all, or for each of the elements. See description for \code{width} argument of \code{\link[rgeos:misc-gBuffer]{gBuffer}} and for \code{dist} argument of \code{\link[sf:geos_unary]{st_buffer}}.
}
  \item{byid}{
Logical. For \code{spatial_union} function, \code{TRUE} does unite of each feature; \code{FALSE} returns a single feature that is the geometric union of the set of features; default \code{NA} is coerced to \code{FALSE} for unary operation (missing \code{y}) and to \code{TRUE} for binary operation.
}
  \item{fname}{
Character. Filename (source or packed) of spatial data.
}
  \item{each}{
Logical. Whether result will be returned for each record (\code{TRUE}) or generalized (\code{FALSE}). Default is \code{FALSE}.
}
  \item{geometry}{
Character. Desired output geometry for \code{engine="sf"}. If \code{"default"} then output geometry is defined internally (e.g., \code{"polygons"} for polygons intersection). If \code{"all"} then no output subsetting. Default is \code{"default"}.
}

  \item{reason}{
Logical. If \code{TRUE}, then the reason for validity ("Valid Geomerty") or invalidity is returned. If \code{FALSE}, then logical value of validity is returned. Default is \code{FALSE}.
}
  \item{verbose}{
Logical. Value \code{TRUE} provides information on console. Default is \code{FALSE}.
}
  \item{\dots}{
Further arguments passed to \code{sf::st_transform} or to \code{sp::spTransform}}.
}

\value{
\code{spatial_engine} returns package name (character string \code{"sf"} or \code{"sp"}), which functionality is used for manipulation with spatial object \code{obj}.

\code{spatial_crs} and \code{spatial_proj4} are synonyms, The \emph{Extract} functions return projection string in the PROJ.4 notation; the \emph{Replace} functions change projection property of the object.

\code{spatial_bbox} (\emph{Extract} function) returns numeric vector of length 4 with names "xmin", "ymin", "xmax" and "ymax".

\code{spatial_bbox<-} (\emph{Replace} function) assigns boundary bbox to the object; it is valid only for objects of Spatial abstract class (package \pkg{sp}).

\code{spatial_data} (\emph{Extract} function) returns attribute table only, without geometry. Subsetting fields can be specified by argument \code{subset} using regular expressions. If \code{drop=TRUE} and selected single column then vector is returned instead of data frame.

\code{spatial_data<-} (\emph{Replace} function) adds spatial data to the object geomerty. Source data (if presents) are droped.

\code{spatial_geometry} (\emph{Extract} function) returns only geometry, which format is depended on class of \code{obj}.

\code{spatial_geometry<-} (\emph{Replace} function) addes geometry to the object.

\code{spatial_transform} does a transformation of spatial coordinates to the new CRS and returns object of the same class as class of \code{obj}.

\code{spatial_geotype} and \code{spatial_shape} are synonyms; each returns type of spatial data: "POINT", "LINESTRING", "POLYGON", "MULTIPOLYGON", \dots.

\code{spatial_coordinates} returns simplified matrix or list of coordinates of original object.

\emph{Extract} functions \code{spatial_fields} and \code{spatial_columns} return column names of spatial attributive table. \code{spatial_columns} is synonym to \code{spatial_fields}.

\emph{Replace} functions \code{spatial_fields<-} and \code{spatial_columns<-} change column names of spatial attributive table. \code{spatial_columns<-} is synonym to \code{spatial_fields<-}.

\code{spatial_area} is valid for polygonal geometry. It returns area of polygons.

\code{spatial_length} is valid for linear geometry. It returns length of lines.

\code{spatial_dim} gets dimension of spatial coordinates; it returns either \code{2L} (XY) or \code{3L} (XYZ).

\code{spatial_count} returns number of items of object geometry.

\code{spatial_nrow} and \code{spatial_ncol} return number of rows and number of columns of attributive table.

\code{spatial_filelist} and its synonym \code{spatial_dir} return list of files with file extensions, which are associated with certain GIS vector formats. The function's basis is \code{\link[base:list.files]{dir}}.

\code{spatial_basename} returns basename (without extension) of file \code{fname} of spatial object.

\code{spatial_pattern} returns pattern of \code{spatial_basename} for using in \link[base:regex]{regular expressions}.

\code{is_spatial} returns logical value does the object belong to the class of spatial data.

\code{is_spatial_points} returns logical value does the object have point geometry.

\code{is_spatial_lines} returns logical value does the object have (multi)linestring geometry.

\code{is_spatial_polygons} returns logical value does the object have (multi)polygonal geometry.

\code{spatial_intersection} returns intersection of two spatial objects.

\code{spatial_symdifference} returns difference of two spatial objects.

\code{spatial_buffer} returns buffered spatial object.

\code{spatial_union} returns combined geometry without internal boundaries.

\code{spatial_trim} returns spatial object without extra attributes added by \pkg{ursa} package.

\code{spatial_grid} generates suitable spatial grid from input vector and returns object of class \code{ursaGrid}.

}
\references{
%% ~put references to the literature/web site here ~
Classes and methods in packages \pkg{\link[sf:st]{sf}} and \pkg{\link[sp:00sp]{sp}} help.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Acknowledgements }{
   The great improvement for development of functions for manipulation with spatial objects has been reached during work in series of projects (2015-2018) for design of marine protected areas in the Arctic, which were supported by WWF Russia.
}

\examples{
session_grid(NULL)
n <- 1e2
x <- runif(n,min=25,max=65)
y <- runif(n,min=55,max=65)
z <- runif(n,min=1,max=10)
da <- data.frame(x=x,y=y,z=z)
if (requireNamespace("sp")) {
   da.sp <- da
   sp::coordinates(da.sp) <- ~x+y
   sp::proj4string(da.sp) <- "+init=epsg:4326"
   print(spatial_bbox(da.sp))
   print(spatial_crs(da.sp))
}
if (requireNamespace("sf")) {
   da.sf <- sf::st_as_sf(da,coords=c("x","y"),crs=4326)
   print(spatial_bbox(da.sf))
   print(spatial_crs(da.sf))
}
}
\keyword{attribute}
