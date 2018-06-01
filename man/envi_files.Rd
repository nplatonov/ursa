\name{envi.files}
\alias{envi_exists}
\alias{envi_list}
\alias{envi_copy}
\alias{envi_rename}
\alias{envi_remove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ENVI File Manipulation 
}
\description{
Management of ENVI files similar to functions of OS file manager.
}
\usage{
envi_exists(pattern = ".+", path = ".", all.files = FALSE, full.names = TRUE,
           recursive = FALSE, ignore.case = TRUE, exact = FALSE)
envi_list(pattern = ".+", path = ".", all.files = FALSE, full.names = FALSE,
          recursive = FALSE, ignore.case = TRUE, exact = FALSE)
envi_remove(pattern = ".+", path = ".", all.files = FALSE, full.names = FALSE,
            recursive = FALSE, ignore.case = TRUE, verbose = FALSE)
envi_copy(src, dst, overwrite = TRUE)
envi_rename(src, dst, overwrite = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
Either filename (like \code{\link[base:basename]{basename}} function) or mask in format of regular expressions or full path name.
}
  \item{path}{
Either path name (like \code{\link[base:basename]{dirname}} function) or ignored if \code{pattern} describes full path.
%%     ~~Describe \code{path} here~~
}
  \item{all.files}{
A logical value. If FALSE, only the names of visible files are returned. If TRUE, all file names will be returned. Similar to \code{all.files} argument in \code{\link[base]{list.files}} function
}
  \item{full.names}{
A logical value. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned. Similar to \code{full.names} argument in \code{\link[base]{list.files}} function
}
  \item{recursive}{
Logical. Should the listing recurse into directories? Similar to \code{recursive} argument in \code{\link[base]{list.files}} function
}
  \item{ignore.case}{
Logical. Should pattern-matching be case-insensitive? Similar to \code{ignore.case} argument in \code{\link[base]{list.files}} function
}
  \item{exact}{
Logical. Attempt to cancel regular expressions.
}
  \item{verbose}{
Logical. \code{TRUE} provides some additional information on console.
}
  \item{src}{
Strings of length 1 or more. Name or directory name or path of source ENVI files.
}
  \item{dst}{
Strings of length 1 or more. Name or directory name path of destination ENVI files. Length is assuming to be equal to length of \code{src}
}
  \item{overwrite}{
Logical. \code{TRUE} overwrites destinations ENVI files. \code{FALSE} does nothing if destinaton ENVI file exists.
}
}
\details{
Functions do not view content of any files. The major identifier of ENVI files in file system is ENVI header (*.hdr) file. Binary file is searching along 1) original *.envi, *.bin, *.img, *.dat extensions, 2) externally packing *.gz. *.bz2, *.xz extensions, or 3) packed by this package *.envigz, *.bingz extensions. Functions \code{envi_copy()} and \code{envi_rename()} keeps original extension of ENVI binary file; use \code{\link[base:files]{file.rename}} to rename ENVI binary file.
}
\value{
\code{envi_exists()} returns integer number of found ENVI files.

\code{envi_list()} returns character vector of found ENVI files.

\code{envi_remove()} returns character vector of deleted ENVI files.

\code{envi_copy()} returns 0L.

\code{envi_rename()} returns value of \code{\link[base:files]{file.rename}}, which is applied to objects in file system.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }

\author{Nikita Platonov \email{platonov@sevin.ru}}

%%~ \note{
%%~ %%  ~~further notes~~
%%~ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
a1 <- create_envi("tmp1.envi")
a2 <- create_envi("tmp2.")
close(a1,a2)
envi_list()
envi_copy("tmp1","tmp3")
envi_copy("tmp2","tmp4")
envi_list()
envi_rename("tmp3","tmp5")
envi_list()
envi_exists("nofilewithsuchname")
envi_exists("tmp[34]")
envi_remove(".+")
envi_list()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
