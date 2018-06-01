\name{seq}
\alias{seq.ursaRaster}
\alias{seq.ursaGrid}
\alias{ursa_seqx}
\alias{ursa_seqy}
\alias{ursa_seqc}
\alias{ursa_seqr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequence Generation for raster image and coordinate grid
}
\description{
Set of functions to generate regular sequences of bands, x-/y-cordinates and columns/rows.
}
\usage{
\method{seq}{ursaRaster}(...)

\method{seq}{ursaGrid}(...)

ursa_seqx(obj)
ursa_seqy(obj)
ursa_seqc(obj)
ursa_seqr(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}), position and classes.
      \describe{
         \item{\code{.*}}{First argument (position 1). Object of classes \code{ursaRaster}, \code{ursaGrid}.}
         \item{\code{.*}}{Second argument (position >1). One-character name. Valid values are in the list \code{c("z","x","y","c","r","lines","samples")}. \code{"c"} (\code{"samples"}) and \code{"r"} (\code{"lines"}) specify to generate cell sequence in the horizontal and vertical directions from bottom-left corner, whereas \code{"z"} specifies to generate sequence of bands. \code{x} and \code{"y"} return cell midpoints in spatial dimension.}
      }
   }
  \item{obj}{
Object of classes \code{ursaRaster}, \code{ursaGrid}. Missing \code{obj} is allowed; in this case the session grid is considered.
}
}
\details{
All ordinal sequences (\verb{axis} is \verb{\dQuote{c}}, \verb{\dQuote{r}}, \verb{\dQuote{z}}) start from \code{1L}.

\verb{axis=\dQuote{z}} is ignored in the function \code{seq} for \code{ursaGrid} object. The returned value is \code{1L}.

\code{seq(obj)} for \code{ursaRaster} objects is suitable for using in cycles across bands.
}
\value{
Functions \code{ursa_seqx} and \code{seq(obj, "x")} return x-coordinates of cell midpoints.
\cr
Functions \code{ursa_seqy} and \code{seq(obj, "y")} return y-coordinates of cell midpoints.
\cr
Functions \code{ursa_seqc}, \code{seq(obj, "samples")} and \code{seq(obj, "c")} return sequence of cells in horizontal direction.
\cr
Functions \code{ursa_seqr}, \code{seq(obj, "lines")} and \code{seq(obj, "r")} return sequence of cells in vertical direction.
\cr
Functions \code{seq(obj)} and \code{seq(obj, "z")} for \code{ursaRaster} object returns sequence of bands.
\cr
Function \code{seq(obj)} and \code{seq(obj, "z")} for \code{ursaGrid} object returns \code{1L}.
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
session_grid(regrid(mul=1/16))
print(session_grid())
a <- ursa_dummy(nband=5)
print(a)
print(seq(a))
print(seq(a,"c"))
print(seq(a,"x"))
print(ursa_seqx())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
