\name{compose_close}
\alias{compose_close}
\alias{.compose_close}
\title{
Finish plotting
}
\description{
Function \code{compose_close} does followed tasks: 1) completes all unfinsished actions before shutting down graphical device, 2) cuts extra margins, and 3) opens resulted PNG file in the associated viewer.
}
\usage{
compose_close(...)

## non-public
.compose_close(kind = c("crop2", "crop", "nocrop"),
               border = 5, bpp = 0, execute = TRUE, verbose = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names and classes, and then passed to \code{.compose_close}:
      \tabular{ll}{
      \strong{Pattern} (\code{compose_close}) \tab \strong{Argument} (\code{.compose_close})
      \cr \code{(^$|crop|kind)} \tab \code{kind}
      \cr \code{(border|frame)} \tab \code{border}
      \cr \code{bpp} \tab \code{bpp}
      \cr \code{(execute|view|open)} \tab \code{execute}
      \cr \code{verb(ose)*} \tab \code{verbose}
      }
   }
   \item{kind}{Character keyword for cutting of excess white spaces. If \code{kind="nocrop"} then there is no cut. If \code{kind="crop"} then only outer margins are cutted. If \code{kind="crop2"} then all outer margins and inner white spaces (\emph{e.g.}, between color bar panel and text caption) are cutted.}
   \item{border}{Non-negative integer. Number of pixels for margins, which are not cropped. Default is \code{5L}.}
   \item{bpp}{Integer. Bits per pixel for output PNG file. Valid values are \code{0L}, \code{8L}, \code{24L}. If \code{bpp=0L}, then \code{8} bpp is used for \code{"windows"} type of PNG device, and \code{24} bpp is used for  \code{"cairo"} type of PNG device. The type of device is specified in \code{\link[ursa]{compose_open}} function.}
   \item{execute}{Logical. Should created PNG file be opened in the associated external program for viewing graphical files? Default is \code{TRUE}.}
   \item{verbose}{Logical. Value \code{TRUE} provides some additional information on console. Default is \code{FALSE}.}
}
\details{
The cut manipulations (\code{crop="crop"} or \code{crop="crop2"}) are implemented using \code{\link[png]{readPNG}} and \code{\link[png]{writePNG}} functions of package \pkg{png}. These fuctions have limitations in the memory allocation.

Function \code{compose_close} clears all internal graphical options, specified during \code{\link[ursa]{compose_open}} executing.

Some parameters are specified in \code{\link[ursa]{compose_open}}: wether output PNG file is deleted after opening (logical \code{delafter}), or what is the time of waiting for file opening and next removing (numerical \code{wait} in seconds).
}
\value{
Function returns \code{NULL} value.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
%%~ \note{
%%~ }

\section{Warning}{
%%~ The function \code{\link[base]{shell}} is used with argument \code{wait=FALSE} to provide viewing of created PNG file. At least, on Windows platform, it is noted, that such calling has a conflict with open \code{\link[base]{connections}}. It is recommended strongly to close all connetions (including \code{\link[ursa:classRaster_close]{close}} for ENVI files) before finishing plotting. If compression is used for ENVI binary files, then temporal file of unpacked ENVI binary file is not removed and is kept on disk.

%%~ The warning \code{"Detected opened connection(s): ..."} is appeared with list of basenames of opened connection, having suffix \code{\sQuote{.unpacked~}}. After warning the \code{"Script is suspended until close of image viewer."} message is appeared. When you close associated program, the script will continue.
Currenty, \code{execute=TRUE} is implemented for Windows platform only using construction \verb{R CMD open \emph{fileout}}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
a <- ursa_dummy(nband=6,min=0,max=255,mul=1/4)

## exam 1
compose_open()
compose_close()

## exam 2
compose_open(a)
compose_close()

## exam 3
compose_open("rgb",fileout="tmp1")
compose_plot(a[1:3])
compose_close()
Sys.sleep(3)
a <- dir(pattern="tmp1.png")
print(a)
file.remove(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
