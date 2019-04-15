\name{ursaProgressBar}
\alias{ursaProgressBar}
\alias{setUrsaProgressBar}
\alias{close.ursaProgressBar}
\title{
Progress bar
}
\description{
Informative progress bars with dispaying elapsed and remained time.
}
\usage{
ursaProgressBar(kind = c("tk", "txt"),title = .argv0(),
                label = "", min = 0, max = 1, initial = min, width = NA,
                style = 1, silent = FALSE)

setUrsaProgressBar(pb, value, title = NULL, label = NULL)
%%~ \method{close}{ursaProgressBar}(con, ...)
\method{close}{ursaProgressBar}(con, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kind}{
Character. Type or progress bar. Valid values are \code{"tk"} to display progress bar in tcl/tk window and \code{"txt"} to display progress bar in terminal mode. Default is \code{"tk"}.
}
\item{style}{
   See description for the same argument in \code{\link[utils:txtProgressBar]{txtProgressBar}}.
}
\item{width}{
   See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}}.
}
\item{title, label, min, max, initial, value, pb, \dots}{
   See description for the same arguments in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
}
\item{silent}{
   Logical. If \code{TRUE} then progress bar will not appear; it can be useful for conditional scripting. Default is \code{FALSE}.
}
%%~ \item{label}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{min}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{max}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{initial}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{width}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{pb}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{value}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
%%~ \item{\dots}{
%%~    See description for the same argument in \code{\link[tcltk:tkProgressBar]{tkProgressBar}} and \code{\link[utils:txtProgressBar]{txtProgressBar}}.
%%~ }
}
\details{
Wrapper to one of
\code{\link[utils:txtProgressBar]{txtProgressBar}},
%%~ \code{\link[utils:winProgressBar]{winProgressBar}},
\code{\link[tcltk:tkProgressBar]{tkProgressBar}}
. 

%%~ Method \code{close(con,...)} depends on class of progress bar.

Visualization of progress bar is updates each 0.5 seconds, it is effective for multiple short-term iterations.

Progress bars should be closed by calling of appropriate method of generic function \code{\link[base:connections]{close}} depending of class of reference progress bar.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
\code{ursaProgressBar} returns object of reference progress bar.
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Function name in style \emph{camelCase} for consistence with other progress bar functions in \R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[utils:txtProgressBar]{txtProgressBar}}
%%~ \code{\link[utils:winProgressBar]{winProgressBar}}
\code{\link[tcltk:tkProgressBar]{tkProgressBar}}

}
\examples{
session_grid(NULL)
n1 <- 3
n2 <- 83
p <- 0.0011
#require(tcltk)
pb <- ursaProgressBar(min=0,max=n1,title="first")
for (i in seq(n1)) {
   pb2 <- ursaProgressBar(min=0,max=n2,title="second")
   for (i in seq(n2)) {
      setUrsaProgressBar(pb2)
      Sys.sleep(p)
   }
   close(pb2)
   setUrsaProgressBar(pb)
}
close(pb)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
