\name{legend_mtext}
\alias{legend_mtext}
\alias{.legend_mtext}
\title{
Write marginal text
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Functions write text outside of maps panels. \code{legend_mtext} (without prefix dot) is a wrapper for non-public \code{.legend_mtext} (with prefix dot).
}
\usage{
legend_mtext(...)

## non-public
.legend_mtext(text = "Annotation", cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names (using \link[base:regex]{regular expressions}) and classes.  Passed to non-public \code{.legend_mtext}, excepting argument \code{mtext}:
      \describe{
         \item{\code{mtext}}{Prefix for indirect use (e.g., in \code{\link[ursa:compose_legend]{compose_legend}}). Separated by a dot \code{"."}, e.g., \code{mtext.cex=0.85}.}
      }
   \tabular{lll}{
      \emph{Matched pattern} (\code{legend_colorbar}) \tab \emph{Argument} (\code{.legend_colorbar}) \tab \emph{Description}
      \cr \code{mtext} \tab \tab Prefix for indirect use (e.g., in \code{\link[ursa:compose_legend]{compose_legend}}). Separated by a dot \code{"."}, e.g., \code{mtext.cex=0.85}.
      \cr \code{text} \tab \code{text} \tab \emph{See below}.
      \cr \code{cex} \tab \code{cex} \tab \emph{See below}.
   }
   }
  \item{text}{
  Argument of class \code{character} or \code{expression} with matching name \code{"text"} (or without name) in \code{legend_mtext}. Text, which is displayed. If \code{character} then text is displayed in bold. Default is \code{"Title/subtitle"}.
  }
  \item{cex}{
  Argument of class \code{numeric} with matching name \code{"cex"} in \code{legend_mtext}. A numerical value giving the amount by which labels should be magnified relative to the default. Default is \code{1}.}
}
\details{

If \code{text} is \code{expression}, then possible way for formatting is:\preformatted{
text=as.expression(substitute(bold(italic("Omega powered by alpha is ",Omega^alpha))))}

}
\value{
Returned value of function \code{\link[graphics:mtext]{mtext}} from package \pkg{graphics}.
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
\code{\link[ursa:compose_legend]{compose_legend}}\cr
\code{\link[ursa:legend_colorbar]{legend_colorbar}}

}
\examples{
session_grid(NULL)
a <- ursa_dummy(1,min=-10,max=+30)
compose_open(legend=list("right","top","bottom","left"))
panel_new()
ct <- panel_raster(a)
legend_colorbar(ct)#,units=as.expression(substitute(bold(degree*C))))
legend_mtext("Characters are in bold")
legend_mtext(as.expression(substitute(italic(
            paste("Units can be interpreted as",~degree*C)))),cex=0.7)
legend_mtext(text=as.expression(substitute(italic(paste("Omega powered by alpha is"
                                                       ,~~Omega^alpha)))))
compose_close(execute=!FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
