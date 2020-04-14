\name{legend_align}
\alias{legend_align}
\title{
Align caption position for legend
}
\description{
When multiple panels on the same axis, the different order of values or different units of values may provoke different shifting of values and caption from panels. \code{legend_align} repairs it by the taking names of classes of the required rasters. The function output is for argument \code{aling} of \code{\link[ursa:legend_colorbar]{legend_colorbar}}.
}
\usage{
legend_align(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaColorTable}, or object of class \code{ursaRaster}, or list of \code{ursaColorTable} or \code{ursaRaster} objects.
}
}
\details{
The function is defined as:\preformatted{
   c(unlist(sapply(obj,function(x) names(ursa_colortable(x)))))}

%%  ~~ If necessary, more details than the description above ~~
}
\value{
Character vector.
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
\code{\link[ursa:legend_colorbar]{legend_colorbar}}
}
\examples{
session_grid(NULL)
a <- ursa_dummy(5,mul=1/4,min=-150,max=200)
a[1] <- a[1]*100
a[2] <- -a[2]*10
a[3] <- a[3]/10
a[4] <- a[4]/1000
b <- lapply(a,colorize)
la.top <- legend_align(b[c(1,2)])
la.left <- legend_align(c(b[[1]],b[[3]]))
la.bottom <- legend_align(b[c(3,4)])
la.right <- legend_align(b[c(2,4)])
leg <- vector("list",12)
leg[[1]] <- list("top",2)
leg[[2]] <- list("top",3)
leg[[3]] <- list("bottom",1)
leg[[4]] <- list("bottom",2)
leg[[5]] <- list(2,"left")
leg[[6]] <- list(1,"right")
leg[[7]] <- list(3,"left")
leg[[8]] <- list(2,"right")
leg[[9]] <- list("top",1)
leg[[10]] <- list("bottom",3)
leg[[11]] <- list(1,"left")
leg[[12]] <- list(3,"right")
cl <- compose_design(layout=c(3,3),legend=leg,byrow=TRUE,skip=5)
print(cl)
compose_open(cl)
ct <- compose_panel(b[c(5,1,2,1,4,3,4,5)],decor=FALSE)
L <- 2
Tr <- 2
legend_colorbar(b[1],trim=Tr,las=L,align=la.top,units="top aligned --->")
legend_colorbar(b[2],trim=Tr,las=L,align=la.top,units="<--- top aligned")
legend_colorbar(b[3],trim=Tr,las=L,align=la.bottom,units="bottom aligned --->")
legend_colorbar(b[4],trim=Tr,las=L,align=la.bottom,units="<--- bottom aligned")
legend_colorbar(b[1],trim=Tr,las=L,align=la.left,units="<--- left aligned")
legend_colorbar(b[2],trim=Tr,las=L,align=la.right,units="<--- right aligned")
legend_colorbar(b[3],trim=Tr,las=L,align=la.left,units="left aligned --->")
legend_colorbar(b[4],trim=Tr,las=L,align=la.right,units="right aligned --->")
legend_colorbar(b[5],trim=Tr,las=L,units=" *** not aligned ***")
legend_colorbar(b[5],trim=Tr,las=L,units=" *** not aligned ***")
legend_colorbar(b[5],trim=Tr,las=L,units=" *** not aligned ***")
legend_colorbar(b[5],trim=Tr,las=L,units=" *** not aligned ***")
compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
