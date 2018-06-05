\name{compose_design}
\alias{compose_design}
\alias{ursaLayout}
\title{
Organize multi-panel layout with images and color bars.
}
\description{
\code{compose_design} prepares scheme for layout of images and color bars.
}
\usage{
compose_design(\dots)

%%~ ## for internal use
%%~ .compose_design(obj = NULL, layout = NA, byrow = TRUE, skip = NULL, legend = NA,
%%~                side = NA, ratio = (16 + 1)/(9 + 1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Set of argumetns, which are recognized via their names and classes:
   \describe{
      \item{\code{obj}}{Object of class \code{ursaRaster} or list of objects of class \code{ursaRaster} or \code{NULL}. Default is \code{NULL}. Used to detect panel layout and coordinate reference system.}
      \item{\code{layout}}{Integer of length 2 or \code{NA}. Layout matrix has dimensions\code{c(nr, nc)}, where \code{nr} is number of rows, and \code{nc} is number of columns. If \code{layout=NA} then layout matrix is recognized internally using number of bands of \code{obj} and argument \code{ratio}. If \code{layout=NA} and \code{obj=NULL} then matrix \code{c(1,1)} is used.}
      \item{\code{byrow}}{Logical. The order of filling of layout matrix. Default is \code{TRUE}. If \code{byrow=TRUE} then matrix is filled by rows (from top row, consequently from left element to right element, then next row). If \code{byrow=FALSE} then matrix is filled by columns.}
      \item{\code{skip}}{Positive integer of variable length. Default in \code{NULL} (length is zero). Indices of panels in the layout matrix, which are not used.}
      \item{\code{legend}}{The descripition of rules how color bars (legends) or panel captions are located in the layout. It is the list of embedded lists of two elements, which describe the color bars position in the layout. of Default is \code{NA}, it means using of internal rules. If \code{legend=NULL} then no plotting of color bars. If \code{legend} is positive integer in the range \code{1L:4L}, then sinlge color bar is used and legend's side is corresponded to margins of \R graphic system.}
      \item{\code{side}}{Positive integer \code{1L}, \code{2L}, \code{3L}, or \code{4L}. Default is \code{NA}. Simplification of color bar position in the case that single color bar is used. The value is corresponded to margins of \R graphic system. The synonym of integer value of \code{legend}.}
      \item{\code{ratio}}{Positive numeric. The desired ratio of layout sides (width per height). If \code{layout=NA} then the dimensions of layout matrix are defined internally to get the given ratio of layout's width per height. The default is \code{(16+1)/(9+1)} in the assumtion of optimal filling on the usual 16:9 screens.}
   }
}
%~   \item{obj}{
%~ %%     ~~Describe \code{obj} here~~
%~ }
%~   \item{layout}{
%~ %%     ~~Describe \code{layout} here~~
%~ }
%~   \item{byrow}{
%~ %%     ~~Describe \code{byrow} here~~
%~ }
%~   \item{skip}{
%~ %%     ~~Describe \code{skip} here~~
%~ }
%~   \item{legend}{
%~ %%     ~~Describe \code{legend} here~~
%~ }
%~   \item{side}{
%~ %%     ~~Describe \code{side} here~~
%~ }
%~   \item{ratio}{
%~ %%     ~~Describe \code{ratio} here~~
%~ }
%~   \item{\dots}{
%~ %%     ~~Describe \code{\dots} here~~
%~ }
}
\details{
Function \code{compose_design} extracts and validates required arguments from a list of parameters (three-dots construct) and passes them to internal function \code{.compose_design}.

Argument \code{legend} is a \code{\link[base]{list}} or coerced to a \code{\link[base]{list}}. The length of this list is equal to number of color bars; each item describes certain color bar. This desctiption is a list again with two elements, which desribes the position of color bar in relation to main panels of images.

If argument \code{legend} is in interval \code{1L:4L} then it is intepreted as argument \code{side} in functions \code{\link[graphics]{axis}}, \code{\link[graphics]{mtext}}.  Argument \code{side} in function \code{compose_design} plays the same role. It is introduced for consistency with \R graphic system.

In the one of example below (See \cite{Examples} section) the layout with dimension of two rows by three columns is considered (\code{layout=c(2,3)}). The dimension of resulting layout matrix is \code{c(7,9)}, where 7=\strong{2}*2+3, and 9=\strong{3}*2+3.
\preformatted{
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
[1,]    0    0    0    0    0    0    0    0    0
[2,]    0    0    0    0    0    0    0    0    0
[3,]    0    0    1    0    2    0    3    0    0
[4,]    0    0    0    0    0    0    0    0    0
[5,]    0    0    4    0    5    0    6    0    0
[6,]    0    0    0    0    0    0    0    0    0
[7,]    0    0    0    0    0    0    0    0    0
}
The complicated color bar structure is specified via \R's \code{\link[base]{list}} function: \preformatted{
>   leg <- list("7"=list(row=1,col=0),"8"=list(2,"left")
+              ,"9"=list("full","right"),"10"=list("top","full")
+              ,"11"=list(99,1:2),"12"=list("bottom",3))
> str(leg)
 $ 7 :List of 2
  ..$ row: num 1
  ..$ col: num 0
 $ 8 :List of 2
  ..$ : num 2
  ..$ : chr "left"
 $ 9 :List of 2
  ..$ : chr "full"
  ..$ : chr "right"
 $ 10:List of 2
  ..$ : chr "top"
  ..$ : chr "full"
 $ 11:List of 2
  ..$ : num 99
  ..$ : int [1:2] 1 2
 $ 12:List of 2
  ..$ : chr "bottom"
  ..$ : num 3
}
Here, six color bars are specified. It is a list of six lists (sub-lists). First item of sub-list is row number, and the second one is column number. Integers can be replaces by character keywords.

For row-position, \code{"top"} means \code{0L} (less than first row), \code{"bottom"} means large integer value (greater than last row, currently, \code{99L}), \code{"first"} means \code{1L}, \code{"last"} means number of last row (\code{2L} in this example), \code{"full"} means whole range from first to last rows (\code{1L:2L} in this example). Values \code{"top"} and \code{"bottom"} are used for horizontal color bars (last three sub-lists), and the rest for vertical color bars (first three sub-lists).

For column-position, \code{"left"} means \code{0L} (less then first column), \code{"bottom"} means large integer value (greater than last column, currently, \code{99L}), \code{"first"} means first column (\code{1L}), \code{"last"} means last column (\code{3L} in this example), \code{"full"} means whole range from first to last columns ((\code{1L:3L} in this example)). Values \code{"left"} and \code{"right"} are used for vertical color bars, and the rest are for horizontal ones.

The resulting layout is a sparse matrix with zero values for each even row and each column. These zeros plays role of white space between panels in the plotted layout. In our example, values \code{1L:6L} are corresponded to six map panels, and values \code{7L:12L} are corresponded to six narrow panels of color bars (legends). \preformatted{
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
[1,]    0    0   10    0   10    0   10    0    0
[2,]    0    0    0    0    0    0    0    0    0
[3,]    7    0    1    0    2    0    3    0    9
[4,]    0    0    0    0    0    0    0    0    0
[5,]    8    0    4    0    5    0    6    0    9
[6,]    0    0    0    0    0    0    0    0    0
[7,]    0    0   11    0   11    0   12    0    0
}
\figure{composedesign.png}{Caption}
}
\value{
It is a list of class \code{ursaLayout}.
\item{layout}{Integer matrix with dimension \code{c(2*nr+3, 2*nc+3)}, where \code{nr} and \code{nc} are number of rows and colunms of the layout matrix. The layout matrix of image panels is surrounded by colorbar panels. The orignial layout matrix is expanded by adding zero columns and rows. In the new matrix each even column has zero values, and each even row has zero values.}
\item{image}{Nonnegative integer. Number of panels with images.}
\item{legend}{Nonnegative integer. Number of panels with color bars (legends).}
The returned value is passed to function \code{\link[ursa]{compose_open}} and further is kept in the \code{\link[base]{options}} until calling of \code{\link[ursa]{compose_close}}.
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
a <- ursa_dummy(nband=5,min=1,max=200,mul=1/8)
b <- list(colorize(a[1:3],pal.rich=240,pal.rotate=0)
         ,colorize(sqrt(a[4:5]),pal.rich=-15,pal.rotate=0,stretch="equal"))

cl1 <- compose_design(layout=c(2,3),byrow=TRUE,legend=NULL)
print(cl1)
compose_open(cl1)
compose_close()

cl2 <- compose_design(layout=c(2,3),byrow=FALSE,legend="left")
print(cl2$layout)
compose_open(cl2)
compose_close()

cl3 <- compose_design(a,side=2)
print(cl3)
compose_open(cl3)
compose_close()

cl4 <- compose_design(b)
print(cl4)
\donttest{ ## to avoid over-time during example check -- begin
   compose_open(cl4)
   compose_plot(b,decor=FALSE,las=2)
   compose_close("nocrop")
} ## to avoid over-time during example check -- end

cl5 <- compose_design(b,byrow=FALSE,skip=3
                     ,legend=list(list("full","left"),list(1:2,"right")))
compose_open(cl5)
compose_plot(b,decor=FALSE)
compose_close("nocrop")

leg <- list(list(1,0),list(2,"left")
           ,list("full","right"),list("top","full")
           ,list(99,1:2),list("bottom",3))
str(leg)
cl6 <- compose_design(layout=c(2,3),skip=NA,legend=leg)
print(cl6)
compose_open(cl6,scale=3,pointsize=16)
compose_close("nocrop")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
