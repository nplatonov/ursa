\name{compose_open}
\alias{compose_open}
\title{
Start displaying
}
\description{
\code{compose_open} create plot layout and open PNG graphic device.
}
\usage{
compose_open(...)
}
%%~ mosaic <- .getPrm(list1,name="",default=NA,class="")
%%~ fileout <- .getPrm(list1,name="^fileout",default="")
%%~ dpi <- .getPrm(list1,name="^dpi",default=NA_real_)
%%~ pointsize <- .getPrm(list1,name="^pointsize",default=NA_real_)
%%~ scale <- .getPrm(list1,name="^scale",class="",default=NA_real_)
%%~ width <- .getPrm(list1,name="^width",class=list("integer","character"),default=NA_real_)
%%~ height <- .getPrm(list1,name="^height",default=NA_real_)
%%~ indent <- .getPrm(list1,name="^(space|offset|indent)",default=NA_real_)
%%~ colorbar <- .getPrm(list1,name="^colorbar(height)*",default=NA_real_)
%%~ box <- .getPrm(list1,name="^box",default=TRUE)
%%~ delafter <- .getPrm(list1,name="^(del|remove)after",default=NA)
%%~ wait <- .getPrm(list1,name="^wait",default=1)
%%~ device <- .getPrm(list1,name="^device",valid=c("cairo","windows"))
%%~ antialias <- .getPrm(list1,name="^antialias",valid=c("default","none"))
%%~ font <- .getPrm(list1,name="^font",valid=ifelse(device=="windows","sans","Tahoma"))
%%~ dev <- .getPrm(list1,name="^dev",default=FALSE)
%%~ verbose <- .getPrm(list1,name="^verb",default=FALSE)
\arguments{
   \item{\dots}{Set of argumetns, which are recognized via their names and classes:
      \describe{
         \item{\code{mosaic}}{Layout matrix or reference object to produce layout matrix. It is permitted to do not use name for this argument. Multiple types of argument are allowed: 1) object of class \code{ursaRaster}, 2) \code{\link[base:list]{list}} of \code{ursaRaster} objects (raster stack), 3) object of class \code{ursaLayout} from function \code{\link[ursa]{compose_design}}, 4) character keyword or 5) missing. Default is \code{NA}.}
         \item{\code{fileout}}{Character. Name for created PNG file. If absent (\code{""}) then temporal file is created and removed in \code{wait} seconds after opening in the associated external viewer. Default is absent (\code{""}).}
         \item{\code{dpi}}{Positive integer. Dots (or pixels) per inch (DPI). The nominal resolution of created PNG file. Default is \code{96L}. The same as \code{res} argument in the \code{\link[grDevices]{png}} function.}
         \item{\code{pointsize}}{Positive integer. The pointsize of plotted text as it is applied in the \code{\link[grDevices]{png}} function. Default is \code{NA}. If \code{pointsize=NA} then it is taken value \code{16L} multiplied to relative DPI (\code{dpi/96}). In the case of unspecified \code{scale} and \code{pointsize} the size of text is defined internally.}
         \item{\code{scale}}{Positive numeric or character. The scale factor applied to dimensions of original raster. Default is \code{NA}. If \code{scale} is unspecified (\code{scale=NA}), then \code{scale} is defined internally for \emph{intuitively} better fitting in HD, FHD displays (single-panelled layout 900x700). If \code{scale} is character (e.g., \code{"8000000"}, \code{"1:8000000"}) then dimensions of image panels are defined using "one centimeter of map is corresponded to 8000000 centimeres of site" rule.}
         \item{\code{width}}{Positive numeric or character. The desired width of each panel of multipanel layout. If \code{width} is numeric, then units are pixels. If \code{width} is character (e.g., \code{"12.5"}, \code{"12.5 cm"}, \code{"12.5cm"}) then units are centimeters in agreement with \code{dpi} argument. Default is \code{NA}. If \code{width} is unspecified (\code{width=NA}) then value \code{900} is used for single panelled layout.}
         \item{\code{height}}{Positive numeric or character. The desired height of each panel of multipanel layout. If \code{height} is numeric, then units are pixels. If \code{height} is character (e.g., \code{"9.6"}, \code{"9.6 cm"}, \code{"9.6cm"}) then units are centimeters in agreement with \code{dpi} argument. Default is \code{NA}. If \code{height} is unspecified (\code{height=NA}) then value \code{700} is used for single panelled layout.}
         \item{\code{colorbar} or \code{colorbarwidth}}{Positive numeric. Scale factor to increase (\code{colorbar>1}) or decrease (\code{colorbar<1}) width (the shortest dimension) of color bars (legends). Default value (\code{NA}) means 2.8\% of image panel width.}
         \item{\code{indent} or \code{space} or \code{offset}.}{Positive numeric. Scale factor to increase (\code{space>1}) or decrease (\code{space<1}) the white space between image panels and between image and color bar panels. Default value (\code{NA}) means 0.8\% of image panel width.}
         \item{\code{box}}{Logical. If \code{TRUE} then boundary box is plotted around image panels and color bar panels. It is a transparent rectangle with black border. Default is \code{TRUE}.}
         \item{\code{delafter}}{Logical. If \code{TRUE} then created PNG file will be deleted after viewing. Default is \code{FALSE} for specified file names and \code{TRUE} for unspecidied (temporal) file names. It is implemented as file removing after opening in the external PNG viewer.}
         \item{\code{wait}}{Positive numeric. Seconds between PNG file opening in the associated program and file removing. It make sense only if \code{delafter=TRUE}. Default is \code{1.0} (one second).}
         \item{\code{device} or \code{type}}{Character keyword, either \code{"cairo"} or \code{"windows"} for OS Windows, and either \code{"cairo"}, \code{"cairo-png"}, \code{Xlib} or \code{quartz} for other OSes. Should be plotting be done using cairographics or Windows GDI? The same as \code{type} argument in the \code{\link[grDevices]{png}} function.  Default is \code{"cairo"}.}
         \item{\code{antialias}}{Characted keyword, either \code{"none"} or \code{"default"}. Defines the effect on fonts. The same as \code{antialias} argument in the \code{\link[grDevices]{png}} function. Default is \code{"default"}.}
         \item{\code{font} or \code{family}}{A length-one character vector. Specifies the font family. The same as \code{family} argument in the \code{\link[grDevices]{png}} function. Default is \code{"sans"} for \code{device="windows"} and \code{"Tahoma"} for \code{device="cairo"}.}
         \item{\code{bg} or \code{background}}{Character. The background color in PNG file. Passed as argument \code{bg} to \code{\link[grDevices]{png}} function. Default is \code{"white"}}
         \item{\code{dev}}{Logical. If \code{TRUE} then this \strong{dev}eloper tool shows created layout without any the followed plot functions from this package are ignored. Default is \code{FALSE}}
         \item{\code{verbose}}{Logical. Shows additional output information in console. Default is \code{FALSE}.}
         \item{\code{...}}{Arguments, which can be passed to \code{\link[ursa]{compose_design}} function.}
      }
   }
}
\details{
Other usage of \code{compose_open(...,dev=TRUE)} is \preformatted{
   compose_open(...,dev=FALSE)
   compose_close()}

The reason to use \code{\link[ursa]{compose_design}} function  before \code{compose_open} is to reduce number of arguments in the case of complicated layout matrix and non-standard settings.

\code{compose_open} passes arguments to \code{\link[grDevices]{png}} function.

If character values are specified for arguments \code{width}, \code{height} or \code{scale}, then layout development is oriented to produce PNG file, which will be used as a paper copy. Character values for \code{width} and \code{height} are in centimeters. Character value \emph{\verb{V}} or \verb{1:}\emph{\verb{V}} of \code{scale} defines scale \verb{1/}\emph{\verb{V}}.

The Cairo device (\code{device="cairo"}) is more quick on MS Windows computers. However Windows GDI may produce less depth of colors (even 8 BPP) in the case of no font antialiasing. Usage of Windows GDI (\code{device="windows"}) is a way to produce illustations for scientific journals with strict requirements of mininal line width, font size, \emph{etc}.

The PNG layout reserves extra margins for captions of color bars. These margins are filled by white spaces. The cropping of layout applies to created PNG file using read-write functions of package \pkg{png}. Only white (\code{"white"}, \code{"#FFFFFF"}) or transparent (\code{"transparent"}) colors are regognized as white spaces. Therefore, specification of \code{bg!="white"} or \code{bg!="transparent"} breaks PNG image cropping.

It is noted that Cyrillics is supported on Windows GDI (\code{device="windows"}) and is not supported on Cairo (\code{device="cairo"}) types of PNG device on MS Windows platform.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Name of created PNG file.

If \code{dev=TRUE} then output on console is layout matrix.

The set of required parameters for plotting are kept until function \code{\link[ursa]{compose_close}} call via \code{\link[base]{options}}.
%%~    \item{ursaPngBackground}{Set \code{0L}. Each \code{\link[ursa][panel_new]}}
%%~    \item{ursaPngBar}{Width (shortest dimension )}
%%~    \item{ursaPngLegend}{NOT USED.}
   \item{ursaPngAuto}{For developers. Indicator of high-level functions for internal use (manual set; value is \code{TRUE}). Or, can be missed.}
   \item{ursaPngBox}{Argument \code{box}. If TRUE then \code{\link[graphics]{box}} is called for each panel of layout at the end of plotting.}
   \item{ursaPngDelafter}{Argument \code{delafter}. Applied in the function \code{\link[ursa]{compose_close}}.}
   \item{ursaPngDevice}{Argument \code{device}. Appied for effective plottting of rasters and checking the ability for final reducing color depth from 24 to 8 bpp.}
   \item{ursaPngDpi}{Argument \code{dpi}. Currently used for verbose only.}
   \item{ursaPngFamily}{Applied for text plotting in annotations and legends.}
   \item{ursaPngFigure}{Set \code{0L}. Specifies number of current panel in layout matrix. Used to detect term for applying \code{$ursaPngBox} option.}
   \item{ursaPngFileout}{Name of created PNG file.}
   \item{ursaPngLayout}{Layout matrix, the object of class \code{\link[ursa:compose_design]{ursaLayout}}.}
   \item{ursaPngPaperScale}{Numeric. Used for scalebar representation on the paper-based maps. If value \code{0}, then scalebar is display-based. If value is greater \code{0} then the scale is exact. If value is \code{-1} then the resonable rounding is used for scale displaying.}
   \item{ursaPngPlot}{The opposite to argument \code{dev}. If \code{$ursaPngPlot} is \code{FALSE} then any plotting functions of this package are ignored.}
   \item{ursaPngScale}{The actual value of argument \code{scale}, specified during function call or stated internally.}
   \item{ursaPngShadow}{Set \code{""}. Used for shadowing of the part of color bars in the case of semi-trasparent land or ocean filling mask in the \code{\link[ursa]{panel_coastline}} function.}
   \item{ursaPngSkipLegend}{Integer vector of non-negative length. Defines list of images panels, for which the color bars are not displayed.}
   \item{ursaPngWaitBeforeRemove}{Argument \code{wait}. Applied in the function \code{\link[ursa]{compose_close}} for temporal PNG file.}
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

b <- ursa_dummy(nband=2,min=0,max=50,mul=1/16)
p <- list(colorize(b[1:2],pal.rich=240,pal.rotate=0)
         ,colorize(sqrt(b[3:4]),pal.rich=-15,pal.rotate=0,stretch="equal"))

## exam #01
ursa:::.ursaOptions()
compose_open()
panel_new()
ursa:::.ursaOptions()
compose_close()
ursa:::.ursaOptions()
   
## exam #02
compose_open(height=400,legend=NULL,dev=TRUE)

## exam #03
compose_open(width=950,dpi=150,pointsize=16,legend=NULL,dev=TRUE)

## exam #04
a <- pixelsize()
compose_open(pointsize=8,dpi=150,scale="1:130000000")
compose_plot(colorize(a),scalebar=TRUE)
compose_close()

## exam #05
cl <- compose_design(layout=c(2,4)
                    ,legend=list(list("top","full"),list("bottom",1:3)))
compose_open(cl,dev=TRUE)

## exam #06
cl <- compose_design(p,layout=c(3,3),skip=c(4,6))
compose_open(cl,dev=TRUE)

\dontrun{display(p)}

## exam #07
cl <- compose_design(p,side=3)
compose_open(cl,dev=FALSE,bg="transparent");compose_close()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
