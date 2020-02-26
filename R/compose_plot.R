'.compose_plot' <- function(img,annotation=NA,decor=!FALSE,scalebar=FALSE
                           ,verbose=NA,...) {
   NULL
}
'compose_plot' <- function(...) {
   fig <- getOption("ursaPngFigure")
   if (is.null(fig))
      return(invisible(NULL))
   if (fig>=getOption("ursaPngLayout")$image)
      return(invisible(NULL))
   ct <- compose_panel(...)
   if (is.null(ct))
      return(invisible(NULL))
   arglist <- list(...)
   myname <- names(arglist)
   if (is.null(myname))
      myname <- ""
   arglist <- arglist[nchar(myname)>0]
   do.call("compose_legend",c(list(ct),arglist))
   invisible(NULL)
}
